##########################################################################
###
### XGBoost Model with Fuzzy Clustering
###
##########################################################################


get_model_xgboost_with_fclus <- function(params = list()){
    model_name <- "xgboost_with_fuzzy_clustering"
    desc <- "xgboost with fuzzy clustering"
    models <- list()
    params <- list()

    preprocess <- function(data, is_train){
        # filter not predictor columns
        data$avatar <- NULL
        data$guild <- NULL
        data$pred_date <- NULL
        data$testset_end_date <- NULL
        data$label <- NULL

        # convert factors to numeric
        data <- convert_factors_to_numeric(data)

        # scale data
        if (is_train){


            params[["final_predictor_column_names"]] <<- names(data)
            #tmp <- scale(data[,names(data)])
            #data[,names(data)] <- tmp
            #params[["scale_centers"]] <<- attr(tmp, "scaled:center")
            #params[["scale_deviations"]] <<- attr(tmp, "scaled:scale")


            # Set of final columns
            params[["final_predictor_columns"]] <<- names(data)

        } else {

            data <- data[, intersect(params$final_predictor_column_names, names(data))]

            #data[,names(data)] <- scale(
            #	data[,names(data)],
            #	center=params$scale_centers,
            #	scale=params$scale_deviations)

            # Keep only the final columns of the training set
            data <- data %>% select_(.dots = params[["final_predictor_columns"]])
        }

        # If predictors were specified keep only those together with the cluster columns
        clust_mem_cols <- names(data)[startsWith(names(data), "Clus")]
        if (!is.null(params[["predictors"]])){
            data <- data %>% select_(.dots = c(params[["predictors"]], clust_mem_cols))
        }

        # convert integers to double for xgboost
        sapply(names(data), function(col_name){
            data[[col_name]] <<- as.numeric(data[[col_name]])
        })

        data
    }

    build <- function(train_data) {
        print(paste("Building model:", model_name))

        target_column <- "label"
        targets <- train_data[[target_column]]
        targets <- as.numeric(targets)  # numeric and >0 target variable as the frbs package requires
        train_data[,target_column] <- NULL

        train_data_preprocessed <- preprocess(train_data, TRUE)

        clust_mem_cols <- names(train_data_preprocessed)[startsWith(names(train_data_preprocessed), "Clus")]
        print(paste("Number of clusters:", length(clust_mem_cols)))
        membership_threshold <- 1.0 / length(clust_mem_cols)

        sapply(clust_mem_cols, function(cluster_name){

            # Exclude cluster columns
            current_train_data <- train_data_preprocessed[, !(names(train_data_preprocessed) %in% clust_mem_cols)]

            # Build model with weighted instances
            dat <- xgb.DMatrix(
                as.matrix(current_train_data),
                label = targets,
                weight = (train_data_preprocessed[[cluster_name]]))
            models[[cluster_name]] <<- xgb.train(list(),
                             data = dat,
                             nrounds = 100,
                             objective = "binary:logistic")
        })

        models
    }

    predict_ <- function(test_data){
        print("predicting...")

        test_data_preprocessed <- preprocess(test_data, FALSE)

        # Exclude cluster columns
        clust_mem_cols <- names(test_data_preprocessed)[startsWith(names(test_data_preprocessed), "Clus")]
        test_data_no_cluster <- test_data_preprocessed[, !(names(test_data_preprocessed) %in% clust_mem_cols)]

        cluster_predictions <- sapply(clust_mem_cols, function(cluster_name){
            current_model <- models[[cluster_name]]
            if (is.numeric(current_model)){
                result <- rep(current_model, nrow(test_data_no_cluster))
            } else {
                result <- predict(current_model, as.matrix(test_data_no_cluster))
            }
            result
        })

        cluster_memberships <- test_data %>% select_(.dots = clust_mem_cols)
        predictions <- cluster_predictions * cluster_memberships
        predictions <- rowSums(predictions) / rowSums(cluster_memberships)
    }

    list(
        build = build,
        predict = predict_,
        model_name = function(){model_name},
        desc = function(){desc},
        params = function(){params},
        models = function(){models}
    )

}



do_cv_with_fuzzy_clustering <- function(train_data, current_model_constructor, num_clusters, fuzziness, features_to_keep, ..., k = 8){
    set.seed(0)

    # Create indecies for rolling forecasting origin
    min_date_index <- length(unique(train_data$pred_date)) - k + 1
    train_index_list <- lapply(min_date_index:length(sort(unique(train_data$pred_date))), function(x){which(as.numeric(factor(train_data$pred_date)) < x)})
    test_index_list <- lapply(min_date_index:length(sort(unique(train_data$pred_date))), function(x){which(as.numeric(factor(train_data$pred_date)) == x)})

    predictions <- data.frame()
    for (i in 1:length(train_index_list)){
        # get train and test sets of current fold
        current_train_data <- train_data[train_index_list[[i]],]
        current_test_data <- train_data[test_index_list[[i]],]

        ### Fuzzy-Clustering of Guilds
        fuzzy_clusters <- fuzzy_clustering_of_guilds(
            rbind(current_train_data, current_test_data),
            num_clusters,
            fuzziness)
        data_with_fuzzy_clusters <- fuzzy_clusters$data

        # Keep only relevant features
        clust_mem_cols <- names(data_with_fuzzy_clusters)[startsWith(names(data_with_fuzzy_clusters), "Clus")]
        features_to_keep <- unique(c(clust_mem_cols, features_to_keep))
        data_with_fuzzy_clusters <- data_with_fuzzy_clusters[,(names(data_with_fuzzy_clusters) %in% features_to_keep)]

        # Re-split the data after clutering
        current_train_data <- data_with_fuzzy_clusters[1:nrow(current_train_data),]
        current_test_data <- data_with_fuzzy_clusters[-1 * (1:nrow(current_train_data)),]

        # Learn model
        current_model <- current_model_constructor()
        current_model$build(current_train_data, ...)

        # Predict with the current model
        current_predictions <- current_model$predict(current_test_data)


        if(nrow(predictions) == 0){
            predictions <- data.frame(avatar = current_test_data$avatar,
                                      pred_date = current_test_data$pred_date,
                                      label = current_test_data$label,
                                      prediction = current_predictions)
        } else {
            predictions <- rbind(predictions,
                                 data.frame(avatar = current_test_data$avatar,
                                            pred_date = current_test_data$pred_date,
                                            label = current_test_data$label,
                                            prediction = current_predictions))
        }
    }
    predictions
}


