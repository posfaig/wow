##########################################################################
###
### XGBoost Model with Fuzzy Clustering
###
##########################################################################


get_model_xgboost_with_fclus <- function(membership_threshold = 0.2, params = list()){
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


        sapply(clust_mem_cols, function(cluster_name){

            # get training set for cluster
            current_train_data <- train_data_preprocessed[train_data_preprocessed[[cluster_name]] >= membership_threshold,]
            current_targets <- targets[train_data_preprocessed[[cluster_name]] >= membership_threshold]

            # exclude cluster columns
            current_train_data <- current_train_data[, !(names(current_train_data) %in% clust_mem_cols)]

            models[[cluster_name]] <<- xgboost(
                as.matrix(current_train_data),
                current_targets,
                nrounds = 50,
                objective = "binary:logistic",
                max.depth = 50)

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
            predict(models[[cluster_name]], as.matrix(test_data_no_cluster))
        })

        cluster_memberships <- (test_data %>% select_(.dots = clust_mem_cols))
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

