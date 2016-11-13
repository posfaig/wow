##########################################################################
###
### XGBoost Model with Fuzzy Clustering
###
##########################################################################


get_model_xgboost_with_kmeans <- function(params = list()){
    model_name <- "xgboost_with_kmeans_clustering"
    desc <- "xgboost with kmeans clustering"
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

        sapply(unique(train_data_preprocessed$guild_cluster), function(cluster_value){

            # get training set for cluster
            current_train_data <- train_data_preprocessed[train_data_preprocessed$guild_cluster == cluster_value,]
            current_targets <- targets[train_data_preprocessed$guild_cluster == cluster_value]

            # exclude cluster columns
            current_train_data$guild_cluster <- NULL

            models[[as.character(cluster_value)]] <<- xgboost(
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
        test_data_preprocessed$id <- 1:(nrow(test_data_preprocessed))

        cluster_predictions <- data.frame()
        sapply(unique(test_data_preprocessed$guild_cluster), function(cluster_value){
            current_test_data <- test_data_preprocessed[test_data_preprocessed$guild_cluster == cluster_value, ]
            current_model <- models[[as.character(cluster_value)]]

            # Exclude cluster columns
            current_test_data_ids <- current_test_data$id
            current_test_data$guild_cluster <- NULL
            current_test_data$id <- NULL

            if (nrow(cluster_predictions) == 0){
                cluster_predictions <<- data.frame(
                    id = current_test_data_ids,
                    prediction = predict(current_model, as.matrix(current_test_data)))
            } else {
                cluster_predictions <<- rbind(
                    cluster_predictions,
                    data.frame(
                        id = current_test_data_ids,
                        prediction = predict(current_model, as.matrix(current_test_data)))
                )
            }
            c()
        })

        predictions_df <- left_join(test_data_preprocessed %>% select(id),
                                    cluster_predictions,
                                    by = "id")
        predictions_df$prediction
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

