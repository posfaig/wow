#######################################################
###
### Function for building and evaluating xgboost models
###
#######################################################

library(dplyr)
library(xgboost)

get_model_xgboost <- function(params = list()){
    model_name <- "xgboost"
    desc <- "xgboost"
    model <- c()

    preprocess <- function(data, is_train){
        # remove not predictor columns
        data$avatar <- NULL
        data$guild <- NULL
        data$pred_date <- NULL
        data$testset_end_date <- NULL
        data$label <- NULL

        # convert factors to numeric
        data <- convert_factors_to_numeric(data)

        if (is_train){
            # save names of final predictors
            params[["final_predictor_column_names"]] <<- names(data)
        } else {
            # keep only predictors that were used during training
            data <- data %>% select_(.dots = intersect(params$final_predictor_column_names, names(data)))
        }

        # convert integers to double for xgboost
        sapply(names(data), function(col_name){
            data[[col_name]] <<- as.numeric(data[[col_name]])
        })

        data
    }

    build <- function(train_data) {
        target_column <- "label"
        targets <- train_data[[target_column]]
        targets <- as.numeric(targets)  # numeric and >0 target variable as the frbs package requires
        train_data[,target_column] <- NULL

        train_data_preprocessed <- preprocess(train_data, TRUE)

        set.seed(0)
        model <<- xgboost(
            as.matrix(train_data_preprocessed),
            targets,
            objective = "binary:logistic",
            nrounds = 100,
            verbose = 0,
            eval_metric = "auc")
        model
    }

    predict_ <- function(test_data){
        test_data <- preprocess(test_data, FALSE)
        predictions <- predict(model, as.matrix(test_data))
        predictions
    }

    list(
        build = build,
        preprocess = preprocess,
        predict = predict_,
        model_name = function(){model_name},
        desc = function(){desc},
        params = function(){params},
        model = function(){model}
    )

}

