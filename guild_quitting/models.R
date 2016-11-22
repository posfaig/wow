######################################################
###
### Functio for Building and Evaluating XGBoost Model
###
######################################################

library(dplyr)
library(xgboost)

get_model_xgboost <- function(params = list()){
    model_name <- "xgboost"
    desc <- "xgboost"
    model <- c()

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




        } else {

            data <- data[, intersect(params$final_predictor_column_names, names(data))]

            #data[,names(data)] <- scale(
            #	data[,names(data)],
            #	center=params$scale_centers,
            #	scale=params$scale_deviations)


        }

        # convert integers to double for xgboost
        sapply(names(data), function(col_name){
            data[[col_name]] <<- as.numeric(data[[col_name]])
        })

        data
    }

    build <- function(train_data, early_stop_round = 4) {
        print(paste("Building model:", model_name))

        target_column <- "label"
        targets <- train_data[[target_column]]
        targets <- as.numeric(targets)  # numeric and >0 target variable as the frbs package requires
        train_data[,target_column] <- NULL

        train_data_preprocessed <- preprocess(train_data, TRUE)

        print(paste("early_stop_round =", early_stop_round))
        print(paste("number of train matrix columns =", ncol(train_data_preprocessed)))

        set.seed(0)
        model <<- xgboost(
            as.matrix(train_data_preprocessed),
            targets,
            objective = "binary:logistic",
            nrounds = 100,
            eval_metric = "auc")

        # dat <- xgb.DMatrix(as.matrix(train_data_preprocessed), label = targets)
        # model <<- xgb.train(list(eta = 0.3,
        #                       max_depth = 2,
        #                       gamma = 0,
        #                       colsample_bytree = 0.8,
        #                       min_child_weight = 1,
        #                       subsample = 1),
        #                  data = dat,
        #                  nrounds = 50,
        #                  objective = "binary:logistic")

        model
    }

    predict_ <- function(test_data){
        print("predicting...")
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

