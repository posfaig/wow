###########################################################################################
###
### Function for building and evaluating xgboost models for guild entering event prediction
###
###########################################################################################

library(dplyr)
library(xgboost)

source("common/util.R")

get_model_xgb_for_guild_entering <- function(){
    model_name <- "xgboost"
    desc <- "xgboost"
    model <- c()
    params <- list()

    preprocess <- function(data, is_train){
        # remove not predictor columns
        data$avatar <- NULL
        data$pred_date <- NULL
        data$known_label <- NULL
        data$target_guild <- NULL
        data$target_prev_guild <- NULL
        data$target_event <- NULL
        data$testset_end_date <- NULL
        data$target_event_date <- NULL

        data$race <- factor(data$race)
        data$charclass <- factor(data$charclass)

        # TODO ??? data$charclass <- factor(data$charclass)
        # TODO ??? data$race <- factor(data$race)

        # convert factors to numeric
        data <- convert_factors_to_numeric(data)

        if (is_train){
            # save names of final predictors
            params[["final_predictor_column_names"]] <<- names(data)
        } else {
            # keep only predictors that were used during training
            #data <- data %>% select_(.dots = intersect(params$final_predictor_column_names, names(data)))
            data <- data %>% select_(.dots = params$final_predictor_column_names)
        }

        # convert integers to double for xgboost
        sapply(names(data), function(col_name){
            data[[col_name]] <<- as.numeric(data[[col_name]])
        })

        data
    }

    build <- function(train_data) {
        target_column <- "target_guild"
        targets <- train_data[[target_column]]

        params[["num_class"]] <<- length(unique(targets))
        params[["classes"]] <<- unique(targets)
        params[["class_levels"]] <<- levels(targets)

        targets <- as.numeric(targets) - 1
        train_data[,target_column] <- NULL

        train_data_preprocessed <- preprocess(train_data, TRUE)



        set.seed(0)
        model <<- xgboost(
            as.matrix(train_data_preprocessed),
            targets,
            objective = "multi:softprob", # multi:softmax
            nrounds = 10,
            verbose = 1,
            eta = 0.0001,
            eval_metric = "mlogloss",
            num_class = length(unique(targets)))
        model
    }

    predict_ <- function(test_data){
        test_data <- preprocess(test_data, FALSE)
        predictions <- predict(model, as.matrix(test_data))

        # Convert predictions into data frame with interpretable column names
        preds_mx <- matrix(predictions, nrow = nrow(test_data))
        preds_df <- as.data.frame(preds_mx)
        output_classes <- 0:(params$num_class - 1)
        names(preds_df) <- paste0("pred_guild_", params$class_levels[output_classes + 1])

        preds_df
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

