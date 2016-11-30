##############################################################################################################
###
### Some functions to facilitate the streamline process of data splitting, preprocessing, model creation etc.
###
##############################################################################################################

library(parallel)
library(cvTools)


# Compute features and labels for a set of prediction dates
get_features_for_pred_dates <- function(pred_dates, graph_creating_function, feature_computing_function, ...){
    dataset <- data.frame()

    datasets_list <- lapply(pred_dates, function(pred_date){
        print(paste("Computing features and labels for prediction date", pred_date))
        testset_end_date <- as.Date(pred_date) + 30

        graph_data <- graph_creating_function(wow, pred_date, ...)
        data_subset <- feature_computing_function(wow, pred_date, testset_end_date, graph_data)
        data_subset
    })

    # concat result datasets
    lapply(datasets_list, function(data_subset){
        if (nrow(dataset) == 0){
            dataset <<- data_subset
        } else {
            dataset <<- rbind(dataset, data_subset)
        }
        c()
    })

    dataset
}


# Do resampling procedure similar to rolling forecasting origin on a training dataset
do_cv <- function(train_data, current_model_constructor, ..., k = 8){
    set.seed(0)

    min_date_index <- length(unique(train_data$pred_date)) - k + 1
    train_index_list <- lapply(min_date_index:length(sort(unique(train_data$pred_date))), function(x){which(as.numeric(factor(train_data$pred_date)) < x)})
    test_index_list <- lapply(min_date_index:length(sort(unique(train_data$pred_date))), function(x){which(as.numeric(factor(train_data$pred_date)) == x)})

    predictions <- data.frame()
    for (i in 1:length(train_index_list)){
    	# get train and test sets for current iteration
        current_train_data <- train_data[train_index_list[[i]],]
        current_test_data <- train_data[test_index_list[[i]],]

        # learn model
        current_model <- current_model_constructor()
        current_model$build(current_train_data, ...)

        # predict with the current model
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



