##############################################################################################################
###
### Some functions to facilitate the streamline process of data splitting, preprocessing, model creation etc.
###
##############################################################################################################


library(cvTools)


# Compute features and labels for a set of prediction dates
get_features_for_pred_dates <- function(pred_dates, graph_creating_function, feature_computing_function){
    dataset <- data.frame()
    for (pred_date in pred_dates){
        print(paste("Computing features and labels for prediction date", pred_date))
        testset_end_date <- as.Date(pred_date) + 30

        graph_data <- graph_creating_function(wow, pred_date)
        data_subset <- feature_computing_function(wow, pred_date, testset_end_date, graph_data)

        # concat
        if (nrow(dataset) == 0){
            dataset <- data_subset
        } else {
            dataset <- rbind(dataset, data_subset)
        }
    }
    dataset
}


# Do resampling procedure similar to rolling forecasting origin on a training dataset
do_cv <- function(train_data, current_model_constructor, params = list(), ..., k = 8){
    set.seed(0)

    min_date_index <- length(unique(train_data$pred_date)) - k + 1
    train_index_list <- lapply(min_date_index:length(unique(train_data$pred_date)), function(x){which(as.numeric(factor(train_data$pred_date)) < x)})
    test_index_list <- lapply(min_date_index:length(unique(train_data$pred_date)), function(x){which(as.numeric(factor(train_data$pred_date)) == x)})

    #folds <- cvFolds(n = nrow(train_data), K = k, R = 1)
    predictions <- data.frame()
    for (i in 1:length(train_index_list)){
    #for (i in 1:k){
        # get indecies of current fold
        #test_set_row_indecies <- folds$subsets[folds$which == i, 1]
        #training_set_row_indecies <- folds$subsets[folds$which != i, 1]

        # get train and test sets of current fold
        #current_train_data <- train_data[training_set_row_indecies,]
        #current_test_data <- train_data[test_set_row_indecies,]
        current_train_data <- train_data[train_index_list[[i]],]
        current_test_data <- train_data[test_index_list[[i]],]

        # learn model
        current_model <- current_model_constructor(params)
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



