##########################################################################
###
### Build XGBoost Model
### and Perform Cross Validation on the Training Dataset
###
##########################################################################
set.seed(0)

library(cvTools)
library(readr)
library(pROC)
library(caret)

##### MAIN #####

### Helper funcitons
source("guild_quitting/models.R")
source("common/streamline_functions_for_modeling.R")
source("common/model_evaluation.R")
source("common/util.R")

### Get training dataset
print("Get train dataset")
train_data <- read_csv("guild_quitting/features_train.csv")
train_data$race <- factor(train_data$race)
train_data$charclass <- factor(train_data$charclass)
train_data$guild_created_recently <- factor(train_data$guild_created_recently)


### Feature Selection by Implicit Featre Selection Method of the caret package for xgboost
feature_selection_with_sa <- function(dataset){
    target_column <- "label"
    targets <- dataset[[target_column]]
    targets <- as.numeric(targets)  # numeric and >0 target variable as the frbs package requires
    dataset[,target_column] <- NULL

    dataset_preprocessed <- get_model_xgboost()$preprocess(dataset, TRUE)


    # pack the training control parameters
    # create index list for resampling iterations in order to avoid using future data for training (~ applying a procedure similar to rolling forecasting origin)
    index_list <- lapply(2:length(unique(train_data$pred_date)), function(k){which(as.numeric(factor(train_data$pred_date)) < k)})
    index_out_list <- lapply(2:length(unique(train_data$pred_date)), function(k){which(as.numeric(factor(train_data$pred_date)) == k)})

    seeds <- vector(mode = "list", length = 9)
    for(i in 1:8) seeds[[i]] <- 1:36
    seeds[[9]] <- 0
    xgb_trcontrol <- trainControl(
        method = "cv",
        number = 8,
        verboseIter = TRUE,
        returnData = FALSE,
        returnResamp = "all",                                                        # save losses across all models
        classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
        summaryFunction = twoClassSummary,
        seeds = seeds,
        allowParallel = TRUE,
        index = index_list,
        indexOut = index_out_list
    )

    print(ncol(dataset_preprocessed))
    print(names(dataset_preprocessed))
    # train the model for each parameter combination in the grid,
    #   using CV to evaluate
    set.seed(0)
    xgb_train <- train(
        x = as.matrix(dataset_preprocessed),
        y = as.factor(paste("x", as.character(targets), sep = "_")),
        trControl = xgb_trcontrol,
        method = "xgbTree"
    )

    xgb_train
}
opt_xgb_model <- feature_selection_with_sa(train_data)
opt_predictors <- predictors(opt_xgb_model)


### Get the optimal value for the early stopping parameter
get_auc_for_earlystopping_param_value <- function(early_stop_round = 4){
    current_model_constructor <- get_model_xgboost
    predictions <- do_cv(train_data, current_model_constructor, list(predictors = opt_predictors), early_stop_round, k = 4)
    results <- get_perf_measures(predictions$label, predictions$prediction)
    results$auc
}
# We do not use the optimize function since we are only interested in integer parameter values
auc_values_by_early_stopping_parameter <- sapply(1:8, get_auc_for_earlystopping_param_value)
print("AUC values by early stopping parameter value:")
print(auc_values_by_early_stopping_parameter)
opt_early_stopping_value_xgb <- which.max(auc_values_by_early_stopping_parameter)
print(paste("Optimal early stopping parameter value:", opt_early_stopping_value_xgb))

### Do cross-validation on training data with various models
print("Do cross-validation on training data")
# Model:
current_model_constructor <- get_model_xgboost
predictions <- do_cv(train_data, current_model_constructor, list(predictors = opt_predictors), opt_early_stopping_value_xgb)
# Get optimal decision threshold:
opt_threshold_xgb <- optimize(fscore_for_decision_threshold, c(0, 1.0), predictions, maximum = TRUE, tol = 0.001)
print(paste("Decision threshold at optimal F-score:", opt_threshold_xgb$maximum))
results <- get_perf_measures(predictions$label, predictions$prediction, opt_threshold_xgb$maximum)
results
write_results_to_file(predictions, results, paste("guild_quitting/", (get_model_xgboost())$model_name(), "/train_cv", sep = ""))

# Model on the whole training set
xgboost_wrapper <- get_model_xgboost(list(predictors = opt_predictors))
invisible(xgboost_wrapper$build(train_data))
features_by_importance <- xgb.importance(feature_names = (xgboost_wrapper$params())$final_predictor_column_names, model = xgboost_wrapper$model())
features_by_importance

# Write selected predictors to file
write.table(data.frame(predictor = opt_predictors),
            "generated/results/guild_quitting/xgboost/train_cv/predictors.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

# Write feature importances to file
write.table(features_by_importance,
            "generated/results/guild_quitting/xgboost/train_cv/feature_importance.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

