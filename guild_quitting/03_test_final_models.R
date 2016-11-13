#####################################################################################
###
### Build XGBoost model on the complete training dataset
### and evaluate its performance on the test dataset
###
#####################################################################################


##### MAIN #####

# Get set of selected predictors
opt_predictors <- read_csv("generated/results/guild_quitting/xgboost/train_cv/predictors.csv") %>%
    collect %>% .[["predictor"]]

# Get train dataset
train_data <- read_csv("guild_quitting/features_train.csv")
train_data$race <- factor(train_data$race)
train_data$charclass <- factor(train_data$charclass)
train_data$guild_created_recently <- factor(train_data$guild_created_recently)

# Get test dataset
test_data <- read_csv("guild_quitting/features_test.csv")
test_data$race <- factor(test_data$race)
test_data$charclass <- factor(test_data$charclass)
test_data$guild_created_recently <- factor(test_data$guild_created_recently)

# Get performance measures on final test set
print("Get performance measures on final test set")
xgboost_wrapper <- get_model_xgboost(list(predictors = opt_predictors))
xgboost_wrapper$build(train_data)

predictions <- data.frame(
    avatar = test_data$avatar,
    pred_date = test_data$pred_date,
    label = test_data$label,
    prediction = xgboost_wrapper$predict(test_data))

results_test <- get_perf_measures(predictions$label, predictions$prediction, opt_threshold_xgb$maximum)
write_results_to_file(predictions, results_test, paste("guild_quitting/", xgboost_wrapper$model_name(), "/test", sep = ""))


