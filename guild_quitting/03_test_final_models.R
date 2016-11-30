#####################################################################################
###
### Build XGBoost model on the complete training dataset
### and evaluate its performance on the test dataset
###
#####################################################################################


##### MAIN #####

# Helper funcitons
source("guild_quitting/models.R")
source("common/streamline_functions_for_modeling.R")
source("common/model_evaluation.R")
source("common/util.R")

# Get optimized parameters
opt_params <- read_csv("generated/results/guild_quitting/xgboost/train_cv/opt_params.csv")
opt_decision_threshold <- opt_params$value[opt_params$parameter == "decision_threshold"]
time_window <- opt_params$value[opt_params$parameter == "time_window"]

# Get train dataset
train_data <- read_csv(paste("generated/tmp/guild_quitting/features_", time_window ,"-day_window_train.csv", sep = ""))
train_data$race <- factor(train_data$race)
train_data$charclass <- factor(train_data$charclass)
train_data$guild_created_recently <- factor(train_data$guild_created_recently)

# Get test dataset
test_data <- read_csv(paste("generated/tmp/guild_quitting/features_", time_window ,"-day_window_test.csv", sep = ""))
test_data$race <- factor(test_data$race)
test_data$charclass <- factor(test_data$charclass)
test_data$guild_created_recently <- factor(test_data$guild_created_recently)

# Read selected features
features_to_keep_df <- read_csv("generated/results/guild_quitting/xgboost/train_cv/selected_features.csv")
features_to_keep <- features_to_keep_df$feature
features_to_keep <- c(features_to_keep, "avatar", "guild", "label", "pred_date", "testset_end_date")

# Get performance measures on final test set
print("Get performance measures on final test set")
train_data <- train_data[,(names(train_data) %in% features_to_keep)]
test_data <- test_data[,(names(test_data) %in% features_to_keep)]
current_model_constructor <- get_model_xgboost
predictions <- do_cv(rbind(train_data, test_data), current_model_constructor, k = 2)
results_test <- get_perf_measures(predictions$label, predictions$prediction, opt_decision_threshold)
results_test
write_results_to_file(predictions, results_test, paste("guild_quitting/", current_model_constructor()$model_name(), "/test", sep = ""))


