#####################################################################################
###
### XGBoost with Fuzzy Clustering
### Build the optimized models on the complete training dataset and evaluate their performance on the test dataset
###
#####################################################################################


##### MAIN #####
source("common/model_evaluation.R")
source("common/util.R")
source("guild_quitting/xgboost_with_fuzzy_clustering.R")
source("guild_quitting/fuzzy_clustering_of_guilds.R")

# Get optimized parameter values
opt_params <- read_csv("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/opt_params.csv")
opt_fuzziness <- opt_params$value[opt_params$parameter == "fuzziness"]
opt_num_clusters <- opt_params$value[opt_params$parameter == "num_clusters"]
opt_decision_threshold <- opt_params$value[opt_params$parameter == "decision_threshold"]
opt_params <- read_csv("generated/results/guild_quitting/xgboost/train_cv/opt_params.csv")
time_window <- opt_params$value[opt_params$parameter == "time_window"]

# Read selected features
features_to_keep_df <- read_csv("generated/results/guild_quitting/xgboost/train_cv/selected_features.csv")
features_to_keep <- features_to_keep_df$feature
features_to_keep <- c(features_to_keep, "avatar", "guild", "label", "pred_date", "testset_end_date")

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

# Do rolling forecasting origin for the months of the test set
# (Since there are two months in the test set, k = 2)
current_model_constructor <- get_model_xgboost_with_fclus
predictions <- do_cv_with_fuzzy_clustering(
    rbind(train_data, test_data),
    current_model_constructor,
    opt_num_clusters,
    opt_fuzziness,
    features_to_keep,
    k = 2)
results_test <- get_perf_measures(predictions$label, predictions$prediction, opt_decision_threshold)
results_test
write_results_to_file(predictions, results_test, paste("guild_quitting/", get_model_xgboost_with_fclus()$model_name(), "/test", sep = ""))
