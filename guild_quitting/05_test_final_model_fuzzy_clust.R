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

# Get set of selected predictors
opt_predictors <- read_csv("generated/results/guild_quitting/xgboost/train_cv/predictors.csv") %>%
    collect %>% .[["predictor"]]

# Get optimized parameter values
opt_params <- read_csv("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/opt_params.csv")
opt_fuzziness <- opt_params$value[opt_params$parameter == "fuzziness"]
opt_num_clusters <- opt_params$value[opt_params$parameter == "num_clusters"]
opt_decision_threshold <- opt_params$value[opt_params$parameter == "decision_threshold"]

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

### Fuzzy clustering of guilds
data_with_fuzzy_clustering <- fuzzy_clustering_of_guilds(rbind(train_data, test_data), opt_num_clusters, opt_fuzziness)
train_data <- data_with_fuzzy_clustering[1:nrow(train_data),]
test_data <- data_with_fuzzy_clustering[-1 * (1:nrow(train_data)),]

# Get performance measures on final test set
print("Get performance measures on final test set")
xgboost_with_fclust_wrapper <- get_model_xgboost_with_fclus(list(predictors = opt_predictors))
xgboost_with_fclust_wrapper$build(train_data)

predictions <- data.frame(
    avatar = test_data$avatar,
    pred_date = test_data$pred_date,
    label = test_data$label,
    prediction = xgboost_with_fclust_wrapper$predict(test_data))

results_test <- get_perf_measures(predictions$label, predictions$prediction, opt_decision_threshold)
write_results_to_file(predictions, results_test, paste("guild_quitting/", xgboost_with_fclust_wrapper$model_name(), "/test", sep = ""))



