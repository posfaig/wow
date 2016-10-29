##########################################################################
###
### Build XGBoost Model with Fuzzy Clustering
### and Perform Cross Validation on the Training Dataset
###
##########################################################################

set.seed(0)

library(readr)

##### MAIN #####

### Helper funcitons
source("common/model_evaluation.R")
source("common/util.R")
source("guild_quitting/xgboost_with_fuzzy_clustering.R")
source("guild_quitting/fuzzy_clustering_of_guilds.R")

### Get training dataset
print("Get train dataset")
train_data <- read_csv("guild_quitting/features_train.csv")
train_data$race <- factor(train_data$race)
train_data$charclass <- factor(train_data$charclass)
train_data$guild_created_recently <- factor(train_data$guild_created_recently)


### Fuzzy-Clustering of Guilds
train_data <- fuzzy_clustering_of_guilds(train_data)


### Do cross-validation on training data with various models
print("Do cross-validation on training data")
# Model:
current_model_constructor <- get_model_xgboost_with_fclus
predictions <- do_cv(train_data, current_model_constructor)
results <- get_perf_measures(predictions$label, predictions$prediction)
results
write_results_to_file(predictions, results, paste("guild_quitting/", (get_model_xgboost_with_fclus())$model_name(), "/train_cv", sep = ""))

