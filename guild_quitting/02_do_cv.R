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

### Do cross-validation on training data with various models
print("Do cross-validation on training data")
# Model:
current_model_constructor <- get_model_xgboost
predictions <- do_cv(train_data, current_model_constructor)
results <- get_perf_measures(predictions$label, predictions$prediction)
results
write_results_to_file(predictions, results, paste("guild_quitting/", (get_model_xgboost())$model_name(), "/train_cv", sep = ""))

# Model on the whole training set
xgboost_wrapper <- get_model_xgboost()
invisible(xgboost_wrapper$build(train_data))
xgb.importance(feature_names = (xgboost_wrapper$params())$final_predictor_column_names, model = xgboost_wrapper$model())


