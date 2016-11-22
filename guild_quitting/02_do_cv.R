##########################################################################
###
### Build XGBoost Model
### and Perform Cross Validation on the Training Dataset
###
##########################################################################
set.seed(0)

library(readr)

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

### Model on the whole training set
xgboost_wrapper <- get_model_xgboost()
invisible(xgboost_wrapper$build(train_data))
features_by_importance <- xgb.importance(feature_names = (xgboost_wrapper$params())$final_predictor_column_names, model = xgboost_wrapper$model())
features_by_importance
# Write feature importances to file
write.table(features_by_importance,
            "generated/results/guild_quitting/xgboost/train_cv/feature_importance.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)


train_data <- train_data_orig
selected_features <- c("avg_daily_act",
                      "member_for_days",
                      "friends_left_guild",
                      "level",
                      "collaboration_with_members"
                      ##"members_of_same_class_ratio",
                      ##"avg_level_in_guild",
                      ##"friends_in_guild"
                      ##"activity_as_member"
                      ##"active_members_ratio"
                      ##"diff_guild_count"
                      )
features_to_keep <- c(selected_features, "avatar", "guild", "label", "pred_date", "testset_end_date")
train_data <- train_data[,(names(train_data) %in% features_to_keep)]

# Write selected features to file
write.table(data.frame(feature = selected_features),
            "generated/results/guild_quitting/xgboost/train_cv/selected_features.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

### Do rolling forecasting origin on training data
print("Do rolling forecasting origin on training data")
# Model:
current_model_constructor <- get_model_xgboost
predictions <- do_cv(train_data, current_model_constructor)

# Get optimal decision threshold:
opt_threshold_xgb <- optimize(fscore_for_decision_threshold, c(0, 1.0), predictions, maximum = TRUE, tol = 0.001)
print(paste("Decision threshold at optimal F-score:", opt_threshold_xgb$maximum))
results <- get_perf_measures(predictions$label, predictions$prediction, opt_threshold_xgb$maximum)
results
write_results_to_file(predictions, results, paste("guild_quitting/", (get_model_xgboost())$model_name(), "/train_cv", sep = ""))

# Write optimized parameter values to file
write.table(data.frame(parameter = c("decision_threshold"), value = c(opt_threshold_xgb$maximum)),
             "generated/results/guild_quitting/xgboost/train_cv/opt_params.csv",
             append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
