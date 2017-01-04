##########################################################################
###
### Simple XGBoost Model
###
### Train and evaluate XGBoost model on the training dataset with rolling forecasting origin resampling
### Tune the time window parameter and the decision threshold value
###
##########################################################################

set.seed(0)
dir.create(file.path("generated/results/guild_quitting/xgboost/train_cv/"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("generated/results/guild_quitting/xgboost/test/"), showWarnings = FALSE, recursive = TRUE)

library(readr)
library(parallel)

##### MAIN #####

### Helper funcitons
source("guild_quitting/model_xgb.R")
source("common/streamline_functions_for_modeling.R")
source("common/model_evaluation.R")
source("common/util.R")


max_time_window <- 0
while (file.exists(paste("generated/tmp/guild_quitting/features_", (max_time_window + 1) ,"-day_window_train.csv", sep = ""))
       && file.exists(paste("generated/tmp/guild_quitting/features_", (max_time_window + 1) ,"-day_window_test.csv", sep = ""))){
    max_time_window <- max_time_window + 1
}

evaluate_time_window <- function(time_window){

    print(paste("Current time window:", time_window))

    ### Get training dataset
    train_data <- read_csv(paste("generated/tmp/guild_quitting/features_", time_window ,"-day_window_train.csv", sep = ""))
    train_data$race <- factor(train_data$race)
    train_data$charclass <- factor(train_data$charclass)
    train_data$guild_created_recently <- factor(train_data$guild_created_recently)

    ### Create model on the records of the first prediction date and extract feature importances
    train_data_first_iteration <- train_data[train_data$pred_date == min(train_data$pred_date),]
    xgboost_wrapper <- get_model_xgboost()
    invisible(xgboost_wrapper$build(train_data_first_iteration))
    features_by_importance <- xgb.importance(feature_names = (xgboost_wrapper$params())$final_predictor_column_names, model = xgboost_wrapper$model())
    feature_names_ordered <- features_by_importance$Feature

    # We have to retrieve the names of the features in the original data frame, since in the preprocessing of the xgboost factors are converted to dummy variables
    extract_original_feature_names <- function(new_feature_names, original_feature_names){
        selected <- sapply(new_feature_names, function(new_feature_name){
            which(sapply(original_feature_names, function(original_feature_name){
                if (new_feature_name == original_feature_name){
                    TRUE
                } else if (!is.factor(train_data[[original_feature_name]])){
                    FALSE
                } else if (startsWith(new_feature_name, original_feature_name)
                           && any(
                               substr(new_feature_name, nchar(original_feature_name) + 1, nchar(new_feature_name)) == unique(as.character(train_data[[original_feature_name]]))
                           )){
                    TRUE
                } else {
                    FALSE
                }

            }))
        })
        unique(original_feature_names[selected])
    }
    feature_names_ordered <- extract_original_feature_names(feature_names_ordered, names(train_data))
    print("Features by decreasing importance:")
    print(feature_names_ordered)

    # Write feature importances to file
    #write.table(features_by_importance,
    #        "generated/results/guild_quitting/xgboost/train_cv/feature_importance.csv",
    #        append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

    evaluate_feature_set <- function(data, feature_names){
        data <- data[,(names(data) %in% features_to_keep)]
        ### Do rolling forecasting origin on training data
        current_model_constructor <- get_model_xgboost
        predictions <- do_cv(data, current_model_constructor)
        results <- get_perf_measures(predictions$label, predictions$prediction)
        list(stats = results, predictions = predictions)
    }

    ###
    # Greedy forward feature selection
    # We go through the ordered features and check one-by-one whether adding the feature improves the AUC value
    # If it does, then we add the feature to the selected feature set
    ###
    best_results <- NULL
    selected_features <- c()
    for (new_feature in feature_names_ordered){
        features_to_keep <- c(selected_features, new_feature)
        print(paste("Current feature set:", paste(features_to_keep, collapse = ",")))
        features_to_keep <- c(features_to_keep, "avatar", "guild", "label", "pred_date", "testset_end_date")
        current_results <- evaluate_feature_set(train_data, features_to_keep)

        print(paste("AUC:", current_results[['stats']]$auc, "(Best AUC before:", best_results[['stats']]$auc, ")"))

        if (is.null(best_results) || current_results[['stats']]$auc > best_results[['stats']]$auc){
            best_results <- current_results
            selected_features <- c(selected_features, new_feature)
        }

    }
    list(stats = best_results[['stats']], predictions = best_results$predictions, selected_features = selected_features)
}

# Evaluate various time window values
#results_by_time_windows <- mclapply(1:max_time_window, evaluate_time_window)
system.time(results_by_time_windows <- lapply(1:max_time_window, evaluate_time_window))

# Convert auc values to vector and data frame
auc_values <- sapply(results_by_time_windows, function(x){x[['stats']]$auc})
auc_values_df <- data.frame(time_window = 1:max_time_window, auc = auc_values)
write.table(auc_values_df,
            "generated/results/guild_quitting/xgboost/train_cv/auc_values_by_time_window.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

best_time_window <- which.max(auc_values)
print(paste("Best time window:", best_time_window))
predictions <- results_by_time_windows[[best_time_window]]$predictions
selected_features <- results_by_time_windows[[best_time_window]]$selected_features

# Write selected features to file
write.table(data.frame(feature = selected_features),
            "generated/results/guild_quitting/xgboost/train_cv/selected_features.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

# Get optimal decision threshold:
opt_threshold_xgb <- optimize(fscore_for_decision_threshold, c(0, 1.0), predictions, maximum = TRUE, tol = 0.001)
print(paste("Decision threshold at maximal F-score:", opt_threshold_xgb$maximum))
results <- get_perf_measures(predictions$label, predictions$prediction, opt_threshold_xgb$maximum)
results
write_results_to_file(predictions, results, paste("guild_quitting/", (get_model_xgboost())$model_name(), "/train_cv", sep = ""))

# Write optimized parameter values to file
write.table(data.frame(parameter = c("time_window", "decision_threshold"), value = c(best_time_window, opt_threshold_xgb$maximum)),
             "generated/results/guild_quitting/xgboost/train_cv/opt_params.csv",
             append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
