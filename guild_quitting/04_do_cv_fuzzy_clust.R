##########################################################################
###
### Build XGBoost Model with Fuzzy Clustering
### and Perform Cross Validation on the Training Dataset
###
##########################################################################

set.seed(0)

library(readr)
library(parallel)

##### MAIN #####

### Helper funcitons
source("common/streamline_functions_for_modeling.R")
source("common/model_evaluation.R")
source("common/util.R")
source("guild_quitting/xgboost_with_fuzzy_clustering.R")
source("guild_quitting/fuzzy_clustering_of_guilds.R")

# Get optimized parameters
opt_params <- read_csv("generated/results/guild_quitting/xgboost/train_cv/opt_params.csv")
time_window <- opt_params$value[opt_params$parameter == "time_window"]

# Read selected features
features_to_keep_df <- read_csv("generated/results/guild_quitting/xgboost/train_cv/selected_features.csv")
features_to_keep <- features_to_keep_df$feature
features_to_keep <- c(features_to_keep, "avatar", "guild", "label", "pred_date", "testset_end_date")

### Get training dataset
print("Get train dataset")
train_data <- read_csv(paste("generated/tmp/guild_quitting/features_", time_window ,"-day_window_train.csv", sep = ""))
train_data$race <- factor(train_data$race)
train_data$charclass <- factor(train_data$charclass)
train_data$guild_created_recently <- factor(train_data$guild_created_recently)


### Parameter tuning: fuzziness and number of fuzzy clusters
get_auc_for_parameter_setting <- function(num_clusters, fuzziness, dataset = train_data){
    print(paste("Number of clusters:", num_clusters))
    print(paste("Fuzziness:", fuzziness))
    current_model_constructor <- get_model_xgboost_with_fclus
    predictions <- do_cv_with_fuzzy_clustering(dataset, current_model_constructor, num_clusters, fuzziness, features_to_keep, k = 2)
    results <- get_perf_measures(predictions$label, predictions$prediction)
    print(paste("AUC:", results$auc))
    results$auc
}

# Evaluate parameter combinations
num_clusters_values <- 2:20
fuzziness_values <- c(1.05, seq(1.5, 8.0, 0.5))
param_values <- expand.grid(fuzziness = fuzziness_values, num_clusters = num_clusters_values)
file.remove("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/params_tuning.csv")
system.time(
param_tuning_results <- apply(param_values, 1, function(params){
    current_time <- system.time(
        current_auc <- get_auc_for_parameter_setting(params['num_clusters'], params['fuzziness'])
    )
    current_results_df <- data.frame(
        fuzziness = params['fuzziness'],
        num_clusters = params['num_clusters'],
        auc = current_auc,
        time = current_time['elapsed'])

    # Write the results of the current iteration of the parameter tuning process to disk
    if (!file.exists("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/params_tuning.csv")){
        write.table(current_results_df,
                    "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/params_tuning.csv",
                    append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
    } else {
        write.table(current_results_df,
                    "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/params_tuning.csv",
                    append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
    }
    list(time = current_time['elapsed'], auc = current_auc)
    })
)
param_values$auc <- sapply(param_tuning_results, function(x){x$auc})
param_values$time <- sapply(param_tuning_results, function(x){x$time})

# Ridge regression
#param_values <- read.csv("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/params_tuning.csv")
lambda <- 1e-5
param_values$objective_function <- param_values$auc -
    lambda * rowSums(cbind(param_values$num_clusters ^ 2, param_values$fuzziness ^ 2))

# Write parameter tuning results to file
write.table(param_values,
            "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/params_tuning.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

# Select the best parameter combination
opt_fuzziness <- param_values[which.max(param_values$objective_function), "fuzziness"]
opt_num_clusters <- param_values[which.max(param_values$objective_function), "num_clusters"]

# Print and save results of parameter tuning
print(paste("Optimal number of fuzzy clusters:", opt_num_clusters))
print(paste("Optimal fuzziness:", opt_fuzziness))
write.table(data.frame(parameter = c("fuzziness", "num_clusters"), value = c(opt_fuzziness, opt_num_clusters)),
            "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/opt_params.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

### Do rolling forecasting origin on training data
print("Do rolling forecasting origin on training data")
# Model:
current_model_constructor <- get_model_xgboost_with_fclus
predictions <- do_cv_with_fuzzy_clustering(train_data, current_model_constructor, opt_num_clusters, opt_fuzziness, features_to_keep)

# Get optimal decision threshold:
opt_threshold_fuzzy_clust <- optimize(fscore_for_decision_threshold, c(0, 1.0), predictions, maximum = TRUE, tol = 0.001)
print(paste("Decision threshold at optimal F-score:", opt_threshold_fuzzy_clust))
results <- get_perf_measures(predictions$label, predictions$prediction, opt_threshold_fuzzy_clust$maximum)
results

write_results_to_file(predictions, results, paste("guild_quitting/", (get_model_xgboost_with_fclus())$model_name(), "/train_cv", sep = ""))

write.table(data.frame(parameter = c("fuzziness", "num_clusters", "decision_threshold"), value = c(opt_fuzziness, opt_num_clusters, opt_threshold_fuzzy_clust$maximum)),
            "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/opt_params.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

