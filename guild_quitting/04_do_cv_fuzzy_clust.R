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

### Get set of selected predictors
opt_predictors <- read_csv("generated/results/guild_quitting/xgboost/train_cv/predictors.csv") %>%
    collect %>% .[["predictor"]]

### Get training dataset
print("Get train dataset")
train_data <- read_csv("guild_quitting/features_train.csv")
train_data$race <- factor(train_data$race)
train_data$charclass <- factor(train_data$charclass)
train_data$guild_created_recently <- factor(train_data$guild_created_recently)

### Parameter tuning: fuzziness and number of fuzzy clusters
get_auc_for_parameter_setting <- function(num_clusters = 4, fuzziness = 3.0, dataset = train_data, k = 2){
    print(paste("Number of clusters:", num_clusters))
    print(paste("Fuzziness:", fuzziness))
    dataset_with_fuzzy_clusters <- fuzzy_clustering_of_guilds(dataset, num_clusters, fuzziness)
    current_model_constructor <- get_model_xgboost_with_fclus
    predictions <- do_cv(dataset_with_fuzzy_clusters, current_model_constructor, list(predictors = opt_predictors), k = k)
    results <- get_perf_measures(predictions$label, predictions$prediction)
    results$auc
}
best_auc <- 0
opt_num_clusters <- 0
get_auc_for_fuzziness_value <- function(fuzziness){
    auc_values_by_num_clusters <- sapply(2:10, get_auc_for_parameter_setting, fuzziness)
    if (max(auc_values_by_num_clusters) > best_auc){
        best_auc <<- max(auc_values_by_num_clusters)
        opt_num_clusters <<- which.max(auc_values_by_num_clusters) + 1
    }
    max(auc_values_by_num_clusters)
}
fuzziness_opt_result <- optimize(get_auc_for_fuzziness_value, c(1.05, 5.0), maximum = TRUE, tol = 0.5)
opt_fuzziness <- fuzziness_opt_result$maximum
print(paste("Optimal number of fuzzy clusters:", opt_num_clusters))
print(paste("Optimal fuzziness:", opt_fuzziness))
print(paste("AUC at optimal parameter setting:", fuzziness_opt_result$objective))
write.table(data.frame(parameter = c("fuzziness", "num_clusters"), value = c(opt_fuzziness, opt_num_clusters)),
            "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/opt_params.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

### Fuzzy-Clustering of Guilds
train_data_with_clusters <- fuzzy_clustering_of_guilds(train_data, opt_num_clusters, opt_fuzziness)

### Do cross-validation on training data with various models
print("Do cross-validation on training data")
# Model:
current_model_constructor <- get_model_xgboost_with_fclus
predictions <- do_cv(train_data_with_clusters, current_model_constructor, list(predictors = opt_predictors))
# Get optimal decision threshold:
opt_threshold_fuzzy_clust <- optimize(fscore_for_decision_threshold, c(0, 1.0), predictions, maximum = TRUE, tol = 0.001)
print(paste("Decision threshold at optimal F-score:", opt_threshold_fuzzy_clust))
results <- get_perf_measures(predictions$label, predictions$prediction, opt_threshold_fuzzy_clust$maximum)
results
write_results_to_file(predictions, results, paste("guild_quitting/", (get_model_xgboost_with_fclus())$model_name(), "/train_cv", sep = ""))

write.table(data.frame(parameter = c("fuzziness", "num_clusters", "decision_threshold"), value = c(opt_fuzziness, opt_num_clusters, opt_threshold_fuzzy_clust$maximum)),
            "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/opt_params.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)


### Assess the impact of parameters

# Impact of number of fuzzy clusters
num_clusters_values <- 1:10
auc_values_by_number_of_clusters <- sapply(num_clusters_values, get_auc_for_parameter_setting, fuzziness = opt_fuzziness, dataset = train_data, k = 4)
auc_values_by_number_of_clusters_df <- data.frame(num_clusters = num_clusters_values, auc = auc_values_by_number_of_clusters)
write.table(auc_values_by_number_of_clusters_df,
            "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/impact_of_num_clusters.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)


# Impact of fuzziness
fuzziness_values <- c(1.05, seq(1.5, 4.5, 0.5))
auc_values_by_fuzziness <- sapply(fuzziness_values, function(x, ...){
    get_auc_for_parameter_setting(fuzziness = x, ...)
}, num_clusters = opt_num_clusters, dataset = train_data, k = 4)
auc_values_by_fuzziness_df <- data.frame(fuzziness = fuzziness_values, auc = fuzziness_values)
write.table(auc_values_by_fuzziness_df,
            "generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/impact_of_fuzziness.csv",
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)



