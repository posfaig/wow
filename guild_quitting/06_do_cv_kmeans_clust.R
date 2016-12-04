##########################################################################
###
### Build XGBoost Model with Fuzzy Clustering
### and Perform Cross Validation on the Training Dataset
###
##########################################################################

set.seed(0)

library(readr)


##### Function to Perform KMeans Clustering #####

library(dplyr)
kmeans_clustering_of_guilds <- function(dataset, num_clusters = 4, remove_guild_cols = FALSE){
    guild_feature_names <- c(
        "guild_members",
        "members_left",
        "new_members",
        "largest_clique",
        "max_cliques",
        "avg_daily_guild_activity",
        #"median_daily_guild_activity",
        "avg_guild_member_activity",
        #"median_guild_member_activity",
        "active_members_ratio",
        #"avg_level_in_guild",
        "median_level_in_guild",
        "guild_created_recently",
        "clustering_coeff")#,
    #"density",
    #"collaboration_10wise",
    #"collaboration_20wise",
    #"collaboration_25wise",
    #"dungeon_activity",
    #"avg_dungeon_activity")

    guild_features <- dataset %>%
        distinct(guild, pred_date) %>%
        select_(.dots = c("guild", "pred_date", guild_feature_names)) %>%
        mutate(guild_created_recently = as.numeric(guild_created_recently))


    guild_clusters <- kmeans(scale(guild_features %>% mutate(guild = NULL, pred_date = NULL)),
                             num_clusters,
                             nstart = 5)

    guild_cluster_features <- as.data.frame(cbind(guild = guild_features$guild, guild_cluster = guild_clusters$cluster)) %>% mutate(pred_date = guild_features$pred_date)
    dataset <- left_join(dataset, guild_cluster_features, by = c("guild", "pred_date"))

    if (remove_guild_cols){
        sapply(guild_feature_names, function(x){
            dataset[[x]] <<- NULL
        })
    }

    dataset
}


##### MAIN #####

### Helper funcitons
source("common/model_evaluation.R")
source("common/util.R")
source("guild_quitting/xgboost_with_kmeans_clustering.R")
source("guild_quitting/fuzzy_clustering_of_guilds.R")

### Get training dataset
print("Get train dataset")
train_data <- read_csv("guild_quitting/features_train.csv")
train_data$race <- factor(train_data$race)
train_data$charclass <- factor(train_data$charclass)
train_data$guild_created_recently <- factor(train_data$guild_created_recently)


### Fuzzy-Clustering of Guilds
train_data <- kmeans_clustering_of_guilds(train_data)


### Do cross-validation on training data with various models
print("Do cross-validation on training data")
# Model:
current_model_constructor <- get_model_xgboost_with_kmeans
predictions <- do_cv(train_data, current_model_constructor)
results <- get_perf_measures(predictions$label, predictions$prediction)
results
write_results_to_file(predictions, results, paste("guild_quitting/", (current_model_constructor())$model_name(), "/train_cv", sep = ""))

