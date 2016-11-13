#####################################################################################
###
### Function to Perform Fuzzy Clustering
###
#####################################################################################

library(dplyr)
library(fclust)

fuzzy_clustering_of_guilds <- function(dataset, num_clusters = 4, fuzziness = 3.0, remove_guild_cols = FALSE){
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
    #"collaboration_3wise",
    #"collaboration_5wise",
    #"collaboration_10wise",
    #"collaboration_20wise",
    #"collaboration_40wise",
    #"dungeon_activity",
    #"avg_dungeon_activity")

    guild_features <- dataset %>%
        distinct(guild, pred_date) %>%
        select_(.dots = c("guild", "pred_date", guild_feature_names)) %>%
        mutate(guild_created_recently = as.numeric(guild_created_recently))

    set.seed(0)
    guild_clusters <- FKM(guild_features %>% mutate(guild = NULL, pred_date = NULL),
                          k = num_clusters,
                          m = fuzziness,
                          stand = 1,
                          RS = 5)

    guild_cluster_features <- as.data.frame(cbind(guild = guild_features$guild, guild_clusters$U)) %>% mutate(pred_date = guild_features$pred_date)
    dataset <- left_join(dataset, guild_cluster_features, by = c("guild", "pred_date"))
    names(dataset) <- gsub(" ", "", names(dataset), fixed = TRUE)

    if (remove_guild_cols){
        sapply(guild_feature_names, function(x){
            dataset[[x]] <<- NULL
        })
    }

    dataset
}

