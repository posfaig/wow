#####################################################################################
###
### Function to Perform Fuzzy Clustering
###
#####################################################################################

library(dplyr)
library(fclust)

fuzzy_clustering_of_guilds <- function(dataset, num_clusters = 3, fuzziness = 2.5, remove_guild_cols = FALSE){
    guild_feature_names <- c(
        "density",
        "guild_members",
        "members_left",
        "avg_daily_guild_activity",
        "avg_guild_member_activity",
        "avg_level_in_guild"


        ## Other possible features (did not improve AUC):
        #"collaboration_10wise",
        #"new_members",
        #"largest_clique",
        #"max_cliques",
        #"median_daily_guild_activity",
        #"median_guild_member_activity",
        #"active_members_ratio",
        #"median_level_in_guild",
        #"guild_created_recently",
        #"clustering_coeff"
        #"collaboration_10wise",
        #"collaboration_20wise",
        #"collaboration_25wise",
        #"dungeon_activity",
        #"avg_dungeon_activity"
    )

    guild_features <- dataset %>%
        distinct(guild, pred_date, .keep_all = TRUE) %>%
        select_(.dots = c("guild", "pred_date", guild_feature_names))

    if ("guild_created_recently" %in% names(guild_features)){
        guild_features <- guild_features %>%
            mutate(guild_created_recently = as.numeric(guild_created_recently))
    }

    set.seed(0)
    guild_clusters <- FKM(guild_features %>% mutate(guild = NULL, pred_date = NULL),
                          k = num_clusters,
                          m = fuzziness,
                          stand = 1,
                          RS = 1,
                          maxit = 100)

    guild_cluster_features <- as.data.frame(cbind(guild = guild_features$guild, guild_clusters$U)) %>% mutate(pred_date = guild_features$pred_date)
    dataset <- left_join(dataset, guild_cluster_features, by = c("guild", "pred_date"))
    names(dataset) <- gsub(" ", "", names(dataset), fixed = TRUE)

    if (remove_guild_cols){
        sapply(guild_feature_names, function(x){
            dataset[[x]] <<- NULL
        })
    }

    list(data = dataset, clusters = guild_clusters)
}

