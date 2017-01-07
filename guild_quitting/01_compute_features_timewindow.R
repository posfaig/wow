#################################################################x
###
### Compute Features
###
#################################################################x

if (!exists("time_window")){
    time_window <- 10
}
print(paste("Current time window:", time_window))

set.seed(0)

library(dplyr)
library(lubridate)
library(data.table)
library(pROC)
library(cvTools)
library(readr)
library(igraph)



##### Feature to Compute and Corresponding Parameters: #####


### Individual featues:
# char
# race
# level
# number of different guild the avatar had been member of
# average/median daily activity in the last N days
#	# hyperparameters:
ind_daily_activity_last_days <- time_window


### Guild-related individual features:
# average daily activity as a guild member in the last N days
gr_activity_as_member_last_days <- time_window
# how many days the avatar has been a member of its current guild for
# collaboration time with guild members
# number of friends in guild
# number of tight friends in guild, tight means having at least M collaboration time (where collaboration time is the sum of the weights of edges)
#tight_friend_min_collab_time <- 6
tight_friend_quantile_boundary <- 0.5
# number of friends already quit the guild
# number of tight friends already quit the guild


### Guild features:
# number of guild members
# number of (former) members who left the guild in the last N days
g_guild_left_in_last_days <- time_window
# number of new members who joined the guild in the last N days
g_guild_joined_in_last_days <- time_window
# clustering coeff
# average/median daily guild activity in the last N days
g_guild_activity_last_days <- time_window
# average/median activity of members in the last N days
g_guild_member_activity_last_days <- time_window
# percentage of members who had at least M activity in the last N days
g_guild_members_ratio_with_activity_last_days <- time_window
g_guild_member_min_activity_thershold <- 1

# average/median level of guild members
# binary: guild was created in the last N days?
g_guild_created_recently_days <- time_window

# percentage and number of the guild members of the same class as the class of the current avatar
# Density: connections between guild members
# Maximum sub-graph size: largest interconnected cluster of members in a guild's social network.
# Mass count: the number of sub-graphs larger than three in a guild's social network, that is, how many independent sub-units there are
# average/median daily M-wise collaboration time in the last N days (time when at least M members were simultanously active in the guild in the same zone, times are summed for different zones, that is if the conditions are satisfied for multiple zones at the same time, we sum the collaboration time corresponding to each zone)
# Average time spent in "instances" (dungeons): an indicator of the importance of planned group activities in a guild, as opposed to ad hoc quest parties and individual quests.
g_time_in_dungeons_in_last_days <- time_window
#
#
##### Other possible features
# ~ prolific members
# ~ graph features from network visualization R-blogger article
# ~ avatars of the same player features
# ~ guilds class-balance: how the distribution of classes within guild members differs from the average
#
##### Additional features for guild entering events:
#	time (in terms of both real time and number of records) being outside of guilds
#
#




##### Compute interactions (collaborations) between avatars
if (!file.exists("generated/tmp/benchmark/number_interactions_by_days.csv")){
    source("guild_quitting/benchmark/compute_interactions.R")
}

##### Helper functions

### Function to create intra-guild social networks
source("guild_quitting/create_intraguild_sn_timewindow.R")

### Compute features and labels for a given prediction date and test date based on the observed data and the current temporal social graph of avatars
compute_features_and_labels <- function(data, pred_date, testset_end_date, intraguild_graphs){
    print("compute_features_and_labels --- start")
    pred_date <- as.Date(pred_date)
    testset_end_date <- as.Date(testset_end_date)

    # Keep only the data regarding known avatars
    avatars_in_guilds <- intraguild_graphs$nodes
    data <- data %>% filter(avatar %in% avatars_in_guilds$avatar)
    min_date <- min(data$current_date)

    ##### Compute labels for avatars
    #
    # true: left the guild in the given period
    # false: not left the guild in the given period
    #
    test_data <- data %>% filter(current_date >= pred_date & current_date < testset_end_date)
    labels <- test_data %>%
        group_by(avatar) %>%
        summarise(label = (sum(event == "Guild Left" | event == "Guild Changed") > 0))

    ##### Compute features

    # helper function for adding a new feature to the existing ones
    # joins the data frame of existing features with the data frame of the new feature
    add_feature <- function(new_feature, new_feature_col_name, by_columns = "avatar", na_value = 0){
        new_feature <- new_feature %>% select_(.dots = c(by_columns, new_feature_col_name))
        features <- left_join(features, new_feature, by = by_columns)
        features[[new_feature_col_name]][is.na(features[[new_feature_col_name]])] <- na_value
        features
    }

    #### Individual features
    print("Computing individual features")
    train_data <- data %>% filter(current_date < pred_date)
    # get the avatars that are member of a guild at prediction date
    current_guilds <- train_data %>%
        group_by(avatar) %>%
        dplyr::slice(n()) %>%
        group_by() %>%
        filter(guild != -1) %>%
        select(avatar, guild) %>%
        mutate(current_guild = guild, guild = NULL)


    ## Features: char, race, level, number of different guild the avatar had been member of
    features <- train_data %>%
        filter(avatar %in% current_guilds$avatar) %>%
        group_by(avatar) %>%
        summarise(
            race = race[1],
            charclass = charclass[1],
            level = max(level),
            diff_guild_count = length(unique(guild[guild != -1])))

    # add guild column to features
    features <- add_feature(current_guilds %>% mutate(guild = current_guild, current_guild = NULL), "guild")

    ## Features: average/median daily activity in the last N days
    tmp <- train_data %>%
        filter(avatar %in% current_guilds$avatar) %>%
        filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= ind_daily_activity_last_days) %>%
        group_by(avatar, current_date) %>%
        summarise(daily_activity = n()) %>%
        summarise(avg_daily_act = sum(daily_activity) / ind_daily_activity_last_days, med_daily_act = median(daily_activity))
    features <- add_feature(tmp, "avg_daily_act")
    features <- add_feature(tmp, "med_daily_act")


    #### Guild-related individual features
    print("Guild-related individual features")

    ## Feature: average daily activity as a guild member in the last N days
    tmp <- left_join(train_data, current_guilds) %>%
        filter(avatar %in% current_guilds$avatar) %>%
        filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= gr_activity_as_member_last_days) %>%
        filter(guild == current_guild) %>%
        group_by(avatar) %>%
        summarise(activity_as_member = n() / gr_activity_as_member_last_days)
    features <- add_feature(tmp, "activity_as_member")

    ## Feature: how many days the avatar has been a member of its current guild for?
    tmp <- left_join(train_data, current_guilds) %>%
        filter(avatar %in% current_guilds$avatar) %>%
        filter(guild == current_guild) %>%
        group_by(avatar) %>%
        summarise(
            new_avatar = new_avatar[1],
            member_for_days =
                as.numeric(
                    difftime(pred_date, max(current_date[event != "No Event" & event != "Guild Left"]), units = "days")),
            days_since_first_guild_occurence = as.numeric(difftime(pred_date, max(current_date[guild == current_guild]), units = "days"))) %>%
        mutate(member_for_days =
                   ifelse(is.infinite(member_for_days),
                          ifelse(	!new_avatar,
                                  as.numeric(difftime(pred_date, min_date, units = "days")),
                                  days_since_first_guild_occurence),
                          member_for_days)) %>%
        mutate(days_since_first_guild_occurence = NULL, new_avatar = NULL)
    features <- add_feature(tmp, "member_for_days")

    ## Feature: collaboration time with guild members: sum of the weights of the avatar's edges in the intraguild social network
    edges_intra <- intraguild_graphs$edges_intra %>% filter(weight > 0)
    edges_intra$node_1 <- as.character(edges_intra$node_1)
    edges_intra$node_2 <- as.character(edges_intra$node_2)

    # duplicate edges in opposite direction
    edges_intra_bidirectional <- rbind(edges_intra,
                                       data.frame(
                                           node_1 = edges_intra$node_2,
                                           node_2 = edges_intra$node_1,
                                           weight = edges_intra$weight,
                                           guild = edges_intra$guild
                                       )) %>% group_by()

    tmp <- edges_intra_bidirectional %>%
        group_by(node_1) %>%
        summarise(collaboration_with_members = sum(weight)) %>%
        mutate(avatar = node_1, node_1 = NULL)
    features <- add_feature(tmp, "collaboration_with_members")

    ## Feature: number of friends in guild
    tmp <- edges_intra_bidirectional %>%
        group_by(node_1) %>%
        summarise(friends_in_guild = n()) %>%
        mutate(avatar = node_1, node_1 = NULL)
    features <- add_feature(tmp, "friends_in_guild")

    # compute edge weight boundary for close friends based on the value of the close-friend-quantile parameter
    positive_weights <- intraguild_graphs[['edges_intra_dash']]$weight
    positive_weights <- positive_weights[positive_weights > 0]
    tight_friend_weight_boundary <- quantile(positive_weights, tight_friend_quantile_boundary)

    ## Feature: number of tight friends in guild, tight means having at least M collaboration time
    tmp <- edges_intra_bidirectional %>%
        filter(weight > tight_friend_weight_boundary) %>%
        group_by(node_1) %>%
        summarise(tight_friends_in_guild = n()) %>%
        mutate(avatar = node_1, node_1 = NULL)
    features <- add_feature(tmp, "tight_friends_in_guild")
    rm(edges_intra_bidirectional)

    ## Feature: number of friends already quit the guild
    edges_intra_former <- intraguild_graphs$edges_intra_former
    edges_intra_former$node_1 <- as.character(edges_intra_former$node_1)
    edges_intra_former$node_2 <- as.character(edges_intra_former$node_2)

    # duplicate edges in opposite direction
    edges_intra_former_bidirectional <- rbind(edges_intra_former,
                                              data.frame(
                                                  node_1 = edges_intra_former$node_2,
                                                  node_2 = edges_intra_former$node_1,
                                                  weight = edges_intra_former$weight,
                                                  guild = edges_intra_former$guild
                                              ))
    rm(edges_intra_former)

    tmp <- edges_intra_former_bidirectional %>%
        group_by(node_1) %>%
        summarise(friends_left_guild = n(), guild = guild[1]) %>%
        mutate(avatar = node_1, node_1 = NULL)
    features <- add_feature(tmp, "friends_left_guild", c("avatar", "guild"))

    ## Feature: number of tight friends already quit the guild
    tmp <- edges_intra_former_bidirectional %>%
        filter(weight > tight_friend_weight_boundary) %>%
        group_by(node_1) %>%
        summarise(tight_friends_left_guild = n(), guild = guild[1]) %>%
        mutate(avatar = node_1, node_1 = NULL)
    features <- add_feature(tmp, "tight_friends_left_guild", c("avatar", "guild"))
    rm(edges_intra_former_bidirectional)

    #### Guild-related features

    ## Feature: number of guild members
    guild_members_df <- current_guilds %>%
        mutate(guild = current_guild, current_guild = NULL) %>%
        group_by(guild) %>%
        summarise(guild_members = n())
    features <- add_feature(guild_members_df, "guild_members", "guild")

    ## Feature: number of (former) members who left the guild in the last N days
    tmp <- train_data %>%
        filter(event == "Guild Left" | event == "Guild Changed") %>%
        filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= g_guild_left_in_last_days) %>%
        select(avatar, prev_guild) %>%  # get events when members left
        distinct(avatar, prev_guild, .keep_all = TRUE) %>%  # only count one event per member per guild
        mutate(guild = prev_guild, prev_guild = NULL) %>%
        left_join(current_guilds %>% mutate(guild = current_guild), by = c("guild", "avatar")) %>%
        filter(is.na(current_guild)) %>%  # filter out events where the avatar rejoined	the guild later
        mutate(current_guild = NULL) %>%
        group_by(guild) %>%
        summarise(members_left = n())  # count events
    features <- add_feature(tmp, "members_left", "guild")

    ## Feature: number of new members who joined the guild in the last N days
    tmp <- train_data %>%
        filter(event == "Guild Entered" | event == "Guild Changed") %>%
        filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= g_guild_joined_in_last_days) %>%
        select(avatar, guild) %>%  # get events when members left
        distinct(avatar, guild, .keep_all = TRUE)  # only count one event per member per guild
    # keep only the events where the avatar is still a member of the joined guild
    tmp <- inner_join(current_guilds %>% mutate(guild = current_guild), tmp, by = c("guild", "avatar")) %>%
        mutate(current_guild = NULL) %>%
        group_by(guild) %>%
        summarise(new_members = n())  # count events
    features <- add_feature(tmp, "new_members", "guild")

    ## Feature: average/median daily guild activity in the last N days
    tmp <- train_data %>%
        filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= g_guild_activity_last_days) %>%
        group_by(guild, current_date) %>%
        summarise(daily_guild_activity = n()) %>%
        summarise(
            avg_daily_guild_activity = mean(daily_guild_activity),
            median_daily_guild_activity = median(daily_guild_activity))
    features <- add_feature(tmp, "avg_daily_guild_activity", "guild")
    features <- add_feature(tmp, "median_daily_guild_activity", "guild")

    ## Feature: average/median activity of members in the last N days
    # we ignore the guild events (leaving joining) of the corresponding time period
    # and simply calculate with the current number of guild members for each day

    # get the current number of guild members for each guild
    number_of_guild_members <- current_guilds %>%
        mutate(guild = current_guild, current_guild = NULL) %>%
        group_by(guild) %>% summarise(member_count = n())
    tmp <- train_data %>%
        filter(guild != -1) %>%
        filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= g_guild_member_activity_last_days) %>%
        inner_join(number_of_guild_members, by = "guild") %>%
        group_by(guild, avatar) %>%
        summarise(avatar_activity = n(), member_count = member_count[1]) %>%
        summarise(
            avg_guild_member_activity = sum(avatar_activity) / member_count[1],
            median_guild_member_activity = median(
                c(avatar_activity, rep(0, max(member_count[1] - length(avatar_activity), 0)))
            )
        )
    features <- add_feature(tmp, "avg_guild_member_activity", "guild")
    features <- add_feature(tmp, "median_guild_member_activity", "guild")

    ## Feature: ratio of members who had at least M activity in the last N days
    tmp <- train_data %>%
        filter(guild != -1) %>%
        filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= g_guild_members_ratio_with_activity_last_days) %>%
        left_join(number_of_guild_members, by = "guild") %>%
        group_by(guild, avatar) %>%
        summarise(avatar_activity = n(), member_count = member_count[1]) %>%
        filter(avatar_activity >= g_guild_member_min_activity_thershold) %>%
        summarise(active_members_ratio = n() / member_count[1])
    features <- add_feature(tmp, "active_members_ratio", "guild")
    rm(number_of_guild_members)

    ## Feature: average/median level of guild members
    current_guilds_with_levels <- train_data %>%
        group_by(avatar) %>%
        dplyr::slice(n()) %>%
        group_by() %>%
        filter(guild != -1) %>%
        select(avatar, guild, level)
    tmp <- current_guilds_with_levels %>%
        group_by(guild) %>%
        summarise(avg_level_in_guild = mean(level), median_level_in_guild = median(level))
    features <- add_feature(tmp, "avg_level_in_guild", "guild")
    features <- add_feature(tmp, "median_level_in_guild", "guild")
    rm(current_guilds_with_levels)

    ## Feature: binary: guild was created in the last N days?
    first_guilds <- train_data %>%
        filter(guild != -1) %>%
        group_by(avatar) %>%
        dplyr::slice(1) %>%
        group_by() %>%
        select(avatar, guild) %>%
        mutate(first_guild = guild, guild = NULL)
    guild_creations <- train_data %>%
        filter(guild != -1) %>%
        left_join(first_guilds) %>%
        group_by(guild, avatar) %>%
        dplyr::slice(1) %>%
        group_by(guild) %>%
        summarise(
            new_guild = (sum(new_avatar | first_guild != guild[1]) == n()),
            guild_creation = ifelse(new_guild, min(current_date), NA)
        )
    guild_creations <- guild_creations %>% mutate(guild_creation = as.Date(guild_creation, origin = "1970-01-01"))
    tmp <- guild_creations %>%
        filter(new_guild) %>%
        filter(as.numeric(difftime(pred_date, guild_creation, units = "days")) <= g_guild_created_recently_days) %>%
        mutate(guild_created_recently = TRUE)
    features <- add_feature(tmp, "guild_created_recently", "guild", FALSE)
    rm(first_guilds, guild_creations)

    ## Feature: ratio and number of the guild members of the same class as the class of the current avatar
    # when computing the ratio we exclude the currently examined avatar, so e.g. the ratio will 0 if the current avatar is the only member from its class
    current_guilds_with_class <- train_data %>%
        group_by(avatar) %>%
        dplyr::slice(n()) %>%
        group_by() %>%
        filter(guild != -1) %>%
        select(avatar, guild, charclass)

    # guild, class, members data frame
    guild_class_members <- current_guilds_with_class %>% group_by(guild, charclass) %>% summarise(members_of_same_class = n() - 1) %>% group_by()
    features <- add_feature(guild_class_members, "members_of_same_class", c("guild", "charclass"), 0)
    features$members_of_same_class_ratio <- features$members_of_same_class / (features$guild_members - 1)
    features$members_of_same_class_ratio[is.na(features$members_of_same_class_ratio)] <- 0
    rm(current_guilds_with_class)

    ## Feature: clustering coeff
    ## Feature: size of the largest clique in the guild's graph
    ## Feature: Mass count: the number of cliques larger than three in the guild's graph (~how many independent sub-units there are)

    # tmp <- edges_intra %>% group_by(guild) %>% do({
    #     current_df <- .
    #     guild_graph <- make_graph(as.vector(t(as.matrix(cbind(current_df$node_1, current_df$node_2)))))
    #     clustering_coeff <- transitivity(guild_graph)
    #     largest_clique <- clique_num(guild_graph)
    #     max_cliques <- length(max_cliques(guild_graph, min = 3))
    #     data.frame(clustering_coeff = clustering_coeff
    #                largest_clique = largest_clique,
    #                max_cliques = max_cliques
    #                )
    # })
    # features <- add_feature(tmp, "clustering_coeff", "guild", 0)
    # features <- add_feature(tmp, "largest_clique", "guild", 1)
    # features <- add_feature(tmp, "max_cliques", "guild", 0)

    ## Feature: Density: percentage of matrix cells that are filled in, in adjacency matrix of the guild's graph
    tmp <- guild_members_df %>%
        left_join(edges_intra %>% group_by(guild) %>% summarise(edges = n()), by = "guild") %>%
        mutate(edges = ifelse(is.na(edges), 0, edges)) %>%
        mutate(density = edges / (guild_members * (guild_members - 1) / 2)) %>%
        mutate(density = ifelse(is.na(density), 0, density))
    features <- add_feature(tmp, "density", "guild", 0)
    rm(guild_members_df, edges_intra)

    ## Feature: average/median daily M-wise collaboration time in the last N days (time when at least M members were simultanously active in the guild in the same zone, times are summed for different zones, that is if the conditions are satisfied for multiple zones at the same time, we sum the collaboration time corresponding to each zone)
    g_mwise_collaboration <- c(10, 20, 25)  # There are 10-, 20- and 25-player raids in this WoW release

    # create a variable identifying the snapshots
    snap_times <- seq(as.POSIXlt(as.Date("2008-01-01")), as.POSIXlt(as.Date("2009-01-01")), 10 * 60)
    train_data$snapshot <- cut(train_data$timestamp, snap_times)

    # create a data frame containing how many avatars were observed in the same zone in the same snapshot from the same guild
    horde_capitals <- c("Shattrath City", "Orgrimmar", "Silvermoon City", "Thunder Bluff", "Undercity", "Dalaran")
    guild_activities_by_snapshots <- train_data %>%
        filter(guild != -1) %>%
        filter(!(zone %in% horde_capitals)) %>%
        group_by(current_date, snapshot, guild, zone) %>%
        summarise(activity = n()) %>%
        group_by()

    for (i in (1:length(g_mwise_collaboration))) {
        feature_name <- paste("collaboration_", g_mwise_collaboration[i], "wise", sep = "")
        tmp <- guild_activities_by_snapshots %>%
            filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= time_window) %>%
            group_by(guild) %>%
            summarise(mwise_collab = sum(activity >= g_mwise_collaboration[i]))
        tmp[[feature_name]] <- tmp$mwise_collab
        tmp$mwise_collab <- NULL
        features <- add_feature(tmp, feature_name, "guild", 0)
    }
    rm(guild_activities_by_snapshots)

    ## Feature: time spent in "instances" (dungeons) by guild members in the last N days
    ## Feature: average time spent in "instances" (dungeons) by guild members in the last N days
    # An indicator of the importance of planned group activities in a guild, as opposed to ad hoc quest parties and individual quests.
    # When computing the average we simply divide the overall time by the number of current members.
    tmp <- train_data %>%
        filter(guild != -1) %>%
        filter(as.numeric(difftime(pred_date, current_date, units = "days")) <= g_time_in_dungeons_in_last_days) %>%
        filter(zone_type == "Dungeon") %>%
        group_by(guild) %>%
        summarise(dungeon_activity = n())
    features <- add_feature(tmp, "dungeon_activity", "guild", 0)
    features$avg_dungeon_activity <- features$dungeon_activity / features$guild_members

    ##### Joining labels
    print("Joining features and labels")
    features_and_labels <- left_join(features, labels, by = "avatar")
    features_and_labels <- features_and_labels %>% mutate(label = ifelse(is.na(label), FALSE, label))  # label is FALSE for avatars that did not appear in the test period
    features_and_labels$pred_date <- pred_date
    features_and_labels$testset_end_date <- testset_end_date
    features_and_labels

    ##### RETURN
    print("compute_features_and_labels --- return")
    features_and_labels
}


##### MAIN #####

source("common/init.R")
source("common/streamline_functions_for_modeling.R")

### Add zone type column
zones <- read_csv(paste(data_dir, "zones.csv", sep = ""))
wow <- left_join(wow, zones %>%
                     select(Type, Zone_Name) %>%
                     mutate(zone_type = Type, zone = Zone_Name, Type = NULL, Zone_Name = NULL),
                 by = "zone")
rm(zones)


### Get training and test dataset
print("Get train and test datasets")
training_data <- get_features_for_pred_dates(
    prediction_dates_train,
    create_intraguild_graphs,
    compute_features_and_labels,
    time_window = time_window)
# Write results to files
dir.create(file.path("generated/tmp/guild_quitting/"), showWarnings = FALSE, recursive = TRUE)
print("Write training data features to file")
write_csv(training_data, paste("generated/tmp/guild_quitting/features_", time_window ,"-day_window_train.csv", sep = ""))
rm(training_data)

test_data <- get_features_for_pred_dates(
    prediction_dates_test,
    create_intraguild_graphs,
    compute_features_and_labels,
    time_window = time_window)
# Write results to files
print("Write test data features to file")
write_csv(test_data, paste("generated/tmp/guild_quitting/features_", time_window ,"-day_window_test.csv", sep = ""))
rm(test_data)

rm(interactions, wow)

