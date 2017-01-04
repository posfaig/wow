########################################################################################################
###
###  Based on the following paper: Modeling Destructive Group Dynamics in Online Gaming Communities  ###
###
########################################################################################################

set.seed(0)

##### Compute collaborations between players #####
if (!file.exists("generated/tmp/benchmark/number_interactions_by_days.csv")){
    source("guild_quitting/benchmark/compute_interactions.R")
}

##### Parameters #####

# theta parameter of the exponentially decaying kernel function for edge weights
theta <- 0.3  # the value was determined based on many cross validation experiments on the training set

##### Helper functions #####

# Create the social graph for a given prediction date based on the observed data
create_graph <- function(data, pred_date){
    pred_date <- as.Date(pred_date)

    # keep only the events before the current prediction date
    data <- data %>% filter(current_date < pred_date)

    # filter out records from horde cities (as only horde avatars are in the dataset)
    data_with_cities <- data
    horde_capitals <- c("Shattrath City", "Orgrimmar", "Silvermoon City", "Thunder Bluff", "Undercity", "Dalaran")
    data <- data %>% filter(!(zone %in% horde_capitals))

    ### Edges:
    edges <- data.frame(node_1 = "-", node_2 = "-", weight = 0)  # create df with auxiliary row
    edges$node_1 <- as.character(edges$node_1)
    edges$node_2 <- as.character(edges$node_2)
    add_edge <- function(node_1, node_2, weight){
        edges <<- rbind(edges, data.frame(node_1 = node_1, node_2 = node_2, weight = weight))
    }
    compute_weight_component <- function(interaction_dates, cooccurence_durations = 1.0){
        t <- as.numeric(difftime(pred_date, interaction_dates, units = "days"))
        weight_components <- theta * cooccurence_durations * ((1-theta) ^ (t))
        sum(weight_components)
    }

    ## Edges from collaboration
    print("Create collaboration edges")
    # Edges are between players who occured in the same zone at the same time (snapshot).
    # Weights are proportional to the duration of collaboration
    # Collaboration durations are also decreased according to a time-decaying function
    #

    tmp <- interactions %>%
        filter(current_date < pred_date) %>%
        group_by(avatar.x, avatar.y) %>%
        summarise(current_weight = compute_weight_component(current_date, collaboration)) %>%
        group_by
    add_edge(tmp$avatar.x, tmp$avatar.y, tmp$current_weight)

    ## Edges from guilds
    print("Create guild edges")
    # Avatars and guilds are only connected if the character is in the guild on the current prediction date.
    # We also add a guild node with id -1 to represent how much time did avatars play outside of guilds recently.

    # Get the guild members at prediction date
    guild_members <- data_with_cities %>% group_by(avatar) %>% dplyr::slice(n()) %>% group_by() %>% filter(guild != -1)
    avatar_date_guild_df <- data %>%
        select(avatar, current_date, guild) %>%
        group_by(avatar, current_date, guild) %>%
        summarise(collaboration = n()) %>%
        group_by()
    tmp <- avatar_date_guild_df %>%
        group_by(avatar, guild) %>%
        summarise(weight = compute_weight_component(current_date, collaboration)) %>%
        group_by
    guild_members <- left_join(guild_members, tmp, by = c("guild", "avatar"))
    guild_members$weight[is.na(guild_members$weight)] <- 0.0

    add_edge(guild_members$guild, guild_members$avatar, guild_members$weight)  # guilds are always node_1

    # Add edges connecting to node -1
    tmp <- tmp %>% filter(guild == -1)
    add_edge(tmp$guild, tmp$avatar, tmp$weight)

    edges <- edges[-1,]  # remove auxiliary row

    # nodes: avatars and guilds
    print("Create nodes")
    avatar_nodes <- data %>% distinct(avatar, .keep_all = TRUE) %>% select(avatar) %>% collect %>% .[["avatar"]]
    guild_nodes <- unique(c(data$guild, guild_members$guild))
    nodes <- c(avatar_nodes, guild_nodes)

    list(nodes = nodes,
         avatars = avatar_nodes,
         guilds = guild_nodes,
         edges = edges,
         current_guilds = guild_members %>% select(avatar, guild, weight))
}

# Compute features and labels for a given prediction date and test date based on the observed data and the current temporal social graph of avatars
compute_features_and_labels <- function(data, pred_date, testset_end_date, graph){
    pred_date <- as.Date(pred_date)
    testset_end_date <- as.Date(testset_end_date)
    guild_member_avatars <- graph$current_guilds$avatar

    # Keep only the data regarding known avatars
    data <- data %>% filter(avatar %in% guild_member_avatars)

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
    #### Personal histories of 14 "day-records" and corresponding features
    print("Computing personal history features")
    train_data <- data %>% filter(current_date < pred_date)
    personal_histories <- train_data %>%
        group_by(avatar, current_date, guild) %>%
        mutate(number_of_records = n()) %>%
        dplyr::slice(1) %>%
        group_by(avatar) %>%
        dplyr::slice(1:14) %>%
        group_by

    features <- personal_histories %>%
        group_by(avatar) %>%
        summarise(
            guild_count = length(unique(guild[guild != -1])),
            time_since_last_event = as.numeric(difftime(pred_date, max(current_date), units = "days")),
            event_count = n(),
            level_begin = min(level),
            level_end = max(level),
            level_change = level_end - level_begin,
            avg_event_duration = mean(number_of_records),
            window_duration = as.numeric(difftime(max(current_date), min(current_date), units = "days")))
    # keep only the avatars that are guild members on the prediction date
    features <- filter(features, avatar %in% guild_member_avatars)


    #### Social features
    print("Computing social features")

    ### Number of guild membership feature
    print("Number of guild membership feature")
    features$number_of_guild_membership <- features$guild_count

    ### Overall and guild clustering coeffecients features
    print("Overall and guild clustering coeffecients features")
    library(igraph)
    current_edges <- as.vector(t(as.matrix(select(graph$edges, node_1, node_2))))
    features$overall_clustering_coeff <- transitivity(make_graph(current_edges))

    clustering_coeffs <- graph$edges %>%
        filter(node_1 %in% graph$guilds) %>%
        group_by(node_1) %>%
        do({
            current_nodes <- unique(c(.$node_1[1], .$node_2))
            current_edges <- graph$edges %>%
                filter((node_1 %in% current_nodes) & (node_2 %in% current_nodes))
            current_edges <- as.vector(t(as.matrix(select(current_edges, node_1, node_2))))
            data.frame(clustering_coeff = transitivity(make_graph(current_edges)))
        }) %>%
        group_by
    clustering_coeffs$clustering_coeff[is.nan(clustering_coeffs$clustering_coeff)] <- 0  # Guilds with 1 member (i.e. only two vertices)
    detach("package:igraph", unload = TRUE)

    clustering_coeffs$guild <- as.numeric(clustering_coeffs$node_1)
    clustering_coeffs$node_1 <- NULL
    features <- left_join(features, graph$current_guilds %>% select(avatar, guild), by = "avatar")
    features <- left_join(features, clustering_coeffs, by = "guild")

    ### Playing time within guild feature = weight of the edge connecting to the guild node
    print("Playing time within guild feature")
    features <- left_join(features, graph$current_guilds %>% select(avatar, weight), by = "avatar") %>%
        mutate(playing_time_within_guild = weight)
    features$weight <- NULL

    ### Collaboration time within guild = sum of the weights of the edges connecting to the other members of the guild
    print("Collaboration time within guild feature")

    # create auxiliary data frame containing edges between avatars, and guild of avatars
    edges_between_avatars <- filter(graph$edges, !(node_1 %in% c(graph$guilds, -1)))
    # duplicate edges in opposite direction
    edges_between_avatars <- rbind(edges_between_avatars,
                                   data.frame(
                                       node_1 = edges_between_avatars$node_2,
                                       node_2 = edges_between_avatars$node_1,
                                       weight = edges_between_avatars$weight
                                   ))
    # add column containing the guild of node_1
    edges_between_avatars_with_guilds <- edges_between_avatars
    edges_between_avatars_with_guilds$avatar <- edges_between_avatars_with_guilds$node_1
    edges_between_avatars_with_guilds <- inner_join(
        edges_between_avatars_with_guilds,
        select(features, avatar, guild))
    edges_between_avatars_with_guilds$guild_node_1 <- edges_between_avatars_with_guilds$guild
    edges_between_avatars_with_guilds$guild <- NULL

    # add column containing the guild of node_2
    edges_between_avatars_with_guilds$avatar <- edges_between_avatars_with_guilds$node_2
    edges_between_avatars_with_guilds <- inner_join(
        edges_between_avatars_with_guilds,
        select(features, avatar, guild))
    edges_between_avatars_with_guilds$guild_node_2 <- edges_between_avatars_with_guilds$guild
    edges_between_avatars_with_guilds$guild <- NULL
    edges_between_avatars_with_guilds$avatar <- NULL

    collaboration_time_within_guild_df <- edges_between_avatars_with_guilds %>%
        group_by(node_1) %>%
        summarise(collaboration_time_within_guild = sum(weight[guild_node_1 == guild_node_2])) %>%
        mutate(avatar = node_1) %>%
        select(avatar, collaboration_time_within_guild)

    features <- left_join(features, collaboration_time_within_guild_df, by = "avatar") %>%
        mutate(collaboration_time_within_guild = ifelse(is.na(collaboration_time_within_guild), 0, collaboration_time_within_guild))

    ### Weighted degree within guild feature = sum of the edges of the avatar, that run within its current guild = the sum of "playing time within guild" and "collaboration time within guild" features
    print("Weighted degree within guild feature")
    features$weighted_degree_within_guild <- features$playing_time_within_guild + features$collaboration_time_within_guild

    features$weight <- NULL


    ### Playing time feature
    # Sum of the weight of the edge that goes to the current guild and the weight of the edge that goes to guild -1
    print("Playing time feature")
    weights_to_noguild <- graph$edges %>% filter(node_1 == -1) %>% select(node_2, weight)
    weights_to_noguild$avatar <- weights_to_noguild$node_2
    weights_to_noguild$playing_time <- weights_to_noguild$weight
    weights_to_noguild$node_2 <- NULL
    weights_to_noguild$weight <- NULL

    features <- left_join(features, weights_to_noguild, by = "avatar") %>%
        mutate(playing_time = ifelse(is.na(playing_time), 0, playing_time))
    features$playing_time <- features$playing_time + features$playing_time_within_guild


    ### Collaboration time feature
    # Sum of the weights of edges that do not go to the current guild or guild -1
    print("Collaboration time feature")
    tmp <- edges_between_avatars_with_guilds %>%
        group_by(node_1) %>%
        summarise(collaboration_time = sum(weight))
    tmp$avatar <- tmp$node_1
    tmp$node_1 <- NULL
    features <- left_join(features, tmp, by = "avatar") %>%
        mutate(collaboration_time = ifelse(is.na(collaboration_time), 0, collaboration_time))

    ### Overall weighted degree feature
    # The sum of the playing_time and collaboration_time features
    print("Overall weighted degree feature")
    features$overall_weighted_degree <- features$playing_time + features$collaboration_time

    ### Number of guild members feature
    print("Number of guild members feature")
    guild_edges <- filter(graph$edges, (node_1 %in% graph$guilds))
    number_of_guild_members <- guild_edges %>% group_by(node_1) %>% summarise(number_of_guild_members = n())
    number_of_guild_members$guild <- as.numeric(number_of_guild_members$node_1)
    number_of_guild_members$node_1 <- NULL
    features <- left_join(features, number_of_guild_members, by = "guild")

    ### Percentage of members played with excessively feature
    print("Percentage of members played with excessively feature")
    played_with_excessively_threshold <- mean(edges_between_avatars$weight) + 2 * sd(edges_between_avatars$weight)
    tmp <- edges_between_avatars_with_guilds
    tmp$avatar <- tmp$node_1
    tmp$guild <- tmp$guild_node_1
    tmp$node_1 <- NULL
    tmp$guild_node_1 <- NULL
    tmp$guild_node_2 <- NULL
    tmp <- left_join(tmp, number_of_guild_members, by = "guild")
    tmp_2 <- tmp %>%
        group_by(avatar) %>%
        summarise(percentage_of_members_played_with_excessively = sum(weight > played_with_excessively_threshold)/number_of_guild_members[1])

    features <- left_join(features, tmp_2, by = "avatar") %>%
        mutate(percentage_of_members_played_with_excessively = ifelse(is.na(percentage_of_members_played_with_excessively), 0, percentage_of_members_played_with_excessively))

    ### Percentage of members played with before feature
    print("Percentage of members played with before feature")
    tmp_2 <- tmp %>%
        group_by(avatar) %>%
        summarise(percentage_of_members_played_with_before = n()/number_of_guild_members[1])
    features <- left_join(features, tmp_2, by = "avatar") %>%
        mutate(percentage_of_members_played_with_before = ifelse(is.na(percentage_of_members_played_with_before), 0, percentage_of_members_played_with_before))

    ### Number of friends feature
    print("Number of friends feature")
    tmp_2 <- tmp %>%
        group_by(avatar) %>%
        summarise(number_of_friends = n())
    features <- left_join(features, tmp_2, by = "avatar") %>%
        mutate(number_of_friends = ifelse(is.na(number_of_friends), 0, number_of_friends))

    ### Number of friends already having quit guild feature
    print("Number of friends already having quit guild feature")

    # get avatars' guild and friends
    avatars_guild_and_friends <- tmp %>% select(avatar, guild, node_2)
    avatars_guild_and_friends$friend <- avatars_guild_and_friends$node_2
    avatars_guild_and_friends$node_2 <- NULL

    # get avatars' exguilds
    exguilds <- train_data %>%
        filter(event == "Guild Changed" | event == "Guild Left") %>%
        select(avatar, prev_guild) %>%
        distinct(avatar, prev_guild, .keep_all = TRUE)
    exguilds$guild <- as.numeric(exguilds$prev_guild)
    exguilds$prev_guild <- NULL
    # remove exguilds if an avatar rejoined its older guild and currently is still a member of it
    exguilds <- left_join(exguilds, features %>% select(avatar, guild, event_count), by = c("avatar", "guild"))
    exguilds <- exguilds %>% filter(is.na(event_count))
    exguilds$event_count <- NULL
    exguilds$friend <- exguilds$avatar
    exguilds$avatar <- NULL

    # quitted friends
    quitted_friends <- inner_join(avatars_guild_and_friends, exguilds, by = c("guild", "friend"))
    number_of_quitted_friends <- quitted_friends %>%
        group_by(avatar) %>%
        summarise(number_of_friends_already_having_quit_guild = n())

    # add to features by joining tables
    features <- left_join(features, number_of_quitted_friends, by = "avatar") %>%
        mutate(number_of_friends_already_having_quit_guild = ifelse(is.na(number_of_friends_already_having_quit_guild), 0, number_of_friends_already_having_quit_guild))


    print("Joining features and labels")
    features$guild <- NULL
    features_and_labels <- left_join(features, labels, by = "avatar")
    features_and_labels <- features_and_labels %>% mutate(label = ifelse(is.na(label), FALSE, label))  # label is FALSE for avatars that did not appear in the test period
    features_and_labels$pred_date <- pred_date
    features_and_labels$testset_end_date <- testset_end_date
    features_and_labels
}

# Random forest model
get_model_benchmark <- function(params = list()){
    model_name <- "benchmark"
    desc <- "The random forest benchmark model proposed in 'Modeling Destructive Group Dynamics in Online Gaming Communities'."
    model <- c()
    params <- list()


    build <- function(train_data) {
        print(paste("Building model:", model_name))

        targets <- factor(train_data$label)
        predictors <- train_data
        predictors$label <- NULL
        predictors$avatar <- NULL
        predictors$pred_date <- NULL
        predictors$testset_end_date <- NULL

        model <<- randomForest(
            x = predictors,
            y = targets,
            ntree = 10,  # as it is in the paper
            importance = TRUE
            #do.trace = TRUE
            #classwt = c(p_false, p_true)
            #sampsize = c(500, 500),
            #strata = targets
        )
        model
    }

    predict_ <- function(test_data){
        print("predicting...")
        predictions <- predict(model, test_data, type="prob")
        predictions[,2]
    }

    list(
        build = build,
        predict = predict_,
        model_name = function(){model_name},
        desc = function(){desc},
        params = function(){params},
        model = function(){model}
    )

}


##### MAIN #####

### Init, data import, utility functions
library(dplyr)
library(lubridate)
library(data.table)
library(randomForest)
library(pROC)
library(cvTools)
library(readr)

source("common/init.R")
source("common/streamline_functions_for_modeling.R")
source("common/model_evaluation.R")

### Get training dataset
print("Get train and test datasets")
training_data <- get_features_for_pred_dates(
    prediction_dates_train,
    create_graph,
    compute_features_and_labels)


### Do cross-validation on training data
print("Do cross-validation on training data")
predictions_train_cv <- do_cv(training_data, get_model_benchmark)
# Get optimal decision threshold:
opt_threshold_benchmark <- optimize(fscore_for_decision_threshold, c(0, 1.0), predictions_train_cv, maximum = TRUE, tol = 0.001)
results_cv <- get_perf_measures(predictions_train_cv$label, predictions_train_cv$prediction, opt_threshold_benchmark$maximum)


### Get performance measures on the final test set
print("Get performance measures on final test set")
test_data <- get_features_for_pred_dates(
    prediction_dates_test,
    create_graph,
    compute_features_and_labels)

benchmark_wrapper <- get_model_benchmark()
benchmark_model <- benchmark_wrapper$build(training_data)
predictions <- data.frame(
    avatar = test_data$avatar,
    pred_date = test_data$pred_date,
    label = test_data$label,
    prediction = benchmark_wrapper$predict(test_data))

results_test <- get_perf_measures(predictions$label, predictions$prediction, opt_threshold_benchmark$maximum)

### Write results to files
write_results_to_file(predictions_train_cv, results_cv, paste("guild_quitting/", "benchmark", "/train_cv", sep = ""))
write_results_to_file(predictions, results_test, paste("guild_quitting/", (get_model_benchmark())$model_name(), "/test", sep = ""))
