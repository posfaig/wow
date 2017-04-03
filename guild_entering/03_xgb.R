#################################################################
###
### Compute Features
###
#################################################################

source("guild_entering/01_compute_samples.R")
source("guild_entering/guild_entering_util.R")

#### Get the avatar network for a given prediction date ####
get_extended_graph_data <- function(current_pred_date){
    print(paste("Getting graph for pred date:", current_pred_date))
    graph_data <- create_intraguild_graphs(wow,
                                           current_pred_date,
                                           time_window = avatar_graph_time_window)

    vertices_df <- wow %>%
        filter(current_date < as.Date(current_pred_date)) %>%
        filter(current_date >= as.Date(current_pred_date) - avatar_graph_time_window) %>%
        group_by(avatar) %>%
        dplyr::slice(n()) %>% group_by() %>%
        select(avatar, guild) %>%
        left_join(dataset %>%
                      filter(pred_date == current_pred_date) %>%
                      select(avatar, target_guild))

    edges_symetric <- rbind(graph_data$edges, data.frame(
        node_1 = graph_data$edges$node_2,
        node_2 = graph_data$edges$node_1,
        weight = graph_data$edges$weight
    )) %>% mutate(node_1 = as.character(node_1), node_2 = as.character(node_2))

    # adding guilds to graph nodes
    edges_symetric <- edges_symetric %>%
        left_join(vertices_df %>% mutate(node_1 = avatar) %>% select(node_1, guild)) %>%
        mutate(node_1_guild = guild, guild = NULL) %>%
        left_join(vertices_df %>% mutate(node_2 = avatar) %>% select(node_2, guild)) %>%
        mutate(node_2_guild = guild, guild = NULL)

    list(graph_data = graph_data,
         vertices_df = vertices_df,
         edges_symetric = edges_symetric,
         pred_date = current_pred_date)
}

avatar_network_list <- sapply(c(prediction_dates_train, prediction_dates_test), function(current_pred_date){
    get_extended_graph_data(current_pred_date)
}, USE.NAMES = TRUE, simplify = FALSE)
edges_symetric_all <- rbindlist(lapply(avatar_network_list, function(x){
    tmp <- x$edges_symetric
    tmp$pred_date <- x$pred_date
    tmp
}))

#### Gets the connectedness of avatars to guilds. ####
# The connection between an avatar and a guild constitutes of the sum of individual connections between the avatar and the members of the guild.
get_conn_between_avatars_and_guilds <- function(current_pred_date, avatar_network, known_guilds = NULL){
    guild_sizes_on_pred_date <- get_guild_sizes_for_pred_date(current_pred_date)

    tmp2 <- avatar_network$edges_symetric %>% mutate(avatar = node_1) %>%
        filter(node_2_guild != -1) %>%
        mutate(guild = node_2_guild) %>%
        left_join(guild_sizes_on_pred_date) %>%
        mutate(node_2_guild = NULL) %>%
        group_by(avatar, guild) %>%
        summarise(
            connected_guilds = n(),
            degree_to_guild = sum(weight),
                  effective_size = effective_size[1],
                  mean_degree_to_guild = ifelse(effective_size[1] < 0.01, 0, sum(weight) / effective_size[1])
                  ) %>%
        mutate(mean_degree_to_guild_rank = frank(mean_degree_to_guild)) %>%
        group_by()

    if (!is.null(known_guilds)){
        # exclude guilds that are not known (~did not appear in the training set)
        # note that although there are excluded guilds, the connectedness ratios and their ranks are kept as if we were to keep all guilds
        tmp2 <- tmp2 %>% filter(as.character(guild) %in% as.character(known_guilds))
    }

    tmp2 <- tmp2 %>%
        mutate(guild = paste0("conn_to_guild_", guild))

    #mutate(mean_degree_to_guild_max = max(mean_degree_to_guild),
        #          mean_degree_to_guild_max_guild = guild[which.max(mean_degree_to_guild)]) %>%

    tmp3 <- spread(
        as.data.table(tmp2)[, .(avatar, mean_degree_to_guild_rank, guild)],
        guild,
        mean_degree_to_guild_rank,
        fill = 0)

    if (!is.null(known_guilds)){
        rank_col_names <- tmp2$guild
        known_guilds_col_names <- paste0("conn_to_guild_", known_guilds)
        new_cols <- setdiff(known_guilds_col_names, rank_col_names)
        if (length(new_cols) > 0){
            max_ranks_of_samples <- apply(
                as.matrix(tmp3 %>% select_(.dots = rank_col_names)),
                1, max)
            lapply(new_cols, function(new_col){
                tmp3[[new_col]] <<- max_ranks_of_samples
                c()
            })
        }
    }

    # tmp4 <- merge(tmp3,
    #               tmp2 %>%
    #                   group_by(avatar) %>%
    #                   summarise(mean_degree_to_guild_max = max(mean_degree_to_guild),
    #                          mean_degree_to_guild_max_guild = guild[which.max(mean_degree_to_guild)]) %>%
    #                   group_by(),
    #               by = "avatar",
    #               all.x = TRUE)
    # tmp4$mean_degree_to_guild_max_guild <- factor(tmp4$mean_degree_to_guild_max_guild)
    tmp4 <- tmp3

    tmp4$pred_date <- current_pred_date
    tmp4
}


# Get samples: takes the original samples and extends them with additional features
get_samples_with_features_for_pred_date <- function(samples, current_pred_date, include_earlier_samples = TRUE, known_guilds = NULL){
    current_pred_date_str <- as.character(current_pred_date)
    current_pred_date <- as.Date(current_pred_date)
    included_pred_dates <- unique(as.data.table(samples)[(pred_date == current_pred_date | (pred_date < current_pred_date & include_earlier_samples)) &
                                                      !is.na(target_guild), pred_date])
    print("Included pred dates:")
    print(included_pred_dates)
    print("Class of pred dates:")
    print(class(included_pred_dates))

    features <- as.data.table(samples)[pred_date %in% included_pred_dates & !is.na(target_guild)]

    if (is.null(known_guilds)){
        known_guilds <- unique(features$target_guild)
    }

    print(paste("Number of known guilds:", length(known_guilds)))

    add_feature <- function(new_feature, new_feature_col_names, by_columns = c("avatar", "pred_date"), na_value = 0){
        new_feature <- new_feature %>% select_(.dots = c(by_columns, new_feature_col_names))
        features <- left_join(features, new_feature, by = by_columns)
        sapply(new_feature_col_names, function(new_feature_col_name){
            features[[new_feature_col_name]][is.na(features[[new_feature_col_name]])] <<- na_value
        })
        features
    }

    #### Guild connectedness features
    guild_connectednesses <- get_conn_between_avatars_and_guilds(current_pred_date, avatar_network_list[[current_pred_date_str]], known_guilds)
    guild_connectednesses$pred_date <- as.Date(current_pred_date)

    features <- add_feature(guild_connectednesses, setdiff(names(guild_connectednesses), c("avatar", "pred_date", "mean_degree_to_guild_max_guild")))
    # features <- add_feature(guild_connectednesses %>%
    #                             mutate(mean_degree_to_guild_max_guild = as.character(mean_degree_to_guild_max_guild))
    #                         , "mean_degree_to_guild_max_guild", na_value = "no_connection")
    # features$mean_degree_to_guild_max_guild <- factor(features$mean_degree_to_guild_max_guild)

    #### Guild sizes: effective guild size ratio for given prediction date
    # Get the current size of the guilds

    guild_sizes_for_pred_date_list <- lapply(included_pred_dates, get_guild_sizes_for_pred_date, known_guilds)
    guild_sizes_for_pred_date_df <- rbindlist(guild_sizes_for_pred_date_list)

    # Compute effecitve guild size ratios
    guild_size_ratios_for_pred_date <-
        guild_sizes_for_pred_date_df[
            ,
            .(guild, effective_size_ratio = effective_size / sum(effective_size)),
            by = pred_date
            ]

    # Tidy the new features
    tidy_pred_df <- guild_size_ratios_for_pred_date %>%
        group_by(pred_date) %>%
        do({
            spread(., guild, effective_size_ratio, fill = 0)
        }) %>%
        group_by()
    names(tidy_pred_df) <- c("pred_date", paste0("size_ratio_guild_", names(tidy_pred_df)[-1]))

    # Join existing features and guild size features
    features <- add_feature(tidy_pred_df,
                            setdiff(names(tidy_pred_df), c("pred_date")),
                            by_columns = c("pred_date"))

    #### Get the guilds of top-connected avatars
    top_n_edges <- 10
    top_edges <- edges_symetric_all %>% mutate(avatar = node_1) %>%
        filter(node_2_guild != -1 & pred_date %in% as.character(included_pred_dates)) %>%
        group_by(avatar, pred_date) %>%
        arrange(desc(weight)) %>%
        dplyr::slice(1:top_n_edges) %>%
        mutate(rank = paste0("top_", seq_len(n()))) %>%
        select(avatar, weight, node_2_guild, rank, pred_date) %>%
        group_by() %>%
        mutate(pred_date = as.Date(pred_date),
               node_2_guild = as.character(node_2_guild))

    # Tidy the new features
    tidy_top_edges_guilds_df <- spread(
        top_edges %>% select(-weight) %>% mutate(rank = paste0(rank, "_edge_guild")),
        rank, node_2_guild)
    tidy_top_edges_weights_df <- spread(
        top_edges %>% select(-node_2_guild) %>% mutate(rank = paste0(rank, "_edge_weight")),
        rank, weight)

    # Join existing features and guild size features
    features <- add_feature(tidy_top_edges_guilds_df,
                            setdiff(names(tidy_top_edges_guilds_df), c("avatar", "pred_date")))
    features <- add_feature(tidy_top_edges_weights_df,
                            setdiff(names(tidy_top_edges_weights_df), c("avatar", "pred_date")))

    features$target_guild <- factor(features$target_guild)
    features
}

source("guild_entering/models/guild_entering_xgb.R")

# current_train_pred_date <- prediction_dates_train[1]
# current_test_pred_date <- prediction_dates_train[2]
# current_train_data <- get_samples_with_features_for_pred_date(dataset, current_train_pred_date)
# xgb_wrapper <- get_model_xgb_for_guild_entering()
# xgb_wrapper$build(current_train_data)

### Wrap the whole into a resampling procedure
build_and_eval_model_for_pred_dates <- function(data_samples, current_train_pred_date, current_test_pred_date){
    # Train model
    current_train_data <- get_samples_with_features_for_pred_date(data_samples, current_train_pred_date)
    xgb_wrapper <- get_model_xgb_for_guild_entering()
    xgb_wrapper$build(current_train_data)

    # Assess feature importance
    #features_by_importance <- xgb.importance(feature_names = (xgb_wrapper$params())$final_predictor_column_names, model = xgb_wrapper$model())
    #feature_names_ordered <- features_by_importance$Feature
    #feature_names_ordered

    # Get test data
    current_test_data <- get_samples_with_features_for_pred_date(data_samples, current_test_pred_date, FALSE, unique(as.character(current_train_data$target_guild)))
    current_test_data <- as.data.table(current_test_data)
    # Keep only the samples from the test data that have a target label that appeared in the current training set
    current_test_data_with_known_target <-
        current_test_data[as.character(target_guild) %in% unique(as.character(current_train_data$target_guild))]

    # The feature sets of the training and test sets should be the same
    if (length(setdiff(names(current_train_data), names(current_test_data_with_known_target))) > 0 || length(setdiff(names(current_test_data_with_known_target), names(current_train_data))) > 0){
        print(setdiff(names(current_train_data), names(current_test_data_with_known_target)))
        print(setdiff(names(current_test_data_with_known_target), names(current_train_data)))
        stop("Feature set of the training and test data are not identical")
    }

    #setdiff(names(current_train_data), names(current_test_data_with_known_target))
    #setdiff(names(current_test_data_with_known_target), names(current_train_data))

    # Predict
    current_preds <- xgb_wrapper$predict(current_test_data_with_known_target)

    # Get stats
    log_loss <- get_multi_log_loss(current_preds, as.character(current_test_data_with_known_target$target_guild))
    print(paste0("Log loss: ", log_loss))
    pred_stats <- data.frame(pred_date = current_test_pred_date,
                             log_loss = log_loss,
                             num_samples = nrow(current_test_data_with_known_target),
                             num_classes = xgb_wrapper$params()$num_class)
    pred_stats
}

#### EVALUATE ON TRAINING SET ####
# Evaluate model on the training set
# Iterate through the prediction dates (except for the first one) and compute mean log loss
pred_stats_by_pred_dates <-
    lapply(seq_along(prediction_dates_train)[-1], function(i){
        build_and_eval_model_for_pred_dates(dataset, prediction_dates_train[i - 1], prediction_dates_train[i])
    })
pred_stats_by_pred_dates_df <- rbindlist(pred_stats_by_pred_dates)
par(mfrow = c(1, 3))
plot(pred_stats_by_pred_dates_df$pred_date, pred_stats_by_pred_dates_df$log_loss)
plot(pred_stats_by_pred_dates_df$pred_date, pred_stats_by_pred_dates_df$num_classes)
plot(pred_stats_by_pred_dates_df$pred_date, pred_stats_by_pred_dates_df$num_samples)
print("Correlation between log loss and the number of known guilds:")
cor(pred_stats_by_pred_dates_df$log_loss, pred_stats_by_pred_dates_df$num_classes)
print("Mean Log Loss on Training Data:")
print(mean(pred_stats_by_pred_dates_df$log_loss))

#### EVALUATE ON TEST SET ####
prediction_dates_test_extended <- c(tail(prediction_dates_train, 1), prediction_dates_test)
pred_stats_by_pred_dates_test <-
    lapply(
        seq_along(prediction_dates_test_extended)[-1], function(i){
            build_and_eval_model_for_pred_dates(dataset, prediction_dates_test_extended[i - 1], prediction_dates_test_extended[i])
        })
pred_stats_by_pred_dates_test_df <- rbindlist(pred_stats_by_pred_dates_test)
par(mfrow = c(1, 3))
plot(pred_stats_by_pred_dates_test_df$pred_date, pred_stats_by_pred_dates_test_df$log_loss)
plot(pred_stats_by_pred_dates_test_df$pred_date, pred_stats_by_pred_dates_test_df$num_classes)
plot(pred_stats_by_pred_dates_test_df$pred_date, pred_stats_by_pred_dates_test_df$num_samples)
print("Correlation between log loss and the number of known guilds:")
cor(pred_stats_by_pred_dates_test_df$log_loss, pred_stats_by_pred_dates_test_df$num_classes)
print("Mean Log Loss on Test Data:")
print(mean(pred_stats_by_pred_dates_test_df$log_loss))

#### Write benchmark results to files ####
write_guild_entering_results_to_file(pred_stats_by_pred_dates_df, "results_by_pred_date.csv", "guild_entering/simple_multiclass_model/train")
write_guild_entering_results_to_file(pred_stats_by_pred_dates_test_df, "results_by_pred_date.csv", "guild_entering/simple_multiclass_model/test")





current_train_data <- get_samples_with_features_for_pred_date(data_samples, current_train_pred_date)

