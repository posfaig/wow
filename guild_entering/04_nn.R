#################################################################
#
# Predicting with Neural Network
#
# Actually, calling this model a NN is quite exagarating.
# Since we have very few data, we only use a single-layer NN, where all the neurons share the same weights.
#
#################################################################

#### Feature-k #####
### Avatar Features
## Level
## A guild a hanyadik legconnectedebb guild ha rangsoroljuk eszerint a guild-eket
#   Simán csak hogy mennyire connected
#   Az egy tagra jutó connectedness
## A top-N legosszekotottebb avatarbol hany tartozik az aktualis guild-hez
#   Lehet nezni hogy a top-N bol hany db/arany tartozik a guild-hez ugy h az NA-kat is szamoljuk
#   Az arany nezheto ugyis hogy az NA-kat nem szamoljuk bele, csak annyival osztunk ahany tenyleges ertek van
#   Lehet kulon feature-kre szetbontani, hogy a top-1, top-2, stb, az ehhez a guildhez tartozik-e
### Guild Features
## Az előző N napban az új tag volt, hányan mentek el
## Guild mérete / Guild effective mérete / Guild méret aránya (Guild effective méret / sum (guild effective mérete))


source("guild_entering/01_compute_samples.R")
source("guild_entering/guild_entering_util.R")

NUM_GUILDS_ALL <- length(unique(wow$guild))
MAX_LEVEL <- max(wow$level)

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
rm(wow, interactions)
gc()
edges_symetric_all <- rbindlist(lapply(avatar_network_list, function(x){
    tmp <- x$edges_symetric
    tmp$pred_date <- x$pred_date
    tmp
}))

#### Gets the connectedness of avatars to guilds. ####
# The connection between an avatar and a guild constitutes of the sum of individual connections between the avatar and the members of the guild.
get_conn_between_avatars_and_guilds <- function(current_pred_date, avatar_network, known_guilds = NULL){
    if (!is.null(known_guilds)){
        known_guilds <- unique(known_guilds)
    }
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
        mutate(mean_degree_to_guild_rank = frank(mean_degree_to_guild) / NUM_GUILDS_ALL) %>%
        group_by()

    if (!is.null(known_guilds)){
        # exclude guilds that are not known (~did not appear in the training set)
        # note that although there are excluded guilds, the connectedness ratios and their ranks are kept as if we were to keep all guilds
        tmp2 <- tmp2 %>% filter(as.character(guild) %in% as.character(known_guilds))
    }

    tmp2 <- tmp2 %>%
        mutate(guild = paste0("guild_", sprintf("%04d", as.numeric(as.character(guild))), "_conn_rank"))

    #mutate(mean_degree_to_guild_max = max(mean_degree_to_guild),
        #          mean_degree_to_guild_max_guild = guild[which.max(mean_degree_to_guild)]) %>%

    tmp3 <- spread(
        as.data.table(tmp2)[, .(avatar, mean_degree_to_guild_rank, guild)],
        guild,
        mean_degree_to_guild_rank,
        fill = 0)

    if (!is.null(known_guilds)){
        rank_col_names <- tmp2$guild
        known_guilds_col_names <- paste0("guild_", sprintf("%04d", as.numeric(as.character(known_guilds))), "_conn_rank")
        new_cols <- setdiff(known_guilds_col_names, rank_col_names)
        # if the new columns have to be extended with additional columns of other known guilds
        # we compute the maximal guild rank for each sample and use that value for the new columns
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
        known_guilds <- unique(as.numeric(as.character(features$target_guild)))
    } else {
        known_guilds <- unique(as.numeric(as.character(known_guilds)))
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

    replicate_feature_for_each_guild <- function(features, current_feature, known_guilds){
        new_cols <- paste0("guild_", sprintf("%04d", as.numeric(as.character(known_guilds))), "_", current_feature)
        lapply(new_cols, function(new_col){
            features[[new_col]] <<- features[[current_feature]]
            c()
        })
        features[[current_feature]] <- NULL
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
    names(tidy_pred_df) <- c("pred_date", paste0("guild_",
                                                 sprintf("%04d", as.numeric(names(tidy_pred_df)[-1])), "_size_ratio"))

    # Join existing features and guild size features
    features <- add_feature(tidy_pred_df,
                            setdiff(names(tidy_pred_df), c("pred_date")),
                            by_columns = c("pred_date"))

    #### Get the guilds of top-connected avatars
    print("Get the guilds of top-connected avatars")
    top_n_edges <- 10
    top_edges <- edges_symetric_all %>% mutate(avatar = node_1) %>%
        filter(node_2_guild != -1 & pred_date %in% as.character(included_pred_dates)) %>%
        group_by(avatar, pred_date) %>%
        arrange(desc(weight)) %>%
        dplyr::slice(1:top_n_edges) %>%
        mutate(rank = paste0("top_", sprintf("%04d", seq_len(n())))) %>%
        select(avatar, weight, node_2_guild, rank, pred_date) %>%
        group_by() %>%
        mutate(pred_date = as.Date(pred_date),
               node_2_guild = as.character(node_2_guild))

    # # Tidy the new features
    # tidy_top_edges_guilds_df <- spread(
    #     top_edges %>% select(-weight) %>% mutate(rank = paste0(rank, "_edge_guild")),
    #     rank, node_2_guild)
    # tidy_top_edges_weights_df <- spread(
    #     top_edges %>% select(-node_2_guild) %>% mutate(rank = paste0(rank, "_edge_weight")),
    #     rank, weight)
    #
    # # Join existing features and guild size features
    # features <- add_feature(tidy_top_edges_guilds_df,
    #                         setdiff(names(tidy_top_edges_guilds_df), c("avatar", "pred_date")))
    # features <- add_feature(tidy_top_edges_weights_df,
    #                         setdiff(names(tidy_top_edges_weights_df), c("avatar", "pred_date")))
    # rm(tidy_top_edges_guilds_df, tidy_top_edges_weights_df)
    # gc()

    ### Get the ratios of guilds among the connected guilds of top edges
    print("Create variables for each guild indicating the ratio of the current guild among the top edges")
    # Compute the ratios of guilds among the top edges
    top_edges_guild_ratios <- top_edges %>%
        mutate(guild = node_2_guild, node_2_guild = NULL) %>%
        group_by(avatar, pred_date) %>%
        mutate(non_na_connections = n()) %>%
        group_by(avatar, pred_date, guild) %>%
        summarise(num_conns_to_guild = n(), ratio_conns_to_guild = n() / non_na_connections[1]) %>%
        #summarise(num_conns_to_guild = n(), ratio_conns_to_guild = n() / top_n_edges) %>%
        do({
            current_df <- .
            current_df <- current_df %>% select(guild, num_conns_to_guild, ratio_conns_to_guild)
            left_join(data.frame(guild = as.character(known_guilds), stringsAsFactors = FALSE), current_df, by = "guild") %>%
                mutate(num_conns_to_guild = ifelse(is.na(num_conns_to_guild), 0, num_conns_to_guild)) %>%
                mutate(ratio_conns_to_guild = ifelse(is.na(ratio_conns_to_guild), 0, ratio_conns_to_guild))
        }) %>%
        group_by() %>%
        filter(as.numeric(as.character(guild)) %in% known_guilds)

    ## Tidy the new features
    # ratio_conns_to_guild
    top_edges_percent_to_guild_df <- spread(
        top_edges_guild_ratios %>%
            select(-num_conns_to_guild)
            %>% mutate(guild = paste0("guild_", sprintf("%04d", as.numeric(as.character(guild))), "_top_edges_percent")),
        guild, ratio_conns_to_guild)
    # num_conns_to_guild
    # top_edges_percent_to_guild_df <- spread(
    #     top_edges_guild_ratios %>% select(-ratio_conns_to_guild) %>% mutate(guild = paste0("top_edges_num_to_guild_", sprintf("%04d", as.numeric(guild)))),
    #     guild, ratio_conns_to_guild)

    # Join with existing features
    features <- add_feature(top_edges_percent_to_guild_df,
                            setdiff(names(top_edges_percent_to_guild_df), c("avatar", "pred_date")))
    rm(top_edges_guild_ratios, top_edges_percent_to_guild_df)
    gc()

    ### Get binary variables for the guilds of the top edges: the variable represents whether the current guild is the guild of the i-th most connected avatar
    print("Create binary variables for the top edges")
    max_rank_to_include <- 2  # for how many of the top edges should we create binary variables

    ## Tidy the new features
    included_rank_values <- paste0("top_", sprintf("%04d", 1:max_rank_to_include))
    binary_top_edge_guild_vars <- spread(
        top_edges %>%
            filter(as.numeric(as.character(node_2_guild)) %in% known_guilds) %>%
            filter(rank %in% included_rank_values) %>%
            mutate(guild = paste0("guild_", sprintf("%04d", as.numeric(as.character(node_2_guild))), "_binary_", rank)) %>%
            select(avatar, pred_date, guild) %>%
            mutate(dummy_var = TRUE),
        guild, dummy_var, fill = FALSE)
    # Add columns for missing guilds
    new_col_names <-
        as.character(
            sapply(included_rank_values,
                   function(x){
                       paste0("guild_",
                    sprintf("%04d", as.numeric(as.character(known_guilds))), "_binary_", x)
                })
        )
    new_col_names <- setdiff(new_col_names, names(binary_top_edge_guild_vars))

    if (length(new_col_names) > 0){
        lapply(new_col_names, function(new_col){
            binary_top_edge_guild_vars[[new_col]] <<- FALSE
            c()
        })
    }

    # Join with existing features
    features <- add_feature(binary_top_edge_guild_vars,
                            setdiff(names(binary_top_edge_guild_vars), c("avatar", "pred_date")))
    rm(binary_top_edge_guild_vars, top_edges)
    gc()

    # Replicate features for each guild
    features$level <- features$level / NUM_GUILDS_ALL
    features$diff_guild_count <- features$diff_guild_count / NUM_GUILDS_ALL
    features <- replicate_feature_for_each_guild(features, "level", known_guilds)
    features <- replicate_feature_for_each_guild(features, "diff_guild_count", known_guilds)

    features$target_guild <- factor(features$target_guild)

    # Keep only the samples from the test data that have a target label that appeared in the current training set
    features <- features %>% filter(as.character(target_guild) %in% unique(as.character(known_guilds)))

    features
}

source("guild_entering/models/guild_entering_xgb.R")

preprocess_dataset_for_mxnet <- function(data_to_preprocess, number_of_output_classes, number_of_features_per_output_class){
    data_to_preprocess$avatar <- NULL
    data_to_preprocess$pred_date <- NULL
    data_to_preprocess$known_label <- NULL
    data_to_preprocess$target_guild <- NULL
    data_to_preprocess$target_prev_guild <- NULL
    data_to_preprocess$target_event <- NULL
    data_to_preprocess$testset_end_date <- NULL
    data_to_preprocess$target_event_date <- NULL
    data_to_preprocess$race <- NULL
    data_to_preprocess$charclass <- NULL

    data_to_preprocess <- data_to_preprocess %>% select_(.dots = sort(names(data_to_preprocess)))
    head(data_to_preprocess)

    data_mx <- data.matrix(data_to_preprocess)
    data_array <- t(data_mx)
    dim(data_array) <-
        c(number_of_features_per_output_class,
          number_of_output_classes,
          1,
          nrow(data_mx))
    data_array
}





############## Single run (~ for unit test) #############
current_train_pred_date <- prediction_dates_train[2]
current_test_pred_date <- prediction_dates_train[3]
current_train_data <- get_samples_with_features_for_pred_date(dataset, current_train_pred_date)

sample_labels_train <- current_train_data$target_guild
sample_avatars_train <- current_train_data$avatar
sample_pred_dates_train <- current_train_data$pred_date

num_class <- length(unique(sample_labels_train))
num_features_per_class <- ncol(current_train_data) / num_class


current_train_data_array <- preprocess_dataset_for_mxnet(current_train_data, num_class, num_features_per_class)
current_test_data <- get_samples_with_features_for_pred_date(dataset, current_test_pred_date, FALSE, unique(as.character(sample_labels_train)))
sample_labels_test <- current_test_data$target_guild
sample_avatars_test <- current_test_data$avatar
sample_pred_dates_test <- current_test_data$pred_date
current_test_data_array <- preprocess_dataset_for_mxnet(current_test_data, num_class, num_features_per_class)

#### Construct a NN ####
library(mxnet)
# Create NN structure
data <- mx.symbol.Variable("data")
layer_1 <- mx.symbol.Convolution(
    data,
    name = "layer",
    kernel = c(num_features_per_class, 1),
    stride = c(num_features_per_class, 1),
    num.filter = 1
)
layer_1_reshape <- mx.symbol.Reshape(layer_1, shape = c(num_class, 0))
tanh_1 <- mx.symbol.Activation(data = layer_1_reshape, act_type = "tanh")
softmax_out <- mx.symbol.SoftmaxOutput(tanh_1, name = "softmax_output")

# Train NN
devices <- mx.cpu()
mx.set.seed(0)
system.time(
    nn_model <- mx.model.FeedForward.create(
        softmax_out,
        X = current_train_data_array,
        y = as.numeric(sample_labels_train),
        ctx = devices,
        num.round = 100,
        array.batch.size = 1,
        learning.rate = 0.07,
        momentum = 0.9)
)
print(graph.viz(nn_model$symbol))
print(nn_model)

preds_train <- predict(nn_model, current_train_data_array)
preds_test <- predict(nn_model, current_test_data_array)

# dim(preds)
# colSums(preds)[1:5]
# preds[,1:5]
# current_train_data_array[1:7, 1, 1, 1]
# current_train_data_array[1:7, 1, 1, 2]

### Compute log loss
# Train
preds_train_df <- data.frame(t(preds_train))
names(preds_train_df) <- paste0("pred_guild_", levels(sample_labels_train)[1:num_class])
log_loss <- get_multi_log_loss(preds_train_df, as.numeric(as.character(sample_labels_train)))
current_train_pred_date
log_loss
nrow(preds_train_df)
num_class
# Test
preds_test_df <- data.frame(t(preds_test))
names(preds_test_df) <- paste0("pred_guild_", levels(sample_labels_train)[1:num_class])
log_loss <- get_multi_log_loss(preds_test_df, as.numeric(as.character(sample_labels_test)))
current_test_pred_date
log_loss
nrow(preds_test_df)
num_class
###########################


### Wrap the whole into a resampling procedure
build_and_eval_model_for_pred_dates <- function(data_samples, current_train_pred_date, current_test_pred_date){
    # Get test data
    current_train_data <- get_samples_with_features_for_pred_date(dataset, current_train_pred_date)
    sample_labels_train <- current_train_data$target_guild
    sample_avatars_train <- current_train_data$avatar
    sample_pred_dates_train <- current_train_data$pred_date
    num_class <- length(unique(sample_labels_train))
    num_features_per_class <- ncol(current_train_data) / num_class
    current_train_data_array <- preprocess_dataset_for_mxnet(current_train_data, num_class, num_features_per_class)

    # Get test data
    current_test_data <- get_samples_with_features_for_pred_date(dataset, current_test_pred_date, FALSE, unique(as.character(sample_labels_train)))
    sample_labels_test <- current_test_data$target_guild
    sample_avatars_test <- current_test_data$avatar
    sample_pred_dates_test <- current_test_data$pred_date
    current_test_data_array <- preprocess_dataset_for_mxnet(current_test_data, num_class, num_features_per_class)

    # Train NN
    require(mxnet)

    # Create NN structure
    data <- mx.symbol.Variable("data")
    layer_1 <- mx.symbol.Convolution(
        data,
        name = "layer",
        kernel = c(num_features_per_class, 1),
        stride = c(num_features_per_class, 1),
        num.filter = 1
    )
    layer_1_reshape <- mx.symbol.Reshape(layer_1, shape = c(num_class, 0))
    tanh_1 <- mx.symbol.Activation(data = layer_1_reshape, act_type = "tanh")
    softmax_out <- mx.symbol.SoftmaxOutput(tanh_1, name = "softmax_output")

    # Train NN
    devices <- mx.cpu()
    mx.set.seed(0)
    system.time(
        nn_model <- mx.model.FeedForward.create(
            softmax_out,
            X = current_train_data_array,
            y = as.numeric(sample_labels_train),
            ctx = devices,
            num.round = 100,
            array.batch.size = 1,
            learning.rate = 0.07,
            momentum = 0.9)
    )
    print(graph.viz(nn_model$symbol))
    print(nn_model)

    preds_test <- predict(nn_model, current_test_data_array)

    ### Compute log loss
    preds_test_df <- data.frame(t(preds_test))
    names(preds_test_df) <- paste0("pred_guild_", levels(sample_labels_train)[1:num_class])
    # Get stats
    log_loss <- get_multi_log_loss(preds_test_df, as.numeric(as.character(sample_labels_test)))
    print(paste0("Log loss: ", log_loss))
    pred_stats <- data.frame(pred_date = current_test_pred_date,
                             log_loss = log_loss,
                             num_samples = nrow(preds_test_df),
                             num_classes = num_class)
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
write_guild_entering_results_to_file(pred_stats_by_pred_dates_df, "results_by_pred_date.csv", "guild_entering/nn_model/train")
write_guild_entering_results_to_file(pred_stats_by_pred_dates_test_df, "results_by_pred_date.csv", "guild_entering/nn_model/test")





