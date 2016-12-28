#
set.seed(0)

library(readr)
library(caret)

##### MAIN #####

### Helper funcitons
source("guild_quitting/model_xgb.R")
source("common/streamline_functions_for_modeling.R")
source("common/model_evaluation.R")
source("common/util.R")

# Get optimized parameters
opt_params <- read_csv("generated/results/guild_quitting/xgboost/train_cv/opt_params.csv")
time_window <- opt_params$value[opt_params$parameter == "time_window"]

### Get train dataset
train_data <- read_csv(paste("generated/tmp/guild_quitting/features_", time_window ,"-day_window_train.csv", sep = ""))
train_data$race <- factor(train_data$race)
train_data$charclass <- factor(train_data$charclass)
train_data$guild_created_recently <- factor(train_data$guild_created_recently)

train_data$avatar_guild_lvl_diff <- train_data$level - train_data$avg_level_in_guild

transparentTheme(trans = .4)
featurePlot(x = train_data[, c("avg_daily_act",
                               "member_for_days",
                               "friends_left_guild",
                               "level",
                               "collaboration_with_members")],
            y = factor(train_data$label),
            plot = "pairs",
            ## Add a key at the top
            auto.key = list()
            )

featurePlot(x = train_data[, c("avg_daily_act",
                               "member_for_days",
                               "friends_left_guild",
                               "level",
                               "collaboration_with_members")],
            y = factor(train_data$label),
            plot = "density",
            ## Pass in options to xyplot() to
            ## make it prettier
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            adjust = 1.5,
            pch = "|",
            layout = c(5, 1),
            auto.key = list())

featurePlot(x = train_data[, c("avg_daily_act",
                               "member_for_days",
                               "friends_left_guild",
                               "level",
                               "collaboration_with_members")],
            y = factor(train_data$label),
            plot = "box",
            ## Pass in options to bwplot()
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),
            layout = c(5, 1),
            auto.key = list())

featurePlot(x = train_data[, c("avg_daily_act",
                               "member_for_days",
                               "friends_left_guild",
                               "level",
                               "collaboration_with_members",
                               "avatar_guild_lvl_diff")],
            y = factor(train_data$label),
            plot = "box",
            ## Pass in options to bwplot()
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),
            layout = c(6, 1),
            auto.key = list())


featurePlot(x = train_data[, c("guild_members",
                               "density",
                               "clustering_coeff",
                               "avg_guild_member_activity",
                               "median_guild_member_activity",
                               "avg_daily_guild_activity",
                               "collaboration_10wise",
                               "avatar_guild_lvl_diff",
                               "members_left",
                               "new_members",
                               "largest_clique",
                               "max_cliques")],
            y = factor(train_data$label),
            plot = "box",
            ## Pass in options to bwplot()
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),
            layout = c(6, 2),
            auto.key = list())

featurePlot(x = train_data[, c("collaboration_10wise",
                               "collaboration_20wise",
                               "collaboration_25wise")],
            y = factor(train_data$label),
            plot = "box",
            ## Pass in options to bwplot()
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),
            layout = c(3, 1),
            auto.key = list())


fuzzy_clustering_columns <- c("density",
                              "guild_members",
                              "members_left",
                              "avg_daily_guild_activity",
                              "avg_guild_member_activity",
                              "avg_level_in_guild")

# Box plots
featurePlot(x = train_data[, fuzzy_clustering_columns],
            y = factor(train_data$label),
            plot = "box",
            ## Pass in options to bwplot()
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),
            layout = c(length(fuzzy_clustering_columns), 1),
            auto.key = list())

# t-tests
t_test_results <- lapply(fuzzy_clustering_columns, function(col_name){
    t.test((train_data[[col_name]])[train_data$label], (train_data[[col_name]])[!train_data$label])

})
lapply(t_test_results, function(x){x$p.value})

# ks-tests
ks_test_results <- lapply(fuzzy_clustering_columns, function(col_name){
    ks.test((train_data[[col_name]])[train_data$label], (train_data[[col_name]])[!train_data$label])

})
lapply(ks_test_results, function(x){x$p.value})

