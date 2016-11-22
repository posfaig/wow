#
set.seed(0)

library(readr)
library(caret)

##### MAIN #####

### Helper funcitons
source("guild_quitting/models.R")
source("common/streamline_functions_for_modeling.R")
source("common/model_evaluation.R")
source("common/util.R")

### Get training dataset
print("Get train dataset")
train_data <- read_csv("guild_quitting/features_train.csv")
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
                               "collaboration_3wise",
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

featurePlot(x = train_data[, c("collaboration_3wise",
                               "collaboration_5wise",
                               "collaboration_10wise",
                               "collaboration_20wise",
                               "collaboration_40wise")],
            y = factor(train_data$label),
            plot = "box",
            ## Pass in options to bwplot()
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),
            layout = c(5, 1),
            auto.key = list())
