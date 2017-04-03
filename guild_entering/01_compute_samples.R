#################################################################x
###
### Compute Features
###
#################################################################x

#### Init ####
if (!exists("avatar_graph_time_window")){
    avatar_graph_time_window <- 30
}
prediction_time_window <- 7

print(paste("Current time window:", avatar_graph_time_window))

library(dplyr)
library(lubridate)
library(data.table)
library(pROC)
library(cvTools)
library(readr)
library(igraph)
library(tidyr)
library(igraph)
library(ggplot2)
library(data.table)

source("common/init.R")
source("guild_quitting/create_intraguild_sn_timewindow.R")
#source("common/streamline_functions_for_modeling.R")

#### Define prediction dates ####
prediction_dates_train <- as.character(seq(as.Date("2008-02-01"), as.Date("2008-10-25"), 7))
prediction_dates_train <- tail(prediction_dates_train, 5)  # TODO: torolni
prediction_dates_test <- as.character(seq(as.Date("2008-11-01"), as.Date("2008-12-25"), 7))

set.seed(0)

#### Add zone type column ####
zones <- read_csv(paste(data_dir, "zones.csv", sep = ""))
wow <- left_join(wow, zones %>%
                     select(Type, Zone_Name) %>%
                     mutate(zone_type = Type, zone = Zone_Name, Type = NULL, Zone_Name = NULL),
                 by = "zone")
wow <- as.data.table(wow)
rm(zones)



#### Compute Samples for a Given Prediction Date ####
compute_features_and_labels_pred_date <- function(data, pred_date){
    print("compute_features_and_labels --- start")
    pred_date <- as.Date(pred_date)
    testset_end_date <- pred_date + prediction_time_window

    # Keep only the data regarding known avatars
    #avatars_in_guilds <- intraguild_graphs$nodes
    #data <- data %>% filter(avatar %in% avatars_in_guilds$avatar)
    min_date <- min(data$current_date)

    ##### Compute labels for avatars
    test_data <- data %>% filter(current_date >= pred_date & current_date < testset_end_date)
    labels <- test_data %>%
        filter(event == "Guild Entered" | event == "Guild Changed") %>%
        group_by(avatar) %>%
        dplyr::slice(1) %>%
        group_by() %>%
        select(avatar,
               target_event_date = current_date,
               target_guild = guild,
               target_event = event,
               target_prev_guild = prev_guild)

    train_data <- data %>%
        filter(current_date < pred_date) %>%
        filter(current_date >= pred_date - avatar_graph_time_window)

    # get the avatars that are not guild members on the prediction date
    # out_of_guild_avatars <- train_data %>%
    #     group_by(avatar) %>%
    #     dplyr::slice(n()) %>%
    #     group_by() %>%
    #     filter(guild == -1) %>%  # ezt kiszedni?
    #     select(avatar, guild)


    ## Features: char, race, level, number of different guild the avatar had been member of
    features <- train_data %>%
        #filter(avatar %in% out_of_guild_avatars$avatar) %>%  # ezt kiszedni?
        group_by(avatar) %>%
        summarise(
            race = race[1],
            charclass = charclass[1],
            level = max(level),
            diff_guild_count = length(unique(guild[guild != -1])))

    ### ...
    ### Compute various features here...
    ### ...

    ##### Joining labels
    print("Joining features and labels")
    features_and_labels <- left_join(features, labels, by = "avatar")
    features_and_labels$pred_date <- pred_date
    features_and_labels$testset_end_date <- testset_end_date
    features_and_labels

    features_and_labels <- features_and_labels #%>% filter(!is.na(target_guild))
    features_and_labels <- features_and_labels %>% mutate(known_label = ifelse(
        !is.na(target_guild) & target_guild %in% unique(train_data$guild), TRUE, FALSE))

    ##### RETURN
    print("compute_features_and_labels --- return")
    features_and_labels
}

compute_features_and_labels <- function(pred_dates){
    dataset <- data.frame()

    datasets_list <- lapply(pred_dates, function(pred_date){
        print(paste("Computing features and labels for prediction date", pred_date))
        data_subset <- compute_features_and_labels_pred_date(wow, pred_date)
        data_subset
    })
    datasets_list

    #concat result datasets
    lapply(datasets_list, function(data_subset){
        if (nrow(dataset) == 0){
            dataset <<- data_subset
        } else {
            dataset <<- rbind(dataset, data_subset)
        }
        c()
    })
    dataset
}

#data_train <- compute_features_and_labels(prediction_dates_train)
#data_test <- compute_features_and_labels(prediction_dates_test)
dataset <- compute_features_and_labels(c(prediction_dates_train, prediction_dates_test))
