######################################################################
###
### Get the general statistics of the datasets
###
######################################################################

library(dplyr)

compute_dataset <- function(data, pred_date, testset_end_date, ...){
    print("compute_dataset")
    pred_date <- as.Date(pred_date)
    testset_end_date <- as.Date(testset_end_date)


    ##### Compute labels for avatars
    #
    # true: left the guild in the given period
    # false: not left the guild in the given period
    #
    test_data <- data %>% filter(current_date >= pred_date & current_date < testset_end_date)
    labels <- test_data %>%
        group_by(avatar) %>%
        summarise(label = (sum(event == "Guild Left" | event == "Guild Changed") > 0))

    #### Records
    train_data <- data %>% filter(current_date < pred_date)
    # get the avatars that are member of a guild at prediction date
    features <- train_data %>%
        group_by(avatar) %>%
        dplyr::slice(n()) %>%
        group_by() %>%
        filter(guild != -1) %>%
        select(avatar, guild)

    ##### Joining labels
    print("Joining features and labels")
    features_and_labels <- left_join(features, labels, by = "avatar")
    features_and_labels <- features_and_labels %>% mutate(label = ifelse(is.na(label), FALSE, label))  # label is FALSE for avatars that did not appear in the test period
    features_and_labels$pred_date <- pred_date
    features_and_labels$testset_end_date <- testset_end_date
    features_and_labels
}

get_dataset_stats <- function(dataset){
    result <- list()
    result[["records"]] <- nrow(dataset)
    result[["pred_dates"]] <- length(unique(dataset[["pred_date"]]))
    result[["guilds"]] <- length(unique(dataset[["guild"]]))
    result[["avatars"]] <- length(unique(dataset[["avatar"]]))
    result[["labels_true"]] <- sum(as.numeric(dataset[["label"]]) == 1)
    result[["labels_false"]] <- sum(as.numeric(dataset[["label"]]) == 0)
    result
}

source("common/init.R")
source("common/streamline_functions_for_modeling.R")

training_data <- get_features_for_pred_dates(
    prediction_dates_train,
    function(...){NULL},
    compute_dataset)
test_data <- get_features_for_pred_dates(
    prediction_dates_test,
    function(...){NULL},
    compute_dataset)

stats_train <- get_dataset_stats(training_data)
stats_test <- get_dataset_stats(test_data)

# Write stats to file
dir.create(file.path("generated/dataset_stats/"), showWarnings = FALSE, recursive = TRUE)
lines_train <- sapply(names(stats_train), function(x) {
    paste(x, ",", stats_train[[x]], sep = "")
})
lines_test <- sapply(names(stats_test), function(x) {
    paste(x, ",", stats_test[[x]], sep = "")
})
fileConn <- file("generated/dataset_stats/stats.txt")
writeLines(c("TRAINING DATASET", lines_train, "TEST DATASET", lines_test), fileConn)
close(fileConn)

