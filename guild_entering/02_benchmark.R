##############################################################################
#
### Benchmark:
#
# Predictions on a given prediction date are the same for all samples
# prediction_for_guild = softmax( guild_size / sum_of_the_size_of_all_known_guilds)
# Guild size: only those avatars are counted who appeared at least once in the previous avatar_graph_time_window days
# Known guilds: guilds that appeared in the training data, i.e. guilds that appeared as a target variable for the preceding prediction dates

#(the guilds who were seen during the avatar_graph_time_window days preceding the prediction date)
#
##############################################################################

source("guild_entering/01_compute_samples.R")
source("guild_entering/guild_entering_util.R")

get_predictions_for_pred_date <- function(current_pred_date, previous_pred_date, data_samples){

    # Get known guilds, i.e. guilds appearing in the current training set
    # known_guilds <- unique(wow[
    #     current_date < as.Date(previous_pred_date) &
    #         current_date >= as.Date(previous_pred_date) - avatar_graph_time_window &
    #         guild != -1,
    #     guild])
    known_guilds <- unique(
        as.data.table(data_samples)[pred_date < as.Date(current_pred_date) & !is.na(target_guild), target_guild]
    )
    print(paste0("Number of known guilds:", length(known_guilds)))
    #known_guilds <- as.data.table(data_samples)[pred_date == as.Date(previous_pred_date) & !is.na(target_guild), target_guild]

    # Get the current size of the guilds
    guild_sizes_for_pred_date <- get_guild_sizes_for_pred_date(current_pred_date)
    missing_known_guilds <- setdiff(as.numeric(known_guilds), guild_sizes_for_pred_date$guild)

    # If a known guild has size 0, add it to the class labels
    if (length(missing_known_guilds) > 0){
        guild_sizes_for_pred_date <-
            rbind(guild_sizes_for_pred_date,
                  data.frame(
                      guild = missing_known_guilds,
                      size = 0,
                      pred_date = current_pred_date,
                      effective_size = 0
                  ))
    }

    # Compute predictions: the softmax of the ratios of the sizes of the given guilds compared the the sum of the sizes of all known guilds
    guild_size_ratios_for_pred_date <-
        guild_sizes_for_pred_date[
            ,
            .(guild, effective_size, effective_size_ratio = effective_size / sum(effective_size))
            ][
                guild %in% known_guilds, .(guild, pred = softmax(effective_size_ratio))
                ]
    tidy_pred_df <- spread(guild_size_ratios_for_pred_date, guild, pred, fill = 0)
    names(tidy_pred_df) <- paste0("pred_guild_", names(tidy_pred_df))

    # Get samples with known guilds for the current prediction date
    data_samples <- as.data.table(data_samples)
    data_samples_for_pred_date_known_labels <-
        data_samples[known_label &
                         pred_date == as.Date(current_pred_date) &
                         target_guild %in% known_guilds, .(avatar, pred_date, target_guild)]

    # Join samples and predictions
    predictions_for_pred_date <-
        cbind(data_samples_for_pred_date_known_labels,
              tidy_pred_df)
    predictions_for_pred_date
}

#### BENCHMARK ON TRAINING ####
# Evaluate benchmark on the training set
# Iterate through the prediction dates (except for the first one) and compute mean log loss
pred_stats_by_pred_dates <-
    lapply(seq_along(prediction_dates_train)[-1], function(i){
        preds <- get_predictions_for_pred_date(prediction_dates_train[i], prediction_dates_train[i - 1], dataset)
        log_loss <- get_multi_log_loss(preds, preds$target_guild)
        list(pred_date = as.Date(prediction_dates_train[i]),
             log_loss = log_loss,
             num_samples = nrow(preds),
             num_classes = sum(startsWith(names(preds), "pred_guild_")))

    })
pred_stats_by_pred_dates_df <- rbindlist(pred_stats_by_pred_dates)
par(mfrow = c(1, 3))
plot(pred_stats_by_pred_dates_df$pred_date, pred_stats_by_pred_dates_df$log_loss)
plot(pred_stats_by_pred_dates_df$pred_date, pred_stats_by_pred_dates_df$num_classes)
plot(pred_stats_by_pred_dates_df$pred_date, pred_stats_by_pred_dates_df$num_samples)
cor(pred_stats_by_pred_dates_df$log_loss, pred_stats_by_pred_dates_df$num_classes)
# As can be seen the log loss and the number of known guilds are highly correlated

print("Benchmark Mean Log Loss on Training Data:")
print(mean(pred_stats_by_pred_dates_df$log_loss))

#### Write benchmark to files ####
write_guild_entering_results_to_file(pred_stats_by_pred_dates_df, "results_by_pred_date.csv", "guild_entering/benchmark/train")

#### BENCHMARK ON TEST ####
prediction_dates_test_extended <- c(tail(prediction_dates_train, 1), prediction_dates_test)
pred_stats_by_pred_dates_test <-
    lapply(
        seq_along(prediction_dates_test_extended)[-1], function(i){
            preds <- get_predictions_for_pred_date(prediction_dates_test_extended[i], prediction_dates_test_extended[i - 1], dataset)
            log_loss <- get_multi_log_loss(preds, preds$target_guild)
            list(pred_date = as.Date(prediction_dates_test_extended[i]),
                 log_loss = log_loss,
                 num_samples = nrow(preds),
                 num_classes = sum(startsWith(names(preds), "pred_guild_")))
        })
pred_stats_by_pred_dates_test_df <- rbindlist(pred_stats_by_pred_dates_test)
par(mfrow = c(1, 3))
plot(pred_stats_by_pred_dates_test_df$pred_date, pred_stats_by_pred_dates_test_df$log_loss)
plot(pred_stats_by_pred_dates_test_df$pred_date, pred_stats_by_pred_dates_test_df$num_classes)
plot(pred_stats_by_pred_dates_test_df$pred_date, pred_stats_by_pred_dates_test_df$num_samples)
cor(pred_stats_by_pred_dates_test_df$log_loss, pred_stats_by_pred_dates_test_df$num_classes)

print("Benchmark Mean Log Loss on Test Data:")
print(mean(pred_stats_by_pred_dates_test_df$log_loss))

#### Write benchmark to files ####
write_guild_entering_results_to_file(pred_stats_by_pred_dates_test_df, "results_by_pred_date.csv", "guild_entering/benchmark/test")
