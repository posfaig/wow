#########################################################
###
### Guild Entering-Related Helper Functions
###
#########################################################

#### Get Guild Sizes for a Given Prediction Date ####
get_guild_sizes_for_pred_date <- function(pred_date, known_guilds = NULL){
    if (is.null(known_guilds)){
        known_guilds_on_pred_date <-
            unique(wow[current_date < as.Date(pred_date) &
                           current_date >= as.Date(pred_date) - avatar_graph_time_window,
                       .(guild)])
    } else {
        known_guilds_on_pred_date <- data.table(guild = as.integer(known_guilds))
    }

    guild_sizes_1 <- wow[current_date < as.Date(pred_date), tail(.SD, 1), by = avatar
                         ][, .(size = .N), by = guild]
    guild_sizes_2 <- wow[(current_date < as.Date(pred_date)) & (current_date >= as.Date(pred_date) - avatar_graph_time_window),
                         tail(.SD, 1),
                         by = avatar
                         ][, .(effective_size = .N), by = guild]
    guild_sizes <- merge(known_guilds_on_pred_date, guild_sizes_1, all.x = TRUE)[
        , size := ifelse(is.na(size), 0, size)][
            , pred_date := pred_date]
    guild_sizes <- merge(guild_sizes, guild_sizes_2, all.x = TRUE)[
        , effective_size := ifelse(is.na(effective_size), 0, effective_size)][
            , pred_date := pred_date]
    guild_sizes
}


softmax <- function(x){
    exp(x) / sum(exp(x))
}

# Compute log loss for the predictions given for a prediction date
get_multi_log_loss <- function(predictions, actual, eps = 1e-15){

    cols_for_preds_of_actual_targets <- paste0("pred_guild_", actual)
    preds_for_target_guilds <-
        sapply(
            seq_len(nrow(predictions)),
            function(i){
                as.data.frame(predictions)[i, c(cols_for_preds_of_actual_targets[i])]
            })

    preds_for_target_guilds <- pmin(pmax(preds_for_target_guilds, eps), 1-eps)
    score <- -sum(log(preds_for_target_guilds))/nrow(predictions)
    score
}

# Write results and predictions to file
write_guild_entering_results_to_file <- function(results_df, file_name, model_dir) {

    dir.create(file.path(paste("generated/results/", model_dir ,"/", sep = "")), showWarnings = FALSE, recursive = TRUE)

    write.table(results_df,
                paste0("generated/results/", model_dir ,"/", file_name),
                append = FALSE,
                row.names = FALSE,
                col.names = TRUE,
                sep = ",",
                quote = FALSE)
}
