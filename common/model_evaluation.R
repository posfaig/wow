##############################################################################################################
###
### Functions to evaluate the performance of models
###
##############################################################################################################

library(pROC)

# Get performance measures for a set of predictions and the corresponding target labels
get_perf_measures <- function(targets, predictions, decision_threshold = 0.5){

    # get performance measures
    roc_plot <- roc(targets, predictions)
    auc <- as.numeric(roc_plot$auc)
    plot(roc_plot)
    title(paste("Probs ROC - AUC =", auc))

    tp <- sum(targets == TRUE & predictions >= decision_threshold)
    tn <- sum(targets == FALSE & predictions < decision_threshold)
    fp <- sum(targets == FALSE & predictions >= decision_threshold)
    fn <- sum(targets == TRUE & predictions < decision_threshold)

    accuracy <- (tp + tn) / length(predictions)
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    f_score <- 2 * tp / (2 * tp + fp + fn)

    # print(paste("Accuracy:", accuracy))
    # print(paste("Precision:", precision))
    # print(paste("Recall:", recall))
    # print(paste("F-score:", f_score))
    # print(paste("AUC:", auc))

    list(accuracy = accuracy,
         precision = precision,
         recall = recall,
         f_score = f_score,
         auc = auc,
         roc_plot = roc_plot)
}

# Write results and predictions to file
write_results_to_file <- function(predictions_df, results, model_dir) {

    dir.create(file.path(paste("generated/results/", model_dir ,"/", sep = "")), showWarnings = FALSE, recursive = TRUE)

    predictions_df <- predictions_df %>%
        select(avatar, label, prediction) %>%
        mutate(label = as.numeric(label))
    write.table(predictions_df,
                paste("generated/results/", model_dir ,"/predictions.csv", sep = ""),
                append = FALSE,
                row.names = FALSE,
                col.names = TRUE,
                sep = ",",
                quote = FALSE)

    plot_roc <- results$roc_plot
    pdf(paste("generated/results/", model_dir ,"/roc_plot.pdf", sep = ""))
    plot(plot_roc)
    title(paste("ROC Curve - AUC = ", plot_roc$auc))
    dev.off()

    results$roc_plot <- NULL
    lines <- sapply(names(results), function(x) {
        paste(x, ",", results[[x]], sep = "")
    })

    fileConn <- file(paste("generated/results/", model_dir,"/results.csv", sep = ""))
    writeLines(c("metric,value", lines), fileConn)
    close(fileConn)
}


