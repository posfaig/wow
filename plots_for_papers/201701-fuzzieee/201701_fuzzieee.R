##########################################################################
###
### Creating the plots for manuscript to be submitted to Fuzz-ieee 2017
###
##########################################################################

output_dir_name <- "201701_fuzzieee"
library(dplyr)
library(readr)
library(ggplot2)
library(plotROC)
library(pROC)
library(RColorBrewer)

source("common/util.R")

get_palette <- function(palette_name, n = 4){
    pal_colors <- brewer.pal(n + 1, palette_name)
    pal_colors[-1]
}

get_theme <- function(){
    theme_bw() +
    theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24),
          axis.title.x=element_text(vjust=-1.5),
          axis.title.y=element_text(vjust=2.0),
          legend.text = element_text(size = 24),
          legend.title = element_text(size = 24))
}

# ### ROC, AUC
# # Training Set
# preds_train_benchmark <- read_csv("generated/results/guild_quitting/benchmark/train_cv/predictions.csv")
# preds_train_benchmark$name <- "Benchmark"
# preds_train_benchmark_auc <- as.numeric(roc(preds_train_benchmark$label, preds_train_benchmark$prediction)$auc)
# preds_train_benchmark$AUC <- as.numeric(roc(preds_train_benchmark$label, preds_train_benchmark$prediction)$auc)
# preds_train_xgb <- read_csv("generated/results/guild_quitting/xgboost/train_cv/predictions.csv")
# preds_train_xgb$name <- "XGBoost"
# preds_train_xgb_auc <- as.numeric(roc(preds_train_xgb$label, preds_train_xgb$prediction)$auc)
# preds_train_xgb$AUC <- as.numeric(roc(preds_train_xgb$label, preds_train_xgb$prediction)$auc)
# preds_train_xgb_fcm <- read_csv("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/predictions.csv")
# preds_train_xgb_fcm$name <- "XGB-FCM"
# preds_train_xgb_fcm_auc <- as.numeric(roc(preds_train_xgb_fcm$label, preds_train_xgb_fcm$prediction)$auc)
# preds_train_xgb_fcm$AUC <- as.numeric(roc(preds_train_xgb_fcm$label, preds_train_xgb_fcm$prediction)$auc)
#
# preds_all_train <- rbind(preds_train_benchmark, preds_train_xgb, preds_train_xgb_fcm)
# roc_train <- ggplot(preds_all_train, aes(d = label, m = prediction, group = name, color = name)) +
#     geom_roc(labels = FALSE, size.point = 0, alpha.line = .5) +
#     labs(x = expression("Specitivity"), y = "Sensitivity") +
#     #scale_color_brewer(guide = guide_legend(title = "Method"), palette="Set1") +
#     #scale_color_manual(guide = guide_legend(title = "Method"), values=c("darkorange", "darkred", "darkgreen")) +
#     scale_color_brewer(guide = guide_legend(title = "Method"), palette="Dark2") +
#     theme_bw() +
#     annotate("text", x = 0.6, y = c(0.3, 0.4, 0.5), colour = "steelblue",
#              label = paste("AUC =", round(c(preds_train_benchmark_auc, preds_train_xgb_auc, preds_train_xgb_fcm_auc), 5)))
# roc_train
# save_plot(roc_train, output_dir_name, "roc_train")
#
# # Test Set
# preds_test_benchmark <- read_csv("generated/results/guild_quitting/benchmark/test/predictions.csv")
# preds_test_benchmark$name <- "Benchmark"
# preds_test_benchmark$AUC <- as.numeric(roc(preds_test_benchmark$label, preds_test_benchmark$prediction)$auc)
# preds_test_benchmark_auc <- as.numeric(roc(preds_test_benchmark$label, preds_test_benchmark$prediction)$auc)
# preds_test_xgb <- read_csv("generated/results/guild_quitting/xgboost/test/predictions.csv")
# preds_test_xgb$name <- "XGBoost"
# preds_test_xgb$AUC <- as.numeric(roc(preds_test_xgb$label, preds_test_xgb$prediction)$auc)
# preds_test_xgb_auc <- as.numeric(roc(preds_test_xgb$label, preds_test_xgb$prediction)$auc)
# preds_test_xgb_fcm <- read_csv("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/test/predictions.csv")
# preds_test_xgb_fcm$name <- "XGB-FCM"
# preds_test_xgb_fcm$AUC <- as.numeric(roc(preds_test_xgb_fcm$label, preds_test_xgb_fcm$prediction)$auc)
# preds_test_xgb_fcm_auc <- as.numeric(roc(preds_test_xgb_fcm$label, preds_test_xgb_fcm$prediction)$auc)
#
# preds_all_test <- rbind(preds_test_benchmark, preds_test_xgb, preds_test_xgb_fcm)
# roc_test <- ggplot(preds_all_test, aes(d = label, m = prediction, group = name, color = name)) +
#     geom_roc(labels = FALSE, size.point = 0, alpha.line = .3) +
#     labs(x = expression("Specitivity"), y = "Sensitivity") +
#     scale_color_brewer(guide = guide_legend(title = "Method"), palette="Dark2") +
#     theme_bw() +
#     annotate("text", x = 0.6, y = c(0.3, 0.4, 0.5), colour = "steelblue",
#          label = paste("AUC =", round(c(preds_test_benchmark_auc, preds_test_xgb_auc, preds_test_xgb_fcm_auc), 5)))
# roc_test
# save_plot(roc_test, output_dir_name, "roc_test")


### Accuracy, precision, recall, f-score
# Training Set
results_train_benchmark <- read_csv("generated/results/guild_quitting/benchmark/train_cv/results.csv")
results_train_benchmark$name <- "Benchmark"
results_train_xgb <- read_csv("generated/results/guild_quitting/xgboost/train_cv/results.csv")
results_train_xgb$name <- "XGBoost"
results_train_xgb_fcm <- read_csv("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/results.csv")
results_train_xgb_fcm$name <- "XGB-FCM"

results_all_train <- rbind(results_train_benchmark, results_train_xgb, results_train_xgb_fcm)
results_all_train <- results_all_train %>%
    mutate(metric = ifelse(metric == "accuracy", "Accuracy", metric)) %>%
    mutate(metric = ifelse(metric == "precision", "Precision", metric)) %>%
    mutate(metric = ifelse(metric == "recall", "Recall", metric)) %>%
    mutate(metric = ifelse(metric == "f_score", "F-Score", metric))

metrics_train <- results_all_train %>%
    filter(metric != "auc") %>%
    mutate(name = factor(name, levels=c("Benchmark", "XGBoost", "XGB-FCM"))) %>%
    mutate(metric = factor(metric, levels=c("Accuracy", "Precision", "Recall", "F-Score"))) %>%
    ggplot(aes(x = metric, y = round(value, 4))) +
    geom_bar(aes(fill = name), stat = "identity", position = "dodge") +
    get_theme() +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "top") +
    labs(x = "Metric", y = "Value") +
    geom_text(aes(label = round(value, 4), color = name ), size = 8, angle = 45, vjust = -0.6, hjust = 0.15, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values=get_palette("Blues", 3), guide = guide_legend(title = ""), labels = c("Benchmark    ", "XGBoost    ", "XGB-FCM")) +
    scale_color_manual(values=get_palette("Blues", 3), guide = guide_legend(title = ""), labels = c("Benchmark    ", "XGBoost    ", "XGB-FCM")) +
    ylim(0,1)

metrics_train

#metrics_train + scale_fill_manual(values=rainbow(5))
#metrics_train + scale_fill_brewer(palette="Set3")
#metrics_train + scale_fill_manual(values=get_palette("Blues"))
#metrics_train + scale_fill_manual(values=get_palette("BuGn"))
#metrics_train + scale_fill_manual(values=get_palette("PuBuGn"))
#metrics_train + scale_fill_manual(values=get_palette("Oranges"))
#metrics_train + scale_fill_manual(values=get_palette("OrRd"))

save_plot(metrics_train, output_dir_name, "metrics_train")

# Test Set
results_test_benchmark <- read_csv("generated/results/guild_quitting/benchmark/test/results.csv")
results_test_benchmark$name <- "Benchmark"
results_test_xgb <- read_csv("generated/results/guild_quitting/xgboost/test/results.csv")
results_test_xgb$name <- "XGBoost"
results_test_xgb_fcm <- read_csv("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/test/results.csv")
results_test_xgb_fcm$name <- "XGB-FCM"

results_all_test <- rbind(results_test_benchmark, results_test_xgb, results_test_xgb_fcm)
results_all_test <- results_all_test %>%
    mutate(metric = ifelse(metric == "accuracy", "Accuracy", metric)) %>%
    mutate(metric = ifelse(metric == "precision", "Precision", metric)) %>%
    mutate(metric = ifelse(metric == "recall", "Recall", metric)) %>%
    mutate(metric = ifelse(metric == "f_score", "F-Score", metric))

metrics_test <- results_all_test %>%
    filter(metric != "auc") %>%
    mutate(name = factor(name, levels=c("Benchmark", "XGBoost", "XGB-FCM"))) %>%
    mutate(metric = factor(metric, levels=c("Accuracy", "Precision", "Recall", "F-Score"))) %>%
    ggplot(aes(x = metric, y = round(value, 4))) +
    geom_bar(aes(fill = name), stat = "identity", position = "dodge") +
    get_theme() +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "top") +
    labs(x = "Metric", y = "Value") +
    geom_text(aes(label = round(value, 4), color = name ), size = 8, angle = 45, vjust = -0.6, hjust = 0.15, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values=get_palette("Blues", 3), guide = guide_legend(title = ""), labels = c("Benchmark    ", "XGBoost    ", "XGB-FCM")) +
    scale_color_manual(values=get_palette("Blues", 3), guide = guide_legend(title = ""), labels = c("Benchmark    ", "XGBoost    ", "XGB-FCM")) +
    ylim(0,1)

metrics_test
save_plot(metrics_test, output_dir_name, "metrics_test")


### Impact of Time Window
time_window_values <- read_csv("generated/results/guild_quitting/xgboost/train_cv/auc_values_by_time_window.csv")
time_window_plot <- ggplot(time_window_values, aes(x = time_window, y = round(auc, 5))) +
    geom_line(color = "steelblue", size = 1.1) +
    geom_point(color = "steelblue", size = 2) +
    #geom_text(aes(label = round(auc, 5)), color = "steelblue", size = 3.0) +
    get_theme() +
    labs(x = expression(T["window"]*" [Days]"), y = "AUC") +
    #geom_text(aes(label = round(auc, 4)), size = 8, angle = 45, vjust = -0.2, hjust = -0.1, color = "darkgrey") +
    ylim(0.7975, .8115) + xlim(0,16)

time_window_plot
save_plot(time_window_plot, output_dir_name, "time_window_plot", width = 10, height = 6)

### Impact of Fuzzy Clustering Parameters


params_tuning <- read_csv("generated/results/guild_quitting/xgboost_with_fuzzy_clustering/train_cv/params_tuning.csv")

param_tuning_plot_auc <- params_tuning %>% filter(fuzziness > 2) %>% ggplot(aes(x = fuzziness, y = num_clusters)) +
    geom_tile(aes(fill = auc), color = "white") +
    get_theme() +
    scale_fill_distiller(palette = "Spectral",
                         guide = guide_colourbar(title = "AUC", barheight = 15),
                         limits=c(.811, .817)) +
    labs(x = "Fuzziness", y = "Number of Clusters")

param_tuning_plot_auc
save_plot(param_tuning_plot_auc, output_dir_name, "param_tuning_auc_plot")

param_tuning_plot_obj <- params_tuning %>% filter(fuzziness > 2) %>% ggplot(aes(x = fuzziness, y = num_clusters)) +
    geom_tile(aes(fill = objective_function), color = "white") +
    scale_fill_distiller(palette = "Spectral", guide = guide_colourbar(title = "Objective\nFunction", barheight = 15)) +
    #scale_fill_gradient(low="darkred", high="orange", guide = guide_colourbar(title = "Objective Function", barheight = 15)) +
    #scale_fill_gradient(guide = guide_colourbar(title = "Objective Function")) +
    get_theme() +
    labs(x = "Fuzziness", y = "Number of Clusters")
param_tuning_plot_obj
save_plot(param_tuning_plot_obj, output_dir_name, "param_tuning_obj_func_plot")
