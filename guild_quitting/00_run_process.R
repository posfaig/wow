##########################################################################
###
### Run the Whole Process of Guild Quitting Event Prediction
###
##########################################################################

### Evaluate benchmark method on the dataset
print(system.time(source("guild_quitting/benchmark/benchmark_implementation.R")))

### Get the general statistics of the dataset
print(system.time(source("common/general_dataset_stats.R")))

### Compute features for various time windows
# library(parallel)
# mclapply(1:15, function(x){
#     time_window <<- x
#     source("guild_quitting/01_compute_features_timewindow.R")
# })
sapply(1:15, function(x){
         time_window <<- x
         print(paste("Time window:", time_window))
         if (!file.exists(paste0("generated/tmp/guild_quitting/features_", time_window ,"-day_window_train.csv"))
             || !file.exists(paste0("generated/tmp/guild_quitting/features_", time_window ,"-day_window_test.csv"))){
            print(system.time(source("guild_quitting/01_compute_features_timewindow.R")))
         }
    })

### Simple XGBoost model on the training data
# Analyses and model learning (time window optimization, feature selection), with rolling forecasting origin resampling
print(system.time(source("guild_quitting/02_xgb_train.R")))

### Evaluation of the final XGBoost model on the test data
print(system.time(source("guild_quitting/03_xgb_test.R")))

### XGBoost with fuzzy clustering on the training data
# Analyses and model learning (fuzzy parameter tuning), with rolling forecasting origin resampling
print(system.time(source("guild_quitting/04_xgb_fcm_train.R")))

### Evaluation of the final XGBoost+Fuzzy clustering model on the test data
print(system.time(source("guild_quitting/05_xgb_fcm_test.R")))

### Create plots for paper
print(system.time(source("plots_for_papers/201701_conf/plots.R")))


