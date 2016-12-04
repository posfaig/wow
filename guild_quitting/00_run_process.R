##########################################################################
###
### Run the Whole Process of Guild Quitting Event Prediction
###
##########################################################################

### Compute features for various time windows
# library(parallel)
# mclapply(1:15, function(x){
#     time_window <<- x
#     source("guild_quitting/01_compute_features_timewindow.R")
# })
sapply(1:15, function(x){
         time_window <<- x
         print(paste("Time window:", time_window))
         print(system.time(source("guild_quitting/01_compute_features_timewindow.R")))
    })

### Simple XGBoost model on the training data
# Analyses and model learning (time window optimization, feature selection), with rolling forecasting origin resampling
print(system.time(source("guild_quitting/02_do_cv.R")))

### Evaluation of the final XGBoost model on the test data
print(system.time(source("guild_quitting/03_test_final_models.R")))

### XGBoost with fuzzy clustering on the training data
# Analyses and model learning (fuzzy parameter tuning), with rolling forecasting origin resampling
print(system.time(source("guild_quitting/04_do_cv_fuzzy_clust.R")))

### Evaluation of the final XGBoost+Fuzzy clustering model on the test data
print(system.time(source("guild_quitting/05_test_final_model_fuzzy_clust.R")))




