##########################################################################
###
### Run the Whole Process of Guild Quitting Event Prediction
###
##########################################################################

# library(parallel)
# mclapply(6:10, function(x){
#     time_window <<- x
#     source("guild_quitting/01_compute_features_timewindow.R")
# })

sapply(1:15, function(x){
         time_window <<- x
         print(paste("Time window:", time_window))
         print(system.time(source("guild_quitting/01_compute_features_timewindow.R")))
    })
