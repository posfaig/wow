###################################################################
###
### Read the main dataset and perform some initial preprocessing
###
###################################################################

library(readr)
library(dplyr)
library(lubridate)
library(data.table)

data_dir <- "../../data/raw/"
interactions <- read_csv("benchmark/number_interactions_by_days.csv")
wow <- tbl_df(fread(paste(data_dir, "wowah_data.csv", sep = "")))
names(wow) <- trimws(names(wow))
wow$race <- gsub(" ", "", wow$race, fixed = TRUE)
wow$charclass <- gsub(" ", "", wow$charclass, fixed = TRUE)

# create a new column for identifying avatars
wow$avatar <- with(wow, paste(char, race, charclass, sep = "."))

# other columns
wow$timestamp <- mdy_hms(wow$timestamp)
wow <- arrange(wow, timestamp)
wow$current_date <- as.Date(wow$timestamp)

# create new column for identifying avatars that were created during the observed period (i.e. avatars who were seen at level 1, or level 55 for Death Knight avatars)
wow <- wow %>% group_by(avatar) %>% mutate(new_avatar = (min(level) == 1 | (min(level) == 55 & charclass[1] == "DeathKnight"))) %>% group_by

min_date <- min(wow$current_date)
max_date <- max(wow$current_date)

wow <- wow %>% group_by(avatar) %>% mutate(prev_guild = lag(guild)) %>% group_by
wow$prev_guild[is.na(wow$prev_guild)] <- -2
wow <- wow %>%
    mutate(event = ifelse(guild == prev_guild, "No Event", "Guild Changed")) %>%
    mutate(event = ifelse(event == "Guild Changed" & prev_guild == -1, "Guild Entered", event)) %>%
    mutate(event = ifelse(event == "Guild Changed" & guild == -1, "Guild Left", event)) %>%
    mutate(event = ifelse(prev_guild == -2, ifelse((guild != -1 & new_avatar), "Guild Entered", "No Event"), event))


### Prediction dates of training and test datasets
prediction_dates_train <- c("2008-02-01", "2008-03-01", "2008-04-01", "2008-05-01", "2008-06-01", "2008-07-01", "2008-08-01", "2008-09-01", "2008-10-01")
prediction_dates_test <- c("2008-11-01", "2008-12-01")





