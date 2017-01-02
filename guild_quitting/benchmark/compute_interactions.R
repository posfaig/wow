########################################################################################################
###
###  Compute interactions (collaborations) between players
###  As it is described in the following paper: Modeling Destructive Group Dynamics in Online Gaming Communities
###
########################################################################################################

print("Compute interactions (collaborations) between players")
set.seed(0)
output_file <- "generated/tmp/benchmark/number_interactions_by_days.csv"
dir.create(file.path("generated/tmp/benchmark/"), showWarnings = FALSE, recursive = TRUE)

# Init, import data
library(dplyr)
library(lubridate)
library(data.table)
library(randomForest)
library(pROC)
library(cvTools)

data_dir <- "data/raw/"

wow <- tbl_df(fread(paste(data_dir, "wowah_data.csv", sep = "")))
names(wow) <- trimws(names(wow))
wow$race <- gsub(" ", "", wow$race, fixed = TRUE)
wow$charclass <- gsub(" ", "", wow$charclass, fixed = TRUE)

# Create a new column for identifying avatars
wow$avatar <- with(wow, paste(char, race, charclass, sep = "."))

# Other columns
wow$timestamp <- mdy_hms(wow$timestamp)
wow <- arrange(wow, timestamp)
wow$current_date <- as.Date(wow$timestamp)


# Remove cities, as collaborations in cities are ignored
horde_capitals <- c("Shattrath City", "Orgrimmar", "Silvermoon City", "Thunder Bluff", "Undercity", "Dalaran")
wow <- wow %>% filter(!(zone %in% horde_capitals))


# Go day by day and write interactions to file
lapply(
    unique(wow$current_date),
    function(cur_date){
        print(cur_date)
        tmp <- wow %>%
            filter(current_date == cur_date) %>%
            select(avatar, zone, timestamp)
        # co-occurence = timestamps are within a 5 min interval
        # --> since the time it takes a snapshots is around 4.5 minutes
        #print(nrow(tmp))
        if (nrow(tmp) == 0){
            return(NULL)
        }
        interactions <- inner_join(tmp, tmp, by = "zone") %>%
            filter(avatar.x != avatar.y & abs(as.numeric(difftime(timestamp.x, timestamp.y, units = "secs"))) < 5 * 60)
        #print(nrow(interactions))
        if (nrow(interactions) == 0){
            return(NULL)
        }
        # remove duplicates
        first_avatar <- ifelse(interactions$avatar.x < interactions$avatar.y, interactions$avatar.x, interactions$avatar.y)
        second_avatar <- ifelse(interactions$avatar.x >= interactions$avatar.y, interactions$avatar.x, interactions$avatar.y)
        interactions$avatar.x <- first_avatar
        interactions$avatar.y <- second_avatar
        interactions_with_collaboration_time <- interactions %>%
            group_by(avatar.x, avatar.y) %>%
            summarise(collaboration = n())
        interactions_with_collaboration_time$current_date <- cur_date
        interactions_with_collaboration_time

        if (file.exists(output_file)){
            write.table(interactions_with_collaboration_time, output_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
        } else {
            write.table(interactions_with_collaboration_time, output_file, append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
        }
        NULL
    }
)
