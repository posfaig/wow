### EDA Focusing on Guilds

set.seed(0)
data_dir <- "data/raw/"

library(lubridate)
library(stringr)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(visNetwork)
library(scales)
library(lazyeval)

### From Thiago Balbo - Reading and converting data
rawwow <- tbl_df(fread(paste(data_dir,"wowah_data.csv",sep="")))
names(rawwow) <- trimws(names(rawwow))
rawwow$race <- gsub(" ", "", rawwow$race, fixed = TRUE)
rawwow$charclass <- gsub(" ", "", rawwow$charclass, fixed = TRUE)

# Sampling if needed
#u.chars <- sample(unique(rawwow$char), 1000) # used to get a small sample (in this case we work with all data)
u.chars <- sample(unique(rawwow$char), length(unique(rawwow$char)))
wow <- filter(rawwow, char %in% u.chars)
rm(rawwow, u.chars)

# Create a new column for identifying avatars
wow$avatar <- with(wow, paste(char, race, charclass, sep="."))

# Other columns
wow$timestamp <- mdy_hms(wow$timestamp)
wow <- arrange(wow, timestamp)
wow$current.date <- as.Date(wow$timestamp)
wow$hour <- hour(wow$timestamp)
wow$month <- format(wow$current.date, "%b")
wow$month.idx <- month(wow$current.date)
# Create a new column with the activation date of each avatar
wow <- wow %>% group_by(avatar) %>% mutate(activation.date = min(current.date), dsi = as.integer(difftime(current.date, activation.date, units = "days"))) %>% group_by()

# Create new column for identify avatars that were created during the observed period (i.e. avatars who were seen at level 1, or level 55 for Death Knight avatars)
wow <- wow %>% group_by(avatar) %>% mutate(new_avatar=(min(level)==1 | (min(level)==55 & charclass[1] == "DeathKnight"))) %>% group_by

# How many different guilds were observed?
wow %>% filter(guild >= 0) %>% summarise(n_distinct(guild))

# Number of avatars who were member of at least one guild
c(unique(wow %>% filter(guild>=0) %>% distinct(avatar) %>% summarise(in_guild=n())),
    length(unique(wow$avatar)) - unique(wow %>% filter(guild>=0) %>% distinct(avatar) %>% summarise(no_guild=n())))

# Percentage of avatars who were a member of a guild at least once by levels
# Ezt szebbre, meg meggyozodni h jol mukodik-e
# 55-os szinten kezd az egyik karakter, ezert van ott a drop!!
wow %>% group_by(level,avatar) %>%
    summarise(in_guild = max(guild)>=0, no_guild = max(guild)<0) %>%
    summarise(in_guild_percent = sum(in_guild)/length(unique(avatar)),
              no_guild_percent = sum(no_guild)/length(unique(avatar))) %>%
    ggplot(aes(x=level)) +
    geom_point(aes(y=in_guild_percent), color='steelblue') +
    geom_point(aes(y=no_guild_percent), color='red')


# Distribution of the number of different guilds avatars were part of
guilds_of_players <- wow %>% group_by(avatar) %>% summarise(number_of_guilds=n_distinct(guild)-1)
guilds_of_players <- guilds_of_players %>% arrange(desc(number_of_guilds))
summary(guilds_of_players$number_of_guilds)
ggplot(guilds_of_players, aes(x=number_of_guilds)) + geom_density()

# By looking at the players with the most different guilds, we can see, that there are short moments when a player leaves his guild, but reenters it immediately in the next snap, e.g.:
as.data.frame(wow %>% filter(avatar == guilds_of_players$avatar[2]) %>% select(avatar, timestamp, guild))[414:416,]
rm(guilds_of_players)

# At what level players enter their first guild - only for new avatars (i.e. seen at level 1)
#
level_at_entering_first_guild <- wow %>% filter(new_avatar & guild>=0) %>% group_by(avatar) %>% mutate(first_guild_entered_at_level = min(level)) %>% select(avatar, first_guild_entered_at_level)
summary(level_at_entering_first_guild$first_guild_entered_at_level)
ggplot(level_at_entering_first_guild, aes(x=first_guild_entered_at_level)) + geom_density()
rm(level_at_entering_first_guild)
# At what level players leave their first guild - only for new avatars (i.e. seen at level 1)
#

# Distribution of Statistics of Guilds
#
# Number members of guilds (Number of avatars who were a member of the guild at least once)
number_of_guild_members <- wow %>% filter(guild >= 0) %>% group_by(guild) %>% summarise(number_of_members=n_distinct(avatar))
summary(number_of_guild_members$number_of_members)
ggplot(number_of_guild_members, aes(x=number_of_members)) + geom_density() + xlim(1,50)
ggplot(number_of_guild_members, aes(x=number_of_members)) + geom_density()+ scale_y_continuous(trans="log10")
ggplot(number_of_guild_members, aes(x=log(number_of_members))) + geom_density()
head(sort(number_of_guild_members$number_of_members, decreasing = TRUE), 25)
rm(number_of_guild_members)

# Create prev_guild column: last observed guild of user. If no prev observation, then the value is -2.
wow <- wow %>% group_by(avatar) %>% mutate(prev_guild = lag(guild)) %>% group_by
wow$prev_guild[is.na(wow$prev_guild)] <- -2

# Create event column: 2: guild changed, 1: guild entered, -1: guild left, 0: otherwise
wow <- wow %>%
    mutate(event = ifelse(guild == prev_guild, 0, 2)) %>%
    mutate(event = ifelse(event==2 & prev_guild==-1, 1, event)) %>%
    mutate(event = ifelse(event==2 & guild==-1, -1, event)) %>%
    mutate(event = ifelse(prev_guild==-2, ifelse((guild != -1 & new_avatar), 1, 0), event))
summary(factor(wow$event))

### Compute statistics for guilds over time

min_date <- min(wow$current.date)
max_date <- max(wow$current.date)

# Auxiliary data frame variable
guild_members <- wow %>% group_by(avatar) %>% dplyr::slice(1) %>% group_by() %>% filter(current.date==min_date | !new_avatar)

time_step <- 60*24 # minutes
snap_times <- seq(as.POSIXlt(as.Date("2008-01-02")), as.POSIXlt(as.Date("2009-01-01")), time_step*60)

race_names <- unique(wow$race)
charclass_names <- unique(wow$charclass)

compute_guild_features <- function(current_time){
    feature_names <- c("number_of_members", "avg_level", "sd_level", "min_level", "max_level", race_names, charclass_names)
    values_for_mising_guilds <- c(0, 0, 0, 0, 0, rep(0, length(race_names) + length(charclass_names)))

    if (!is.null(guild_members) && nrow(guild_members) > 0){
        stats_df <- guild_members %>% group_by(guild) %>% summarise(
            number_of_members = length(level),
            avg_level = mean(level),
            sd_level = ifelse(number_of_members == 1, 0, sd(level)),
            min_level = min(level),
            max_level = max(level),
            Orc = sum(race == "Orc"),
            Tauren = sum(race == "Tauren"),
            Troll = sum(race == "Troll"),
            Undead = sum(race == "Undead"),
            BloodElf = sum(race == "BloodElf"),
            Rogue = sum(charclass == "Rogue"),
            Hunter = sum(charclass == "Hunter"),
            Warrior = sum(charclass == "Warrior"),
            Shaman = sum(charclass == "Shaman"),
            Warlock = sum(charclass == "Warlock"),
            Druid = sum(charclass == "Druid"),
            Priest = sum(charclass == "Priest"),
            Mage = sum(charclass == "Mage"),
            Paladin = sum(charclass == "Paladin"),
            DeathKnight = sum(charclass == "DeathKnight")
        )
        missing_guilds <- setdiff(unique(wow$guild), stats_df$guild)
    } else {
        missing_guilds <- unique(wow$guild)
    }

    if (!is.null(missing_guilds) && length(missing_guilds)>0){
        sapply(missing_guilds, function(missing_guild){
            stats_df <<- rbind(stats_df, c(missing_guild, values_for_mising_guilds))
        })
    }

    stats_df$time <- current_time
    stats_df
}

guild_stats_over_time <- data.frame()
row_index <- 1
system.time(
    for (current_time in snap_times){

        # getting the next block of records containing data up to the time of the next snap
        new_records <- wow[(row_index:nrow(wow)),] %>% filter(timestamp <= current_time)
        print(nrow(new_records))
        row_index <- row_index + nrow(new_records)

        if (!is.null(new_records) && nrow(new_records)>0){
            # keeping only the last record of each avatar in the current time interval
            guild_members <- rbind(guild_members, new_records)
            guild_members <- guild_members %>% group_by(avatar) %>% dplyr::slice(n()) %>% group_by()
        }
        #print(paste(row_index, nrow(wow), sep="/"))

        # compute guild stats for the time of the snap
        if (is.null(guild_stats_over_time) || nrow(guild_stats_over_time) == 0){
            guild_stats_over_time <- compute_guild_features(current_time)
        } else {
            guild_stats_over_time <- rbind(guild_stats_over_time, compute_guild_features(current_time))
        }
    }
)
rm(guild_members, new_records)

# not sure why the time column lost its class
guild_stats_over_time$time <- as.POSIXct(guild_stats_over_time$time, origin = '1970-01-01')
guild_stats_over_time$date <- as.Date(guild_stats_over_time$time)
guild_stats_over_time$guild <- as.character(guild_stats_over_time$guild)
names(guild_stats_over_time) <- gsub(" ", "", names(guild_stats_over_time), fixed = TRUE)

#
# # Average level of the members of guilds over time
# #
#

# Spaghetti plots
# TODO: ezt vmi fgv-re cserelni
guild_groups_breaks <- c(-2,-1,90,180,270,360,450,540)
guild_stats_over_time$guild_group <- cut(as.numeric(guild_stats_over_time$guild), breaks = guild_groups_breaks)
guilds_over_time_plot_avg_level <- list()
guild_stats_over_time %>% group_by(guild_group) %>% do({
    guilds_over_time_plot <- ggplot(data = ., aes(x=date))
    guilds_over_time_plot <- guilds_over_time_plot + geom_line(aes(y = number_of_members, group=guild, color=guild), size=1.2, alpha = 0.5)
    #print(guilds_over_time_plot)
    guilds_over_time_plot_avg_level[[length(guilds_over_time_plot_avg_level)+1]] <<- guilds_over_time_plot
    data.frame()
})
guilds_over_time_plot_avg_level

getAvgLevelPlotGuildsOverTime <- function(min.level, max.level){
    guild_stats_over_time %>% filter(level >= min.level & level <= max.level) %>%
        ggplot(aes(x=date)) +
        geom_line(aes(y = number_of_members, group=guild, color=guild), size=1.2, alpha = 0.5)
}

## Guild ID -1 (Avatars with No Guild)
getAvgLevelPlotGuildsOverTime(-1,-1)

## Guild ID [0,90]
getAvgLevelPlotGuildsOverTime(0,90)

## Guild ID [91,180]
getAvgLevelPlotGuildsOverTime(91,180)

## Guild ID [181,270]
getAvgLevelPlotGuildsOverTime(181,270)

## Guild ID [451,540]
getAvgLevelPlotGuildsOverTime(451,540)

## Guild ID [451,540]
getAvgLevelPlotGuildsOverTime(451,540)

## Guild ID [451,540]
getAvgLevelPlotGuildsOverTime(451,540)


guilds_over_time_plot <- ggplot(data = guild_stats_over_time %>% filter(guild %in% sample(unique(guild_stats_over_time$guild), 2)), aes(x=date))
#guilds_over_time_plot + geom_line(aes(y = log(number_of_members + 1), group=guild, color=guild), size=1.5, alpha = 0.5)
guilds_over_time_plot +
    geom_line(aes(y = number_of_members, group=guild, color=guild), size=1.2, alpha = 0.5) +
    geom_line(aes(y = avg_level, group=guild, color=guild))
guilds_over_time_plot + geom_line(aes(y = avg_level, group=guild, color=guild))
guilds_over_time_plot + geom_line(aes(y = sd_level, group=guild, color=guild))
guilds_over_time_plot + geom_line(aes(y = min_level, group=guild, color=guild))
guilds_over_time_plot + geom_line(aes(y = max_level, group=guild, color=guild))

#
# # Heatmaps
# guild_stats_over_time_sample <- guild_stats_over_time %>% filter(guild %in% sample(guild_stats_over_time$guild, 100))
# guilds_over_time_plot <- ggplot(data = guild_stats_over_time_sample, aes(x=date, y=guild))
# guilds_over_time_plot +
#     geom_tile(aes(fill = log(number_of_members + 1)), color="white") +
#     scale_fill_gradient(low = "white", high = "steelblue")
# guilds_over_time_plot +
#     geom_tile(aes(fill = avg_level), color="white") +
#     scale_fill_gradient(low = "white", high = "steelblue")
#
# # Sometimes quite heavy fluctuation --> Take a closer look on one of these guilds, e.g. guild 273
#

guild_id <- 273
current_guild_over_time <- guild_stats_over_time %>% filter(guild == guild_id)
guilds_over_time_plot <- ggplot(data = current_guild_over_time, aes(x=date))
guilds_over_time_plot +
    geom_line(aes(y = number_of_members, group=guild), color="steelblue") +
    geom_line(aes(y = avg_level, group=guild), color="red")
rm(current_guild_over_time)

# The fluctuation is evident

guilds_over_time_plot + geom_line(aes(y = avg_level, group=guild, color=guild))
guilds_over_time_plot + geom_line(aes(y = sd_level, group=guild, color=guild))
guilds_over_time_plot + geom_line(aes(y = min_level, group=guild, color=guild))
guilds_over_time_plot + geom_line(aes(y = max_level, group=guild, color=guild))

who_left <- wow %>% filter(prev_guild==guild_id & guild!=guild_id) %>% select(avatar) %>% distinct(avatar)
wow %>%
    filter(avatar %in% who_left$avatar) %>%
    select(avatar, guild, prev_guild, timestamp) %>%
    group_by(avatar) %>% do({
        rows <- which(.$prev_guild==guild_id & .$guild!=guild_id)
        if (length(rows) > 0){
            rows <- c(rows, rows-1, rows+1)
            rows <- sort(rows[rows > 0 & rows <= nrow(.)])
            print(cbind(row=rows, .[rows,]))
        }
        data.frame()
    })
rm(who_left)
# It can be seen there are temporal exits, where an avatar leaves his guild, but in the next step the avatar is already a member of it again.


# Take a look at the number of transitions by date and level
transitions_by_date <- tbl_df(data.frame(current.date = seq.Date(min_date, max_date, "days")))
transitions_by_date_full <- full_join(transitions_by_date, wow %>% filter(event != 0))
transitions_by_date_left <- left_join(transitions_by_date, wow %>% filter(event != 0))
transitions_by_date_inner <- inner_join(transitions_by_date, wow %>% filter(event != 0))
summary(transitions_by_date_full)
summary(transitions_by_date_left)
summary(transitions_by_date_inner)
transitions_by_date <- transitions_by_date %>% mutate(EventType = ifelse(event == -1, "Guild Left", ifelse(event == 1, "Guild Entered", "Guild Changed")))

ggplot(data = transitions_by_date, aes(x=current.date)) + geom_bar(aes(fill=EventType))
ggplot(data = transitions_by_date_left, aes(x=current.date)) + geom_bar(aes(fill=factor(event)))
ggplotly(ggplot(data = transitions_by_date_inner, aes(x=current.date)) + geom_bar(aes(fill=factor(event))))

# There is a huge peak. What is that date?
transitions_by_date2 <- transitions_by_date %>% group_by(current.date) %>% summarise(transitions = n())
transitions_by_date2[which(transitions_by_date2$transitions > 400), "current.date"]

# Quite strange. There might have been a particular event, I do not know.

rm(transitions_by_date, transitions_by_date2)

transitions_by_level <- wow %>%
	filter(event != 0) %>%
	mutate(EventType = ifelse(event == -1, "Guild Left", ifelse(event == 1, "Guild Entered", "Guild Changed")))
ggplot(data = transitions_by_level, aes(x=level)) + geom_bar(aes(fill=EventType))
rm(transitions_by_level)

# Érdekes: a max szinten van a sok csere,
# viszont ez amiatt is lehet, hogy egyszerűen a max szintű játékosok játszanak a legtöbbet: egyrészt sokat játszanak, másrészt nem is tudnak továbblépni.
# Nézzük meg a level-ek eloszlását:

# VS:
plot(density(wow$level))
ggplot(data = wow, aes(x=level)) + geom_density(aes(group = factor(event), color = factor(event)))
#ggplot(data = wow %>% filter(event != 0), aes(x=level)) + geom_density(aes(group = factor(event), color = factor(event)))

# Igen, tehát nem állíthatjuk, hogy a magasabb szintűek gyakrabban váltanak guild-et.
# (Ugyanakkor az a korábbi ábrán és itt is látható, hogy a nem nulla típusú event-ek arányai mások az also és a felső level-eknél)
# Nézzük meg chi-squared test-tel, hogy azt a nullhipotézist, hogy az event-ek eloszlása független-e a level-től:
# To do so we create a contingency table, on which we perform the chi squared test of independence:

#level_breaks <- seq(0, 80, 5)
level_breaks <- c(0, 2, 8, 15, 30, 45, 60, 69, 71, 78, 80)
wow$level_group <- cut(wow$level, breaks = level_breaks)

# average of the whole sample
prop.table(table(wow$event!=0))

tbl <- table((wow$event!=0), wow$level_group)

# averages by level groups
prop.table(tbl, 2)

# test for significant disproportions
chisq.test(tbl)

# That tells us that somewhere across the contingency table there is a disproportionate number of guild events across the level groups in one or more of the guild event types.
# Post-hoc tests with Bonferroni-corrected p-values
number_of_tests <- length(unique(wow$level_group))
tTestResults <- list()
for (current_group in unique(wow$level_group)){
    tbl <- table((wow$event!=0), factor(wow$level_group == current_group))
    tTestResults[[current_group]] <- chisq.test(tbl)
}
tTestCorrectedPValues <- lapply(tTestResults, function(x){x$p.value * number_of_tests})
sum(unlist(lapply(tTestCorrectedPValues, function(x){x>=0.05})))
lapply(tTestCorrectedPValues, function(x){paste("Null hypothesis rejected:",x<0.05,"(Bonferroni-corrected p-value:",x,")")})
#
# Graph of guild transitions
# TODO: Valami group-ot legördülő menübe beletenni: vagya  node-size metric-et, vagy a transition threshold-ot
#

nodes <- data.frame(id = unique(wow$guild))
edges <- ungroup(wow) %>% filter(event != 0) %>% select(avatar, level, level_group, prev_guild, guild)
names(edges) <- c("avatar", "level", "level_group", "from", "to")

plot_level_group <- function(min_level = 0, max_level = 5, transition_threshold_for_nodes = 5, transition_threshold_for_edges = 5, node_size_metric=c("entered", "left", "entered+left", "entered-left"), log_node_size = TRUE){
	edges <- edges %>% filter(level >= min_level, level <= max_level)

	# compute nodes' size
	entered_avatars <- edges %>% group_by(to) %>% summarise(entered = n())
	left_avatars <- edges %>% group_by(from) %>% summarise(left = n())
	nodes <- left_join(nodes, entered_avatars, by = c("id" = "to"))
	nodes <- left_join(nodes, left_avatars, by = c("id" = "from"))

	nodes$entered[is.na(nodes$entered)] <- 0
	nodes$left[is.na(nodes$left)] <- 0

	if (node_size_metric == "entered"){
		nodes$value <- nodes$entered
	} else if (node_size_metric == "left"){
		nodes$value <- nodes$left
	} else if (node_size_metric == "entered+left"){
		nodes$value <- nodes$entered + nodes$left
	} else if (node_size_metric == "entered-left"){
		nodes$value <- nodes$entered - nodes$left
	} else {
		stop("Unknown node size metric")
	}
	nodes$value <- abs(nodes$value)

	# compute edges' size
	edges <- edges %>% group_by(from, to) %>% summarise(transitions = n())
	edges$value <- edges$transitions
	#edges$value <- round(log(edges$transitions), 2)

	# node attributes
	nodes$label <- nodes$id
	nodes$title <- str_c("Entered: <b>", nodes$entered, "</b><br>Left: <b>", nodes$left, "</b>")
	#nodes$shape <- "triangleDown"
	#nodes$shape[nodes$entered == nodes$left] <- "dot"
	#nodes$shape[nodes$entered > nodes$left] <- "diamond"
	nodes$color <- "#D41313"
	nodes$color[nodes$entered == nodes$left] <- "#E4E63F"
	nodes$color[nodes$entered > nodes$left] <- "#17A019"

	# edge attributes
	edges$title <- paste(paste(edges$from, edges$to, sep=" -> "), edges$transitions, sep=" : ")
	edges$color <- "#618CC1"
	edges$color[edges$to == -1] <- "#DD5A5A"
	edges$color[edges$from == -1] <- "#61C163"

	# filter nodes and edges
	print(paste("All transitions:", sum(edges$transitions)))
	nodes <- nodes %>% filter(entered+left >= transition_threshold_for_nodes)
	edges <- edges %>% filter(from %in% nodes$id & to %in% nodes$id)
    edges <- edges %>% filter(value >= transition_threshold_for_edges)

	if (log_node_size){
		if (min(nodes$value == 0)){
			nodes$value <- nodes$value + 1
		}
		nodes$value <- log(nodes$value)
	}
	print(paste("Shown edges:",nrow(edges)))
	print(paste("Shown nodes:",nrow(nodes)))

	visNetwork(nodes, edges, width="100%") %>%
		visEdges(arrows ="to") #%>%
		#visNodes(shape = "ellipse")
		#visGroups(groupname = "left", color = "red") %>%
		#visGroups(groupname = "entered", color = "green") %>%
		#visGroups(groupname = "changed", color = "steelblue")

}

plot_level_group(
    min_level = 1,
    max_level = 5,
    transition_threshold_for_nodes = 10,
    transition_threshold_for_edges = 0,
    node_size_metric = "entered+left",
    log_node_size = TRUE)
plot_level_group(
    min_level = 6,
    max_level = 15,
    transition_threshold_for_nodes = 10,
    transition_threshold_for_edges = 0,
    node_size_metric = "entered+left",
    log_node_size = TRUE)
plot_level_group(
    min_level = 16,
    max_level = 30,
    transition_threshold_for_nodes = 15,
    transition_threshold_for_edges = 0,
    node_size_metric = "entered+left",
    log_node_size = TRUE)
plot_level_group(
    min_level = 31,
    max_level = 50,
    transition_threshold_for_nodes = 15,
    transition_threshold_for_edges = 0,
    node_size_metric = "entered+left",
    log_node_size = TRUE)
plot_level_group(
    min_level = 51,
    max_level = 65,
    transition_threshold_for_nodes = 20,
    transition_threshold_for_edges = 2,
    node_size_metric = "entered+left",
    log_node_size = TRUE)
plot_level_group(
    min_level = 66,
    max_level = 70,
    transition_threshold_for_nodes = 50,
    transition_threshold_for_edges = 5,
    node_size_metric = "entered+left",
    log_node_size = TRUE)
plot_level_group(
    min_level = 71,
    max_level = 75,
    transition_threshold_for_nodes = 5,
    transition_threshold_for_edges = 0,
    node_size_metric = "entered+left",
    log_node_size = TRUE)
plot_level_group(
    min_level = 76,
    max_level = 80,
    transition_threshold_for_nodes = 5,
    transition_threshold_for_edges = 0,
    node_size_metric = "entered+left",
    log_node_size = TRUE)

# Impact of user customs
# Impact of races and character classes
#

100*prop.table(table(wow$race, factor(wow$event)), margin = 1)

wow_groupped <- wow %>%
    filter(event != 0) %>%
    group_by(race, event) %>%
    summarise(count = n()) %>%
    mutate(percentage = count/sum(count))%>%
    group_by()

ggplot(wow_groupped, aes(x = race, y = percentage)) +
    geom_bar(aes(fill = factor(event)), stat = "identity", position = "dodge")
rm(wow_groupped)

100*prop.table(table(wow$charclass, factor(wow$event)), margin = 1)
wow %>%
    filter(event != 0) %>%
    group_by(charclass, event) %>%
    summarise(count = n()) %>%
    mutate(percentage = count/sum(count))%>%
    group_by() %>%
    ggplot(aes(x = charclass, y = percentage)) +
    geom_bar(aes(fill = factor(event)), stat = "identity", position = "dodge")

#visNetwork(nodes, edges)
# ehhez hasonlona
# Standard deviation of level of members of guilds
# Range of level of members of guilds

# TODO
# Number of records per day, vagy inkább: Number of records / number of players

# Guild lifetime - distribution of the length of intervals between first and last time a guild was seen
guild_lifetime <- wow %>%
    group_by(guild) %>%
    summarise(startDate = min(current.date), endDate = max(current.date)) %>%
    mutate(lifetime = as.numeric(endDate-startDate))
ggplot(guild_lifetime, aes(x = lifetime)) + geom_density()
guild_lifetime <- rbind(
    data.frame(guild = guild_lifetime$guild, date=guild_lifetime$startDate, type="start"),
    data.frame(guild = guild_lifetime$guild, date=guild_lifetime$endDate, type="end"))
guild_lifetime$guild <- factor(guild_lifetime$guild)
ggplot(guild_lifetime, aes(x=date, y=guild)) +
    geom_line(aes(color = guild)) +
    theme_bw() + theme(axis.text.y=element_blank(), legend.position="none") +
    labs(title = "Guild Lifetimes", x = "Date", y="Guild")
rm(guild_lifetime)

# What are the guilds that were created during the observed period?
# Guilds whose members either had been seen earlier as not being the member of the guild or are newly created avatars
wow <- wow %>%
    group_by(avatar) %>%
    mutate(this.is.first.guild = sapply(1:length(guild),function(x,y){length(unique(y[1:x])) == 1}, guild)) %>%
    group_by()

newGuilds <- wow %>%
    group_by(guild, avatar) %>%
    dplyr::slice(1) %>%
    group_by(guild) %>%
    summarise(new_guild = (sum(new_avatar | !this.is.first.guild) == length(new_avatar)))
summary(newGuilds$new_guild)

# When new guilds were created?
#
wow <- left_join(wow, newGuilds, by = "guild")
rm(newGuilds)
wow <- wow %>% group_by(guild) %>% mutate(guild_creation = c(new_guild[1], rep(FALSE, length(new_guild)-1))) %>% group_by()

tbl_df(data.frame(current.date = seq.Date(min_date, max_date, "days"))) %>%
full_join(wow %>% filter(guild_creation) %>% group_by(current.date) %>% summarise(n=n())) %>%
 mutate(n = ifelse(is.na(n), 0, n)) %>%
ggplot(aes(x = current.date, y=n)) + geom_bar(stat = "identity")

# How much does one avatar play?
#
wow <- wow %>% group_by(avatar) %>% mutate(records = n()) %>% group_by()
records_of_avatars <- wow %>% group_by(avatar) %>% summarise(records = n())
ggplot(records_of_avatars, aes(x=records)) + geom_density() # the majority of avatars played only few times, however, many others played a lot
ggplot(wow, aes(x=records)) + geom_density() + geom_vline(xintercept = 365, size = 1.5, alpha = 0.5, color = "steelblue") # most of the records belong to regular players
summary(records_of_avatars$records)
rm(records_of_avatars)

# Impact of previous guilds
# tesztek:
# # aki nincs guildben és még nem is volt vs. aki nincs guildben de már volt: ki lép be könnyebben?
# # aki guildben van és ez az első guild-je, ill. aki guildben van de már volt másik guildje, ki lép ki könnyebben? --> csak új karakterekre
# #
#

# First create a column indicating how many guilds had the avatar been member of till the current time?
# ~ 15 min
wow <- wow %>%
    group_by(avatar) %>%
    #mutate(this.is.first.guild = (guild == guild[1])) %>%
    mutate(number_of_joined_guilds =
               sapply(1:length(guild),
                      function(x,y){
                          tmp <- y[1:x]
                          tmp <- tmp[tmp != -1]
                          length(unique(tmp))},
                      guild)) %>%
    group_by()

# Filter avatars, take into consideration only the ones who had been recorded at least 365 times.
#
# If the avatar is a new avatar, then we can observe how much time did it take for him/her to join his/her first guild. We consider only the avatars that joined a guild at some point.
# For each avatar, for whom there are at least 2 observed guilds, we compute the length intervals the avatar spent between guilds. That is, how much time passed unti he/she joined his/her next guild.
# We compute the intervals in two units: time passed, and the number the avatar's of observed records during that time
#

min_number_of_observations <- 365
time_intervals_for_new_guild_members <- c()
time_intervals_for_ex_guild_members <- c()
record_intervals_for_new_guild_members <- c()
record_intervals_for_ex_guild_members <- c()
# ~20 minutes
wow %>% filter(records >= min_number_of_observations) %>% group_by(avatar) %>% do({
    if (.$new_avatar[1]){
        tmp_ind <- which(.$number_of_joined_guilds == 1)[1] # First guild joined.
        if (!is.na(tmp_ind)){
            time_intervals_for_new_guild_members <<- c(time_intervals_for_new_guild_members, .$timestamp[tmp_ind]-.$timestamp[1])
            record_intervals_for_new_guild_members <<- c(record_intervals_for_new_guild_members, tmp_ind-1)
        }
    }
    last_record_in_guild <- which(.$guild != -1)
    if (length(which(.$guild != -1))> 0){
        last_record_in_guild <- max(last_record_in_guild)
        tmp_df <- .[1:last_record_in_guild, c("guild", "number_of_joined_guilds", "timestamp")]
        tmp_df <- tmp_df %>% filter(guild == -1 & number_of_joined_guilds >=1)
        tmp <- tmp_df %>% group_by(number_of_joined_guilds) %>% summarise(interval = max(timestamp) - min(timestamp))
        time_intervals_for_ex_guild_members <<- c(time_intervals_for_ex_guild_members, tmp$interval)
        record_intervals_for_ex_guild_members <<- c(record_intervals_for_ex_guild_members, table(tmp_df$number_of_joined_guilds))
    }
    data.frame()
})

summary(time_intervals_for_new_guild_members)
length(time_intervals_for_new_guild_members)
summary(time_intervals_for_ex_guild_members)
length(time_intervals_for_ex_guild_members)
summary(record_intervals_for_new_guild_members)
length(record_intervals_for_new_guild_members)
summary(record_intervals_for_ex_guild_members)
length(record_intervals_for_ex_guild_members)

intervals <- data.frame(Interval = time_intervals_for_new_guild_members, unit = "Interval Units: Days", Type = "Before Joining First Guild")
intervals <- rbind(intervals, data.frame(Interval = time_intervals_for_ex_guild_members, unit = "Interval Units: Days", Type = "Before Joining Subsequent Guilds"))
intervals <- rbind(intervals, data.frame(Interval = record_intervals_for_new_guild_members, unit = "Interval Units: Records", Type = "Before Joining First Guild"))
intervals <- rbind(intervals, data.frame(Interval = record_intervals_for_ex_guild_members, unit = "Interval Units: Records", Type = "Before Joining Subsequent Guilds"))

ggplot(intervals, aes(Type, Interval)) + facet_wrap( ~ unit, scales="free") +
    geom_boxplot(aes(fill = Type)) +
    theme_bw() +
    ggtitle("Time Spent Outside of Guilds Until Joining First and Subsequent Guilds") +
    ylab("Length of Intervals Spent Outside of Guilds")
rm(intervals)

# What was the first guild of each avatar they were member of? If an avatar was not observed as a member of a guild at any time, then the value is -1.
wow <- wow %>%
    group_by(avatar) %>%
    mutate(first.real.guild = guild[which(guild != -1)[1]]) %>%
    group_by() %>%
    mutate(first.real.guild = ifelse(is.na(first.real.guild), -1, first.real.guild))
tmp <- wow %>% filter(first.real.guild != -1) %>% group_by(avatar) %>% summarise(first.real.guild = first.real.guild[1])
head(sort(table(tmp$first.real.guild), decreasing = TRUE), 15)
rm(tmp)

# Distribution of races/classes within guilds
# Facet-elni mindent race/class/10-es level csoportok szerint


# klaszterezni guildeket
# PCA
#


# Feature-k:
# jatekos level, guild avg level
# ugyanez: race, charclass vs. guild-beli eloszlasok
# guild klasztere
# guild letszama
# mennyi ideje lepett ki utoljara ember a guildbol
# mennyire aktiv a jatekos/guild

#
# GUILD LETSZAM hogyan befolyasolja a guild-be belepok ill. a guild-bol kilepok szamat:
# nezzuk meg annak az eloszlasat, hogy a belepes eventeknel mekkora a guild letszama amibe beleptek
#

wow <- left_join(wow,
            guild_stats_over_time %>%
                mutate(current.date = date, guild = as.numeric(guild)) %>%
                select(guild, current.date, number_of_members, avg_level,
                       Orc, Tauren, Troll, Undead, BloodElf, Rogue, Hunter, Warrior, Shaman, Warlock, Druid, Priest,
                       Mage, Paladin, DeathKnight),
            by = c("current.date", "guild"))

wow <- left_join(wow,
                  guild_stats_over_time %>%
                      mutate(current.date = date,
                             prev_guild = as.numeric(guild),
                             number_of_prev_guild_members = number_of_members,
                             avg_level_of_prev_guild = avg_level) %>%
                      select(prev_guild, current.date, number_of_prev_guild_members, avg_level_of_prev_guild),
                  by = c("current.date", "prev_guild"))
summary(wow$number_of_members) # NA's: first day
summary(wow$number_of_prev_guild) # NA's: first day and other first records of avatars, where prev_guild do not have valid guild value

ggplot(wow %>% filter(!is.na(number_of_members) & number_of_members > 0 & (event == 1 | event == 2)) %>% select(number_of_members)) +
    geom_density(aes(x = number_of_members))+ xlim(c(1, 1750)) + ylim(0, 0.0026)

ggplot(wow %>% filter(!is.na(number_of_prev_guild_members) & number_of_prev_guild_members > 0 & (event == -1 | event == 2)) %>% select(number_of_prev_guild_members)) +
    geom_density(aes(x = number_of_prev_guild_members))+ xlim(1, 1750) + ylim(0, 0.0026)

summary(wow %>%
            filter(!is.na(number_of_members) & (event == 1 | event == 2)) %>%
            select(number_of_members) %>%
            collect %>%
            .[["number_of_members"]])

#
# A legtobb belepes viszonylag kisebb tagszam eseten varhato, ugyanakkor nagyobb tagszamok eseten is vannak kiugrasok, sot 1000 és 1500 felett is láthatunk határozott kiemelkedést.
# Ugyanakkor figyelni kell, hogy itt egy guild is nagyban befolyasolhatja az abrat, ha sok belepes volt, így ez a nagy létszámú guild-ekre különösen igaz.
# Nézzük meg annak az eloszlását hogy az egye guild-ek mekkora létszámot értek el maximum
# Korábban látható, hogy 500 felett két peak is van, de az eloszlásból látszik, hogy 500 tagot már csak nagyon kevés guild ért el, 3 db.
wow %>%
    group_by(guild) %>%
    summarise(max_number_of_members = max(number_of_members, na.rm = TRUE)) %>%
    filter(guild != -1) %>%
ggplot() +
    geom_histogram(aes(x = max_number_of_members, y = (..count..)/sum(..count..)), size= 1.0, alpha = 0.6, bins=40) +
    geom_vline(xintercept = 500, color = "red", size= 1.0, alpha = 0.6) +
    scale_y_continuous(labels=percent) +
    theme_bw() +
    labs(title = "Histogram of the Maximal Number of Guild Members for each Guild", x="Maximal Number of Members", y="Percentage of Guilds")
#summary(tmp$max_number_of_members[tmp$guild != -1])
wow %>% filter(guild != -1 & number_of_members > 500) %>% distinct(guild) %>% select(guild, number_of_members)
#big_guilds
#rm(tmp)

# A peakek az előző plot-okon azt jelzik, hogy egy időre megállt ott a növekedés és sokan léptek ki is, amellett hogy be is.
# Nézzük meg ennek a 3 guildnek a members over time plotját

guild_stats_over_time %>% filter(guild %in% big_guilds$guild) %>% do({
    guilds_over_time_plot <- ggplot(data = ., aes(x=date))
    guilds_over_time_plot <- guilds_over_time_plot + geom_line(aes(y = number_of_members, group=guild, color=guild), size=1.2, alpha = 0.5)
    print(guilds_over_time_plot)
    data.frame()
})
as.data.frame(guild_stats_over_time %>% filter(guild == 460) %>% select(date, number_of_members))
rm(big_guilds)

nrow(wow %>% filter(event != 0 & current.date== "2008-10-08"))
nrow(wow %>% filter(event != 0 & current.date== "2008-10-08" & guild == 460))

# Ha ezt levonjuk az aznapi transition-kbol, ugy mar nem is kiugro


# Nézzük meg a belépések/kilépések számát a guild-ek átlagos level-je alapján
summary(wow$avg_level)
summary(wow$avg_level_of_prev_guild)

ggplot(wow %>% filter(!is.na(avg_level) & (event == 1 | event == 2)) %>% select(avg_level)) +
    geom_density(aes(x = avg_level))

ggplot(wow %>% filter(!is.na(avg_level_of_prev_guild) & (event == -1 | event == 2)) %>% select(avg_level_of_prev_guild)) +
    geom_density(aes(x = avg_level_of_prev_guild))

# Nagyon hasonló, az erős guild-eket érinti főként, ott van a legtöbb ki/belépés
# Nézzük meg ezt:
#
ggplot(wow %>% filter(!is.na(avg_level) & (event == 1 | event == 2)),
       aes(x = avg_level, y = level))+
    stat_density2d(aes(alpha=..level..), geom="polygon") +
    scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
    geom_point(colour="red",alpha=0.02) + labs(alpha = "Density Level")+
    theme_bw()

ggplot(wow %>% filter(!is.na(avg_level_of_prev_guild) & (event == -1 | event == 2)),
       aes(x = avg_level_of_prev_guild, y = level))+
    stat_density2d(aes(alpha=..level..), geom="polygon") +
    scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
    geom_point(colour="red",alpha=0.02)+
    theme_bw()

######*--------------
# Mekkorák "átlagosan" a guild-ek:
summary(guild_stats_over_time %>% filter(guild != -1 & number_of_members > 0) %>% group_by(guild) %>% summarise(avg_number_of_members = mean(number_of_members)))


names(wow) <- gsub(" ", "", names(wow), fixed = TRUE)
wow$enter_guild_with_matching_race <- FALSE

#wow$enter_guild_with_matching_race <- apply((wow$event == 1 | wow$event == 2) & wow[[gsub(" ", "", wow$race, fixed = TRUE)]] > 0)

wow$enter_guild_with_matching_race

# TODO szebbre
wow <- wow %>% mutate(enter_guild_with_matching_race = ifelse((enter_guild_with_matching_race | ((event == 1 | event == 2) & race == "Orc" & Orc > 0)), TRUE, FALSE))
wow <- wow %>% mutate(enter_guild_with_matching_race = ifelse((enter_guild_with_matching_race | ((event == 1 | event == 2) & race == "Tauren" & Tauren > 0)), TRUE, FALSE))
wow <- wow %>% mutate(enter_guild_with_matching_race = ifelse((enter_guild_with_matching_race | ((event == 1 | event == 2) & race == "Troll" & Troll > 0)), TRUE, FALSE))
wow <- wow %>% mutate(enter_guild_with_matching_race = ifelse((enter_guild_with_matching_race | ((event == 1 | event == 2) & race == "Undead" & Undead > 0)), TRUE, FALSE))
wow <- wow %>% mutate(enter_guild_with_matching_race = ifelse((enter_guild_with_matching_race | ((event == 1 | event == 2) & race == "Blood Elf" & BloodElf > 0)), TRUE, FALSE))

# race / charclass
# # minden race-re meg charclass-re: a belepesek szama hogyan oszlik el annak fgv-eben hogy a korabbi tagok kozott mekkora aranyban van a belepovel azonos race-ű/charclass-ű
# # ugyanez kilepesre
#
#wow$race <- factor(wow$race)

# Impact of Races
getRaceOrCharclassPlotData <- function(currentColValue, colName = c("race", "charclass")){
    colName2 <- gsub(" ", "", currentColValue, fixed = TRUE)
    tmp <- wow %>%
        filter(guild != 460 &
                   guild != -1 &
                   !is.na(number_of_members) &
                   number_of_members > 0 &
                   (event == 1 | event == 2) &
                   ((colName == "charclass" & charclass == currentColValue) | (colName == "race" & race == currentColValue))
               )

    if (colName == "charclass"){
        tmp <- tmp %>% group_by_(ratioOfTheSameCharclassBetweenMembers = interp(~ round(100*(var/number_of_members)), var = as.name(colName2)))
    } else {
        tmp <- tmp %>% group_by_(ratioOfTheSameRaceBetweenMembers = interp(~ round(100*(var/number_of_members)), var = as.name(colName2)))
    }
    tmp <- tmp %>%
        summarise("EnteredAvatars" = n(),
                  "DifferentGuilds" = length(unique(guild)),
                  "Top1Guild" = max(table(guild)),
                  "OtherGuilds" = EnteredAvatars - Top1Guild) %>% ungroup
    tmp[,colName] <- colName2
    tmp
}
race_plot_data <- c()
for (raceName in race_names){
    race_plot_data <- rbind(race_plot_data, getRaceOrCharclassPlotData(raceName, "race"))
}

plotAgainstRaceRatio <- function(data = race_plot_data, x_col = "ratioOfTheSameRaceBetweenMembers", y_col = "EnteredAvatars", color_col = "race"){
    ggplot(data, aes_string(x = x_col)) +
        geom_line(aes_string(y = y_col, color=color_col, group=color_col), size=1.3, alpha = 0.5) +
        theme_bw()
}
plotAgainstRaceRatio()

#plotAgainstRaceRatio(y_col = "Top1Guild") # impact of the most dominant guild
#plotAgainstRaceRatio(y_col = "OtherGuilds") # impact of guilds other than the most dominant
#plotAgainstRaceRatio(y_col = "DifferentGuilds") # how many guilds constituted to the whole impact

# were there any specific day that may have significantly distorted the results? No
ggplot(wow %>% filter(guild != -1 & guild != 460 & (event == 1 | event == 2)), aes(x = current.date)) + facet_wrap(~race) + geom_bar()


# Impact of Charclasses
# getCharClassPlotData <- function(currentCharClass){
#     col_name <- gsub(" ", "", currentCharClass, fixed = TRUE)
#     tmp <- wow %>%
#         filter(guild != 460 &
#                    guild != -1 &
#                    !is.na(number_of_members) &
#                    number_of_members > 0 &
#                    (event == 1 | event == 2) &
#                    charclass == currentCharClass) %>%
#         group_by_(ratioOfTheSameCharclassBetweenMembers = interp(~ round(100*(var/number_of_members)), var = as.name(col_name))) %>%
#         summarise("EnteredAvatars" = n(),
#                   "DifferentGuilds" = length(unique(guild)),
#                   "Top1Guild" = max(table(guild)),
#                   "OtherGuilds" = EnteredAvatars - Top1Guild) %>%
#         ungroup
#     tmp$charclass <- col_name
#     tmp
# }
charclass_plot_data <- c()
for (charClassName in charclass_names){
    charclass_plot_data <- rbind(charclass_plot_data, getRaceOrCharclassPlotData(charClassName, "charclass"))
}

plotAgainstRaceRatio(charclass_plot_data, "ratioOfTheSameCharclassBetweenMembers", color_col = "charclass")

#plotAgainstRaceRatio(charclass_plot_data, "ratioOfTheSameCharclassBetweenMembers", "Top1Guild", "charclass") # impact of the most dominant guild
#plotAgainstRaceRatio(charclass_plot_data, "ratioOfTheSameCharclassBetweenMembers", "OtherGuilds", "charclass") # impact of guilds other than the most dominant
#plotAgainstRaceRatio(charclass_plot_data, "ratioOfTheSameCharclassBetweenMembers", "DifferentGuilds", "charclass") # how many guilds constituted to the whole impact

# Were there any specific day that may have significantly distorted the results? No
ggplot(wow %>% filter(guild != -1 & guild != 460 & (event == 1 | event == 2)), aes(x = current.date)) + facet_wrap(~charclass) + geom_bar()

# Take a look at the correlation between
#  - the activity of avatars entering guilds
#  - and the average activity of the members of the corresponding guild, i.e. the guild the avatar is entering
# We only going to consider the average daily activity in the last N days before the entering events. And we are gonna do this on a daily basis, that is computing the average of the daily averages for the last N days. (So if someone leaves the guild in the meantime, then he won't be considered as a member after that day when computing daily averages.)



# Create a data.frame containg the average activity of the members of guilds for each day, where the guild existed
# Get daily sum of activities for each guild
guild_daily_activities <- wow %>%
    mutate(guild_creation_date = ifelse(guild_creation, current.date, NA)) %>%
    group_by(guild, current.date) %>%
    summarise(activity = n(), new_guild = new_guild[1], guild_creation_date=guild_creation_date[1]) %>%
    group_by() %>%
    mutate(date = current.date)
guild_daily_activities$current.date <- NULL

# Join with data frame that contains daily guild statistics including number of members
guild_daily_activities <- left_join(guild_daily_activities,
                                    guild_stats_over_time %>%
                                        select(guild, date, number_of_members) %>%
                                        mutate(date = date - 1, guild = as.numeric(guild)),
                                    by = c("guild", "date"))

#
# Although only those days were kept for every guild for which there was observed activity, it is still possible to have 0 number_of_members value, because this value represent the number of members at the end of the corresponding day. Let's simply remove them.
# And get the averages.
#

guild_daily_activities <- guild_daily_activities %>%
    filter(guild != -1 & number_of_members != 0) %>%
    mutate(avg_daily_activity_of_members = activity / number_of_members)
summary(guild_daily_activities$avg_daily_activity_of_members)

# Create rows for every date when the guild existed, regardless having any activity on that day or not
guild_daily_activities <- left_join(
    tbl_df(data.frame(date = seq.Date(min_date, max_date, "days"))),
    guild_daily_activities,
    by = "date") %>%
    filter(!new_guild | guild_creation_date <= date) %>%
    mutate(avg_daily_activity_of_members = ifelse(is.na(avg_daily_activity_of_members), 0, avg_daily_activity_of_members))

# Create a similar DF for avatars, i.e. containing the average daily activity of the last N days by dates
#
avatarDailyActivities_ <- wow %>%
    group_by(avatar, current.date) %>%
    summarise(daily_activity = n(), new_avatar = new_avatar[1], avatar_creation_date=activation.date[1]) %>%
    group_by() %>%
    mutate(date = current.date)
avatarDailyActivities$current.date <- NULL
# Create rows for every date when the avatar existed, regardless having any activity on that day or not
avatarDailyActivities_ <- left_join(
    tbl_df(data.frame(date = seq.Date(min_date, max_date, "days"))),
    avatarDailyActivities_,
    by = "date") %>%
    filter(!new_avatar | avatar_creation_date <= date)


# Get the average of daily averages for the last N days
aggregateMeanForLastNDays <- function(dailyMeansData, groupByColumn, dailyMeanColumn, N){
    dailyMeansData <- arrange(dailyMeansData, date)
    result <- dailyMeansData %>%
        group_by_(groupByColumn) %>%
        do({
            current_df <- .
            avg_last_N_day <- c()
            if (N < length(.$date)){
                avg_last_N_day <- sapply((1+N):nrow(current_df), function(index){mean((current_df[[dailyMeanColumn]])[(index-N-1):(index-1)])})
                data.frame(date = .$date[-(1:N)], avg_last_N_day = avg_last_N_day)
            } else {
                data.frame(date = c(), avg_last_N_day = c())
            }
        })
    ungroup(result)
}




N <- 5

getActivityStats <- function(lastNDaysToAggregate, guildDailyActivitiesData, avatarsDailyActivitiesData){
    # Get averages for last lastNDaysToAggregate days
    # For guilds
    guild_daily_activities <- aggregateMeanForLastNDays(guildDailyActivitiesData, "guild", "avg_daily_activity_of_members", lastNDaysToAggregate)
    # For avatars
    avatarDailyActivities <- aggregateMeanForLastNDays(avatarsDailyActivitiesData, "avatar", "daily_activity", lastNDaysToAggregate)

    # Combine stats into one DF together guild entering events
    activitiesCombinedDf <- left_join(
        wow %>% filter(event == 1) %>%
            select(current.date, avatar, guild, level, number_of_members) %>%
            mutate(date = current.date),
        guild_daily_activities,
        by = c("date", "guild")
    )

    activitiesCombinedDf$activityLastNDay <- activitiesCombinedDf$avg_last_N_day
    activitiesCombinedDf$avg_last_N_day <- NULL

    activitiesCombinedDf <- left_join(activitiesCombinedDf, avatarDailyActivities, by = c("date", "avatar"))
    activitiesCombinedDf$current.date <- NULL
    # make sure there are no NAs
    summary(activitiesCombinedDf)
    #which(is.na(activitiesCombinedDf$activityLastNDay))
    #activitiesCombinedDf[31827,]
    #
    activitiesCombinedDfFiltered <- activitiesCombinedDf %>%
        filter(!is.na(activityLastNDay) &
                   (date > (min_date + lastNDaysToAggregate)) &
                   !is.na(activitiesCombinedDf$activityLastNDay))
    plot <- ggplot(activitiesCombinedDfFiltered,
           aes(x = avg_last_N_day, y = activityLastNDay)) +
        stat_density2d(aes(alpha=..level..), geom="polygon") +
        scale_alpha_continuous(limits=c(0,0.1),breaks=seq(0,0.1,by=0.01))+
        geom_point(colour="red",alpha=0.02) +
        ylim(0, 150) +
        ylim(0, 25) +
        theme_bw()
    correlation <- cor(activitiesCombinedDfFiltered$avg_last_N_day, activitiesCombinedDfFiltered$activityLastNDay)
    correlationTest <- cor.test(activitiesCombinedDfFiltered$avg_last_N_day, activitiesCombinedDfFiltered$activityLastNDay)
    list(plot = plot, correlation = correlation, correlationTest = correlationTest)

}

activitiesWhenEntering <- list()
for (i in 1:7){
    activitiesWhenEntering[[i]] <- getActivityStats(i, guild_daily_activities, avatarDailyActivities_)
}







# Number of Guild Events by Date
#
# istribution of Event Types Within Races and Ch
