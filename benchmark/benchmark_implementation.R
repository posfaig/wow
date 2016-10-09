########################################################################################################
###  Based on the following paper: Modeling Destructive Group Dynamics in Online Gaming Communities  ###
########################################################################################################

set.seed(0)
# 
# Takes ~5 hours on my machine
source("benchmark/compute_interactions.R")

##### Parameters

# theta parameter of the exponentially decaying kernel function for edge weights
theta <- 0.3  # the value was determined based on many cross validation experiments on the training set

##### Helper functions 

# Create the social graph for a given prediction date based on the observed data
create_graph <- function(data, pred_date){
	min_time_decay_weight <- 0.00001
	t_max <- ceiling(log(min_time_decay_weight / theta, base = (1 - theta)))
	pred_date <- as.Date(pred_date)
	
	# filter out records from horde cities (as only horde avatars are in the dataset)
	horde_capitals <- c("Shattrath City", "Orgrimmar", "Silvermoon City", "Thunder Bluff", "Undercity", "Dalaran")
	data <- data %>% filter(!(zone %in% horde_capitals))
	
	# keep only the events before the current prediction date
	data <- data %>% filter(current_date < pred_date)
	
	### Edges:
	edges <- data.frame(node_1 = "-", node_2 = "-", weight = 0)  # create df with auxiliary row
	edges$node_1 <- as.character(edges$node_1)
	edges$node_2 <- as.character(edges$node_2)
	add_edge <- function(node_1, node_2, weight){
		edges <<- rbind(edges, data.frame(node_1 = node_1, node_2 = node_2, weight = weight))
	}
	compute_weight_component <- function(interaction_dates, cooccurence_durations = 1.0){
		t <- as.numeric(difftime(pred_date, interaction_dates, units = "days"))
		weight_components <- theta * cooccurence_durations * ((1-theta) ^ (t))
		sum(weight_components)
	}
	
	## Edges from collaboration
	print("Create collaboration edges")
	# Edges are between players who occured in the same zone at the same time (snapshot). Each day when there was a cooccurence is assigned a weight equal to the number of cooccurence snapshots (~ collaboration time). Then these initial weights are decreased according to time decay and summed for each edge.
	#
	
	tmp <- interactions %>% 
		filter(current_date < pred_date) %>%
		group_by(avatar.x, avatar.y) %>% 
		summarise(current_weight = compute_weight_component(current_date, collaboration)) %>%
		group_by
	add_edge(tmp$avatar.x, tmp$avatar.y, tmp$current_weight)
	
	## Edges from guilds
	print("Create guild edges")
	# Avatars and guilds are only connected if the character is in the guild on the current prediction date.
	# We also add a guild node with id -1 to represent how much time did avatars play outside of guilds recently.
	
	# Get the guild members at prediction date 
        guild_members <- data %>% group_by(avatar) %>% slice(n()) %>% group_by() %>% filter(guild != -1)
        avatar_date_guild_df <- data %>% 
        	select(avatar, current_date, guild) %>% 
        	group_by(avatar, current_date, guild) %>%
        	summarise(collaboration = n()) %>%
        	group_by()
        tmp <- avatar_date_guild_df %>% 
		group_by(avatar, guild) %>% 
		summarise(weight = compute_weight_component(current_date, collaboration)) %>%
		group_by
        guild_members <- left_join(guild_members, tmp, by = c("guild", "avatar"))
        guild_members$weight[is.na(guild_members$weight)] <- 0.0
        
	add_edge(guild_members$guild, guild_members$avatar, guild_members$weight)  # guilds are always node_1
	
	# Add edges connecting to node -1
	tmp <- tmp %>% filter(guild == -1)
	add_edge(tmp$guild, tmp$avatar, tmp$weight)
	
	edges <- edges[-1,]  # remove auxiliary row
	
	# nodes: avatars and guilds
	print("Create nodes")
	avatar_nodes <- data %>% distinct(avatar) %>% select(avatar) %>% collect %>% .[["avatar"]]
	guild_nodes <- unique(c(data$guild, guild_members$guild))
	nodes <- c(avatar_nodes, guild_nodes)
	
	list(nodes = nodes, 
		avatars = avatar_nodes, 
		guilds = guild_nodes, 
		edges = edges, 
		current_guilds = guild_members %>% select(avatar, guild, weight))
}

# Compute features and labels for a given prediction date and test date based on the observed data and the current temporal social graph of avatars
compute_features_and_labels <- function(data, pred_date, testset_end_date, graph){

	pred_date <- as.Date(pred_date)
	testset_end_date <- as.Date(testset_end_date)
	guild_member_avatars <- graph$current_guilds$avatar
	
	# Keep only the data regarding known avatars
	data <- data %>% filter(avatar %in% graph$avatars)
	
	##### Compute labels for avatars
	# 
	# true: left the guild in the given period
	# false: not left the guild in the given period
	#
	test_data <- data %>% filter(current_date >= pred_date & current_date < testset_end_date) 
	labels <- test_data %>% 
		group_by(avatar) %>% 
		summarise(label = (sum(event == "Guild Left" | event == "Guild Changed") > 0))
	
	##### Compute features
	#### Personal histories of 14 "day-records" and corresponding features
	print("Computing personal history features")
	train_data <- data %>% filter(current_date < pred_date)
	personal_histories <- train_data %>% 
		group_by(avatar, current_date, guild) %>% 
		mutate(number_of_records = n()) %>%
		slice(1) %>% 
		group_by(avatar) %>%
		slice(1:14) %>% 
		group_by
		
	features <- personal_histories %>%
		group_by(avatar) %>% 
		summarise(
			guild_count = length(unique(guild[guild != -1])),
			time_since_last_event = as.numeric(difftime(pred_date, max(current_date), units = "days")),
			event_count = n(),
			level_begin = min(level),
			level_end = max(level),
			level_change = level_end - level_begin,
			avg_event_duration = mean(number_of_records),
			window_duration = as.numeric(difftime(max(current_date), min(current_date), units = "days")))
	# keep only the avatars that are guild members on the prediction date
	features <- filter(features, avatar %in% guild_member_avatars)
	
	
	#### Social features
	print("Computing social features")
	
	### Number of guild membership feature
	print("Number of guild membership feature")
	features$number_of_guild_membership <- features$guild_count
	
	### Overall and guild clustering coeffecients features
	print("Overall and guild clustering coeffecients features")
	library(igraph)
	current_edges <- as.vector(t(as.matrix(select(graph$edges, node_1, node_2))))
	features$overall_clustering_coeff <- transitivity(make_graph(current_edges))
	
	clustering_coeffs <- graph$edges %>%
		filter(node_1 %in% graph$guilds) %>%
		group_by(node_1) %>%
		do({
			current_nodes <- unique(c(.$node_1[1], .$node_2))
			current_edges <- graph$edges %>% 
				filter((node_1 %in% current_nodes) & (node_2 %in% current_nodes))
			current_edges <- as.vector(t(as.matrix(select(current_edges, node_1, node_2))))
			data.frame(clustering_coeff = transitivity(make_graph(current_edges)))
		}) %>%
		group_by
	clustering_coeffs$clustering_coeff[is.nan(clustering_coeffs$clustering_coeff)] <- 0  # Guilds with 1 member (i.e. only two vertices)
	detach("package:igraph", unload = TRUE)
	
	clustering_coeffs$guild <- as.numeric(clustering_coeffs$node_1)
	clustering_coeffs$node_1 <- NULL
	features <- left_join(features, graph$current_guilds %>% select(avatar, guild), by = "avatar")
	features <- left_join(features, clustering_coeffs, by = "guild")
	
	### Playing time within guild feature = weight of the edge connecting to the guild node
	print("Playing time within guild feature")
	features <- left_join(features, graph$current_guilds %>% select(avatar, weight), by = "avatar") %>%
		mutate(playing_time_within_guild = weight)
	features$weight <- NULL
		
	### Collaboration time within guild = sum of the weights of the edges connecting to the other members of the guild
	print("Collaboration time within guild feature")
	
	# create auxiliary data frame containing edges between avatars, and guild of avatars
	edges_between_avatars <- filter(graph$edges, !(node_1 %in% c(graph$guilds, -1)))
	# duplicate edges in opposite direction
	edges_between_avatars <- rbind(edges_between_avatars, 
		data.frame(
			node_1 = edges_between_avatars$node_2,
			node_2 = edges_between_avatars$node_1,
			weight = edges_between_avatars$weight
		))
	# add column containing the guild of node_1
	edges_between_avatars_with_guilds <- edges_between_avatars
	edges_between_avatars_with_guilds$avatar <- edges_between_avatars_with_guilds$node_1
	edges_between_avatars_with_guilds <- inner_join(
			edges_between_avatars_with_guilds,
			select(features, avatar, guild))
	edges_between_avatars_with_guilds$guild_node_1 <- edges_between_avatars_with_guilds$guild
	edges_between_avatars_with_guilds$guild <- NULL
	
	# add column containing the guild of node_2
	edges_between_avatars_with_guilds$avatar <- edges_between_avatars_with_guilds$node_2
	edges_between_avatars_with_guilds <- inner_join(
			edges_between_avatars_with_guilds,
			select(features, avatar, guild))
	edges_between_avatars_with_guilds$guild_node_2 <- edges_between_avatars_with_guilds$guild
	edges_between_avatars_with_guilds$guild <- NULL
	edges_between_avatars_with_guilds$avatar <- NULL
		
	collaboration_time_within_guild_df <- edges_between_avatars_with_guilds %>%
		group_by(node_1) %>%
		summarise(collaboration_time_within_guild = sum(weight[guild_node_1 == guild_node_2])) %>%
		mutate(avatar = node_1) %>%
		select(avatar, collaboration_time_within_guild)
		
	features <- left_join(features, collaboration_time_within_guild_df, by = "avatar") %>%
		mutate(collaboration_time_within_guild = ifelse(is.na(collaboration_time_within_guild), 0, collaboration_time_within_guild))

	### Weighted degree within guild feature = sum of the edges of the avatar, that run within its current guild = the sum of "playing time within guild" and "collaboration time within guild" features
	print("Weighted degree within guild feature")
	features$weighted_degree_within_guild <- features$playing_time_within_guild + features$collaboration_time_within_guild
	
	features$weight <- NULL
	
	
	### Playing time feature
	# Sum of the weight of the edge that goes to the current guild and the weight of the edge that goes to guild -1
	print("Playing time feature")
	weights_to_noguild <- graph$edges %>% filter(node_1 == -1) %>% select(node_2, weight)
	weights_to_noguild$avatar <- weights_to_noguild$node_2
	weights_to_noguild$playing_time <- weights_to_noguild$weight
	weights_to_noguild$node_2 <- NULL
	weights_to_noguild$weight <- NULL
	
	features <- left_join(features, weights_to_noguild, by = "avatar") %>%
		mutate(playing_time = ifelse(is.na(playing_time), 0, playing_time))
	features$playing_time <- features$playing_time + features$playing_time_within_guild
	
	
	### Collaboration time feature
	# Sum of the weights of edges that do not go to the current guild or guild -1
	print("Collaboration time feature")
	tmp <- edges_between_avatars_with_guilds %>%
		group_by(node_1) %>%
		summarise(collaboration_time = sum(weight))
	tmp$avatar <- tmp$node_1
	tmp$node_1 <- NULL
	features <- left_join(features, tmp, by = "avatar") %>%
		mutate(collaboration_time = ifelse(is.na(collaboration_time), 0, collaboration_time))
		
	### Overall weighted degree feature 
	# The sum of the playing_time and collaboration_time features
	print("Overall weighted degree feature")
	features$overall_weighted_degree <- features$playing_time + features$collaboration_time
	
	### Number of guild members feature
	print("Number of guild members feature")
	guild_edges <- filter(graph$edges, (node_1 %in% graph$guilds))
	number_of_guild_members <- guild_edges %>% group_by(node_1) %>% summarise(number_of_guild_members = n())
	number_of_guild_members$guild <- as.numeric(number_of_guild_members$node_1)
	number_of_guild_members$node_1 <- NULL
	features <- left_join(features, number_of_guild_members, by = "guild")
		
	### Percentage of members played with excessively feature
	print("Percentage of members played with excessively feature")
	played_with_excessively_threshold <- mean(edges_between_avatars$weight) + 2 * sd(edges_between_avatars$weight)
	tmp <- edges_between_avatars_with_guilds
	tmp$avatar <- tmp$node_1
	tmp$guild <- tmp$guild_node_1
	tmp$node_1 <- NULL
	tmp$guild_node_1 <- NULL
	tmp$guild_node_2 <- NULL
	tmp <- left_join(tmp, number_of_guild_members, by = "guild")
	tmp_2 <- tmp %>%
		group_by(avatar) %>%
		summarise(percentage_of_members_played_with_excessively = sum(weight > played_with_excessively_threshold)/number_of_guild_members[1])
	
	features <- left_join(features, tmp_2, by = "avatar") %>%
		mutate(percentage_of_members_played_with_excessively = ifelse(is.na(percentage_of_members_played_with_excessively), 0, percentage_of_members_played_with_excessively))
		
	### Percentage of members played with before feature
	print("Percentage of members played with before feature")
	tmp_2 <- tmp %>%
		group_by(avatar) %>%
		summarise(percentage_of_members_played_with_before = n()/number_of_guild_members[1])
	features <- left_join(features, tmp_2, by = "avatar") %>%
		mutate(percentage_of_members_played_with_before = ifelse(is.na(percentage_of_members_played_with_before), 0, percentage_of_members_played_with_before))
	
	### Number of friends feature
	print("Number of friends feature")
	tmp_2 <- tmp %>%
		group_by(avatar) %>%
		summarise(number_of_friends = n())
	features <- left_join(features, tmp_2, by = "avatar") %>%
		mutate(number_of_friends = ifelse(is.na(number_of_friends), 0, number_of_friends))
	
	### Number of friends already having quit guild feature
	print("Number of friends already having quit guild feature")
	
	# get avatars' guild and friends
	avatars_guild_and_friends <- tmp %>% select(avatar, guild, node_2)
	avatars_guild_and_friends$friend <- avatars_guild_and_friends$node_2
	avatars_guild_and_friends$node_2 <- NULL
	
	# get avatars' exguilds
	exguilds <- train_data %>% 
		filter(event == "Guild Changed" | event == "Guild Left") %>% 
		select(avatar, prev_guild) %>%
		distinct(avatar, prev_guild)
	exguilds$guild <- as.numeric(exguilds$prev_guild)
	exguilds$prev_guild <- NULL
	# remove exguilds if an avatar rejoined its older guild and currently is still a member of it
	exguilds <- left_join(exguilds, features %>% select(avatar, guild, event_count), by = c("avatar", "guild"))
	exguilds <- exguilds %>% filter(is.na(event_count))
	exguilds$event_count <- NULL
	exguilds$friend <- exguilds$avatar
	exguilds$avatar <- NULL
	
	# quitted friends
	quitted_friends <- inner_join(avatars_guild_and_friends, exguilds, by = c("guild", "friend"))
	number_of_quitted_friends <- quitted_friends %>% 
		group_by(avatar) %>%
		summarise(number_of_friends_already_having_quit_guild = n())
	
	# add to features by joining tables
	features <- left_join(features, number_of_quitted_friends, by = "avatar") %>%
		mutate(number_of_friends_already_having_quit_guild = ifelse(is.na(number_of_friends_already_having_quit_guild), 0, number_of_friends_already_having_quit_guild))
		
	
	print("Joining features and labels")
	features$guild <- NULL
	features_and_labels <- left_join(features, labels, by = "avatar")
	features_and_labels <- features_and_labels %>% mutate(label = ifelse(is.na(label), FALSE, label))  # label is FALSE for avatars that did not appear in the test period
	features_and_labels$pred_date <- pred_date
	features_and_labels$testset_end_date <- testset_end_date
	features_and_labels
}

# Train a random forest model on a given dataset
train <- function(dataset){
	targets <- factor(dataset$label)
	predictors <- dataset
	predictors$label <- NULL
	predictors$avatar <- NULL
	predictors$pred_date <- NULL
	predictors$testset_end_date <- NULL
	
	rf_model <- randomForest(
            x = predictors,
            y = targets,
            data = dataset,
            ntree = 10,  # as it is in the paper
            importance = TRUE
            #do.trace = TRUE
            #classwt = c(p_false, p_true)
            #sampsize = c(500, 500),
            #strata = targets
        )
        rf_model
}

# Compute predictions of a given model for a dataset
test <- function(model, dataset){
	predictions <- predict(model, dataset, type="prob")
	dataset$prediction <- predictions[,2]
	dataset
}

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
    
	print(paste("Accuracy:", accuracy))
	print(paste("Precision:", precision))
	print(paste("Recall:", recall))
	print(paste("F-score:", f_score))
	print(paste("AUC:", auc))
	
	list(accuracy = accuracy, precision = precision, recall = recall, f_score = f_score, auc = auc, roc_plot = roc_plot)
}

# write results and predictions to file
write_results_to_file <- function(predictions_df, results_cv, results_test) {
	
	predictions_df <- predictions_df %>% 
		select(avatar, label, prediction) %>%
		mutate(label = as.numeric(label))
	write.table(predictions_df, "benchmark/predictions.csv", append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
	
	plot_roc <- results_cv$roc_plot
	pdf("benchmark/roc_plot_cv.pdf")
	plot(plot_roc)
	title(paste("ROC curve, Cross validation - AUC =", plot_roc$auc))
	dev.off()
	
	plot_roc <- results_test$roc_plot
	pdf("benchmark/roc_plot_test.pdf")
	plot(plot_roc)
	title(paste("ROC curve, Test - AUC =", plot_roc$auc))
	dev.off()
	
	results_cv$roc_plot <- NULL
	results_test$roc_plot <- NULL
	lines_cv <- sapply(names(results_cv), function(x) {
		paste(x, "=", results_cv[[x]])
	})
	lines_test <- sapply(names(results_test), function(x){
		paste(x, "=", results_test[[x]])
	})
	
	fileConn <- file("benchmark/results.txt")
	writeLines(c("Cross validation results", lines_cv, "Test set results:", lines_test), fileConn)
	close(fileConn)
}

# Do cross validation on a training dataset
do_cv <- function(train_data, k = 10){
	folds <- cvFolds(n = nrow(train_data), K = k, R = 1)
	predictions <- data.frame()
	for (i in 1:k){
		# get indecies of current fold
		test_set_row_indecies <- folds$subsets[folds$which == i, 1]
		training_set_row_indecies <- folds$subsets[folds$which != i, 1]

		# get train and test sets of current fold
		current_train_data <- train_data[training_set_row_indecies,]
		current_test_data <- train_data[test_set_row_indecies,]
		
		# learn model
		rf_model <- train(current_train_data)
		
		# predict with the current model
		current_predictions <- test(rf_model, current_test_data)

		if(nrow(predictions) == 0){
			predictions <- current_predictions
		} else {
			predictions <- rbind(predictions, current_predictions)
		}
	}
	
	print("Cross validation results:")
	#thresh_holds <- seq(0.05, 0.95, 0.05)
	#for (thresh_hold in thresh_holds) {
	#	print(paste("thresh_hold=", thresh_hold))
	#	get_perf_measures(predictions$label, predictions$prediction, thresh_hold)
	#}
	get_perf_measures(predictions$label, predictions$prediction)
}

# Compute features and labels for a set of prediction dates
get_features_for_pred_dates <- function(pred_dates){
	dataset <- data.frame()
	for (pred_date in pred_dates){
		print(paste("Computing features and labels for prediction date", pred_date))
		testset_end_date <- as.Date(pred_date) + 30
	
		graph <- create_graph(wow, pred_date)
		data_subset <- compute_features_and_labels(wow, pred_date, testset_end_date, graph)
	
		# concat
		if (nrow(dataset) == 0){
			dataset <- data_subset
		} else {
			dataset <- rbind(dataset, data_subset)
		}
	}
	dataset
}


##### MAIN

### init, data import
library(dplyr)
library(lubridate)
library(data.table)
library(randomForest)
library(pROC)
library(cvTools)
library(readr)

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


### Get training dataset
print("Get train and test datasets")
training_data <- get_features_for_pred_dates(prediction_dates_train)

### Do cross-validation on training data
print("Do cross-validation on training data")
results_cv <- do_cv(training_data)


### Get performance measures on final test set
print("Get performance measures on final test set")
test_data <- get_features_for_pred_dates(prediction_dates_test)
rf_model <- train(training_data)
predictions <- test(rf_model, test_data)
results_test <- get_perf_measures(predictions$label, predictions$prediction)
write_results_to_file(predictions, results_cv, results_test)


