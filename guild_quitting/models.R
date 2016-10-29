######################################################
###
### Functions for Building and Evaluating Models
###
######################################################

library(dummies)
library(dplyr)
library(xgboost)

##### UTIL FUNCTIONS #####

#convert_factors_to_numeric <- function(dataset) {
#	numeric_dataset <- as.data.frame(dataset)
#	factor_columns <- names(dataset)[sapply(dataset, is.factor)]
#	sapply(factor_columns, function(col_name){
#		numeric_dataset <<- cbind(numeric_dataset,
#			dummy(col_name, numeric_dataset, drop = FALSE))
#		numeric_dataset[,col_name] <<- NULL})
#	numeric_dataset
#}


##### EVALUATION OF MODELS #####

# Get performance measures for a set of predictions and the corresponding target labels
#get_perf_measures <- function(targets, predictions, decision_threshold = 0.5){
#
#	# get performance measures
#	roc_plot <- roc(targets, predictions)
#	auc <- as.numeric(roc_plot$auc)
#	plot(roc_plot)
#	title(paste("Probs ROC - AUC =", auc))
#
#	tp <- sum(targets == TRUE & predictions >= decision_threshold)
#	tn <- sum(targets == FALSE & predictions < decision_threshold)
#	fp <- sum(targets == FALSE & predictions >= decision_threshold)
#	fn <- sum(targets == TRUE & predictions < decision_threshold)
#
#	accuracy <- (tp + tn) / length(predictions)
#	precision <- tp / (tp + fp)
#	recall <- tp / (tp + fn)
#	f_score <- 2 * tp / (2 * tp + fp + fn)
#
#	# print(paste("Accuracy:", accuracy))
#	# print(paste("Precision:", precision))
#	# print(paste("Recall:", recall))
#	# print(paste("F-score:", f_score))
#	# print(paste("AUC:", auc))
#
#	list(accuracy = accuracy,
#		precision = precision,
#		recall = recall,
#		f_score = f_score,
#		auc = auc,
#		roc_plot = roc_plot)
#}
#
## Write results and predictions to file
#write_results_to_file <- function(predictions_df, results, model_name) {
#
#	dir.create(file.path(paste("generated/results/guild_quitting/", model_name ,"/", sep = "")), showWarnings = FALSE, recursive = TRUE)

#	predictions_df <- predictions_df %>%
#		select(avatar, label, prediction) %>%
#		mutate(label = as.numeric(label))
#	write.table(predictions_df,
#		paste("results/", model_name ,"/predictions.csv", sep = ""),
#		append = FALSE,
#		row.names = FALSE,
#		col.names = TRUE,
#		sep = ",",
#		quote = FALSE)
#
#	plot_roc <- results$roc_plot
#	pdf(paste("results/", model_name ,"/roc_plot.pdf", sep = ""))
#	plot(plot_roc)
#	title(paste("ROC Curve - AUC = ", plot_roc$auc))
#	dev.off()
#
#	results$roc_plot <- NULL
#	lines <- sapply(names(results), function(x) {
#		paste(x, "=", results[[x]])
#	})
#
#	fileConn <- file(paste("generated/results/guild_quitting/", model_name ,"/results.txt", sep = ""))
#	writeLines(c("Cross validation results", lines), fileConn)
#	close(fileConn)
#}



##### BUILD MODELS AND MAKE PREDICTIONS #####





##########################################
### XGBoost
##########################################

get_model_xgboost <- function(params = list()){
    model_name <- "xgboost"
    desc <- "xgboost"
    model <- c()
    params <- list()

    preprocess <- function(data, is_train){
        # filter not predictor columns
        data$avatar <- NULL
        data$guild <- NULL
        data$pred_date <- NULL
        data$testset_end_date <- NULL
        data$label <- NULL

        # convert factors to numeric
        data <- convert_factors_to_numeric(data)

        # scale data
        if (is_train){


            params[["final_predictor_column_names"]] <<- names(data)
            #tmp <- scale(data[,names(data)])
            #data[,names(data)] <- tmp
            #params[["scale_centers"]] <<- attr(tmp, "scaled:center")
            #params[["scale_deviations"]] <<- attr(tmp, "scaled:scale")


            # Set of final columns
            params[["final_predictor_columns"]] <<- names(data)

        } else {

            data <- data[, intersect(params$final_predictor_column_names, names(data))]

            #data[,names(data)] <- scale(
            #	data[,names(data)],
            #	center=params$scale_centers,
            #	scale=params$scale_deviations)

            # Keep only the final columns of the training set
            data <- data %>% select_(.dots = params[["final_predictor_columns"]])
        }

        # convert integers to double for xgboost
        sapply(names(data), function(col_name){
            data[[col_name]] <<- as.numeric(data[[col_name]])
        })

        data
    }

    build <- function(train_data) {
        print(paste("Building model:", model_name))

        target_column <- "label"
        targets <- train_data[[target_column]]
        targets <- as.numeric(targets)  # numeric and >0 target variable as the frbs package requires
        train_data[,target_column] <- NULL

        train_data_preprocessed <- preprocess(train_data, TRUE)

        model <<- xgboost(
            as.matrix(train_data_preprocessed),
            targets,
            nrounds = 50,
            objective = "binary:logistic",
            max.depth = 100)

        model
    }

    predict_ <- function(test_data){
        print("predicting...")
        test_data <- preprocess(test_data, FALSE)
        predictions <- predict(model, as.matrix(test_data))
        predictions
    }

    list(
        build = build,
        predict = predict_,
        model_name = function(){model_name},
        desc = function(){desc},
        params = function(){params},
        model = function(){model}
    )

}

