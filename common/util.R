##############################################################################################################
###
### Simple util functions
###
##############################################################################################################

library(dummies)
library(ggplot2)

convert_factors_to_numeric <- function(dataset) {
    numeric_dataset <- as.data.frame(dataset)
    factor_columns <- names(dataset)[sapply(dataset, is.factor)]
    sapply(factor_columns, function(col_name){
        numeric_dataset <<- cbind(
                                  dummy(col_name, numeric_dataset, drop = FALSE),
                                  numeric_dataset)
        numeric_dataset[,col_name] <<- NULL})
    numeric_dataset
}


save_plot <- function(ggplot_plot, dir_name, file_name, width = 10, height = 8, ...){
    output_path <- paste("generated/plots/", dir_name, "/", sep = "")
    dir.create(file.path("./", "generated/"), showWarnings = FALSE)
    dir.create(file.path("generated/", "plots/"), showWarnings = FALSE)
    dir.create(file.path("generated/plots/", dir_name), showWarnings = FALSE)

    ggsave(file=paste(output_path, paste(file_name, ".eps", sep = ""), sep = ""), plot=ggplot_plot, width = width, height = height, dpi=1200, ...)
    ggsave(file=paste(output_path, paste(file_name, ".png", sep = ""), sep = ""), plot=ggplot_plot, width = width, height = height, dpi=300, ...)
}




