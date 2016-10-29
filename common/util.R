##############################################################################################################
###
### Simple util functions
###
##############################################################################################################


convert_factors_to_numeric <- function(dataset) {
    numeric_dataset <- as.data.frame(dataset)
    factor_columns <- names(dataset)[sapply(dataset, is.factor)]
    sapply(factor_columns, function(col_name){
        numeric_dataset <<- cbind(numeric_dataset,
                                  dummy(col_name, numeric_dataset, drop = FALSE))
        numeric_dataset[,col_name] <<- NULL})
    numeric_dataset
}





