corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    
    source("readPollutantCsv.R")
    source("complete.R")

    com <- complete(directory)

    # subset the data.frame according to the threshold for the nobs
    data <- com[com$nobs > threshold, ]

    # result is a numeric vector
    result <- numeric(0)

    # for each data point, read CSV, calculate the cor and append to the result
    for(id in data$id) {
        csv <- readPollutantCsv(directory, id)

        # logical vector of valid rows
        tf <- !is.na(csv$sulfate) & !is.na(csv$nitrate)

        # subset of valid rows
        x <- csv[tf, ]  

        result <- c(result, cor(x$sulfate, x$nitrate))
    }

    result
}