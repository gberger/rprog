readPollutantCsv <- function(directory, id) {
    read.csv(paste(directory, "/", sprintf("%03d", id), ".csv", sep=''))
}
