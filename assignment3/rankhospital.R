rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Filter the state
    data <- data[data$State == state, ]

    ## Check invalid state
    if(nrow(data) == 0) {
        stop("invalid state")
    }

    ## Get the col number
    coln <- if(outcome == "heart attack") {
        11
    } else if(outcome == "heart failure") {
        17
    } else if(outcome == "pneumonia") {
        23
    } else {
        stop("invalid outcome")        
    }

    ## Convert character to numeric
    data[, coln] <- suppressWarnings(as.numeric(data[, coln]))

    ## Remove NAs
    data <- data[complete.cases(data[,coln]),]

    ## Sort by rate and then by name
    data <- data[order(data[coln], data[2]), ]

    ## Return the desired
    if(num == "best"){
        data[1, 2]
    } else if(num == "worst"){
        tail(data, 1)[1, 2]
    } else {
        data[num, 2]
    }
}