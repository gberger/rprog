rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

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

    ## Remove NAs
    data <- data[complete.cases(data[,coln]),]

    ## Sort by state, rate and name
    data <- data[order(data[7], data[coln], data[2]), ]

    ## Split by state
    s <- split(data, data[7])

    hospitals <- character(0)
    states <- character(0)

    for(name in names(s)){
        hospName <- if(num == "best"){
            s[[name]][1, 2]
        } else if(num == "worst"){
            tail(s[[name]], 1)[1, 2]
        } else {
            s[[name]][num, 2]
        }
        hospitals <- append(hospitals, hospName)
        states <- append(states, name)
    }

    data.frame(hospital=hospitals, state=states)
}