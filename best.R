best <- function(state, outcome) {
    source('getDataByOutcome.R')
    dframe = data.frame()
    dframe = getDataByOutcome(outcome)
    if (state!=dframe[,3]) {
        stop("invalid state")
    }
    temp = split(dframe[,2],dframe[,3])
    lowest = min(as.numeric(temp[[state]]))
    best = dframe[,1][dframe[,2] == lowest & dframe[,3] == state]
    best
}


