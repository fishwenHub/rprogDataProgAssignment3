getDataByOutcome <- function(outcome) 
{
    result = data.frame()
    ## Read data from csv file
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # filter data containing 3 columns:
    # - hospital name,
    # - death rate based on outcome, and
    # - state (useful for rankall function)
    if (outcome == "heart attack") {
        n = 11
    } else if (outcome == "heart failure") {
        n = 17
    } else if (outcome == "pneumonia") {
        n = 23
    } else {
        #print Warning msg
    }
    result = cbind(df[,2],df[,n], df[,7], deparse.level = 2)
    # convert death rate (column 2) to numeric
    result[,2] = as.numeric(result[,2])
    # remove rows with NAs in column 2
    result = na.omit(result)
    result
}

