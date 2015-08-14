getDataByState <- function(data, state) {
    sapply(split(data, state),min)
    
    result = data.frame()
    data =data.frame()
    ## Read data from csv file
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # filter by state
    result = cbind(df$Hospital.Name[df$State == state], df[,11][df$State == state],df[,17][df$State == state],df[,23][df$State == state])
    result = cbind(df$Hospital.Name[df$State == state], df[,17][df$State == state])
    result[,2] = as.numeric(result[,2])
    # remove rows with NAs
    result = na.omit(result)
    # stop on invalid state - no rows in data
    ...
    # sort on column 2 (death rate) to have lowest value on top
    arrange = order(result[,2])
    ## Return all data based on state and outcome
    arrangedRateForState = rbind(result[,3])[,arrange[1:285]]
    arragnedHospitalForState = rbind(result[,1])[,arrange]
}