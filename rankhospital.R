rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    #print(outcome)
    
    if((outcome!="pneumonia") & (outcome!="heart attack") & (outcome!="heart failure")) {
        #return(NA)
        stop("invalid outcome")
    }
    
    source('getDataByOutcome.R')
    dframe = data.frame()
    dframe = getDataByOutcome(outcome)
    
    if (!state %in% dframe[,3]) {
        return(NA)
        stop("invalid state")
    }
    
    splitRateByStates = split(dframe[,2],dframe[,3])
    splitHospitalByStates = split(dframe[,1],dframe[,3])
    if (num == "best") {
        place = 1
        print("I am the best")
    } else if (num == "worst") {
        place = length(splitRateByStates[[state]])
        print(place)
        print("I am the worst")
    } else {
        place = num
        #print(num)
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    print(state)
    if(length(splitRateByStates[[state]]) == 1 )
    {
        sortedMatrix  <- dframe[dframe[,3] == state,]
        return(sortedMatrix[1])
        
    } else if (place <= length(splitRateByStates[[state]])) {
    
        rateArrange  <-  lapply(splitRateByStates, as.numeric)
        
        arrange  <- mapply(order,rateArrange,splitHospitalByStates)

        sortedMatrix  <- dframe[dframe[,3] == state,][arrange[[state]],]
        
        #foo[order(foo["hospital"]),]
        #studentdata[studentdata$Drink == 'water',]
        
        #state = "FL"
        #test = dframe[dframe[,3] == state,]
        #arrangeFL <- order(rateArrange[[state]],splitHospitalByStates[[state]])
        #testOrder  <- test[arrangeFL,]
        
        sortedMatrix  <-  cbind(sortedMatrix,1:length(sortedMatrix[,1]))
        
            hospital  <-  sortedMatrix[,1][sortedMatrix[,4] == place]
            hospital
    } else {
        return(NA)
    }
    
}
