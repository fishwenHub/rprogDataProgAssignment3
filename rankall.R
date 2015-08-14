rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    ## Read outcome data
    ## Check that state and outcome are valid
    source("rankhospital.R")
    print(outcome)
    
    if((outcome!="pneumonia") & (outcome!="heart attack") & (outcome!="heart failure")) {
        #return(NA)
        stop("invalid outcome")
    }
    
    source('getDataByOutcome.R')
    dframe  <-  data.frame()
    dframe  <-  getDataByOutcome(outcome)
    
    place  <-  vector('numeric')
    splitRateByStates  <-  split(dframe[,2],dframe[,3])
    splitHospitalByStates  <-  split(dframe[,1],dframe[,3])
    if (num == "best") {
        place  <-  1
        print("I am the best")
    } else if (num == "worst") {
        for (i in 1:length(splitRateByStates)) {
            place  <-  c(place,length(splitRateByStates[[i]]))
        }
        print(place)
        print("I am the worst")
    } else {
        place  <-  num
        print(num)
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    #numArrange = lapply(splitRateByStates, as.numeric)
    #arrange = lapply(numArrange,order)
    
    #get list element, splitByStates, names within lapply / sapply 
    States  <-  names(sapply(splitRateByStates, names));
    Hospitals  <-  mapply(rankhospital, States[1:length(States)], outcome, place, SIMPLIFY = F)
    #sapply(people, function(x){as.numeric(x[2])})str(Hospitals)
    stateCol  <-  names(Hospitals)
    HospitalsCol  <-  vector('character')
    for (i in 1:length(names(Hospitals))) {
        HospitalsCol  <-  c(HospitalsCol, Hospitals[[stateCol[i]]])
    }
    HospitalsCol<<-HospitalsCol
    outputDF  <-  data.frame()
    outputDF  <-  rbind(outputDF, as.data.frame(HospitalsCol))
    outputDF  <-  cbind(outputDF,as.data.frame(stateCol))
    
    colnames(outputDF)  <-  c( 'hospital' , 'state')
    outputDF
}