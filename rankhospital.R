rankhospital <- function(state, outcomeName, num = "best") {
    ## Read outcome data
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    
    colIndex <- 0
    if (outcomeName == 'heart attack')
    {
        colIndex <- 11
    } else if (outcomeName == 'heart failure')
    {
        colIndex <- 17
    } else if (outcomeName == 'pneumonia')
    {
        colIndex <- 23
    } 
    if (colIndex == 0)
    {
        stop('invalid outcome')
        return;
    }
    
    outcomeInState <- outcome[outcome$State == state,c(2, colIndex)]
    #print (outcomeInState)

    if (nrow(outcomeInState) == 0)
    {
        stop('invalid state')
        return;
    }
    
    availOutcomesInState <- as.numeric(outcomeInState[,2])
    availOutcomesInState <- availOutcomesInState[!is.na(availOutcomesInState)]
    # View(availOutcomesInState)
    
    ## Return hospital name in that state with the given rank
    hospitalsOrdered<-outcomeInState[
        order(
            as.numeric(outcomeInState[,2]), 
            outcomeInState$Hospital.Name,
            na.last = TRUE),
        c(1,2)]
    availHospitalsOrdered <- hospitalsOrdered[hospitalsOrdered[2]!="Not Available",c(1,2)]
    
#     ranked <- rank(availOutcomesInState, na.last = TRUE,ties.method = "first")
#     View(ranked)
    
    ## 30-day death rate
    if (num == "best")
        result <- availHospitalsOrdered[1,1]
    else if (num == "worst")
        result <- availHospitalsOrdered[nrow(availHospitalsOrdered),1]
    else
        result <- availHospitalsOrdered[num,1]
    
    result
}