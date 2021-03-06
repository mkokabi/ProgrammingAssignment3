best <- function(state, outcomeName) {
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
    if (nrow(outcomeInState) == 0)
    {
        stop('invalid state')
        return;
    }
    
    ## Return hospital name in that state with lowest 30-day death
    hospitalsOrdered<-outcomeInState[
        order(
            as.numeric(outcomeInState[,2]), 
            outcomeInState$Hospital.Name,
            na.last = TRUE),
        1]
    
    ## rate
    hospitalsOrdered[1]
}