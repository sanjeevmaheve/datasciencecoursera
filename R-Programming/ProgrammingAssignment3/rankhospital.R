## The columns of interest are:
##
## 11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the 
##     risk adjusted rate (percentage) for each hospital.
## 17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the 
##     risk adjusted rate (percentage) for each hospital.
## 23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the risk 
##     adjusted rate (percentage) for each hospital.
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    inputdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid
    state_data <- inputdata[, "State"]
    if(is.na(match(state, state_data))) stop ('invalid state')
    
    ## Prepare the valid input as it is not the exact match unlike state.
    ## Its just like a lookup table for extractng the column index using name.
    outcome_lookup <- data.frame(outcome = c("heart attack", 
                                             "heart failure", 
                                             "pneumonia"), 
                                 index = c(11, 17, 23))
    ## Check that the outcome is valid.
    if(is.na(match(outcome, outcome_lookup[,1]))) stop ('invalid outcome')
    
    ## Get new dataframe from the input big frame. Get the outcome indices
    ## as first step. as.numeric from "Not Available" -> "NA" throws warning,
    ## so suppressing it momentarlly.
    column.index = outcome_lookup[grep(outcome, outcome_lookup$outcome),]$index
    desired_outcome <- suppressWarnings(
        data.frame(hospital = inputdata$Hospital.Name, 
                   state = inputdata$State, 
                   mortality = as.numeric(inputdata[,column.index]))
    )
    
    ## Get the dataframe by filtering NAs from the input.
    complete_outcome <- desired_outcome[complete.cases(desired_outcome),]
    
    ## Filter the input data only around desired state.
    filtered_outcome <- complete_outcome[complete_outcome$state == state,]
    
    ## Handling ties. If there is a tie for the best hospital 
    ## for a given outcome, then the hospital names are sorted 
    ## in alphabetical order and the first hospital in that set 
    ## should be chosen (i.e. if hospitals “b”, “c”, and “f” 
    ## are tied for best, then hospital “b” should be returned).
    sorted_outcome <- filtered_outcome[order(filtered_outcome$mortality, 
                                             filtered_outcome$hospital),]
    ## Rank the filtered outcome dataframe
    ranked_outcome <- data.frame(sorted_outcome, 
                                 rank = 1:nrow(sorted_outcome))
    ## Compute the desired index for returning the result.
    if (num == "best") {
        index <- 1
    } else if (num == "worst") {
        index <- nrow(ranked_outcome)
    } else {
        index <- as.numeric(num)
    }
    ## Return hospital name in that state with lowest 30-day death rate.
    as.character(sorted_outcome[index,1])
}