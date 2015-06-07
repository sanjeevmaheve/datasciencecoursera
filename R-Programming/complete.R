complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    # (1) Initialize the empty vector for storing output nobs
    nobs = vector(length=length(id))
    # (2) Walk through each monitor and check for the complete cases
    for(i in seq_along(id)) {
        dataframe <- read.csv(file.path(getwd(), directory, sprintf("%03d.csv", id[i])))
        good <- complete.cases(dataframe)
        nobs[i] <- nrow(dataframe[good,])
    }
    # (3) Prepare data frame for the output
    data.frame(id, nobs)
}