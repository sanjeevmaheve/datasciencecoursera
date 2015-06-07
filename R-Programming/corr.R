corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    # (1) Get the dataframe (assignment 1 part 2) which gives id and
    # complete cases for the monitors data
    nobs_data <- complete(directory)
    # (2) Filter dataframe against given threashold
    nobs_data <- nobs_data[nobs_data[,2] > threshold,]
    # (3) Initialize the vector with length of filtered dataframe
    corr_data <- vector(mode="numeric", length=0)
    if (nrow(nobs_data) > 0) {
        corr_data <- vector(mode="numeric", length=nrow(nobs_data))
        # (4) Walk through each monitor and compute correlation for completed cases
        for(i in seq_along(corr_data)) {
            # Get the monitor id for which the correlation to be computed
            id <- nobs_data[i,1]
            dataframe <- read.csv(file.path(getwd(), directory, sprintf("%03d.csv", id)))
            good <- complete.cases(dataframe)
            corr_data[i] <- cor(dataframe[good,][,2], dataframe[good,][,3])
        }
    }
    corr_data
}