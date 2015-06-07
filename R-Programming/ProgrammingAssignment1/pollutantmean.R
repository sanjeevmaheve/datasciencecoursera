pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    # (1) Read the list of the file name present under given directory.
    files <- dir(directory, pattern = '\\.csv', full.names = TRUE)
    # (2) Filter only required CSV file(s) depending on 
    # the range specified in the formal argument 'id'.
    files <- files[grep (paste(sprintf("%03d", id), collapse="|"), files)]
    
    # (3) Prepare a single list from the desired set of CSV input files.
    data <- lapply(files, read.csv)
    # (4) Convert the list into data frame for processing.
    table <- do.call(rbind, data)
    
    # (5) Compute mean of the pollutant after ignoring NAs
    pollutant_col <- table[, pollutant]
    # (6) Take out all unwanted values i.e. NAs and compute mean
    mean(pollutant_col[!is.na(pollutant_col)])
}