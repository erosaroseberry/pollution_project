## E Rosa
## May 24, 2020

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
    
    files <- list.files(directory)
    
    IDs <- c()
    nobs <- c()
    for (i in id) {
        
        df <- read.csv(file = paste(directory, '/', files[i], sep = ''))
        IDs[length(IDs) + 1] <- c(i)
        nobs[length(nobs) + 1] <- c(sum(complete.cases(df)))
        
    }
    
    df <- data.frame('id' = IDs, 'nobs' = nobs, stringsAsFactors = F)
    return(df)
    
}
