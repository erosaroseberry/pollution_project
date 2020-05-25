## E Rosa
## May 24, 2020

corr <- function(directory, threshold = 0) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations

    files <- list.files(directory)
    
    corrs <- c()
    for (i in files) {
        
        df <- read.csv(file = paste(directory, '/', i, sep = ''))
        
        if (sum(complete.cases(df)) > threshold) {
            
            nitrate <- df$nitrate[complete.cases(df)]
            sulfate <- df$sulfate[complete.cases(df)]
            corrs[length(corrs) + 1] <- cor(nitrate, sulfate)
        
        }
        
        else {
            
            next
            
        }
        
    }
    
    return(corrs)
    
}
