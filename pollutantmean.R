## E Rosa
## May 24, 2020

pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; may only be 'sulfate' or 'nitrate'
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant accross all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    
    if ( xor(pollutant == 'sulfate', pollutant == 'nitrate') ) {
    
        files <- list.files(directory)
        column <- ifelse(pollutant == 'sulfate', 2, 3)
    
        pol_vals <- c()
        for (i in id) {
        
            df <- read.csv(file = paste(directory, '/', files[i], sep = ''))
            pol_data <- df[ , column][which(!is.na(df[ , column]))]
        
            if (length(pol_data) > 0) {
            
                pol_vals[(length(pol_vals) + 1):(length(pol_vals) + length(pol_data))] <- pol_data
        
            }
            else {
            
                next
            
            }
        
        }
    
        return(mean(pol_vals))
        
    }
    
    else {
        
        message("Error: pollutant must be a character vector of length 1 with value either 'nitrate' or 'sulfate'")
        break
        
    }
    
}
