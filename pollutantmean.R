
pollutantmean <- function(directory, pollutant, id = 1:332) {
    
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
