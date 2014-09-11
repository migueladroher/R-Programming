corr <- function(directory, threshold = 0) 
      {
            ## 'directory' is a character vector of length 1 indicating
            ## the location of the CSV files
            
            ## 'threshold' is a numeric vector of length 1 indicating the
            ## number of completely observed observations (on all
            ## variables) required to compute the correlation between
            ## nitrate and sulfate; the default is 0
            
            ## Return a numeric vector of correlations
      
      id <- 1:332
      
      corr <- c()
      
      for( i in id )
      {
            this_data <- read.csv( paste( directory, "//", sprintf( "%03d", i), ".csv", sep = ""))
            this_complete_cases <- complete.cases(this_data)
            nobs_thisfile <- sum(this_complete_cases)
            if( nobs_thisfile > threshold )
            {
                  correlation_this_data <- cor(this_data[this_complete_cases,]["sulfate"], this_data[this_complete_cases,]["nitrate"] )
                  corr <- c( corr, correlation_this_data )
            }      

      }      
      
      corr

      }