complete <- function(directory, id = 1:332) 
      {
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
      
      data <- data.frame()

      for( i in id )
      {
            this_data <- read.csv( paste( directory, "//", sprintf( "%03d", i), ".csv", sep = ""))
            nobs_thisfile <- sum(complete.cases(this_data))
            this_file <- data.frame(id = i, nobs = nobs_thisfile)            
            data <- rbind( data, this_file )
      }      
      
      data

      }