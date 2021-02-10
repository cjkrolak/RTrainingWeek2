corr <- function(directory, threshold=0) {
  ## return a numeric vector of correlations
  ## NOTE do not round the result
  
  ## directory: character vector length 1 indicating location of CSV files
  ## threshold is a numeric vector of length 1 indicating the
  ## number of complete observed observations (on all variables) required to
  ## compute the correlation between nitrate and sulfate; the default is 0
  
  ## initialize variables
  corr_vector = numeric()
  
  ## iterate over all monitors (files)
  for (monitor in 1:332) {
    
    ## open the file and load it
    file_path <- paste0(directory, "\\", sprintf("%03d", monitor), ".csv")
    # print(paste("opening file: ", file_path))
    t <- read.csv(file_path, TRUE)
    # print(paste("table size=", dim(t)))
    
    # subset table to filter out the NA
    table_mask = !is.na(t)[,"sulfate"] & !is.na(t)[,"nitrate"]
    filtered_table = t[table_mask,]
    
    nobs = sum(as.numeric(table_mask))
    if (nobs > threshold) {
      corr_result = cor(filtered_table$sulfate, filtered_table$nitrate)
      # print(paste("monitor=", monitor, ", threshold=", nobs, " corr=", corr_result))
      corr_vector = c(corr_vector, corr_result)
    }
    else {
      # print(paste("monitor=", monitor, ", nobs=", nobs, " corr=", -1))
    }
  }  # for
  
  # return the coefficent vector
  corr_vector
  
}  # complete