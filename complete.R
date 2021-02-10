complete <- function(directory, id = 1:332) {
  ## return a data frame of the form:
  ## id nobs
  ## 1 117
  ## 2 1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## directory: character vector length 1 indicating location of CSV files
  ## id is an integer vector indicating the monitoring ID numbers to be used.
  
  ## initialize variables
  df <- data.frame("id"=integer(), "nobs"=integer(), stringsAsFactors=FALSE)
  
  ## iterate over all monitors (files)
  for (monitor in id) {
    
    ## open the file and load it
    file_path <- paste0(directory, "\\", sprintf("%03d", monitor), ".csv")
    # print(paste("opening file: ", file_path))
    t <- read.csv(file_path, TRUE)
    # print(paste("table size=", dim(t)))
    
    # subset table to filter out the NA
    nobs = sum(as.numeric(!is.na(t)[,"sulfate"] & !is.na(t)[,"nitrate"]))
    # print(paste("monitor=", monitor, ", nobs=", nobs))
    df <- rbind(df, data.frame("id"=monitor, "nobs"=nobs))
  }  # for
  
  # return the data table
  df
  
}  # complete