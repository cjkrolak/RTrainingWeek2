pollutantmean <- function(directory, pollutant, id=1:332) {
  ## return the mean of the pollutant across all monitors listed in the ID vector
  ## ignore the NA values, no rounding.
  
  ## directory: character vector length 1 indicating location of CSV files
  ## pollutant: character vector of length 1 indicating the name of the pollutant,
  ##            either "sulfate" or "nitrate".
  ## id is an integer vector indicating the monitoring ID numbers to be used.
  
  ## initialize variables
  pollutant_sum <- 0
  pollutant_count <- 0
  id_vector <- vector()
  
  ## iterate over all monitors (files)
  for (monitor in id) {

    ## open the file and load it
    file_path <- paste0(directory, "\\", sprintf("%03d", monitor), ".csv")
    # print(paste("opening file: ", file_path))
    pollutant_table <- read.csv(file_path, TRUE)
    # print(paste("table size=", dim(pollutant_table)))
    
    # grab vector of interest and filter out the NA
    pollutant_vector <- pollutant_table[!is.na(pollutant_table[,pollutant]),pollutant]
    vector_size <- length(pollutant_vector)
    # print(paste("vector size=", vector_size))
    vector_sum <- sum(pollutant_vector)
    # print(paste("vector sum=", vector_sum))
  
    ## calculate the sum and counts for running total
    pollutant_mean = vector_sum / vector_size
    # print(paste("pollutant mean for ID=", monitor, ": ", pollutant_mean))
    
    ## running totals for aggregate stats
    pollutant_sum <- pollutant_sum + vector_sum
    pollutant_count <- pollutant_count + vector_size
    id_vector <- c(id_vector, id)
    
  }  # for
  
  # calculate the mean
  pollutant_sum / pollutant_count

}  # pollutantmean