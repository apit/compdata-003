corr <- function(directory, threshold = 0) {
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  correlations <- vector()
  complete.data <- complete(directory)
  ids <- subset(complete.data, nobs > threshold)[,1]
  for(i in ids) {
    data <- getmonitor(i, directory)
    data <- data[!is.na(data$sulfate) & !is.na(data$nitrate), ]
    correlations <- append(correlations, cor(data$sulfate, data$nitrate)) 
  }
  return(correlations)
}