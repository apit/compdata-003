complete <- function(directory, id = 1:332) {
  nobs = vector()
  for (i in id) {
    monitor.data <- getmonitor(i, directory)
    nob <- complete.cases(monitor.data)
    nobs <- append(nobs, length(nob[nob==TRUE]))
  }
  
  result = cbind.data.frame(id, nobs)
  return(result)
}