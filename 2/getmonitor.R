getmonitor <- function(id, directory, summarize = FALSE) {
  path <- paste(directory, "/", sprintf("%03s", id), ".csv", sep="")
  data <- read.csv(path)
  if (summarize) {
    print(summary(data))
  }
  
  return(data)
}