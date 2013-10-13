rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  states <- factor(data$State)
  outcome.name <- c("heart attack", "heart failure", "pneumonia")
  outcome.col <- c(11, 17, 23)
  
  ## Check that state and outcome are valid
  if (!(state %in% states)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% outcome.name)) {
    stop("invalid outcome")
  }
        
  op <- options(warn = (-1))
  outcome.opts <- data.frame(col=outcome.col, name=outcome.name)
  col <- outcome.opts[outcome.opts$name == outcome, ][["col"]]
  data[, col] <- as.numeric(data[, col])
  state.data <- data[data$State == state & !is.na(data[,col]), c(2, col)]
  
  if (is.integer(num) & num > nrow(state.data)) {
    return(NA)
  } 
      
  names(state.data) <- c("Name", "Death")
  options(op)
  
  ## Return hospital name in that state with lowest 30-day death rate
  row <- state.data[order(state.data$Death, state.data$Name), ]
  if (num == "best") {
    idx <- 1
  } else if (num == "worst") {
    idx <- nrow(state.data)
  } else {
    idx <- num
  }
  result <- row[idx, ][["Name"]]
  return(result)
}