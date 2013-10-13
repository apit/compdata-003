best <- function(state, outcome) {
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
  state.data <- data[data$State == state & !is.na(data[,col]), c(2, col)] # Name, outcome-death
  names(state.data) <- c("Name", "Death")
  state.data[, 2] <- as.numeric(state.data[, 2])
  options(op)
  
  ## Return hospital name in that state with lowest 30-day death rate
  name <- state.data[order(state.data$Death, state.data$Name), ][1, ][["Name"]]
  return(name)
}