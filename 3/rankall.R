rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  states <- levels(factor(data$State))
  outcome.name <- c("heart attack", "heart failure", "pneumonia")
  outcome.col <- c(11, 17, 23)
  
  ## Check that outcome are valid
  if (!(outcome %in% outcome.name)) {
    stop("invalid outcome")
  }
  
  op <- options(warn = (-1))
  outcome.opts <- data.frame(col=outcome.col, name=outcome.name)
  col <- outcome.opts[outcome.opts$name == outcome, ][["col"]]
  data[, col] <- as.numeric(data[, col])
  outcome.data <- data[!is.na(data[,col]), c(2, 7, col)] #name, state, outcome

  
  names(outcome.data) <- c("Name", "State", "Death")
  rows <- outcome.data[order(outcome.data$State, outcome.data$Death, outcome.data$Name), ]
  options(op)
  
  ## Return hospital name in that state with lowest 30-day death rate
  result <- data.frame(hospital=character(), state=character())
  
  for (state in states) {
    row.state <- rows[rows[,2]==state, ]
    idx <- num
    if (num == "best") {
      idx <- 1
    } else if (num == "worst") {
      idx <- nrow(row.state)
    }
    name <- row.state[idx, ][["Name"]]
    result <- rbind(result, data.frame(hospital=name, state=state))
  }
  return(result)
}
