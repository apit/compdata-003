agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (is.null(age) | !is.numeric(age)) {
    stop("Please specify age, as integer")
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is given
  r <- regexec("(\\d+) years old</dd>", homicides)
  m <- regmatches(homicides, r)
  m <- sapply(m, function(x) x[2])
  ages <- levels(as.factor(m))
  
  if (!(age %in% ages)) {
    return(0)
  }
  
  age.table <- table(m)
  
  ## Return integer containing count of homicides for that age
  age.table[[as.character(age)]]
}