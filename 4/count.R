count <- function(cause=NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {
    stop("Please specify cause of death")
  }
  
  ## Check that specific "cause" is allowed; else throw error
  causes <- c("asphyxiation", "blunt force", "other", "shooting", 
              "stabbing", "unknown")
  if (!(cause %in% causes)) {
    causes.str <- paste(causes, collapse=", ")
    stop(paste("Please specify one of", causes.str, " "))
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract causes of death
  r <- regexec("Cause: (.*?)</dd>", homicides)
  m <- regmatches(homicides, r)
  m <- sapply(m, function(x) tolower(x[2]))
  causes.table <- table(m)
  
  ## Return integer containing count of homicides for that cause
  causes.table[[cause]]
}
