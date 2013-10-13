library(lattice)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
deathCol <- grep("^Hospital.30.Day.Death", names(outcome))
for(i in deathCol) outcome[,i] <- as.numeric(outcome[,i])

# 1 Plot the 30-day mortality rates for heart attack

hist(outcome[, deathCol[1]], main="Heart Attack 30−day Death Rate", xlab="30−day Death Rate")

# 2 Plot the 30-day mortality rates for heart attack, heart failure,and pneumonia

oldpar <- par(no.readonly=TRUE)
par(mfrow=c(3,1))
titles = c("Heart Attack", "Heart Failure", "Pneumonia")
for (i in 1:length(deathCol)) {
  col <- deathCol[i]
  x <- outcome[, col]
  mean <- mean(x, na.rm=TRUE)
  lim <- range(outcome[,deathCol], na.rm=TRUE)
  title <- bquote(.(titles[i])~"("~bar(X)==.(mean)~")")
  hist(x, main=title, xlab="30-day Death Rate", xlim=lim, prob=TRUE)
  lines(density(x, na.rm=TRUE), col="red")
  m <- median(x, na.rm=TRUE)
  abline(v=m)
}
par(oldpar)

# 3 Plot 30-day death rates by state

death <- aggregate(Provider.Number~State, data=outcome, FUN=length)
names(death) <- c("State", "Death")
death <- subset(death, !is.na(Death) & Death>20)
outcome2 <- subset(outcome, 
                   !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) &
                   State %in% death$State, 
                   select=c(State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
names(outcome2) <- c("State", "Death")
byMedian <- with(outcome2, reorder(State, Death, median))
oldpar <- par(no.readonly=TRUE)
par(las=2, cex.axis=.7)
boxplot(Death ~ byMedian, data=outcome2,
        main="Heart Attack 30-day Death Rate by State",
        ylab="30-day Death Rate")
par(oldpar)

# 4 Plot 30-day death rates and numbers of patients
hospital <- read.csv("hospital-data.csv", colClasses="character")
outcome.hospital <- merge(outcome, hospital, by="Provider.Number")
death <- as.numeric(outcome.hospital[, 11])
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)
xy <- xyplot(death ~ npatient|owner, 
       xlab="Number of Patients Seen", 
       ylab="30-day Death Rate", 
       main="Heart Attack 30-day Death Rate by Ownership",
       panel=function(x,y,...) {
         panel.xyplot(x,y,...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })

