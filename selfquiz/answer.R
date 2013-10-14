
dataset <- read.csv("selfquiz-data.csv", colClasses="numeric")

# What are the column names of the data frame?
names(dataset)

# What are the row names of the data frame?
row.names(dataset)

# Extract the first 6 rows of the data frame and print them to the console
print(dataset[1:6,])
head(dataset, 6)

# How many observations (i.e. rows) are in this data frame?
nrow(dataset)

# Extract the last 6 rows of the data frame and print them to the console
tail(dataset, 6)

# How many missing values are in the "Ozone" column of this data frame?
nrow(dataset[is.na(dataset$Ozone), ])

# What is the mean of the "Ozone" column in this dataset? Exclude 
# missing values (coded as NA) from this calculation.
mean(dataset$Ozone, na.rm=TRUE)

# Extract the subset of rows of the data frame 
# where Ozone values are above 31 and Temp values are above 90.
subset(dataset, Ozone > 31 & Temp > 90)

# Use a for loop to create a vector of length 6 containing the mean 
# of each column in the data frame (excluding all missing values).
v <- vector()
for (col in names(dataset)) {
  v <- append(v, mean(dataset[[col]], na.rm=TRUE))
}
print(v)

# Use the apply function to calculate the standard deviation 
# of each column in the data frame (excluding all missing values).
apply(dataset, 2, sd, na.rm=TRUE)

# Calculate the mean of "Ozone" for each Month in the data frame and 
# create a vector containing the monthly means (exclude all missing values).
aggregate(dataset$Ozone, list(Month=dataset$Month), mean, na.rm=TRUE)[,2]

# Draw a random sample of 5 rows from the data frame
set.seed(1)
dataset[sample(nrow(dataset), 5), ]
