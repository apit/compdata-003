## Programming 1 Quiz

```{r}
setwd('/Volumes/Misc/online-learning//assignments/cda//r-project')
h <- read.csv("hw1_data.csv")
```

#### 1) What are the column names of the dataset?
```{r}
colnames(h)
```

#### 2) Extract the first 2 rows of the data frame and print them to the console. What does the output look like?
```{r}
h[1:2,]
```

#### 3) How many observations (i.e. rows) are in this data frame?
```{r}
length(h)
```

#### 4) Extract the last 2 rows of the data frame and print them to the console. What does the output look like?
```{r}
tail(h, 2)
```

#### 5) What is the value of Ozone in the 47th row?
```{r}
h[,1][47]
```

#### 6) How many missing values are in the Ozone column of this data frame?
```{r}
length(h[,1][is.na(h[,1])])
```

#### 7) What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
```{r}
mean(h[,1][!is.na(h[,1])])
```

#### 8) Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
```{r}
z <- h[h[,1] > 31 & !is.na(h[,1]) & h[,4] > 90, ] #, or
z <- subset(h, Ozone > 31 & Temp > 90)
mean(z[,2])
```

#### 9) What is the mean of "Temp" when "Month" is equal to 6?
```{r}
z <- subset(h, Month == 6)
mean(z[,4])
```

#### 10)What was the maximum ozone value in the month of May (i.e. Month = 5)?
```{r}
z <- subset(h, Month == 5 & !is.na(Ozone))
max(z[,1])
```

### Extra programming assignment

```{r}
my.unique<- function(x){
  ones<-levels(factor(unlist(x)))
  return(ones)
}

#or

my.unique<- function(x){
 ifelse(is.numeric(x),
        ones<-as.numeric(levels(factor(unlist(x)))),
        ones<-levels(factor(unlist(x)))
        )
  return(ones)
}
```

