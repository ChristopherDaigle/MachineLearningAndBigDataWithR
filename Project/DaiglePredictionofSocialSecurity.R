#' ___
#' Chris Daigle
#' Prediction of Social Security Awards
#' 
#' 

# Prepare workspace ####
rm(list = ls())
setwd('~/Git/MachineLearningAndBigDataWithR/Data')
dataName <- 'assembled.csv'
df <- read.csv(dataName, stringsAsFactors = FALSE)
# Summarize and clean data ####
head(df)
df <- df[-1]
head(df)
str(df)

# Variable Manipulation ####
# Set dates
df$date <- as.Date(df$date, "%Y-%m-%d")
# Functions to clean data
spaceless <- function(x) {
  x <- gsub(" ", ".", x)
  x
}

commaless <- function(x) {
  x <- gsub(",", "", x)
  x
}

dollarless <- function(x) {
  x <- gsub("\\$", "", x)
  x
}

#loops to apply functions
for (i in 15:20) {
  df[,i] <- commaless(df[,i])
}
for (i in 15:20) {
  df[,i] <- dollarless(df[,i])
}

#loop to transform variable types
for (i in 15:20) {
  df[,i] <- as.numeric(df[,i])
}
# Check that the transformations applied
str(df)

# Timeseries Evaluation ####
#library(tseries)
df1 <-
  ts(
    df,
    start = head(df$date, n = 1),
    end = tail(df$date, n = 1),
    frequency = 12
  )
summary(df1)

# Variable Creation ####
df$diffTotalSSRetired <- rep(0,nrow(df))
tempdiffTotalSSRetired <- diff(df$totalSSRetired, lag = 1)
df$diffTotalSSRetired[2:405] <- tempdiffTotalSSRetired
df$totalSSRetiredPos <- 0
df$totalSSRetiredPos[df$diffTotalSSRetired > 0] <- 1

#CPI Inflator
latestDate <- tail(df$date, n=1)
baseCpi <- df$cpi[df$date == latestDate]
df$inflator <- baseCpi / df$cpi
realNames <- paste('real', colnames(df[,c(2:6, 8:12, 16,18,20)]),sep = "")

df[, realNames] <-df$inflator * df[,c(2:6, 8:12, 16,18,20)] 




