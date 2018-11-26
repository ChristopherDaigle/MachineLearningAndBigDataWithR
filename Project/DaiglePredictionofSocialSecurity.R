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

# Variable Creation ####
latestDate <- tail(df$date, n=1)
df$inflator <- df[df$date == latestDate]$cpi / df$cpi
