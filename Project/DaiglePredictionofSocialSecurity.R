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
baseCpi <- df$cpi[df$date == latestDate]
df$inflator <- baseCpi / df$cpi

names <- colnames(df[,c(2:6, 8:12, 16,18,20)])
realNames <- paste('real', colnames(df[,c(2:6, 8:12, 16,18,20)]),sep = "")

df$realDJIopen <- df$inflator * df$DJIopen
df$realDJIhigh <- df$inflator * df$DJIhigh
df$realDJIlow <- df$inflator * df$DJIlow
df$realDJIclose <- df$inflator * df$DJIclose
df$realDJIadjClose <- df$inflator * df$DJIadjClose

df$realSPopen <- df$inflator * df$SPopen
df$realSPhigh <- df$inflator * df$SPhigh
df$realSPlow <- df$inflator * df$SPlow
df$realSPclose <- df$inflator * df$SPclose
df$realSPadjClose <- df$inflator * df$SPadjClose

df$realAverageSSRetiredPay <- 



# for (i in realNames){
#   for (j in names){
#     assign(realNames[i], df$inflator * df[j])
#   }
# }
