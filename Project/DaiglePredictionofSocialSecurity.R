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
