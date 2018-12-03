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
  df[, i] <- commaless(df[, i])
}
for (i in 15:20) {
  df[, i] <- dollarless(df[, i])
}

#loop to transform variable types
for (i in 15:20) {
  df[, i] <- as.numeric(df[, i])
}
# Names with Index ####
# 1 date                     : Date
# 2 DJIopen                  : num
# 3 DJIhigh                  : num
# 4 DJIlow                   : num
# 5 DJIclose                 : num
# 6 DJIadjClose              : num
# 7 DJIvolume                : num
# 8 SPopen                   : num
# 9 SPhigh                   : num
# 10 SPlow                    : num
# 11 SPclose                  : num
# 12 SPadjClose               : num
# 13 SPvolume                 : num
# 14 fedFundRate              : num
# 15 totalSSRetired           : num
# 16 averageSSRetiredPay      : num
# 17 totalMaleSSRetired       : num
# 18 averageMaleSSRetiredPay  : num 
# 19 totalFemaleSSRetired     : num
# 20 averageFemaleSSRetiredPay: num
# 21 cpi                      : num
#
# Names with Index ####
df <- df[, c(1, 15, 17, 19, 21, 14, 7, 13, 2:6, 8:12, 16, 18, 20)]
# 1 date                     : Date
# 2 totalSSRetired           : num
# 3 totalMaleSSRetired       : num
# 4 totalFemaleSSRetired     : num
# 5 cpi                      : num
# 6 fedFundRate              : num
# 7 DJIvolume                : num
# 8 SPvolume                 : num
# 9 DJIopen                  : num
# 10 DJIhigh                  : num
# 11 DJIlow                   : num
# 12 DJIclose                 : num
# 13 DJIadjClose              : num
# 14 SPopen                   : num
# 15 SPhigh                   : num
# 16 SPlow                    : num
# 17 SPclose                  : num
# 18 SPadjClose               : num
# 19 averageSSRetiredPay      : num
# 20 averageMaleSSRetiredPay  : num
# 21 averageFemaleSSRetiredPay: num
#
# Variable Creation ####
# CPI Inflator
latestDate <- tail(df$date, n = 1)

baseCpi <- df$cpi[df$date == latestDate]
df$inflator <- baseCpi / df$cpi

df <- df[, c(1:6, 22, 7:21)]

realNames <-
  paste('real',
        colnames(df[, 10:22]),
        sep = "")

df[, realNames] <- df$inflator * df[10:22]

# Differences
diffNames <-
  paste('diff',
        c(colnames(df[10:22]),
          paste('Real', colnames(df[10:22]),
                sep = "")),
        sep = "")
df[, diffNames] <- rep(NA, nrow(df))
for (i in 36:61) {
  df[, i][2:nrow(df)] <- diff(df[, i - 26], lag = 1)
}
diffTargetNames <-
  paste('diff',
        c(colnames(df[2:4])),
        sep = "")
df[, diffTargetNames] <- rep(NA, nrow(df))
for (i in 62:64) {
  df[, i][2:nrow(df)] <- diff(df[, i - 60], lag = 1)
}

# Positive Indicator
posNames <- paste('pos',
                  c(colnames(df[10:22]),
                    paste('Real', colnames(df[10:22]), sep = "")),
                  sep = "")
df[, posNames] <- rep(0, nrow(df))
for (i in 65:90) {
  df[, i][df[, i - 29] > 0] <- 1
}

posTargetNames <-
  paste('pos',
        c(colnames(df[2:4])),
        sep = "")
df[, posTargetNames] <- rep(0, nrow(df))
for (i in 91:93) {
  df[, i][df[, i - 29] > 0] <- 1
}

df <- df[, c(1:9, 62:64, 91:93, 10:61, 65:90)]
df <- df[complete.cases(df),]
# Visulizations ####
plot(
  x = df$date,
  y = df$diffRealDJIclose,
  col = 'blue',
  lwd = 1,
  type = 'l',
  ylab = 'US Dollars ($)',
  xlab = 'Date'
)
points(
  x = df$date[df$postotalSSRetired == 1],
  y = df$diffRealDJIclose[df$postotalSSRetired == 1],
  pch = 24,
  col = 'darkgreen',
  cex = 0.8,
  lwd = 3
)
points(
  x = df$date[df$postotalSSRetired == 0],
  y = df$diffRealDJIclose[df$postotalSSRetired == 0],
  pch = 25,
  col = 'darkred',
  cex = 0.8,
  lwd = 3
)
legend(
  'topleft',
  legend = c(
    'Monthly Change in DJI Close',
    c('Positive ∆ in SS', 'Negative ∆ in SS')
  ),
  lty = c(1, c(NA, NA)),
  pch = c(NA, c(24, 25)),
  col = c('blue', c('darkgreen', 'darkred')),
  bg = c(NA, c('darkgreen', 'darkred')),
  lwd = c(2, c(3, 3))
)
title(main = 'Change in DJI Close')

# Timeseries Evaluation ####
library(tseries)
diffDJIOpen <-
  ts(
    df$diffDJIOpen,
    start = c(1985, 1),
    end = c(2018, 9),
    frequency = 12)

diffTotalSS <-
  ts(
    df$diffTotalSSRetired,
    start = c(1985, 1),
    end = c(2018, 9),
    frequency = 12)
PosRet <-
  ts(
    df$totalSSRetiredPos,
    start = c(1985, 1),
    end = c(2018, 9),
    frequency = 12)


plot(stl(DJIOpen, s.window = "period"), lwd = 1)
plot(stl(log(DJIOpen), s.window = "period"), lwd = 1)
stl(DJIOpen, s.window = "period")