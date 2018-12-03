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
str(df)
df <- df[, c()]
# Variable Creation ####

# CPI Inflator
latestDate <- tail(df$date, n = 1)
baseCpi <- df$cpi[df$date == latestDate]
df$inflator <- baseCpi / df$cpi
realNames <-
  paste('real',
        colnames(df[, c(2:6, 8:12, 16, 18, 20)]),
        sep = "")

df[, realNames] <- df$inflator * df[, c(2:6, 8:12, 16, 18, 20)]

# Differences
diffNames <-
  paste('diff',
        c(colnames(df[, c(2:6, 8:12, 16, 18, 20)]),
          paste('Real', colnames(df[, c(2:6, 8:12, 16, 18, 20)]),
                sep = "")),
        sep = "")
df[, diffNames] <- rep(NA, nrow(df))
for (i in c(36:61)) {
  df[, i][2:nrow(df)] <- diff(df[, i - 34], lag = 1)
}

# Positive Indicator
posNames <- paste('pos',
                  c(colnames(df[,c(2:6, 8:12, 16,18,20)]),
                    paste('Real', colnames(df[,c(2:6, 8:12, 16,18,20)]),sep = "")),
                  sep = "")
df[ ,posNames] <- rep(0, nrow(df))
# for (i in c(62:87)) {
#   df[,i][df[,i-26] > 0] <- 1
# }

for (i in c(2:6, 8:12, 16,18,20)) {
  df[,i+61][df[,i+35] > 0] <- 1
}

# Visulizations ####
plot(x = df$date,
     y = df$diffDJIopen,
     col = 'blue',
     lwd = 1,
     type = 'l',
     ylab = 'US Dollars ($)',
     xlab = 'Date')

points(x = df$date[df$totalSSRetired == 1],
       y = df$diffDJIopen[df$totalSSRetiredPos == 1],
       pch = 24,
       col = 'darkgreen',
       cex = 0.8,
       lwd = 3)
points(x = df$date[df$totalSSRetiredPos == 0],
       y = df$diffDJIopen[df$totalSSRetiredPos == 0],
       pch = 25,
       col = 'darkred',
       cex = 0.8,
       lwd = 3
       )

legend('topleft',
       legend = c('Monthly Change in DJI Open', c('Rise in Retired', 'Fall in Retired')),
       lty = c(1, c(NA, NA)),
       pch = c(NA, c(24, 25)),
       col = c('blue', c('darkgreen', 'darkred')),
       bg = c(NA, c('darkgreen', 'darkred')),
       lwd = c(2, c(3, 3))
       )
title(main = 'Change in DJIOpen')

plot(x = df$date, y = df$diffDJIClose, col = 'blue', lwd = 1, type = 'l', ylab = 'US Dollars ($)', xlab = 'Date')
points(x = df$date[df$totalSSRetiredPos == 1], y = df$diffDJIClose[df$totalSSRetiredPos == 1], pch = 24, col = 'darkgreen', cex = 0.8, lwd = 3)
points(x = df$date[df$totalSSRetiredPos == 0], y = df$diffDJIClose[df$totalSSRetiredPos == 0], pch = 25, col = 'darkred', cex = 0.8, lwd = 3)
legend('topleft',
       legend = c('Monthly Change in DJI Close', c('Rise in Retired', 'Fall in Retired')),
       lty = c(1, c(NA, NA)),
       pch = c(NA, c(24, 25)),
       col = c('blue', c('darkgreen', 'darkred')),
       bg = c(NA, c('darkgreen', 'darkred')),
       lwd = c(2, c(3, 3))
)
title(main = 'Change in DJIClose')

# Timeseries Evaluation ####
library(tseries)
diffDJIOpen <-
  ts(
    df$diffDJIOpen,
    start = c(1985, 1),
    end = c(2018, 9),
    frequency = 12
  )

diffTotalSS <-
  ts(
    df$diffTotalSSRetired,
    start = c(1985, 1),
    end = c(2018, 9),
    frequency = 12
  )
PosRet <-
  ts(
    df$totalSSRetiredPos,
    start = c(1985, 1),
    end = c(2018, 9),
    frequency = 12
  )

plot(diffDJIOpen, col = 'blue', lwd = 1, ylab = 'Changes in DJI Open Monthly')
points(x = df$date[PosRet == 0], y = diffDJIOpen[PosRet == 0], pch = 19, col = 'darkred', cex = 0.5)
points(diffDJIOpen[PosRet == 1], pch = 19, col = 'darkgreen', cex = 0.5)


plot(stl(DJIOpen, s.window = "period"), lwd = 1)
plot(stl(log(DJIOpen), s.window = "period"), lwd = 1)
stl(DJIOpen, s.window = "period")