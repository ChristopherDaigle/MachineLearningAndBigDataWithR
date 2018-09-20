# Chris Daigle
# Exercise 3

# Download "Auto.csv" from http://www-bcf.usc.edu/~gareth/ISL/data.html and
# estimate the regression mpg on weight using the KNN method. Draw the
# regression line on the scatterplot and compare it with liner regression line.

rm(list = ls())

setwd('~/Git/MachineLearningAndBigDataWithR')
dataName <- 'Auto.csv'
Adata <- read.csv(dataName, stringsAsFactors = FALSE)

str(Adata)
Adata$horsepower <- as.numeric(Adata$horsepower, na.omit = TRUE)
str(Adata)

mpg <- Adata$mpg
weight <- Adata$weight
plot(weight, mpg)
lm1 <- lm(mpg~weight)
summary(lm1)
abline(lm1, lwd = 3, col = 'blue')

knn <- function(x0, X, Y, K) {
  x0 <- matrix(rep(x0, length(Y)), byrow = TRUE)
  X <- matrix(X)
  distance <- rowSums((x0 - X)^2)
  rank <- order(distance)
  Y_K <- Y[rank][1:K]
  mean(Y_K)
}

knn(2500, x, y, 5)

x <- sort(weight)
fhat <- matrix(rep(NA, 397*6), 397, 6)
for (j in 1:6) {
  K = 2*j - 1
  for (i in 1:397) {
    fhat[i,j] <- knn(x[i], weight, mpg, K)
  }
}

lines(x,fhat[,1], col = 'red', lwd= 2)
lines(x,fhat[,2], col = 'purple', lwd= 2)
lines(x,fhat[,3], col = 'black', lwd= 2, lty = 10)
lines(x,fhat[,4], col = 'blue', lwd= 2)
lines(x,fhat[,5], col = 'green', lwd= 2)
lines(x,fhat[,6], col = 'yellow', lwd= 2)