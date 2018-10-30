#' ___
#' Chris Daigle
#' 
#' Bootstrap Lecture
#' 
#' 
#' 
#' ___
#' 

library(boot)

# Estimating the accuracy of a statistic of interest
library(ISLR)

head(Portfolio)

alpha.fn <- function(data, index) {
  X <- data[index, 1]
  Y <- data[index, 2]
  alpha <- (var(Y) - cov(X,Y)) / (var(X)+var(Y)-2*cov(X,Y))
  return(alpha)
}

alpha.fn(Portfolio, 1:nrow(Portfolio))

bootstrap <- boot(data = Portfolio, statistic = alpha.fn, R = 100)
bootstrap

bootstrap$t0
bootstrap$t # Every t-stat estimate from R 
# Estimateing the accuracy of a linear regression model
lm.cof <- function(data, index) {
  data.1 <- data[index,]
  names(data.1) <- letters[1:ncol(data.1)]
  coef(lm(a~., data = data.1))
}

Auto.boot <- Auto[, c(1, 4, 5)]
boot(data = Auto.boot, statistic = lm.cof, R = 100)
summary(lm(mpg ~ horsepower + weight, data = Auto.boot))

