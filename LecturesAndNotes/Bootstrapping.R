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

# Another bootstrap method in a linear regression model is to
# resample residuals instead of (X, Y). The former is called Residual
# Bootstrapping and the latter is called Pairwise Bootstrapping. I beleve that
# boot() is based on the Pairwise Bootstrap. Estimate the standard error and
# bootstrap the 95% confidence intervals for the coeficient of horsepower with
# Residual Bootstrapping.

lm.fit <- lm(mpg~horsepower + weight, data = Auto.boot)
t <- summary(lm.fit)$coefficients[2,3]

tstar <- NA

for (r in 1:1000) {
  ustar <- sample(lm.fit$residuals, nrow(Auto), replace = TRUE)
  ystar <- lm.fit$coefficients[1] + lm.fit$coefficients[2] * Auto$horsepower + lm.fit$coefficients[3] * Auto$weight + ustar
  lm.fit.star <- lm(ystar ~ Auto$horsepower + Auto$weight)
  tstar[r] <- (lm.fit.star$coefficients[2] - lm.fit$coefficients[2])/ summary(lm.fit.star)$coefficients[2,2]
}

tstar.ordered <- sort(tstar, decreasing = FALSE)

cv.star <- c(tstar.ordered[25], tstar.ordered[975])
t
cv.star


