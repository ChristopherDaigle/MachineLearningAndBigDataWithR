# Chris Daigle
# Homework 2
# Exercise 3

set.seed(102)

Sales <- rep(NA, 100)
Online <- rep(NA, 100)
e <- rep(NA, 100)

Online[1] <- 2 * rnorm(1)
e[1] <- rnorm(1)
b0 <- 1
b1 <- 0
Sales[1] <- b0 + b1 * Online[1] + e[1]
rho1 <- 0.7
rho2 <- 0.7

reject <- 0
for (i in 1:1000) {
  for (t in 2:100) {
    Online[t] <- rho1 * Online[t - 1] + rnorm(1)
    e[t] <- rho2 * e[t - 1] + rnorm(1)
    Sales[t] <- b0 + b1 * Online[t] + e[t]
  }
  linear.fit <- lm(Sales ~ Online)
  summary(linear.fit)
  confint(linear.fit)[2,]
  if (confint(linear.fit)[2,][1] > 0 |
      confint(linear.fit)[2,][2] < 0) {
    reject <-
      reject + 1
  }
}
# because we said true DGP b0 = 0, so when the CI doesn't pass through zero, the
# null is rejected

percRej <- (reject / 1000) * 100
percRej

sprintf("The null hypothesis is rejected %s percent of the time.", percRej)
