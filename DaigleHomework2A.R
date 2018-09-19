# Chris Daigle
# Exercise 3

library('sandwich')
set.seed(102)
A <- 1000
Sales <- rep(NA, A)
Online <- rep(NA, A)
e <- rep(NA, A)

Online[1] <- 2 * rnorm(1)
e[1] <- rnorm(1)
b0 <- 1
b1 <- 0
Sales[1] <- b0 + b1 * Online[1] + e[1]
rho1 <- 0.7
rho2 <- 0.7

reject <- 0
for (i in 1:A) {
  for (t in 2:A) {
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
    rej[i] <- (o)
  }
}

for (i in 1:A) {
  Online[1] <- 2*rnorm(1)
  e[1] <- rnorm(1)
  b0 <- 1
  b1 <- 0
  Sales[1] <- b0 + b1 * Online[1] + e[1]
  rho1 <- 0.7
  rho2 <- 0.7
  
  for (t in 2:100) {
    online[t] <- rho1 * online[t-1] + rnorm(1)
    e[t] <- rho2 * e[t-1] + rnorm(1)
    Sales[t] <- b0 + b1 * online[t] + e[t]
  }
  
  linear.fit <- lm(Sales~Online)
  rej[i] <- (0< confint(linear.fit)[2,1]) + (0 > confint(linear.fit)[2,2])
  se2 <- sqrt(NeweyWest(linear.fit)[2,2])
  ci2 <- c(coefficients(linear.fit)[2] - 1.96 * se2, coefficients(linear.fit)[2] + 1.96 * se2)
  rej2[i] <- (0<ci2[1]) + (0>ci2[2])
}
# because we said true DGP b0 = 0, so when the CI doesn't pass through zero, the
# null is rejected

percRej <- (reject / 1000) * 100
percRej

sprintf("The null hypothesis is rejected %s percent of the time.", percRej)
