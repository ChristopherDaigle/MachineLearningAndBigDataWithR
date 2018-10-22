#' ___
#' Chris Daigle
#' 
#' Homework 0
#' 
#' Exercise: 1
#' 
#' ___

# Example 1: Wage Data ####

# install.packages('ISLR')
library('ISLR')
# data()

#
str(Wage)
head(Wage)

x1 <- Wage$age
x2 <- Wage$year
x3 <- Wage$education

y <- Wage$wage

plot(x1, y)
linear <- lm(y~x1)
abline(linear, col = 'blue', lwd = 3)

data1Temp <- data.frame(x1, y)
data1 <- data1Temp[order(data1Temp$x1),]
head(data1)
tail(data1)

plot(data1$x1, data1$y, xlab = 'wage', ylab = 'age')
lpr <- loess(data1$y~data1$x1)
lines(data1$x1, lpr$fitted, col = 'red', lty = 1, lwd = 3)

plot(x2, y, ylab = 'wage', xlab = 'year')
linear2 <- lm(y~x2)
abline(linear2, col = 'blue', lwd = 3)
summary(linear2)

str(Wage$education)
summary(Wage$education)
x3 <- as.numeric(x3)
boxplot(y~x3, col = 2:6, xlab = 'education level', ylab = 'wage')

# Example 2 ####
# Stock Market Data
rm(list = ls())

head(Smarket)
tail(Smarket)
par(mfrow = c(1,3))
boxplot(Lag1~Direction, data = Smarket, col = c('blue', 'red'), main = 'Yesterday', xlab = 'Todays direction')
boxplot(Lag2~Direction, data = Smarket, col = c('blue', 'red'), main = 'Two Days Ago', xlab = 'Todays direction')
boxplot(Lag3~Direction, data = Smarket, col = c('blue', 'red'), main = 'Three Days Ago', xlab = 'Todays direction')

# Example 3 ####
# Gene expressions: consists of 64 cancer cell lines. Each cancer cell line has 6830 gene expression measurements
rm(list = ls())
par(mfrow= c(1,2))
head(NCI60)
summary(NCI60)

cc <- as.factor(NCI60$labs)
ccn <- as.numeric(cc)

pc.data <- prcomp(NCI60$data, scale. = TRUE)
plot(pc.data$x[,1], pc.data$x[,2], pch = 20, lwd = 5)
plot(pc.data$x[,1], pc.data$x[,2], col = 2 * ccn, pch = 20, lwd = 5)


#' ___
#' Chris Daigle
#' 
#' Homework 1
#' 
#' Exercise: 2
#' 
#' ___
#
# 1 ####
# Download the housing dataset from https://www.kaggle.com/harlfoxem/housesalesprediction and run a regression to predict housing prices.

rm(list = ls())

setwd('~/Git/MachineLearningAndBigDataWithR/Data')

# dataurl  = "https://www.kaggle.com/harlfoxem/housesalesprediction/downloads/kc_house_data.csv/1"
dataName <- 'kcHouseData.csv'
# download.file(dataurl, destfile = dataName)
data <- read.csv(dataName, stringsAsFactors = FALSE)
str(data)

# price: price of home
# bedrooms: number of bedrooms
# floors: number of floors
# sqft_living: square footage of living space
# yr_built

lm.fit <-
  lm(price ~ sqft_living + bedrooms + floors + yr_built, data = data)
summary(lm.fit)
names(lm.fit)

# 2 ####
# Build a model to predict the housing price given characteristics of a house in the dataset.
# Consider the following predictors: season, sqft_living, yr_built, interaction of sqft_living yr_built, and waterfront
#     Create a variable "season" which equals
#     "Winter" if a house was sold in Jan, Feb, Mar, Dec.
#     "Spring" if it was sold in Apr, May, Jun
#     "Summer" if it was sold in Jul, Aug
#     "Fall" if it was sold in Sep, Oct, Nov.
# Do you find any seasonality in housing price?

data <-
  transform(
    data,
    Year = substr(date, 1, 4),
    Month = substr(date, 5, 6),
    Day = substr(date, 7, 8)
  )

data$cleanDate <-
  as.Date(paste0(data$Year, '-', data$Month, '-', data$Day))

data$season[data$Month == '01' |
              data$Month == '02' |
              data$Month == '03' | data$Month == '12'] <- 'Winter'
data$season[data$Month == '04' |
              data$Month == '05' | data$Month == '06'] <- 'Spring'
data$season[data$Month == '07' | data$Month == '08'] <- 'Summer'
data$season[data$Month == '09' |
              data$Month == '10' | data$Month == '11'] <- 'Fall'

lm.fit1 <-
  lm(price ~ season + sqft_living + yr_built + sqft_living * yr_built + waterfront,
     data = data)
summary(lm.fit1)
# Results
# Spring: 2.182e+04 = 2.182*(10^4) = 21820
# Summer: 5.037e+03 = 5.037*(10^3) = 5037
# Winter: 7.067e+03 = 7.067*(10^3) = 7067
# sqft_living: 1.055e+03 = 1.055*(10^3) = 1055
# yr_built: -1.438e+03 = -1.438*(10^3) = -1438
# waterfront: 7.828e+05 = 7.828*(10^5) = 782800
# sqft_living*yr_built: -3.841-e01 = -3.841*(10^(-1)) = -0.3841

# I find seasonality in housing price - it can be seen that selling in different
# seasons contributes differently to the housing price with Spring showing
# economically significant results ($21,820) as well as statistically
# significant ones (approximately zero p-value).

# 3 #### Do you find any nonlinearity or heteroskedasticity? YES. BREUSCH PAGAN
# TEST and NOTE FUNNEL LIKE RESIDUALS
#What is the problem if the error term is heteroskedastic? PREDICTION
#How can you address these problems (if you have here)?
# TRANSFORM DEPENDENT/RESPONSE VARIABLE, WHITE STANDARD ERRORS

par(mfrow = c(2, 2))
plot(lm.fit1)
par(mfrow = c(1, 1))
library(lmtest)
bptest(lm.fit1)
# The Breusch-Pagan test reveals that the null hypothesis of homoskedasticity is
# rejected, the p-value is 2.2e-16 (or incredbly near zero). We can also see the
# presence of a funnel type shape in the residuals plot and we can see the
# magnitude of the residuals tends to increase with the fitted values.

# Our ability to predict is diminished because the the variances of the error
# terms are non-constant, or put differently, the magnitude of the error terms
# changes with values of the independent variable(s).

library(sandwich)
lm.fit2 <-
  lm(log(price) ~ season + sqft_living + yr_built + sqft_living * yr_built + waterfront,
     data = data)
summary(lm.fit2)
par(mfrow = c(2, 2))
plot(lm.fit2)
par(mfrow = c(1, 1))
bptest(lm.fit2)
# Employing the White Standard Errors
coeftest(lm.fit2, vcov = vcovHC(lm.fit2, type = "HC1"))
coeftest(lm.fit1, vcov = vcovHC(lm.fit1, type = "HC1"))
# We can transform the response variable, Y, by predicting on the square-root
# or, more commonly employed because of ease of intepretation, logarithmic which
# demonstrates an elasticity (the responsiveness of the variable to the
# independent variables). Applying the log transform diminishes the amount of
# funneling as described in the residuals, but it is not completely gone. We can
# see from the Breusch-Pagan test that the null hypothesis of homoskedasticity
# continues to be rejected. We can also use White Standard Errors.

# 4 ####
# Conduct an F test for the following hypotheses.
# H0: there is no seasonality on the housing price. H1: H0 is not true.
# THE NULL HYPOTHESIS FAILS TO HOLD

yHatU <-
  lm(price ~ season + sqft_living + yr_built + sqft_living * yr_built + waterfront,
     data = data)
yHatR <-
  lm(price ~ sqft_living + yr_built + sqft_living * yr_built + waterfront,
     data = data)
ssr_u <- sum((fitted(yHatU) - data$price) ^ 2)
ssr_r <- sum((fitted(yHatR) - data$price) ^ 2)
f <- ((ssr_r - ssr_u) / 3) / (ssr_u / (21613 - 5 - 1))
var.test(yHatU, yHatR, alternative = "two.sided")

# Min's solution
# SSR <- sum((yHatU$residuals)^2)
# SSRR <- sum((yHatR$residuals)^2)
# FSTAT <- ((SSRR - SSR)/3)/(SSR/(dim(data)[1] - 7 - 1))
# qf(0.95, df1 = 3, df2 = dim(data)[1] - 7 - 1)

# 5 ####
# Predict the housing price when season = spring, sqft_living=2500, yr_built=2000, waterfront=0:
# PREDICTION RESULT: $594406.1
# 95% CONFIDENCE INTERVAL: ($589801.9, $599010.4)
lm.fit <- lm(price ~ sqft_living + yr_built + waterfront, data = data)
predict(
  object = lm.fit,
  newdata = data.frame(
    season = "Spring",
    sqft_living = 2500,
    yr_built = 2000,
    waterfront = 0
  ),
  interval = "prediction"
)
predict(
  object = lm.fit,
  newdata = data.frame(
    season = "Spring",
    sqft_living = 2500,
    yr_built = 2000,
    waterfront = 0
  ),
  interval = "confidence"
)
predict(
  object = lm.fit,
  newdata = data.frame(
    season = "Spring",
    sqft_living = 2500,
    yr_built = 2000,
    waterfront = 0
  )
)


#' ___
#' Chris Daigle
#' 
#' Homework 2
#' 
#' ___
#

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


#' ___
#' Chris Daigle
#' 
#' Homework 3
#' 
#' ___
#

# Download "Auto.csv" from http://www-bcf.usc.edu/~gareth/ISL/data.html and
# estimate the regression mpg on weight using the KNN method. Draw the
# regression line on the scatterplot and compare it with liner regression line.

rm(list = ls())

setwd('~/Git/MachineLearningAndBigDataWithR')
dataName <- 'Auto.csv'
Adata <- read.csv(dataName, stringsAsFactors = FALSE)

# str(Adata)
# Adata$horsepower <- as.numeric(Adata$horsepower, na.omit = TRUE)
# str(Adata)

mpg <- Adata$mpg
weight <- Adata$weight
plot(weight, mpg)
lm1 <- lm(mpg ~ weight)
summary(lm1)
abline(lm1, lwd = 3, col = 'blue')

knn <- function(x0, X, Y, K) {
  x0 <- matrix(rep(x0, length(Y)), byrow = TRUE)
  X <- matrix(X)
  distance <- rowSums((x0 - X) ^ 2)
  rank <- order(distance)
  Y_K <- Y[rank][1:K]
  mean(Y_K)
}

x <-
  seq.int(
    from = min(weight),
    to = max(weight),
    length.out = length(weight)
  )

fhat <- matrix(rep(NA, length(x) * 6), length(x), 6)

for (j in 1:6) {
  K = 2 * j - 1
  for (i in 1:length(x)) {
    fhat[i, j] <- knn(x[i], weight, mpg, K)
  }
}

lines(x, fhat[, 1], col = 'red', lwd = 2)
lines(x, fhat[, 2], col = 'purple', lwd = 2)
lines(x,
      fhat[, 3],
      col = 'black',
      lwd = 2,
      lty = 10)
lines(x, fhat[, 4], col = 'blue', lwd = 2)
lines(x, fhat[, 5], col = 'green', lwd = 2)
lines(x, fhat[, 6], col = 'yellow', lwd = 2)

# Bias Variance Tradeoff ####
# I am still working on this section
B <- matrix(rep(NA, length(x)), length(x), 6)
V <- matrix(rep(NA, length(x)), length(x), 6)

x <-
  seq.int(
    from = min(weight),
    to = max(weight),
    length.out = length(weight)
  )

fhat <- matrix(rep(NA, length(x) * 6), length(x), 6)

for (j in 1:6) {
  K = 2 * j - 1
  for (i in 1:length(x)) {
    fhat[i, j] <- knn(x[i], weight, mpg, K)
    B[i, j] <- knn(x[i], weight, mpg, K) - fhat[i, j]
    V[i, j] <- knn(x[i], weight, mpg, K)
  }
}


Bias <- colMeans(B)
Bias2 <- Bias ^ 2
Var <-
  c(var(V[, 1]), var(V[, 2]), var(V[, 3]), var(V[, 4]), var(V[, 5]), var(V[, 6]))
MSE <- Bias2 + Var

KVec <- 2 * (1:6) - 1
plot(
  KVec,
  Bias2,
  col = 'blue',
  type = 'l',
  lty = 5,
  lwd = 3,
  ylim = c(0, 1)
)
points(
  KVec,
  Var,
  col = 'red',
  type = 'l',
  lty = 10,
  lwd = 3
)
points(KVec,
       MSE,
       type = 'l',
       lty = 1,
       lwd = 3)


#' ___
#' Chris Daigle
#' 
#' Homework 4
#' 
#' ___
#


# Insurance data: You can find the insurance data from HuskyCT
#
# * Create a categorical variable lowcharge which equals 1 if insurance$charges
# < 7000 and equals 0 otherwise
#
# * Run the logit regression of this on age, sex, bmi, smoker, region
#
# * Split the data by choosing 1000 observations for training and by using the
# other observations for test.
#
# * Assess the accuracy of this model

rm(list = ls())

setwd('~/Git/MachineLearningAndBigDataWithR/Data')
dataName <- 'insurance.csv'
data <- read.csv(dataName, stringsAsFactors = FALSE)
str(data)

data$lowCharge <- 0
data$lowCharge[data$charges < 7000] <- 1

trainSample <- sample(nrow(data), 1000, replace = F)

trainData <- data[trainSample,]
testData <- data[-trainSample,]

glmTrain <-
  glm(lowCharge ~ age + sex + bmi + smoker + region, family = binomial, trainData)
summary(glmTrain)

glmProbs <- predict(glmTrain, testData, type = 'response')

glmPred <- rep(0, dim(testData)[1])
glmPred[glmProbs > 0.5] = 1

table(glmPred, testData$lowCharge)
mean(glmPred == testData$lowCharge)

# As about a 70:30 train/test split, the model predicts accurately at about 91%.
# This seems pretty good


#' ___
#' Chris Daigle
#' 
#' Homework 5
#' 
#' ___
#
# Apply logit, LDA, and QDA to Caravan ####
library(MASS)
library(ISLR)
# Logit ####
test <- Caravan[1:1000,]
train <- Caravan[-(1:1000), ]

glmFit <- glm(Purchase ~ ., family = binomial, data = train)
glmProb <- predict(glmFit, test, type = 'response')

glmPred <- rep('No', dim(test)[1])
glmPred[glmProb > 0.5] <- 'Yes'

table(glmPred)
table(glmPred, test$Purchase)

# LDA ####
test2 <- Caravan[1:1000,]
train2 <- Caravan[-(1:1000), ]

ldaFit2 <- lda(Purchase ~ ., data = train2)
# ldaFit2

plot(ldaFit2)

ldaPred2 <- predict(ldaFit2, test2)
# ldaPred2

ldaClass2 <- ldaPred2$class
table(ldaClass2)
table(ldaClass2, test2$Purchase)
mean(ldaClass2 == test2$Purchase)

# QDA ####
# ISSUE WITH RANK (VAR/COV MATRIX) - WON'T RUN
# qdaFit <- lda(Caravan$Purchase ~ ., data = train2)
# qdaFit
# qdaPred <- predict(qdaFit, testX)
# qdaPred
# qdaClass <- qdaPred$class
# table(qdaClass)
# table(qdaClass,trainY)
# mean(qdaClass == testY)