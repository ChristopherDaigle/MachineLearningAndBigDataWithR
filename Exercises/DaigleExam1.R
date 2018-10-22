# Christopher Daigle
# Big Data - Exam 1

# Question 5 ####
rm(list = ls())
set.seed(1)
library(MASS)

data<- Boston

trainSample <- sample(nrow(data), 400, replace = F)

trainData <- data[trainSample,]
testData <- data[-trainSample,]

reg1Train <-
  lm(crim ~ medv + age + lstat,  trainData)
summary(reg1Train)

lstat2 <- trainData$lstat^2

reg2Train <-
  lm(crim ~ medv + age + lstat + lstat2,  trainData)
summary(reg2Train)

lstat3 <- trainData$lstat^3

reg3Train <- lm(crim ~ medv + age + lstat + lstat2 + lstat3,  trainData)
summary(reg3Train)

reg1Probs <- predict(reg1Train, testData, type = 'response')
reg2Probs <- predict(reg2Train, testData, type = 'response')
reg3Probs <- predict(reg2Train, testData, type = 'response')

reg1Pred <- rep(0, dim(testData)[1])
reg2Pred <- rep(0, dim(testData)[1])
reg3Pred <- rep(0, dim(testData)[1])

reg1Pred[reg1Probs > 0.5] = 1
reg2Pred[reg2Probs > 0.5] = 1
reg3Pred[reg3Probs > 0.5] = 1

table(reg1Pred, testData$crim)
mean(reg1Pred == testData$crim)

table(reg2Pred, testData$crim)
mean(reg2Pred == testData$crim)

table(reg3Pred, testData$crim)
mean(reg3Pred == testData$crim)


# Question 6 ####
rm(list = ls())
library(ISLR)
data <- Auto

data$mpg01 <- 0
medMPG <- median(data$mpg)
data$mpg01[data$mpg > medMPG] <- 1

trainSample <- sample(nrow(data), 30, replace = F)

trainData <- data[trainSample,]
testData <- data[-trainSample,]

# Logit
glmFit <- glm(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, family = binomial, data = trainData)
glmProb <- predict(glmFit, testData, type = 'response')

glmPred <- rep('No', dim(testData)[1])
glmPred[glmProb > 0.5] <- 'Yes'

table(glmPred)
table(glmPred, testData$mpg01)

# LDA
ldaFit <- lda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = trainData)
plot(ldaFit)

ldaPred <- predict(ldaFit, testData)

ldaClass <- ldaPred$class
table(ldaClass)
table(ldaClass, testData$mpg01)
mean(ldaClass == testData$mpg01)

# KNN

mpg <- data$mpg
weight <- data$weight
plot(weight, mpg)
lm1 <- lm(mpg ~ mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = trainData)
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