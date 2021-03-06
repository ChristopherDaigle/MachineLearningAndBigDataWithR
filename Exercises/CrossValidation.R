#' ---
#' title: "Cross Validation"
#' author: "Christopher Daigle"
#' date: "Oct. 23 2018"
#' output: pdf_document
#' ---
library(boot)
library(ISLR)
rm(list = ls())
set.seed(1)

#' Exercise 1 ####
#' Cross validation can also be used to estimate the test error for a
#' classification problem. Run a logit model with the Smarket data. The dependent
#' variable is Direction

glm.fit <-
  glm(Direction ~ Lag1 + Lag2, family = binomial, data = Smarket)
summary(glm.fit)

#' Compare this model with the following models using K-Fold cross-validation
#' with K=10 Direction ~Lag1+Lag2+Lag3, Direction ~Lag1+Lag2+Lag3+Lag4, Direction
#' ~Lag1+Lag2+Lag3+Lag5

# Creating K-Fold 'bins'
n <- nrow(Smarket)
x <- 1:n
cv.error <- matrix(NA, 4, 11)
rownames(cv.error) <- c('Model1', 'Model2', 'Model3', 'Model4')
colnames(cv.error) <-
  c(
    'MSEK-Fold1',
    'MSEK-Fold2',
    'MSEK-Fold3',
    'MSEK-Fold4',
    'MSEK-Fold5',
    'MSEK-Fold6',
    'MSEK-Fold7',
    'MSEK-Fold8',
    'MSEK-Fold9',
    'MSEK-Fold10',
    'MeanMSE'
  )

for (i in 1:4) {
  for (j in 1:10) {
    glm.fit1 <-
      glm(Direction ~ Lag1 + Lag2, family = binomial, data = Smarket)
    glm.fit2  <-
      glm(Direction ~ Lag1 + Lag2 + Lag3,
          family = binomial,
          data = Smarket)
    glm.fit3  <-
      glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4,
          family = binomial,
          data = Smarket)
    glm.fit4  <-
      glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5,
          family = binomial,
          data = Smarket)
    cv.error[i, j] <- cv.glm(Smarket, glm.fit1, K = 10)$delta[1]
  }
  cv.error[i, 11] <- mean(cv.error[i,], na.rm = TRUE)
}
cv.error

numbers = c('first', 'second', 'third', 'fourth')
for (i in 1:4) {
  print(paste('The MSE for the', numbers[i], 'model is:', cv.error[i,11]))
}

#' Exercise 2 ####
#' Consider KNN Estimation to predict direction using Lag1 and Lag2. To choose
#' the optimal number of neighbors, use the K=10. Fold cross validation. Use only
#' 2004 and 2005 year data. Instructed to use all of the data set for divisibility.
library(ISLR)
library(class)
rm(list = ls())
set.seed(1)

df <- Smarket
X <- df[, c('Lag1', 'Lag2')]
y <- df$Direction

n <- nrow(df)
ind <- 1:n

optKs <- rep(NA,10)

for (i in 1:10) {
  testSplit <- sample(ind, size = n/10, replace = FALSE)
  
  train <- df[-testSplit, ]
  test <- df[testSplit, ]
  
  trainX <- train[, c('Lag1', 'Lag2')]
  testX <- test[, c('Lag1', 'Lag2')]
  trainY <- train$Direction
  testY <- test$Direction
  
  ind <- ind[-testSplit]
  
  Acc <- rep(NA, 100)
  
  for (j in 1:100) {
    knnPred <- knn(trainX, testX, trainY, k = j)
    Acc[j] <- mean(testY == knnPred)
  }
  optKs <- which.max(Acc)
}
optKs
print(paste('The best K for this is', optKs))
