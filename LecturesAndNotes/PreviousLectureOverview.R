# Linear Discriminatn Analysis

library(MASS)
library(ISLR)

str(Smarket)
train <- subset(Smarket, Smarket$Year < 2005)
Smarket2005 <- Smarket[Smarket == 2005,]

ldaFit <- lda(Direction ~ Lag1 + Lag2, data = train)
ldaFit

plot(ldaFit)

ldaPred <- predict(ldaFit, Smarket2005)
ldaPred

par(mfrow = c(1,2))
a <- rep('red', dim(Smarket2005)[1])
a[ldaPred$class == 'Up'] <- 'blue'
plot(1:length(ldaPred$x), ldaPred$x, col = a)

b <- rep('red', dim(Smarket2005)[1])
b[Smarket2005$Direction == 'Up'] <- 'blue'
plot(1:length(ldaPred$x), ldaPred$x, col = b)

names(ldaPred)
ldaPred$posterior

ldaClass <- ldaPred$class
table(ldaClass)
table(ldaClass, Smarket2005$Direction)

head(iris)
table(iris$Species)

set.seed(1)
trainIndex <- sample(1:150, 120, replace = FALSE)
ldaFit1 <- lda(Species ~ ., data = iris[trainIndex,])
ldaFit1
par(mfrow = c(1,1))
plot(ldaFit1, col = as.numeric(iris$Species[trainIndex]))
# We can see that LD1 is more influential to classify the observation. We can see this from the plot.


td <- iris[-trainIndex,]
ldaPred1 <- predict(ldaFit1, td)
ldaPred1

a <- rep(1, dim(td)[1])
a[ldaPred1$class == 'versicolor'] <- 15
a[ldaPred1$class == 'virginica'] <- 19
plot(ldaPred1$x[,1], ldaPred1$x[,2], col = as.numeric(ldaPred1$class), pch = a)
text(ldaPred1$x[,1]-0.1, ldaPred1$x[,2]-0.1, td$Species)
legend('topright', legend = levels(iris$Species), col = c(1,2,3), pch = c(1,15,19), cex = 0.8)

table(ldaPred1$class)
table(ldaPred1$class, td$Species)


# multinomial logit model
library(nnet)
ml <- multinom(Species ~ ., data = iris[trainIndex,])
mlPred <- predict(ml, iris[-trainIndex,])

mlPred
table(mlPred)
table(mlPred, td$Species)

# Quadratic Discriminant Analysis: The syntax is the same as lqa

qdaFit <- qda(Direction ~ Lag1 + Lag2, data = train)
qdaFit
qdaPred <- predict(qdaFit, Smarket2005)
qdaPred
qdaClass <- qdaPred$class
table(qdaClass)
table(qdaClass,Smarket2005$Direction)
mean(qdaClass == Smarket2005$Direction)

# KNN method for classification
library(class) # knn() is part of the class library
library(ISLR)
dim(Caravan)

head(Caravan)

summary(Caravan$Purchase)

# How to define neighbors when units are different
# Normalize each variable to have a zero mean and a variance of 1: scale()

stX <- scale(Caravan[, -86])
var(Caravan[,1])
mean(Caravan[,1])
var(Caravan[,2])
mean(Caravan[,2])

var(stX[,1])
mean(stX[,1])
var(stX[,2])
mean(stX[,2])


test <- 1:1000
trainX <- stX[-test,]
testX <- stX[test,]
trainY <- Caravan$Purchase[-test]
testY <- Caravan$Purchase[test]

knnPred <- knn(trainX, testX, trainY, k=3)
mean(testY != knnPred)
mean(testY == 'Yes')
table(knnPred,testY)

# Apply logit, LDA, and QDA to Caravan ####
# Logit ####
test <- Caravan[1:1000,]
train <- Caravan[-(1:1000), ]

glmFit <- glm(Purchase ~ ., family = binomial, data = train)
glmProb <- predict(glmFit, test, type = 'response')

glmPred <- rep('No', dim(testX)[1])
glmPred[glmProb > 0.5] <- 'Yes'

table(glmPred)
table(glmPred, test$Purchase)

# LDA ####
test2 <- Caravan[1:1000,]
train2 <- Caravan[-(1:1000), ]

ldaFit2 <- lda(Purchase ~ ., data = train2)
ldaFit2

plot(ldaFit2)

ldaPred2 <- predict(ldaFit2, test2)
ldaPred2

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

