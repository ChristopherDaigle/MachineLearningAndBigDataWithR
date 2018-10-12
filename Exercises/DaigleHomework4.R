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