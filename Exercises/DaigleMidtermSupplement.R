#' ---
#' title: "Midterm Supplement"
#' author: "Christopher Daigle"
#' date: "Nov. 5 2018"
#' output: pdf_document
#' ---
rm(list = ls())
library(boot)
library(ISLR)
set.seed(1)

#' Cross validation Exercise #### Cross validation can be used to estimate the
#' test error for a classification problem. Run a logit model with the insurance
#' data. The dependent variable is lowCharge and independent variables are age,
#' sex, bmi, smoker, and region.

setwd('~/Git/MachineLearningAndBigDataWithR/Data')
dataName <- 'insurance.csv'
df <- read.csv(dataName, stringsAsFactors = FALSE)
str(df)

df$lowCharge <- 0
df$lowCharge[df$charges < 7000] <- 1
head(df)

y <- df$lowCharge
y <- as.integer(y)

X1 <- df$age
X1 <- as.integer(X1)

X2 <- df$sex
X2[X2 == 'male'] <- 1
X2[X2 == 'female'] <- 0
X2 <- as.integer(X2)

X3 <- df$bmi

X4 <- df$smoker
X4[X4 == 'yes'] <- 1
X4[X4 == 'no'] <- 0

X5 <- df$region
unique(X5)
X5[X5 == 'southwest'] <- 1
X5[X5 == 'southeast'] <- 2
X5[X5 == 'northwest'] <- 3
X5[X5 == 'northeast'] <- 4

glmFit <-
  glm(y ~ X1 + X2, family = binomial, data = df)
summary(glmFit)

#' Compare this model with the following models using K-Fold cross-validation
#' with K=10 lowCharge ~age+sex+bmi, lowCharge ~age+sex+bmi+smoker, lowCharge
#' ~age+sex+bmi+smoker+region

# Creating K-Fold 'bins' ####
n <- nrow(df)
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
    glmFit1 <-
      glm(y ~ X1 + X2, family = binomial, data = df)
    glmFit2  <-
      glm(y ~ X1 + X2 + X3,
          family = binomial,
          data = df)
    glmFit3  <-
      glm(y ~ X1 + X2 + X3 + X4,
          family = binomial,
          data = df)
    glmFit4  <-
      glm(y ~ X1 + X2 + X3 + X4 + X5,
          family = binomial,
          data = df)
    cv.error[i, j] <- cv.glm(df, glmFit1, K = 10)$delta[1]
  }
  cv.error[i, 11] <- mean(cv.error[i, ], na.rm = TRUE)
}
cv.error

numbers = c('first', 'second', 'third', 'fourth')
for (i in 1:4) {
  print(paste('The MSE for the', numbers[i], 'model is:', cv.error[i, 11]))
}