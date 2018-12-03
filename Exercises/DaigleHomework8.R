#' ___
#' Chris Daigle
#' 
#' Homework 8
#' 
#' ___
#

# Exercise: We want to predict the number of applications using the other
# variables in the College data set. You can find this data set in ISLR library.
# Try subset selection, shrinkage methods, and dimension reduction methods and
# examine which method is working best based on training set and test set split.
rm(list = ls())
library(ISLR)
df <- College

set.seed(1)
train <- sample(1:nrow(df), round(nrow(df)) / 2)
dfTrain <- df[train, ]
dfTest <- df[-train, ]
xTest <- dfTest[, -2]
yTest <- dfTest[, 2]
xTrain <- dfTrain[, -2]
yTrain <- dfTrain[, 2]

### Subset Selection
library(leaps)
regFitFull <- regsubsets(Apps ~ ., data = df)
summary(regFitFull)

regFitFull <- regsubsets(Apps ~ ., data = df, nvmax = 19)
regFitFullSummary <- summary(regFitFull)
names(regFitFullSummary)
regFitFullSummary$rsq
regFitFullSummary$adjr2
regFitFullSummary$rss

par(mfrow = c(2, 2))
plot(
  regFitFullSummary$rsq,
  xlab = "Number of regressors",
  ylab = "R-square",
  type = "l"
)
a <- which.max(regFitFullSummary$rsq)
points(
  a,
  regFitFullSummary$rsq[a],
  col = "red",
  cex = 2,
  pch = 20
)
text(a, regFitFullSummary$rsq[a], labels = a, pos = 1)

plot(
  regFitFullSummary$adjr2,
  xlab = "Number of regressors",
  ylab = "Adjusted R-square",
  type = "l"
)
a1 <- which.max(regFitFullSummary$adjr2)
points(
  a1,
  regFitFullSummary$adjr2[a1],
  col = "red",
  cex = 2,
  pch = 20
)
text(a1,
     regFitFullSummary$adjr2[a1],
     labels = a1,
     pos = 1)

plot(regFitFullSummary$cp,
     xlab = "Number of regressors",
     ylab = "Cp",
     type = "l")
a2 <- which.min(regFitFullSummary$cp)
points(
  a2,
  regFitFullSummary$cp[a2],
  col = "red",
  cex = 2,
  pch = 20
)
text(a2, regFitFullSummary$cp[a2], labels = a2, pos = 3)

plot(
  regFitFullSummary$bic,
  xlab = "Number of regressors",
  ylab = "BIC",
  type = "l"
)
a3 <- which.min(regFitFullSummary$bic)
points(
  a3,
  regFitFullSummary$bic[a3],
  col = "red",
  cex = 2,
  pch = 20
)
text(a3, regFitFullSummary$bic[a3], labels = a3, pos = 3)

par(mfrow = c(1, 1))
plot(
  regFitFullSummary$rss,
  xlab = "Number of regressors",
  ylab = "RSS",
  type = "l"
)
a4 <- which.min(regFitFullSummary$rss)
points(
  a4,
  regFitFullSummary$rss[a4],
  col = "red",
  cex = 2,
  pch = 20
)
text(a4, regFitFullSummary$rss[a4], labels = a4, pos = 3)

par(mfrow = c(2, 2))
plot(regFitFull, scale = "r2")
plot(regFitFull, scale = "adjr2")
plot(regFitFull, scale = "Cp")
plot(regFitFull, scale = "bic")

coef(regFitFull, 12)
#paste(names(coef(regFitFull, 12))[2:length(coef(regFitFull, 12))], collapse='+')

# Selecting the info from the 12th model
regFit <-
  lm(
    Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad +
      P.Undergrad + Outstate + Room.Board + PhD + Expend + Grad.Rate,
    data = dfTrain
  )
regPred <- predict(regFit, xTest)

subMSEP <- mean((regPred - yTest) ^ 2)

### Shrinkage Method: Ridge
library(glmnet)
xTemp <- model.matrix(Apps ~ ., df)

head(xTemp)
x <- xTemp[, -2]
y <- df$Apps

grid <- 10 ^ seq(10,-2, length = 100)
ridgeMod <- glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridgeMod))

# Cross validation to choose lambda
train <- sample(1:nrow(x), round(nrow(x) / 2))
yTrain1 <- y[train]
xTrain1 <- x[train, ]
yTest1 <- y[-train]
xTest1 <- x[-train, ]

ridgeMod <-
  glmnet(xTrain1,
         yTrain1,
         alpha = 0,
         lambda = grid,
         thresh = 1e-12)
ridgePred <- predict(ridgeMod, s = 4, newx = xTest1)
ridgeMSEP <- mean((ridgePred - yTest) ^ 2)

ridgePred <-
  predict(
    ridgeMod,
    s = 0,
    newx = xTest1,
    exact = TRUE,
    x = xTrain1,
    y = yTrain1
  )
mean((ridgePred - yTest1) ^ 2)

cvOut <- cv.glmnet(xTrain1, yTrain1, alpha = 0)
plot(cvOut)
bestLam <- cvOut$lambda.min
bestLam

ridgePred <- predict(ridgeMod, s = bestLam, newx = xTest1)
ridgeMSEP <- mean((ridgePred - yTest1) ^ 2)

### Dimensional Reduction
library(pls)
pcrFit <- pcr(Apps ~ .,
              data = df,
              scale = TRUE,
              validation = "CV")
summary(pcrFit)
validationplot(pcrFit, val.type = "MSEP")
pcrFit <- pcr(Apps ~ .,
              data = dfTrain,
              scale = TRUE,
              ncomp = 5)
pcrPred <- predict(pcrFit, xTest, ncomps = 5)
pcrMSEP <- mean((pcrPred - yTest) ^ 2)

pcrFit <- pcr(Apps ~ ., data = df, scale = TRUE)
summary(pcrFit)

plsFit <- plsr(Apps ~ .,
               data = df,
               scale = TRUE,
               validation = "CV")
summary(plsFit)
validationplot(plsFit, val.type = "MSEP")

plsFit <- plsr(Apps ~ .,
               data = dfTrain,
               scale = TRUE,
               ncomp = 5)
plsPred <- predict(plsFit, xTest, ncomp = 5)

plsMSEP <- mean((plsPred - yTest) ^ 2)

plsFit <- plsr(Apps ~ .,
               data = df,
               scale = TRUE,
               ncomp = 5)
summary(plsFit)

msep <- list(subMSEP, ridgeMSEP, plsMSEP, pcrMSEP)
bestMethod <- which.min(msep)
bestMethod
#' Partial Least Squares and Principal Component Regression, 3, corresponds to
#' best subset selection method