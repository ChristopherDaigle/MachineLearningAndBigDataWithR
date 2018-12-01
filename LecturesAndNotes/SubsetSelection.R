# My Notes
# 11/15/18

# Subset Selection

### Subset Selection

# 1. Best Subset Selection: Consider all possible models: 2^p. 
#    Comparaing models with the same number of predictors, we use R^2 or SSR.
#    Comparing models different number of predictors, we use CV, Cp, AIC, BIC, adjusted R^2.
# 2. Forward Stepwise Selection:
#    Consider the null model. Then, compare p univariate models using R^2 or SSR.
#    Choose the best one and add a predictor (p-1 models). Compare them 
#    with R^2 or SSR. Add a predictor in the same manner.
#    Finally, we have p+1 models with different # of predictors and compare them
#    with CV, Cp, AIC, BIC, adjusted R^2.
# 3. Backward Stepwise Selection:
#    Consider the full model. Then, compare p models with (p-1) predictors using R^2 or SSR.
#    Choose the best model and drop one predictor from it to have (p-1) models with (p-2) predictors.
#    Finally, we have p+1 models with different # of predictors and compare them
#    with CV, Cp, AIC, BIC, adjusted R^2.

rm(list=ls())

library(ISLR)
head(Hitters)
dim(Hitters)

sum(is.na(Hitters))

Hitters <- na.omit(Hitters)
dim(Hitters) # 263 observations, 20 variables
sum(is.na(Hitters)) # 0 na values left

# Best subset selection
# considers all possible models; this is best option compared with stepwise

# install.packages("leaps")
library(leaps)
regfit.full <- regsubsets(Salary~.,data=Hitters)  # function for model selection
# from full model up to the best eight-regressor model
summary(regfit.full) 
# regsubset function is for subset selection methods
# Asterisk sign is the variables to choose
# but no comparison among models


# If you want to consider up to the full model
regfit.full <- regsubsets(Salary~.,data=Hitters, nvmax=19) # nvmax: maximum number of subset to examine
# nvmax option for full model, default is only for 8 regressors
reg.summary.full <- summary(regfit.full)
names(reg.summary.full)
reg.summary.full$rsq     
# rsq for the best model with given number of predictors
# gives best model for each number of regressors
# stars give best regressors for each size

reg.summary.full$adjr2

par(mfrow=c(2,2))
plot(reg.summary.full$rsq,xlab="Number of Regressors", ylab="R-square",type="l")
plot(reg.summary.full$adjr2,xlab="Number of Regressors", ylab="Adjusted R-square",type="l")
a <- which.max(reg.summary.full$adjr2)
points(a,reg.summary.full$adjr2[a], col="red", cex=2, pch=20)
# adjr2 want the max, Cp, AIC, want smaller value


plot(reg.summary.full$cp,xlab="Number of Regressors", ylab="Cp",type="l")
a1 <- which.min(reg.summary.full$cp) 
points(a1,reg.summary.full$cp[a1], col="red", cex=2, pch=20)

plot(reg.summary.full$bic,xlab="Number of Regressors", ylab="BIC",type="l")
a2 <- which.min(reg.summary.full$bic)
points(a2,reg.summary.full$bic[a2], col="red", cex=2, pch=20)

# regsubsets() function has a built-in plot() function which displays the selected regressors 
# for the best model with given number of predictors, ranked accoriding to the BIC, Cp, adjusted R square, 
# or AIC.

###### Plot to show best regressors
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")
# If sample is large, BIC is consistent, but in finite number of data, not totally sure if BIC is best
# AIC and Cp are essentially the same


coef(regfit.full,6) 
# best model with 6 regressors based on SSR, gives the coefficients

# We see computational power is pretty quick here


########### 
# Forward and Backward Stepwise selection
regfit.fwd <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
reg.summary.fwd <- summary(regfit.fwd)
reg.summary.fwd

regfit.bwd <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
reg.summary.bwd <- summary(regfit.bwd)
reg.summary.bwd

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)
# We see forward method and backward method produce different best variables

plot(reg.summary.full$bic,xlab="Number of Regressors", ylab="BIC",type="l", main="Best Subset Selection")
a2 <- which.min(reg.summary.full$bic)
points(a2,reg.summary.full$bic[a2], col="red", cex=2, pch=20)

plot(reg.summary.fwd$bic,xlab="Number of Regressors", ylab="BIC",type="l", main="Forward Stepwise Selection")
a2 <- which.min(reg.summary.fwd$bic)
points(a2,reg.summary.fwd$bic[a2], col="red", cex=2, pch=20)

plot(reg.summary.bwd$bic,xlab="Number of Regressors", ylab="BIC",type="l", main="Backward Stepwise Selection")
a2 <- which.min(reg.summary.bwd$bic)
points(a2,reg.summary.bwd$bic[a2], col="red", cex=2, pch=20)

plot(regfit.full, scale="bic", main="Best Subset Selection")
plot(regfit.fwd, scale="bic", main="Forward Stepwise Selection")
plot(regfit.bwd, scale="bic", main="Backward Stepwise Selection")         
# Depending on what subset selection method you use,
# you can end up with a different model.

##########

###  Shrinkage Methods: Ridge and Lasso.
# The objective function is SSR + penalty term.

## Ridge regression

# "glmnet" package needs to be installed for ridge regression and the lasso.  
# glmnet package: generalized linear model net
#install.packages("glmnet")


rm(list=ls())
Hitters <- na.omit(Hitters)
x.temp <- model.matrix(Salary~.,Hitters)        # model.matrix: produces a set of regressors.
# The first column is the intercept.
# automatically transforms any qualitative variables into dummy variables.
# set up design matrix
head(x.temp)
x <- x.temp[,-1]
y <- Hitters$Salary

library(glmnet)
grid <- 10^seq(10,-2,length=100) # get 100 different lambdas between range of infinity to -infinity essentially, 10^10 and 10^-2 
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)   # By default, the glmnet() function standardizes
# the variables so that they are on the same scale.
# alpha = 0, ridge//// alpha=1; lasso
# Note that the grammar is different from lm() function.
dim(coef(ridge.mod))     # Each column includes coefficients of predictors and an intercept for each lambda.
# many betas (20 of them) but each for 100 columns

ridge.mod$lambda[50] # tuning parameter value in column 50
coef(ridge.mod)[,50] 
sqrt(sum(coef(ridge.mod)[-1,50]^2))             # Why do we have -1 in the row index? exclude the first component, the intercept
# magnitude of beta is 6.3


ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2)) 

predict(ridge.mod, s=50, type="coefficients")[1:20,]  # Estimates of ridge regression coefficients with lambda=5.
# Lambda is high, price is high: more shrinkage
# s is new value of lambda, that wasn't in previous lambdas, so new lambda to estimate

##########
# Cross validation to choose lambda

set.seed(1)
train <- sample(1:nrow(x), round(nrow(x)/2))
y.train <- y[train]
x.train <- x[train,]
y.test <- y[-train]
x.test <- x[-train,]

ridge.mod <- glmnet(x.train,y.train,alpha=0,lambda=grid,thresh=1e-12)  # The default is thresh=1e-7 which determine convergence.
ridge.pred <- predict(ridge.mod,s=4,newx=x.test) # lambda is 4
mean((ridge.pred - y.test)^2)  # test mean square error

# We can see whether ridge regression with lambda=4 performs better than the LS.
# lambda = 0, means there is no penalty, lambda reduces to zero


ridge.pred <- predict(ridge.mod,s=0,newx=x.test,exact=TRUE,x=x.train,y=y.train)  # "exact=TRUE" fits the model again since
# lambda=0: equivalent to a linear fit, was not used with training data.             
# "exact=FALSE" is default and uses approximation.
mean((ridge.pred - y.test)^2)

# we see that prediction error with LS is larger than Ridge, so Ridge performs better

# Cross validation: don't need to split into train and test data
# find optimal lambda that minimizes the prediction error

train.data <- data.frame(y.train,x.train)
linear.fit <- lm(y.train~., data=train.data)
newX <- data.frame(x.test)
linear.pred <- predict(object=linear.fit,newdata=newX)
mean((linear.pred - y.test)^2)

set.seed(1)
cv.out <- cv.glmnet(x.train,y.train,alpha=0)    # default is 10-fold CV and can change this with "nfolds=" 
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod,s=bestlam,newx=x.test)
mean((ridge.pred - y.test)^2)

# Finally we refit our ridge regression model on the full data set using lambda chosen by cross-validation.
out <- glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]


########## Best selection and Ridge are best methods #######

## The Lasso

# Lasso shrinks coefficients to zero of covariates that are irrelevant
# Lasso performs better when there are many covariates that are irrelevant
# sparcity: when have many predictors, but only a couple are relevant

lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x.train,y.train,alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x.test)
mean((lasso.pred-y.test)^2)

out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef <- predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef                                          # Sparcity: We can see that many lasso coefficients
# are exactly zero.

