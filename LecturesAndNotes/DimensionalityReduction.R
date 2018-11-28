library(ISLR)
head(Hitters)
dim(Hitters)

sum(is.na(Hitters))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

set.seed(1)
train <- sample(1:nrow(Hitters), round(nrow(Hitters)/2))
train.set <- Hitters[train,]
test.set <- Hitters[-train,]
x.test <- test.set[,-19]
y.test <- test.set[,19]

# Principal components regression (Principan Component Analysis)
library(pls)

set.seed(10)
pcr.fit <- pcr(Salary ~., data = Hitters, scale = TRUE, validation = 'CV')

summary(pcr.fit)
validationplot(pcr.fit, val.type = 'MSEP')

pcr.fit <- pcr(Salary ~., data = train.set, scale = TRUE, ncomp = 6)
pcr.pred <- predict(pcr.fit, x.test, ncomp = 6)

mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(Salary ~., data = Hitters, scale = TRUE)
summary(pcr.fit)

# PArtial least squares (supervised method) : use plsr() function which is also in library(pls)
set.seed(10)
pls.fit <- plsr(Salary ~., data = Hitters, scale = TRUE, ncomp = 12)
summary(pls.fit)
validationplot(pls.fit, val.type = 'MSEP')
