# Chris Daigle
# Exercise 5

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