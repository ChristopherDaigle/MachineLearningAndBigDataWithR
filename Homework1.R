# Chris Daigle
# Homework 1
#  Exercise 2


# 1 ####
# Download the housing dataset from https://www.kaggle.com/harlfoxem/housesalesprediction and run a regression to predict housing prices. 

rm(list = ls())

setwd('~/Git/MachineLearningAndBigDataWithR')

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

lm.fit <- lm(price~sqft_living + bedrooms + floors + yr_built, data=data)
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

data <- transform(data, Year = substr(date, 1, 4), Month = substr(date, 5, 6), Day = substr(date, 7, 8))

data$cleanDate <- as.Date(paste0(data$Year,'-',data$Month,'-',data$Day))

data$season[data$Month == '01' | data$Month == '02' | data$Month == '03' | data$Month == '12' ] <- 'Winter'
data$season[data$Month == '04' | data$Month == '05' | data$Month == '06'] <- 'Spring'
data$season[data$Month == '07' | data$Month == '08'] <- 'Summer'
data$season[data$Month == '09' | data$Month == '10' | data$Month == '11'] <- 'Fall'

lm.fit1 <- lm(price~ season + sqft_living + yr_built + sqft_living*yr_built + waterfront, data = data)
summary(lm.fit1)
# Results
# Spring: 2.182e+04 = 2.182*(10^4) = 21820
# Summer: 5.037e+03 = 5.037*(10^3) = 5037
# Winter: 7.067e+03 = 7.067*(10^3) = 7067
# sqft_living: 1.055e+03 = 1.055*(10^3) = 1055
# yr_built: -1.438e+03 = -1.438*(10^3) = -1438
# waterfront: 7.828e+05 = 7.828*(10^5) = 782800
# sqft_living*yr_built: -3.841-e01 = -3.841*(10^(-1)) = -0.3841

# I find seasonality in housing price - it can be seen that selling in different seasons contributes differently to the housing price with Spring showing economically significant results ($21,820) as well as statistically significant ones (approximately zero p-value).

# 3 ####
# Do you find any nonlinearity or heteroskedasticity? YES. BREUSCH PAGAN TEST and NOTE FUNNEL LIKE RESIDUALS
# What is the problem if the error term is heteroskedastic? PREDICTION
# How can you address these problems (if you have here)? TRANSFORM DEPENDENT/RESPONSE VARIABLE, WHITE STANDARD ERRORS

par(mfrow=c(2,2))
plot(lm.fit1)
par(mfrow=c(1,1))
library(lmtest)
bptest(lm.fit1)
# The Breusch-Pagan test reveals that the null hypothesis of homoskedasticity is rejected, the p-value is 2.2e-16 (or incredbly near zero).
# We can also see the presence of a funnel type shape in the residuals plot and we can see the magnitude of the residuals tends to increase with the fitted values.

# Our ability to predict is diminished because the the variances of the error terms are non-constant, or put differently, the magnitude of the error terms changes with values of the independent variable(s).

library(sandwich)
lm.fit2 <- lm(log(price)~ season + sqft_living + yr_built + sqft_living*yr_built + waterfront, data = data)
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(1,1))
bptest(lm.fit2)
# Employing the White Standard Errors
coeftest(lm.fit2, vcov = vcovHC(lm.fit2, type="HC1"))
coeftest(lm.fit1, vcov = vcovHC(lm.fit1, type="HC1"))
# We can transform the response variable, Y, by predicting on the square-root or, more commonly employed because of ease of intepretation, logarithmic which demonstrates an elasticity (the responsiveness of the variable to the independent variables). Applying the log transform diminishes the amount of funneling as described in the residuals, but it is not completely gone. We can see from the Breusch-Pagan test that the null hypothesis of homoskedasticity continues to be rejected. We can also use White Standard Errors.

# 4 ####
# Conduct an F test for the following hypotheses.
# H0: there is no seasonality on the housing price. H1: H0 is not true.

res.ftest <- var.test(price~ season =='Spring', data = data)
res.ftest

# 5 ####
# Predict the housing price when season = spring, sqft_living=2500, yr_built=2000, waterfront=0.

predict(object=lm.fit,newdata=data.frame(season="Spring",sqft_living=2500, yr_built=2000, waterfront=0), interval="prediction")
predict(object=lm.fit,newdata=data.frame(season="Spring",sqft_living=2500, yr_built=2000, waterfront=0), interval="confidence")
predict(object=lm.fit,newdata=data.frame(season="Spring",sqft_living=2500, yr_built=2000, waterfront=0))



