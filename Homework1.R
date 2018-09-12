# Chris Daigle
# Homework 1
#  Exercise 2 
# (1) Download the housing dataset from https://www.kaggle.com/harlfoxem/housesalesprediction
#     and run a regression to predict housing prices. 

# (2) Build a model to predict the housing price given characteristics of a house in the dataset.
#     Consider the following predictors:
#    
#     season, sqft_living, yr_built, interaction of sqft_living yr_built, and waterfront

#     Create a variable "season" which equals 
#     "Winter" if a house was sold in Jan, Feb, Mar, Dec.
#     "Spring" if it was sold in Apr, May, Jun
#     "Summer" if it was sold in Jul, Aug
#     "Fall" if it was sold in Sep, Oct, Nov.
#     Do you find any seasonality in housing price?

# (3) Do you find any nonlinearity or heteroskedasticity? 
#     What is the problem if the error term is heteroskedastic?
#     How can you address these problems (if you have here)?

# (4) Conduct an F test for the following hypotheses.
#     H0: there is no seasonality on the housing price. H1: H0 is not true.

# (5) Predict the housing price when season = spring, sqft_living=2500, yr_built=2000, waterfront=0.

# 1 ####
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
data <- transform(data, Year = substr(date, 1, 4), Month = substr(date, 5, 6), Day = substr(date, 7, 8))

data$cleanDate <- format(as.Date(paste0(data$Year,'-',data$Month,'-',data$Day)), '%Y-%m-%d')

data$Season[data$Month == '01' | data$Month == '02' | data$Month == '03' | data$Month == '12' ] <- 'Winter'
data$Season[data$Month == '04' | data$Month == '05' | data$Month == '06'] <- 'Spring'
data$Season[data$Month == '07' | data$Month == '08'] <- 'Summer'
data$Season[data$Month == '09' | data$Month == '10' | data$Month == '11'] <- 'Fall'



