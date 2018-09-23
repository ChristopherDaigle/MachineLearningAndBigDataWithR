# R Refreshers - Ch2 Lab
x <- c(1, 6, 2)
length(x)

y <- c(1,4,3)

x+y

ls()
rm(list = ls())

x <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
matrix(c(1,2,3,4), 2, 2, byrow = TRUE)
x

x <- rnorm(50)
y <- x + rnorm(50, mean = 50, sd = 0.1)
cor(x,y)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# Graphics ####
rm(list = ls())
x <- rnorm(100)
y <- rnorm(100)
plot(x,y)
plot(x, y, xlab = 'x', ylab = 'y', main = 'title')
pdf('Figure.pdf')

dev.off

x <- seq(1,10)
x
x<- 10:1.5
x
x <- seq(-pi, pi, length = 50)
x
y <- x
f1 <- outer(x,y,"+")
f1
f2 <- outer(x,y, function(x,y)cos(y)/(1+x^2))
contour(x,y,f2)

fa <- (f2 - t(f2))/2
contour(x, y, fa, nlevels = 15)

image(x, y, fa)
persp(x,y, fa)
persp(x,y, fa, theta = 30, phi = 20)
persp(x,y, fa, theta = 30, phi = 70)

# Indexing ####
rm(list = ls())

A <- matrix(1:16, nrow = 4, ncol = 4)
A

A[2,3]
A[2,]
A[1:2,4]

A[c(1,3), c(2,4)]
A[-c(1,2),]
dim(A)

# Loading Data ####
rm(list = ls())

setwd('Git/MachineLearningAndBigDataWithR/')
list.files()
Auto <- read.csv("Auto.csv", header = TRUE, na.strings = "?")
View(Auto)
