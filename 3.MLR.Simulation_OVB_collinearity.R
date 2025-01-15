#==============================================================================
# This scrips illustrates the concepts of the omitted variable bias and colinearity
# by constructing simulation where one can vary the parameters. Simulations are useful
# to isolate the effect of change in the assumptions. Real data rarely permit such 
# experiment.

remove(list=ls())

#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
# install.packages("stargazer")
library(stargazer)


# --------------------------------------------------------------------------
# An example to illustrate addition of the 2nd explanatory variable and signs of biases
e_y   <- rnorm(1000, mean=0, sd=1)
e_x1  <- rnorm(1000, mean=0, sd=1)
x1 <- runif(1000, min=0, max=20)
beta1 <- .7
beta2 <- .5
delta1 <- 1

x2 <- delta1*x1+e_x1
y <- x1*beta1+x2*beta2+e_y

# Illustrate the omitted variable bias
stargazer(
  lm(y~x1),
  lm(y~x1+x2), 
    type="text", 
    column.labels=c("Biased","Unbiased")
  )


# Illustrate what happens if we split one variable (say x1) is split into two random variables
x1_part1 <- rnorm(1000, mean=0, sd=2)
x1_part1 <- runif(1000, min=1, max=5)
x1_part2 <- x1 - x1_part1
cor(x1_part1, x1_part2)
stargazer(lm(y~x1),lm(y~x1_part1+x1_part2), type="text")

# Illustrate partialling-out interpretation
x1_x2res <- residuals(lm(x1 ~ x2))
x2_x1res <- residuals(lm(x2 ~ x1))
y_x1res <- residuals(lm(y ~ x1))
y_x2res <- residuals(lm(y ~ x2))
stargazer(lm(y ~ x1 + x2),lm(y~x1_x2res),lm(y~x2_x1res), type="text")
stargazer(lm(y ~ x1 + x2),lm(y_x2res ~ x1_x2res), lm(y_x1res ~ x2_x1res), type="text")

#-----------------------------------
# Illustrate the effect of correlation 
e_y   <- rnorm(100, mean=0, sd=10)
e_x1  <- rnorm(100, mean=0, sd=10)

x2 <- delta1*x1+e_x1
y <- x1*beta1+x2*beta2+e_y
lmy <- lm(y ~ x1 +x2)

x2 <- delta1*x1+e_x1*10 # same relation, more noise
y1 <- x1*beta1+x2*beta2+e_y
lmy1 <- lm(y1 ~ x1 +x2)

x2 <- 5*delta1*x1+e_x1 # stronger relation
y2 <- x1*beta1+x2*beta2+e_y
lmy2 <- lm(y2 ~ x1 +x2)

x2 <- delta1*x1+e_x1*10
y3 <- x1*beta1+x2*beta2+e_y*10
lmy3 <- lm(y3 ~ x1 +x2)


stargazer(lmy,lmy1,lmy2,lmy3, type="text")

