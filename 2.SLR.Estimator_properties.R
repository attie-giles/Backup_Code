# ------------------------------------------------------
# Basic estimation of the SLR and properties
remove(list=ls())
#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
install.packages("stargazer")
library(stargazer)


# == Load data from the "wage" example
data('wage1')
# == Inspect the variables and learn about the dataset
?wage1


# == Visually explore the distributions of variables of interest

# Show the distribution of ln(wages) and show mean and median
ggplot(data=wage1)+
  geom_histogram(aes(x=lwage), alpha=.5) +
  geom_vline(xintercept=mean(wage1$lwage), color="navy") +
  geom_vline(xintercept=median(wage1$lwage), color="maroon")

# Show the distribution of education
ggplot(data=wage1)+geom_histogram(aes(x=educ))

# == Visually inspect the relationship between wages and education

ggplot(data=wage1, aes(x=educ, y=lwage)) + 
  geom_point() +
  geom_smooth(method="lm", se=T)

ggplot(data=wage1) + 
    geom_jitter(aes(x=educ, y=lwage), alpha=.5, width = 0,1, height = .1) +
    geom_smooth(aes(x=educ, y=lwage), method="lm", se=F)
# Simple linear regression model is often stated in terms of the model of conditional mean of Y.
# That is the expected Y conditional on the values of X. The next bit illustrates this idea.

# The wage seems to depend on the level of education. LWAGE is NOT mean-independent of EDUC
wage1 %>% group_by(educ) %>% select(educ,wage) %>% summarize(mean(wage))

# Pick two levels of education: 8 (maroon) and 16 (gray) years and show the distributions of lwage and means
ggplot() + 
  geom_density(data=subset(wage1,educ==8), aes(x=lwage, stat(density)), alpha=.5, color="maroon", fill="maroon", adjust = 1.5) +
  geom_vline(xintercept=mean(wage1[wage1$educ==8,]$lwage), color="maroon") +
  geom_density(data=subset(wage1,educ==16), aes(x=lwage, stat(density)), alpha=.75, color="gray", fill="gray", adjust = 1.5) +
  geom_vline(xintercept=mean(wage1[wage1$educ==16,]$lwage), color="darkgray")

# ------------------------------------------------------
# Perform all SLR calculations "by hand"
# ------------------------------------------------------

# Calculate the regression coefficients 
beta1 <- cov(wage1$lwage,wage1$educ)/(var(wage1$educ))
# Q: What is another formula for beta1 that uses correlation instead of covariance?
beta0 <- mean(wage1$lwage) - beta1*mean(wage1$educ)

# ------------------------------------------------------
# Calculate errors and predicted lwages and add them to the data.frame
wage1$lwage_hat <- beta0 + beta1*wage1$educ
wage1$u_hat <- wage1$lwage - wage1$lwage_hat

# ------------------------------------------------------
# Check assumptions and properties of the estimators

# Recall that the residuals are calculated based on the estimates. 
# The estimates are calculated based on the assumption that errors 
# satisfy the two assumptions: E(u)=0 and E(Xu)=0. Let's check if those
# assumptions hold in the sample. 

# Assumption 1: Is the mean of the residuals equal to zero, E(u)=0
mean(wage1$u_hat) # note the scientific notation
round( mean(wage1$u_hat), digits=7) # rounding to 7 digits

# Assumption 2: Are residuals correlated with the X? E(Xu)=0
cor(wage1$u_hat,wage1$educ) # this is a very precise number very close to zero
round( cor(wage1$u_hat,wage1$educ), digits=7)

# ------------------------------------------------------
# What about the correlation of u_hat with the predicted lwage_hat?
cor(wage1$u_hat,wage1$lwage_hat) # this is a very precise number very close to zero
round( cor(wage1$u_hat,wage1$lwage_hat), digits=7)

# ------------------------------------------------------
# Explore the residual plot (the plot of u_hat on x) for 
# possible violations of the zero conditional mean assumption. 
# Recall that lack of correlation between u_hat and x does not imply 
# zero conditional mean. 

ggplot(data=wage1,aes(x=educ,y=u_hat)) +
  geom_jitter( alpha=.5) + 
  geom_smooth( method="lm", se=F, color="red") + 
  geom_smooth( se=F, color="orange") 

#With more than one variable we can plot each variable on 
ggplot(data=wage1,aes(x=lwage_hat,y=u_hat)) +
  geom_jitter( alpha=.5) + 
  geom_smooth( method="lm", se=F, color="red") + 
  geom_smooth( se=F, color="orange") 

# Q: What do you make of the orange line of flexible best fit?

# ------------------------------------------------------
# Check whether the predicted plot goes through the averages of the data
ggplot(data=wage1) +
  geom_jitter(aes(x=educ,y=lwage), alpha=.5) +
  geom_line(aes(x=educ,y=lwage_hat)) +
  geom_vline(xintercept=mean(wage1$educ), color="orange")+
  geom_hline(yintercept=mean(wage1$lwage_hat), color="orange")

ggplot(data=wage1) +
  geom_jitter(aes(x=educ,y=lwage), alpha=.5) +
  geom_line(aes(x=educ,y=lwage_hat)) +
  geom_vline(xintercept=mean(wage1$educ), color="orange")+
  geom_hline(yintercept=mean(wage1$lwage), color="orange")
# ------------------------------------------------------
# Investigate the goodness of fit

# calculate the sums of squares
SST <- sum( (wage1$lwage-mean(wage1$lwage))^2 )
SSE <- sum( (wage1$lwage_hat-mean(wage1$lwage))^2 )
SSR <- sum( (wage1$u_hat)^2 )
# calculate n and SSTx
SSTx <- sum((wage1$educ-mean(wage1$educ))^2)
n <- length(wage1$lwage)

# calculate R-squared 
Rsq <- SSE/SST

# verify that Rsq=cor(x,y)=cor(y_hat,y)
Rsq

# calculate correlation between y and y_hat
(cor(wage1$lwage,wage1$lwage_hat))^2

# calculate correlation between y and x
(cor(wage1$lwage,wage1$educ))^2

# quick check that the total sum is equal to the sum of the two components
SST - SSE - SSR

# How does it look that the variances of y_hat and u_hat are smaller than the total 
# variance in y?
ggplot(data=wage1) +
  geom_histogram(aes(x=lwage), fill="gray") +
  geom_histogram(aes(x=lwage_hat), alpha=.75, fill="maroon") 

ggplot(data=wage1) +
  geom_histogram(aes(x=c(lwage-mean(lwage))), fill="gray") +
  geom_histogram(aes(x=u_hat), alpha=.5, fill="orange")

# ------------------------------------------------------
# Calculate standard error of beta1 and beta0

# Calculate standard error of the regression residuals
SER <- (SSR/(n-2))^(0.5)
sigma_hat <- SER

# Calculate standard error of beta1 and beta0
beta1_se <- sigma_hat/(SSTx^(1/2))
beta0_se <- sigma_hat*(sum(wage1$educ^2)/n/SSTx)^(1/2)

# ------------------------------------------------------
# PREVIEW Preview of inference: What does the standard error tell us?
# If beta1 was normally distributed what does it mean for our estimate?
ggplot() + 
  geom_density(aes(x=rnorm(1000, mean=beta1, sd=beta1_se)), adjust=2) +
  geom_vline(xintercept=beta1, color="red") 
# Why do we have a distribution if we calculated an actual number for beta1?

# How far is the distribution from zero?
ggplot() + 
  geom_density(aes(x=rnorm(1000, mean=beta1, sd=beta1_se)), adjust=2) +
  geom_vline(xintercept=beta1, color="red") + 
  geom_vline(xintercept=0, color="blue")  

# Do the same for beta0
ggplot() + 
  geom_density(aes(x=rnorm(1000, mean=beta0, sd=beta0_se)), adjust=2) +
  geom_vline(xintercept=beta0, color="red") +
  geom_vline(xintercept=0, color="blue") 

# ------------------------------------------------------
# Let's plot the predicted and the actual values of the dependent variable
# Note that the variance of the predicted values is smaller than the variance
# of the actual values.
jitter <- position_jitter(width = .5, height = 0.05) # add some jitter
ggplot(wage1) + 
  geom_point(aes(x=educ, y=lwage), alpha=.9, color="gray", position=jitter) +
  geom_point(aes(x=educ, y=lwage_hat), alpha=.3, color="maroon", position=jitter)

# You can also see how the histogram of the predicted values (maroon) is taller
# meaning that the variance is smaller
ggplot(wage1) + 
  geom_histogram(aes(x=lwage), alpha=.8, color="black", fill="gray") +
  geom_histogram(aes(x=lwage_hat), alpha=.3, color="maroon", fill="maroon")

# ------------------------------------
# The entire estimation can be done with one command
ols_estimates <- lm(lwage ~ educ, data=wage1)
  #ols_estimates is an object
  coef(ols_estimates) # extracts the estimated coefficients
  
  summary(ols_estimates) # present the table of results. This summary is also an object, a list
  # to access coefficients take the first column
  summary(ols_estimates)$coefficients[,1]
  # to access standard errors take the second column
  summary(ols_estimates)$coefficients[,2]
  
  wage1$lwage_hat <- predict(ols_estimates) # create predictions: instead of beta0_hat+beta1_hat*wage1$educ
  wage1$u_hat <- residuals(ols_estimates) # calculate residuals


# A neat way to present the result of the estimation is to put into the table
stargazer(ols_estimates, type="text")


