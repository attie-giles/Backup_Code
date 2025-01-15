# --------------------------------------------------------------------------
# An example simulation to illustrate the role of SSTx and sigma_u 

#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
library(stargazer)

# In order to explore the effect of the increase in the amount of unexplained variation we 
# simulate two sets of errors to vary the amount of noisee in the model
nobs <- 20
beta <- 1
e_tight   <- rnorm(nobs, mean=0, sd=1)
e_spread  <- rnorm(nobs, mean=0, sd=5)
x <- runif(nobs, min=0, max=20)
x_lowvar <- runif(nobs, min=5, max=15)

# Here is how the two residuals are distributed
ggplot() + 
  geom_histogram(aes(e_tight), alpha=.3, color="maroon", fill="maroon") +
  geom_histogram(aes(e_spread), alpha=.7, color="gray")

# The resulting dependent variable is simulated
y_tight <-  x*beta + e_tight
y_spread <-  x*beta + e_spread
y_tight_lowvar <- x_lowvar*beta + e_tight
y_spread_lowvar <- x_lowvar*beta + e_spread


# Here is how the three relationships look
ggplot() +
  geom_point(aes(x=x,y=y_spread), alpha=.9, color="gray") +
  geom_point(aes(x=x_lowvar, y=y_spread_lowvar), alpha=.5, color="darkgreen") +
  geom_point(aes(x=x,y=y_tight), alpha=.5, color="maroon") +
  geom_point(aes(x=x_lowvar,y=y_tight_lowvar), alpha=.25, color="navy") 
  

# Now compare the correlation coefficient to the slope

cor(x,y_tight) 
cor(x,y_spread) 
cor(x_lowvar, y_spread_lowvar) 
cor(x_lowvar, y_tight_lowvar) # the correlations are very different

cov(x,y_spread)/var(x)
cov(x,y_tight)/var(x) 
cov(x_lowvar, y_spread_lowvar)/var(x_lowvar) 
cov(x_lowvar, y_tight_lowvar)/var(x_lowvar) # the slopes are similar

# The regression 
m1 <- lm(y_tight ~ x)
m2 <- lm(y_spread ~ x)
m3 <- lm(y_tight_lowvar ~ x_lowvar)
m4 <- lm(y_spread_lowvar ~ x_lowvar)
stargazer(m1,m2,m3,m4,  type="text")

# Is the difference in R-squared what you expected? Why?
# Given the picture, which graph has lower R-squared and why?
# Is the difference in the standard errors of the estimates what you expected? Why?

# Change the amount of variation in the error. What do you expect to happen?
# Change the spread of x (which also changes the variance). What do you expect to happen
# Change the number of observations. What do you expect to happen?

# ------------
# Sometimes regression is framed as a model of the conditional mean of y. 
# That is how the mean of y changes when x changes. 

# Show the conditional mean of y for two ranges of y
y_1 <- y_spread[x>0 & x<2]
y_2 <- y_spread[x>16 & x<18]
ggplot() + 
  geom_histogram( aes(x=y_1, stat(density)), alpha=.5, color="maroon", fill="maroon", binwidth=3) +
  geom_vline(xintercept=mean(y_1), color="red") +
  geom_histogram( aes(x=y_2, stat(density)), alpha=.5, color="gray", binwidth=3) +
  geom_vline(xintercept=mean(y_2), color="black")


# Go back and increase the standard deviation of the e_spread and recalculate.
# Is the result what you expected?
