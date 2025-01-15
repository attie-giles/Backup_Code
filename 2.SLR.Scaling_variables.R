# ----------------------------------
# Effect of variable transformation on the coefficients.
# We will focus on transforming variables by adding and multiplying
remove(list=ls())
library(stargazer)
library(wooldridge)
library(dplyr)

data("wage1")

# To avoid issues when calculating log(educ) drop observations where educ=0
wage1<-subset(wage1, educ!=0) 

# -------------------------
# What happens if we add/subtract a constant to x and y?

stargazer(
  lm(wage ~ educ, data=wage1),
  lm(wage ~ I(educ+2), data=wage1),
  lm(I(wage+2) ~ educ, data=wage1),
  type="text")
# Which coefficient changed? Which stayed the same?
# Does the magnitude and the direction of change in 
# match the theoretical prediction?

# Re-do the above table by changing the transformation to build intuition.

# ------------------------
# What happens if we multiply/divide x or y by a constant?

stargazer(
  lm(wage ~ educ, data=wage1),
  lm(wage ~ I(educ*12), data=wage1),
  lm(I(wage*40) ~ educ, data=wage1),
  type="text")

# Which coefficient changed? Which stayed the same?
# Does the magnitude and the direction of change in 
# match the theoretical prediction?

# Re-do the above table by changing the transformation to build intuition.

# ------------------------
# What happens if we multiply/divide x or y by a constant AND
# transform the variables by taking logs?

stargazer(
  lm(I(log(wage)) ~ I(log(educ)), data=wage1),
  lm(I(log(wage*40)) ~ I(log(educ)), data=wage1),
  lm(I(log(wage)) ~ I(log(educ*12)), data=wage1),
  type="text")

# In the log-transformed data units do not matter. Is this consistent with 
# our conclusion for adding and subtracting? Hint: what is log(X*A)=?


# ------------------------
# What happens if we standardize the data?

# Recall: standardization involves subtracting the mean and dividing the difference by standard deviation.

# PREVIEW: This section previews the "standardized beta coefficients"

# Standardize the data
wage1<-wage1 %>% 
  mutate(
    lwage.std=(lwage-mean(lwage))/sd(lwage), 
    wage.std=(wage-mean(wage))/sd(wage), 
    educ.std=(educ-mean(educ))/sd(educ), 
    exper.std=(exper-mean(exper))/sd(exper)
  )

# Present the results
stargazer(
  lm(wage.std ~ educ.std, data=wage1),
  lm(lwage.std ~ educ.std, data=wage1),
  lm(lwage ~ educ, data=wage1),
  lm(lwage.std ~ exper.std, data=wage1),
  lm(lwage ~ exper, data=wage1),
  type="text")

# Why is the intercept zero?

# What do we learn about effects of education and experience on earnings 
# by comparing columns (2) and (4) that we did not know by comparing 
# columns (3) and (5)

# Comparison of estimation results across the variables has three possible issues:
# 1) units across similar variables, taken care by rescaling (km to miles, adjustments for inflation)
# 2) units across different variables, taken care by taking logs (elasticity interpretation does not depend on units)
# 3) varying explanatory power across variables, taken care of by standardization (one standard deviation change in x leads to beta standard deviations change in y)
