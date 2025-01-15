# In this script we will explore several examples of omitted variable bias

remove(list=ls()) #Clear out the environment

library(wooldridge) 
library(dplyr)

library(stargazer)

library(ggplot2)
#install.packages("ggcorrplot")
library(ggcorrplot) # explores correlations on the graph

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)


# The expression for the omitted variable bias

# ---- Example: Housing price and size, bedrooms omitted
data(hprice1) 
?hprice1

# -- Explore correlations visually with ggcorrplot
corr <- round(cor(hprice1), digits=2)
ggcorrplot(corr, 
           hc.order = T,      # puts the higest correlations on top
           type = "lower",    # lower bottom triangle
           lab = TRUE,        # include the actual number of correlation
           lab_size = 3,      # makes correlations fit better inside the graph
           method="square",   # other options include "circle"
           colors = c("tomato2",      # color for corr>0
                      "white",        # color for corr=0
                      "springgreen3"),# color for corr<0 
           title="Correlogram of hprice1", # Title
           ggtheme=theme_bw)

# -- Explore correlations visually with PerformanceAnalytics


# The call below puts together: 
# - pairwise correlation, 
# - test of significance, 
# - histogram of each variable, 
# - pairwise scatterplot, and 
# - pairwise fitted line 



# Estimate the model with omitted variable

model.ovb <-lm(lprice ~ lsqrft, hprice1) # model with omitted variable
model.full<-lm(lprice ~ lsqrft + bdrms, hprice1) # full model
model.vars<-lm(                  bdrms ~ lsqrft, hprice1) # relationship between the variables

stargazer( # NOTE storing of the regression outcomes INSIDE the stargazer call
  model.ovb , # model with omitted variable
  model.full, # full model
  model.vars, # relationship between the variables
    type="text"
)

# Calculate the bias as the difference in coefficients
# NOTE $coefficients refers to the named vector of the coefficients stored after
# estimation. The names are the names of the variables. 
model.ovb$coefficients["lsqrft"] - model.full$coefficients["lsqrft"]

# Calculate the theoretical bias
model.full$coefficients["bdrms"] * model.vars$coefficients["lsqrft"]

# ---- Example: Housing price and bedrooms, size omitted

model.ovb <-lm(lprice ~ bdrms, hprice1)
model.full<-lm(lprice ~ bdrms+ lsqrft , hprice1)
model.vars<-lm(                lsqrft ~ bdrms, hprice1)


stargazer( # NOTE storing of the regression outcomes INSIDE the stargazer call
  model.ovb , # model with omitted variable
  model.full, # full model
  model.vars, # relationship between the variables
  type="text"
)

# Why do you think "bdrms" become insignificant? Write down a formula and explain by referring to that formula

# Calculate the bias as the difference in coefficients
model.ovb$coefficients["bdrms"] - model.full$coefficients["bdrms"]

# Calculate the theoretical bias
model.full$coefficients["lsqrft"] * model.vars$coefficients["bdrms"]

# ---- Example: Housing price and rooms, omitted crime

data('hprice2')
?hprice2

# -- Explore correlations visually with ggcorrplot
corr <- round(cor(hprice2), digits=2)
ggcorrplot(corr, 
           hc.order = T,      # puts the higest correlations on top
           type = "lower",    # lower bottom triangle
           lab = TRUE,        # include the actual number of correlation
           lab_size = 3,      # makes correlations fit better inside the graph
           method="square",   # other options include "circle"
           colors = c("tomato2",      # color for corr>0
                      "white",        # color for corr=0
                      "springgreen3"),# color for corr<0 
           title="Correlogram of hprice2", # Title
           ggtheme=theme_bw)

# -- Explore correlations visually with PerformanceAnalytics
chart.Correlation(hprice2, histogram=TRUE)

model.ovb <-lm(lprice ~ rooms, hprice2)
model.full<-lm(lprice ~ rooms + crime, hprice2)
model.vars<-lm(                 crime ~ rooms, hprice2)

stargazer( # NOTE storing of the regression outcomes INSIDE the stargazer call
  model.ovb , # model with omitted variable
  model.full, # full model
  model.vars, # relationship between the variables
  type="text"
)
# Calculate the bias as the difference in coefficients
model.ovb$coefficients["rooms"] - model.full$coefficients["rooms"]

# Calculate the theoretical bias
model.full$coefficients["crime"] * model.vars$coefficients["rooms"]

# ---- Example: Housing price and crime omitted
round(cor(hprice2), digits=2)

model.ovb <-lm(lprice ~ crime, hprice2)
model.full<-lm(lprice ~ crime + dist, hprice2)
model.vars<-lm(                 dist ~ crime, hprice2)

stargazer( # NOTE storing of the regression outcomes INSIDE the stargazer call
  model.ovb , # model with omitted variable
  model.full, # full model
  model.vars, # relationship between the variables
  type="text"
)
# Calculate the bias as the difference in coefficients
model.ovb$coefficients["crime"] - model.full$coefficients["crime"]

# Calculate the theoretical bias
model.full$coefficients["dist"] * model.vars$coefficients["crime"]

# -- Advanced discussion of the omitted variable bias in the case of more than two variables

# With more than two variables things get more complicated. Let's take a look at a model of housing prices as we add more variables to the mix
lm.1 <- lm(lprice ~ rooms + dist + radial + stratio, # benchmark hedonic model before we start adding variables
   hprice2) 
lm.2 <- lm(lprice ~ rooms + dist + radial + stratio + crime, 
   hprice2)
lm.3 <- lm(lprice ~ rooms + dist + radial + stratio + crime + proptax , 
   hprice2)
lm.4 <- lm(lprice ~ rooms + dist + radial + stratio + crime + proptax + lowstat, 
   hprice2)
lm.5 <- lm(lprice ~ rooms + dist + radial + stratio + crime + proptax + lowstat + nox, 
   hprice2)

stargazer(
  lm.1,
  lm.2,
  lm.3,
  lm.4,
  lm.5,
  type="text"
)

# Go back to the correlation matrix and discuss the OVB as we increase the number of variables

# Which sings change? Which magnitudes change? Which standard errors change?

# What are the tradeoffs from adding variables to a MLR model?
