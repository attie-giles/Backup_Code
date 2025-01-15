# ----------------------------------
# Interpreting coefficients of the logarithmic transformation
# install.packages("stargazer") 
remove(list=ls())
library(stargazer)
library(wooldridge)

# ----------------------------------------------------------------------------
# Example 1: Wage model

data("wage1") #Use data from WAGE1 dataset on wages


# To avoid issues when calculating log(educ) drop observations where educ=0
wage1<-subset(wage1, educ!=0) 


level_level <- lm(wage ~ educ, wage1) # ∆y = β∆x
log_level <- lm(lwage ~ educ, wage1) # ∆%y = (100*β)∆x
level_log <- lm(wage ~ log(educ), wage1) # ∆y = (β/100)∆%x
log_log <- lm(lwage ~ log(educ), wage1) # ∆%y = β∆%x

stargazer(level_level, 
          log_level, 
          level_log, 
          log_log, 
          type="text")

# ---- Discrete variables 
# Interpret the effect of 2 year increase in education on wage. 

# The usual ∆%y = (100*β)∆x formula would produce a large error. 
# Instead we need to use the formula for discrete (or large) changes

0.087*4*100

# (exp(β∆x)-1)*100%
(exp(0.087*4)-1)*100

# Interpret the effect of 2 year decrease in education on lwage
(exp(0.087*(-4))-1)*100

# ----------------------------------------------------------------------------
# Example 2: Model of the house price

# Interpret all coefficients in the table. 
# Which one do you think makes the most sense (if you can make such a comparison)

data("hprice1") # Use data from HPRICE1 dataset on housing prices 
?hprice1

level_level <- lm(price ~ sqrft, data=hprice1) # ∆y = β∆x
log_level <- lm(lprice ~ sqrft, data=hprice1) # ∆%y = (100*β)∆x
level_log <- lm(price ~ lsqrft, data=hprice1) # ∆y = (β/100)∆%x
log_log <- lm(lprice ~ lsqrft, data=hprice1) # ∆%y = β∆%x

stargazer(level_level, 
          log_level, 
          level_log, 
          log_log, 
          type="text")

