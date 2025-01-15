
# ----------------------------------
# Qualitative information in the regressions
remove(list=ls())
#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
library(stargazer)


# == Load data from the "wage" example
data('wage1')

# Explore the data set for some binary variables
# Binary variables take two values and represent the simplest kind of qualitative information.
# The estimated coefficient on the dummy  variable (binary categorical variable) captures the difference in the means 
# for the two values of the explanatory variables.

wage1$female <- as.factor(wage1$female)

# Example 1. Estimate the model and show that the difference between groups is the same as the estimated coefficients
qplot(lwage,data=wage1, geom="density", color=female) # Q: Why is it important to use factor()
# Calculate means for both groups
wage1 %>% group_by(female) %>% summarize(mean.lwage=mean(lwage)) 
# Estimate a model with the group dummy
stargazer(
  lm(lwage ~ female, wage1)
  , type="text")

# Note. This interpretation is similar to the "Treatment effect" interpretation

# Example 2. Estimate a similar model for the "nonwhite" binary variable
qplot(lwage,data=wage1, geom="density", color=factor(nonwhite)) 
wage1 %>% group_by(nonwhite) %>% summarize(mean(lwage))
stargazer(
  lm(lwage ~ nonwhite, wage1)
  , type="text")

# Example 3. The binary or other discrete variables can also be used to put continuous variables
# into discrete bins.
#
# Some continuous data can be turned into binary. For example does the person have dependents?

# Perhaps for our purposes the number of dependents in an unnecessarily 
# detail that may obfuscate the big picture result
table(wage1$numdep)
qplot(lwage,data=wage1, geom="density", color=factor(factor(numdep))) 
wage1 %>% group_by(numdep) %>% summarize(mean(lwage))

# Let's "binarize" the number of dependents
wage1$dep <- as.factor((wage1$numdep!=0)*1 )
table(wage1$dep)

qplot(lwage,data=wage1, geom="density", color=factor(dep)) 
wage1 %>% group_by(dep) %>% summarize(mean(lwage))
stargazer(
  lm(lm(lwage ~ dep, wage1))
  , type="text")



# PREVIEW EXAMPLE: Note that this same idea applies to more than one levels

# let's break education in three levels <=12, 13-17, and >17
wage1$educlevel[(wage1$educ<=12)] <- 1
wage1$educlevel[wage1$educ %in% c(13:17)] <- 2 #note the use of %in%
wage1$educlevel[wage1$educ>17] <- 3
wage1$educlevel <- as.factor(wage1$educlevel)
table(wage1$educlevel)

qplot(lwage,data=wage1, geom="density", color=factor(educlevel)) 
wage1 %>% group_by(educlevel) %>% summarize(mean(lwage))
summary(lm(lwage ~ educlevel, wage1))
stargazer(
  lm(lm(lwage ~ educlevel, wage1))
  , type="text")
# Do the numbers match the table of the means?

# One question you might ask is why do we turn the continuous variable "edu" into
# a discrete? The reason is that this way we can see if the relationship is the same
# for all increases in "edu". It is a flexible functional form.

# ----------------------------------------------------------------------------
# Example 2: Model of the house price

data("hprice1") # Use data from HPRICE1 dataset on housing prices 
?hprice1

table(hprice1$bdrms)
hprice1$large <- as.factor((hprice1$bdrms>4)*1 )
stargazer(lm(lprice ~ bdrms, data=hprice1),
          lm(lprice ~ large, data=hprice1), 
          type="text")

# ----------------------------------------------------------------------------
# Example 2: Model of earnings

data("cps91") 
?cps91

stargazer(lm(lwage ~ kidlt6, data=cps91),
          lm(lwage ~ kidge6, data=cps91),
          lm(lwage ~ kidlt6 + kidge6, data=cps91),
          type="text")

stargazer(lm(lwage ~ union, data=cps91),
          lm(lwage ~ union + kidlt6 + kidge6, data=cps91),
         type="text")

data("jtrain2")
?jtrain2
stargazer(lm(lre78 ~ train, data=jtrain2), 
          type="text")


