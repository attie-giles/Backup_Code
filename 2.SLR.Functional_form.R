# ------------------------------------------
# Functional form and Intrinsically linear functional forms
#
# In this script we will see how a non-linear transformation results in the 
# the estimation of a linear model (of the transformed data) and yet fits a non-
# linear relationship of the underlying (non-transformed) data.
#

remove(list=ls())

#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
library(stargazer)


data('wage1')
?wage1

# To avoid issues when calculating log(educ) drop observations where educ=0
wage1<-subset(wage1, educ!=0) 

# Illustrate the linear fit to the level-level data
level.level.wage <- lm(wage ~ educ, wage1)

ggplot(wage1)+
  geom_jitter(aes(x=educ, y=wage), alpha=.3, width = .5, height = 0.5)+
  geom_line(aes(x=educ,y=predict(level.level.wage))) + # non-linear for the non-transformed data
  labs(
    title="Level plot for the model fitted on non-transformed data",
    y='Wage, $/hour',
    x='Education, years'
  )


# Illustrate the estimated vs. non-transformed data for the log-level estimation

log.level.lwage <- lm(lwage ~ educ, wage1)
stargazer(log.level.lwage, type="text")


#
ggplot(wage1)+
  geom_jitter(aes(x=educ, y=lwage), alpha=.3, width = .5, height = 0.5)+
  geom_line(aes(x=educ,y=(predict(log.level.lwage)))) +  # linear for estimation
  labs(
    title="Linear fit to the log-transformed dependent variable",
    y='log(Wage)',
    x='Education, years'
  )

ggplot(wage1)+
  geom_jitter(aes(x=educ, y=wage), alpha=.3, width = .5, height = 0.5)+
  geom_line(aes(x=educ,y=exp(predict(log.level.lwage)))) + # non-linear for the non-transformed data
  labs(
    title="Level plot for the model fitted on log-transformed dependent variable",
    y='Wage, $/hour',
    x='Education, years'
  )

# How does y=exp(beta*x) look like?
beta<- -.5
x<-seq(from=-5,to=5,by=.01) 
y<-exp(beta*x)
ggplot() +
  geom_line(
    aes(x,y))

# Illustrate the estimated vs. non-transformed for the log-log estimation

log.log.lwage <-lm(lwage ~ log(educ), wage1)

stargazer(log.log.lwage, type="text")  

ggplot(data=wage1, aes(x=log(educ))) + 
  geom_jitter(aes(y=lwage), alpha=.3, width = .5, height = 0.5) +
  geom_line(aes(y=(predict(log.log.lwage)))) + # linear for estimation
  labs(
    title="Linear fit to the log-transformed variables",
    y='log(Wage)',
    x='log(Education)'
  )

ggplot(data=wage1) + 
  geom_jitter(aes(x=educ, y=wage), alpha=.3, width = .5, height = 0.5) +
  geom_line(aes(x=educ,y=exp(predict(log.log.lwage)))) + # non-linear for the non-transformed data
  labs(
    title="Level plot for the model fitted on log-transformed dependent variable",
    y='Wage, $/hour',
    x='Education, years'
  )

# Compare the three estimators
stargazer(log.log.lwage, 
          level.level.wage,
          log.level.lwage,
          type="text")  


# -------
# PREVIEW of multiple regressions: estimate regression with quadratic terms
# 
# Several scripts have "Previews" that are look ahead applications of the material
# we cover in class that represent direct extensions of the material we cover.
# I choose to do so to capitalize on the momentum.
#
# I do not expect you to fully internalize the material and it will not be included
# on the test until we cover the corresponding chapter.
#
# Note that quadratic form 

wage1$educ2 <-wage1$educ^2

# Fit a model with dependent variable in level
squared.wage <- lm(wage ~ educ + educ2, wage1)
stargazer(squared.wage, type="text")  
ggplot(wage1)+
  geom_jitter(aes(x=educ, y=wage), alpha=.3, width = .5, height = 0.5)+
  geom_line(aes(x=educ,y=predict(squared.wage)))

# Fit a model with log dependent variable
squared.lwage <- lm(lwage ~ educ + educ2, wage1)

stargazer(squared.lwage, type="text")  
ggplot(wage1)+
  geom_jitter(aes(x=educ, y=lwage), alpha=.3, width = .5, height = 0.5)+
  geom_line(aes(x=educ,y=predict(squared.lwage)))


# -------
# PREVIEW and advanced non-parametric forms

# We can also use a local smoothed regression where we fit a line
# to the moving interval of the explanatory variable

# Fit a model with dependent variable in level
ggplot(wage1, aes(x=educ, y=wage))+
  geom_jitter( alpha=.3, width = .5, height = 0.5) +
  geom_smooth(se=F)

# Fit a model with log dependent variable
ggplot(wage1, aes(x=educ, y=lwage))+
  geom_jitter( alpha=.3, width = .5, height = 0.5) +
  geom_smooth(se=F)

# PREVIEW
# We can also turn the data into discrete bins 

# let's break education in three bins <=12, 13-17, and >17
wage1 <- wage1 %>% 
  mutate(educlevel=case_when( 
    educ<=12 ~ 1,
    educ<=17 ~ 2, 
    TRUE     ~ 3
  ))
level.3bins.wage <- lm(wage ~ factor(educlevel), wage1)

ggplot(wage1)+
  geom_jitter(aes(x=educ, y=wage), alpha=.3, width = .5, height = 0.5)+
  geom_line(aes(x=educ,y=predict(level.3bins.wage))) + # non-linear for the non-transformed data
  labs(
    title="Level plot for the model fitted to explanatory variable in 3 bins",
    y='Wage, $/hour',
    x='Education, years'
  )

# We can also assign bins to all levels separately
level.bins.wage <- lm(wage ~ factor(educ), wage1)

ggplot(wage1)+
  geom_jitter(aes(x=educ, y=wage), alpha=.3, width = .5, height = 0.5)+
  geom_line(aes(x=educ,y=predict(level.bins.wage))) + # non-linear for the non-transformed data
  labs(
    title="Level plot for the model fitted to explanatory variable in bins",
    y='Wage, $/hour',
    x='Education, years'
  )

