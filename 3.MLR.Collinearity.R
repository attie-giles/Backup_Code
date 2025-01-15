# In this script we breifly consider the detrimental effect of collinearity

remove(list=ls())

library(wooldridge) 
library(dplyr)
library(ggplot2)
library(stargazer)
library(foreign)

data("hprice1")
m1 <- lm(lprice ~ bdrms + lsqrft + llotsize, hprice1)
m2 <- lm(lprice ~ bdrms , hprice1)
stargazer(m1, m2,
  type="text"
)


m1 <- lm(lprice ~ lsqrft + bdrms + llotsize + colonial, hprice1)
m2 <- lm(lprice ~ lsqrft , hprice1)
stargazer(m1, m2,
  type="text"
)

m1 <- lm(lprice ~ lsqrft  + llotsize, hprice1)
m2 <- lm(lassess ~ lsqrft  + llotsize, hprice1)
m3 <- lm(lprice ~  lsqrft  + llotsize + lassess, hprice1)
stargazer(m1, m2, m3,
  type="text"
)

