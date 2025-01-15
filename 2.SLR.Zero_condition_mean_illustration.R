# This script uses the data on home prices to show consequences of violating assumptions 
# of E(u)=0 and cov(u,x)=0

# Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
library(grid) # Helps combining two graphs into one 

data(hprice1)
?hprice1

# Consider a line of best fit of square feet and the log of price
ggplot(data=hprice1, aes( x=lsqrft))+
  geom_point(aes(y=lprice), alpha=0.5, size=0.75, color="navy")+
  geom_smooth(aes(y=lprice), color="navy", method="lm", se=F) +
  labs( title="Line of best fit",
        subtitle="both variables are in logs", 
        x="Living area of house square feet, log",
        y="Price of house, log")
  
# Here I will create two "phantom" price data to illustrate zero conditional mean assumption
hprice1 <- hprice1 %>% 
  mutate(lprice_low=lprice-0.5) %>% #shift
  mutate(lprice_twisted=lprice+(-0.75)*(lprice-mean(lprice))) #pivot around the mean

xyplot <- ggplot(data=hprice1, aes( x=lsqrft))+
  geom_smooth(aes(y=lprice), color="navy", method="lm", se=F) +
  geom_smooth(aes(y=lprice_low), color="maroon", method="lm", se=F) +
  geom_smooth(aes(y=lprice_twisted), color="darkgreen", method="lm", se=F) +
  geom_point(aes(y=lprice), alpha=0.5, size=0.75, color="navy") + 
  labs(x="Living area of house square feet, log",
       y="Price of house, log")


xyplot

# In the figure above:
# - the navy line fits the data by satisfying both E(u)=0 and E(u|x)=0
# - the green line overshoots at low values of x (which is the log of square feet)
#   of the house, and undershoots at the high values of x. Resulting in a positive
#   correlation between residuals and x. The line has the same mean so it
#   satisfies E(u)=0, but does not satisfy E(u|x)=0.
# - the red line is parallel to the navy line, for this line the E(u|x)=Constant
#   that constant is not zero. There is still no correlation between u and x, so
#   cov(u,x)=0 is satisfied even though E(u)=0 is not satisfied.

resplot <- ggplot(data=hprice1, aes( x=lsqrft))+
  geom_point(aes(
    y=(lprice-predict(lm(lprice ~ lsqrft, data=hprice1)))), 
    color="navy", alpha=.5) +
  geom_smooth(aes(
    y=(lprice-predict(lm(lprice ~ lsqrft, data=hprice1)))), 
    color="navy", method="lm", se=F) +
  geom_point(aes(
    y=(lprice-predict(lm(lprice_low ~ lsqrft, data=hprice1)))), 
    color="maroon", alpha=.5) + 
  geom_smooth(aes(
    y=(lprice-predict(lm(lprice_low ~ lsqrft, data=hprice1)))), 
    color="maroon", method="lm", se=F) + 
  geom_point(aes(
    y=(lprice-predict(lm(lprice_twisted ~ lsqrft, data=hprice1)))), 
    color="darkgreen", alpha=.5)+
  geom_smooth(aes(
    y=(lprice-predict(lm(lprice_twisted ~ lsqrft, data=hprice1)))), 
    color="darkgreen", method="lm", se=F)  +
    labs(x="",
     y="Residual")


resplot

# The figure shows residuals that correspond to the lines of best fit. 

grid.newpage()
grid.draw(rbind(ggplotGrob(xyplot), ggplotGrob(resplot), size = "last"))

