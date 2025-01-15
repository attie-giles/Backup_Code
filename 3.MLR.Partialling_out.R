# In this script we will explore partialling out and draw parallels 

remove(list=ls())
library(wooldridge) 
library(dplyr)
library(ggplot2)
library(stargazer)
library(foreign)
library(grid) # Helps combining two graphs into one 

# ====================================================================================
# ==== Does partialling out work? ====

data("hprice1")
?hprice1

#Partial out from the lprice and the lsqrft
lprice.lsqrft.res   <- residuals(lm(lprice ~ bdrms + llotsize,
                                      hprice1))
       lsqrft.res   <- residuals(lm(lsqrft ~ bdrms + llotsize,
                                      hprice1))
#Partial out from the lprice and the bdrms
lprice.bdrms.res    <- residuals(lm(lprice ~ lsqrft + llotsize,
                                      hprice1))
       bdrms.res    <- residuals(lm(bdrms ~ lsqrft + llotsize,
                                      hprice1))
#Partial out from the lprice and the llotsize
lprice.llotsize.res <- residuals(lm(lprice ~ lsqrft + bdrms , 
                                      hprice1))
       llotsize.res <- residuals(lm(llotsize ~ lsqrft + bdrms ,
                                      hprice1))

# Show that the coefficients of MLR are identical to the partialled out SLRs
m1 <-    lm(lprice ~ lsqrft + bdrms + llotsize, hprice1)
m2 <-    lm(lprice.lsqrft.res ~ lsqrft.res)
m3 <-    lm(lprice.bdrms.res ~ bdrms.res)
m4 <-    lm(lprice.llotsize.res ~ llotsize.res)
stargazer(m1, m2, m3, m4, 
  type="text"
)

m1 <-    lm(lprice ~ lsqrft + bdrms + llotsize, hprice1)
m2 <-    lm(lprice ~ lsqrft.res, hprice1)
m3 <-    lm(lprice ~ bdrms.res, hprice1)
m4 <-    lm(lprice ~ llotsize.res, hprice1)

stargazer(m1, m2, m3, m4, 
  type="text"
)
# Why do you think the standard errors are off by a little bit?

# Why do you think the constant is zero?

# ==== Two important examples of partialling out ====

# ---- De-trending the data ----

# Illustrate partialling out by applying it to the data that trends. 

# Trend - a variable that increases at constant intervals between periods of observation. For example trend=year.

# Recall the effect of the omitting variable. What if this omitted variable is the trend?

# Let's load the data on presidential approval from Wooldridge
data("approval")

?approval # what are the variables in this data set?

# How to deal with date?
approval <- approval %>% 
            mutate( time=paste( # function to combine strings
                      approval$year, # first string
                      formatC(approval$month, width=2, flag="0"), # second string, the formatC() adds a zero and makes 3 into 03
                      sep="-") # separator between elements to be pasted 
            ) %>% 
            mutate(date = as.Date(
                                  paste(time, "-01", sep=''))
                   ) %>% # standard format requires a day so I add -01 for the first day of the month# closes the mutate() verb
            arrange(date) %>%
            mutate(trend=1:n()) %>%
            select(date, trend, lrgasprice, rgasprice, approve)
            

# Combine the two time series plots 
trended.timeseries <- ggplot(data=approval, aes(x=date)) + 
  geom_line(aes(y=approve), color="navyblue") +
  geom_line(aes(y=rgasprice), color="salmon") +
  geom_smooth(aes(y=approve), color="navyblue", size=.5, method="lm", se=F) +
  geom_smooth(aes(y=rgasprice), color="salmon", size=.5, method="lm", se=F) +
  labs(y="Approval,% / Gas price, Cents",
       x="Date")
trended.timeseries
# How do you think the trend contributes to the relationship between the two variables.

# What are the directions of the trends? What is the sign of the omitted variable bias?
cor(approval$approve,approval$trend)
cor(approval$approve,approval$trend)^2 # what does this calculation tell you?

cor(approval$rgasprice,approval$trend)
cor(approval$rgasprice,approval$trend)^2  # what does this calculation tell you?

# Explore partialling out in the context of MLR

model.notrend <- lm(approve ~ rgasprice, approval)
model.trend   <- lm(approve ~ rgasprice + trend, approval)
gasp.trend    <- lm(rgasprice ~ trend, approval)
appr.trend    <- lm(approve   ~ trend, approval)
model.detrended <- lm(residuals(appr.trend) ~ residuals(gasp.trend),
                      approval)


stargazer(model.notrend, model.trend, gasp.trend, appr.trend, model.detrended,
  type="text"
)

# Note the difference in R-squared between (2) and (5). What do you think it tells us?

# Combine the two plots with the trend 

# First, show the data around the trends
detrended.timeseries <- ggplot(data=approval, aes(x=date)) + 
  geom_line(aes(y=residuals(appr.trend)),color="navyblue") +
  geom_smooth(aes(y=residuals(appr.trend)), size=.5, color="navyblue", method="lm", se=F) +
  geom_line(aes(y=residuals(gasp.trend)), color="salmon") +
  geom_smooth(aes(y=residuals(gasp.trend)+.2), size=.5, color="salmon", method="lm", se=F) +
  labs(y="Detrended: Approval,% / Gas price, Cents",
       x="Date")
detrended.timeseries 

grid.newpage()
grid.draw(rbind(ggplotGrob(trended.timeseries), ggplotGrob(detrended.timeseries), size = "last"))

# Show the relationship between the two time series for trended and detrended data
ggplot(data=approval, aes(x=rgasprice, y=approve)) +
  geom_point(color="maroon", alpha=.5) +
  geom_smooth(color="maroon", size=.5, method="lm", se=F) +
  geom_point( aes (x=residuals(gasp.trend) + mean(rgasprice), 
                          y=residuals(appr.trend) + mean(approve)
                  ), color="navyblue", alpha=.5) +
  geom_smooth( aes (x=residuals(gasp.trend) + mean(rgasprice), # adding back the means so the two graphs have the same means
                    y=residuals(appr.trend) + mean(approve)
                   ),  color="navyblue", size=.5, method="lm", se=F) +
  labs(y="Approval,%",
       x="Real gasoline price, cents")
# Is the difference in slope expected?
# Why do you think the variance of the gas price in the blue scatter is smaller?
# Which line has larger variance of the residuals? How do you know? 


# Trending data can lead to spurious correlation explainable by the omitted variable bias

# ---- Removing group heterogeneity ---- 
data("prison")
?prison
any(
  duplicated(
    subset(prison, 
           select=c(year, state)
    )
  )
)
m1<-lm(lcrip ~ log(polpc), prison)
m2<-lm(lcrip ~ log(polpc) + factor(state), prison)
stargazer(m1, m2,
          type="text")

ggplot(prison, aes(x=log(polpc), y=lcrip)) +
  geom_point(aes(color=factor(state)), size=.5, alpha=.5) +
  geom_smooth(method='lm', se=F, size=.5)+
  geom_smooth(aes(color=factor(state)), method='lm', se=F, size=.5)+
  theme(legend.position = "none")

prison <- prison %>%
  mutate( lcrip.res     = residuals(lm(lcrip ~ factor(state), prison)) + mean(prison$lcrip), 
          log.polpc.res = residuals(lm(I(log(polpc)) ~ factor(state), prison)) + mean(log(prison$polpc)))

ggplot(prison) +
  geom_point( aes(x=log(polpc), y=lcrip), 
              color="maroon", size=.5, alpha=.5) +
  geom_smooth( aes(x=log(polpc), y=lcrip),
              color="maroon", method='lm', se=F, size=.5) +
  geom_point(aes(x=log.polpc.res, y=lcrip.res), 
             color="navy", size=.5, alpha=.5) +
  geom_smooth(aes(x=log.polpc.res, y=lcrip.res), 
              color="navy", method='lm', se=F, size=.5) +
  theme(legend.position = "none") +
  labs(title="Comparison of raw and group heterogeneity partialled out data",
       subtitle = "Note the difference in variation of x and y variables",
       y="Police per capita,log",
       x="Property crime rate, log")


# Show how a state moves to the mean
highlightfips=9 # state fips code to highlight

ggplot(prison) +
  geom_point( aes(x=log(polpc), y=lcrip), 
              color="maroon", size=.5, alpha=.15) +
  geom_smooth( aes(x=log(polpc), y=lcrip),
               color="maroon", method='lm', se=F, size=.15) +
  geom_point(aes(x=log.polpc.res, y=lcrip.res), 
             color="navy", size=.5, alpha=.15) +
  geom_smooth(aes(x=log.polpc.res, y=lcrip.res), 
              color="navy", method='lm', se=F, size=.15) +
  geom_point( data=subset(prison, state==highlightfips), 
              aes(x=log.polpc.res, y=lcrip.res), 
              color="navy", size=.75) +
  geom_point( data=subset(prison, state==highlightfips),
              aes(x=log(polpc), y=lcrip), 
              color="maroon", size=.75) +
  theme(legend.position = "none") +
  labs(title="Comparison of raw and group heterogeneity partialled out data",
       subtitle = "Highlighted dots correspond to the same state",
       y="Police per capita,log",
       x="Property crime rate, log")


  
