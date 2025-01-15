#Clear the environment, optional
remove(list=ls())
#====================================================================================
# Module 3 : Additional visualizations
#====================================================================================

# In this script we learn about:
# - use of color to bring information on the third variable into a two dimensional graph
# - dealing with overplotting
# - interactive graphs
# ------------------------------------

# Install required packages

#install.packages("wooldridge")
#install.packages("foreign")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("quantmod")
#install.packages("WDI")
#install.packages("zoo") # Makes working with time series easier
#install.packages("lubridate") # Makes working with dates easier
#install.packages("plotly")

#====================================================================================
# ===== Downloading several basic packages

library(wooldridge)
library(quantmod) #https://www.quantmod.com/ describes the package and https://fred.stlouisfed.org/ contains information about the Federal Reserve data depository
library(WDI) # Please see https://cran.r-project.org/web/packages/WDI/WDI.pdf for documentation about the package and https://data.worldbank.org/indicator for general information about the data
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2) # https://www.r-graph-gallery.com/index.html for graph examples
library(plotly)
library(foreign)

# -----------------
# Dealing with over-plotting 

data('wage1') #a sample data set on wages, education, experience
?wage1

# some data points overlap and conceal information
ggplot(data=subset(wage1,educ>5), aes(x=educ, y=exper))+ 
  geom_point() +
  geom_smooth( color="black", method="lm", se=F)

ggplot(data=subset(wage1,educ>7), aes(x=educ, y=lwage))+ 
  geom_point() +
  geom_smooth( color="black", method="lm", se=F)

# adding noise and transparency to the data may help alleviate the issue
jitter <- position_jitter(width = 0.2, height = 0.2) # add some jitter or noise

ggplot(data=wage1, aes(x=educ, y=exper))+ 
  geom_point(alpha=.3, position=jitter) +
  geom_smooth( color="black", method="lm", se=F)

ggplot(data=wage1, aes(x=educ, y=lwage))+ 
  geom_point( alpha=.3, position=jitter) +
  geom_smooth( color="black", method="lm", se=F)

# In addition to jitter, counts of observations can be used to highlight "dense" areas on the graph
ggplot(data=wage1, aes(x=educ, y=exper))+ 
  geom_count(alpha=.3, position=jitter) +
  geom_smooth( color="black", method="lm", se=F)

ggplot(data=wage1, aes(x=educ, y=lwage))+ 
  geom_count( alpha=.3, position=jitter) +
  geom_smooth( color="black", method="lm", se=F)


ggplot(data=wage1, aes(x=educ, y=lwage))+ 
  geom_count( aes(color=factor(married)), alpha=.3, position=jitter) +
  geom_smooth( color="black", method="lm", se=F)+
  labs(color="Married, 1-yes")

# -----------------
# Producing interactive graphs with plotly
# https://plotly.com/r/ 
# ------
# Download data from the WDI
WDI <- WDI(country = "all", 
           indicator =c(taxrevenue2GDP="GC.TAX.TOTL.GD.ZS", 
                        rGDPpc="NY.GDP.PCAP.PP.KD"), 
           start = 2000, end = 2019, # pick three years for the homework
           extra = TRUE, cache = NULL) %>%
  filter(region!="Aggregates") %>% 
  select(-longitude, -latitude, -income, -lending, -region, -iso2c) %>%
  group_by(country, iso3c) %>%
  summarize( ave.tax = mean(taxrevenue2GDP, na.rm=TRUE) , 
             ave.rGDPpc=mean(rGDPpc, na.rm=TRUE) ) %>%
  mutate(income.group=cut(rank(-ave.rGDPpc),5, labels=c("1st","2nd","3rd","4th","5th")))

ggplot_wdi <- ggplot(data=WDI, aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() +
  scale_x_log10()
ggplot_wdi

# graphs can be transferred into plotly()
plotly_wdi <- ggplotly(ggplot_wdi)
plotly_wdi
# or created from scratch in plotly()
plot_ly(x=WDI$ave.rGDPpc, 
        y=WDI$ave.tax, 
        type = 'scatter')

plot_ly(x=WDI$ave.rGDPpc, 
        y=WDI$ave.tax, 
        type = 'scatter', 
        mode = 'markers',
        text=WDI$iso3c, 
        hoverinfo='text') %>%
  layout(xaxis = list(type="log"))


# ----
# Download time series of inflation based on the CPI and on PCE
getSymbols('CPIAUCNS',src='FRED') #
getSymbols('PCEPI',src='FRED') 

start_date = as.Date("1980-01-01")
decade_breaks = floor_date(c(start_date, start_date+dyears(10)+dmonths(1), start_date+dyears(20)+dmonths(1), start_date+dyears(30)+dmonths(1)), "month")
decade_labels = c("1980s", "1990s", "2000s")

inflation <- data.frame(PCEPI, date=index(PCEPI)) %>% 
  inner_join(data.frame(CPIAUCNS, date=index(CPIAUCNS)), by="date") %>%
  mutate(pceInfl = round((PCEPI-lag(PCEPI))/(lag(PCEPI))*100, digits=2), 
         cpiInfl = round((CPIAUCNS-lag(CPIAUCNS))/(lag(CPIAUCNS))*100, digits=2)) %>%
  filter(date>=as.Date(start_date)) %>%
  mutate(decade=cut(date, breaks=decade_breaks, labels=decade_labels))

inflation90s <- inflation %>% filter(decade=="1990s")

ggplot_inflation <- ggplot(data=inflation90s, aes(x=date)) +
  geom_line(aes(y=pceInfl), color="red") +
  geom_line(aes(y=cpiInfl), color="blue")

ggplotly(ggplot_inflation)

# or created from scratch in plotly()
plot_ly(x=inflation90s$cpiInfl, 
        y=inflation90s$pceInfl, 
        type = 'scatter', 
        mode = 'markers',
        text=inflation90s$date, 
        hoverinfo='text')

#------------------
# Using color to bring information on another data set

tradedata <- read.dta("https://www.dropbox.com/s/7cn5hdhd09hsbdi/trade_data_small.dta?dl=1")

# create GDP per capita
tradedata <- tradedata %>%
  mutate(gdppc_o=gdp_o/pop_o, gdppc_d=gdp_d/pop_d)

# subset the data
exports1 <- tradedata %>% 
  filter(year==2001, isoexp=="DEU" ) %>% 
  select(isoexp, isoimp, tradeflow, distw, gdp_d, pop_d, gdppc_d, rta)

# use color=VARIABLE to denote significance of another VARIABLE (same can be done with size=)
g <- ggplot(exports1, aes(x=(gdp_d), y=(tradeflow))) + 
  geom_point() +
  scale_x_log10() + scale_y_log10()
g
ggplotly(g)


ggplot(exports1, aes(x=(gdp_d), y=(tradeflow))) + 
  geom_point(aes(color=log10(distw))) + 
  geom_smooth(method = "lm", se = F) +
  scale_x_log10() + scale_y_log10()

ggplot(exports1, aes(x=(gdp_d), y=(tradeflow))) + 
  geom_point(aes(color=factor(rta))) + # What if we remove the factor() function
  geom_smooth(method = "lm", se = F) +
  scale_x_log10() + scale_y_log10() +
  labs(
    y='Export value',
    x='GDP of destination country',
    color="RTA status"
  )

ggplot(subset(exports1,!is.na(rta)), aes(x=(gdp_d), y=(tradeflow))) + # note the use of is.na()
  geom_point(aes(color=factor(rta))) + # What if we remove the factor() function
  geom_smooth(method = "lm", 
              aes(color=factor(rta)), se = F) +
  geom_smooth(method = "lm", se = F) +
  scale_x_log10() + scale_y_log10() +
  labs(
    y='Export value',
    x='GDP of destination country',
    color="RTA status"
  )

ggplot(subset(exports1,!is.na(rta)), aes(x=(gdp_d), y=(distw))) + 
  geom_point(aes(color=factor(rta))) + # What if we remove the factor() function
  geom_smooth(method = "lm", 
              aes(color=factor(rta)), se = F) +
  geom_smooth(method = "lm", se = F) +
  scale_x_log10() + scale_y_log10() +
  labs(
    y='Export value',
    x='Distance to the trading partner',
    color="RTA status"
  )

