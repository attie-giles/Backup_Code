#Clear the environment, optional
remove(list=ls())
#====================================================================================
# Module 3 : Basics of visualization
#====================================================================================

# In this script we learn about:
# - Basic syntax of ggplot 
# - Types of graphs or geoms (point, line, path, histogram, density)
# - Labeling with text
# - Lines of best fit with geom_smooth
# - The role of aesthetics, inheriting of attributes, overlaying
# - Use of color, size, shape
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

#====================================================================================
# ===== Downloading several basic data sets 

library(quantmod) #https://www.quantmod.com/ describes the package and https://fred.stlouisfed.org/ contains information about the Federal Reserve data depository
library(WDI) # Please see https://cran.r-project.org/web/packages/WDI/WDI.pdf for documentation about the package and https://data.worldbank.org/indicator for general information about the data
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2) # https://www.r-graph-gallery.com/index.html for graph examples



#====================================================================================
# ---- Time series for the US

# ----
# Download time series of inflation based on the CPI and on PCE
getSymbols('CPIAUCNS',src='FRED') #
getSymbols('PCEPI',src='FRED') 

start_date = as.Date("1980-01-01")
decade_breaks = floor_date(c(start_date, start_date+dyears(10), start_date+dyears(20), start_date+dyears(30)), "month")
decade_labels = c("1980s", "1990s", "2000s")

inflation <- data.frame(PCEPI, date=index(PCEPI)) %>% 
  inner_join(data.frame(CPIAUCNS, date=index(CPIAUCNS)), by="date") %>%
  filter(date>as.Date(start_date)) %>%
  mutate(pceInfl = round((PCEPI-lag(PCEPI))/(lag(PCEPI))*100, digits=2), 
         cpiInfl = round((CPIAUCNS-lag(CPIAUCNS))/(lag(CPIAUCNS))*100, digits=2)) %>%
  mutate(decade=cut(date, breaks=decade_breaks, labels=decade_labels))
# Why are there missing observations for the decade? How would you fix it?

# Do we have the same number of observations per decade? How would you fix it?
table(inflation$decade)

# Extract one decade
inflation90s <- inflation %>% filter(decade=="1990s")

# ----
# Elements of a typical ggplot() graph

# The basic graphs need
# - ggplot call
# - data frame to use
# - variabls to plot
# - type of graph or the geom_

# ----
# Start by describing a distribution of one variable

# Consider the distribution of inflation rates. Identify elements. 
ggplot() +
  geom_histogram(data=inflation90s, aes(x=pceInfl)) +
  labs(x="PCE inflation, %", y = "Number of months")

# The elements are passed on or inherited from the ggplot(). Consider these two calls producing the same histogram
ggplot(data=inflation90s) +
  geom_histogram(aes(x=pceInfl))+
  labs(x="PCE inflation, %", y = "Number of months")

ggplot(data=inflation90s, aes(x=pceInfl)) +
  geom_histogram()+
  labs(x="PCE inflation, %", y = "Number of months")

# ----
# Layers

# ggplot() builds graphs in layers that can be combined. Consider adding a vertical line at the average
ggplot(data=inflation90s, aes(x=pceInfl)) +
  geom_histogram() + 
  geom_vline(xintercept=mean(inflation90s$pceInfl))+
  labs(x="PCE inflation, %")

# Note what happens when we change the order       
ggplot(data=inflation90s, aes(x=pceInfl)) +
  geom_vline(xintercept=mean(inflation90s$pceInfl)) +
  geom_histogram()+
  labs(x="PCE inflation, %")

# The same as above can be done sequentially
g <- ggplot(data=inflation90s, aes(x=pceInfl)) +
  geom_histogram() # graph with just the histogram
g
g <- g + geom_vline(xintercept=mean(inflation90s$pceInfl)) # adds the vertical line
g

# ----
# Color

# The color can be adjusted       
ggplot(data=inflation90s, aes(x=pceInfl)) +
  geom_vline(xintercept=mean(inflation90s$pceInfl)) +
  geom_histogram(color="maroon", fill="maroon")

# The transparency can be adjusted as well with alpha=
ggplot(data=inflation90s, aes(x=pceInfl)) +
  geom_vline(xintercept=mean(inflation90s$pceInfl)) +
  geom_histogram(color="maroon", fill="maroon", alpha=.50)
# Change the color and the transparency
# Take a look at color names: https://www.r-graph-gallery.com/42-colors-names.html 

# The color can also be used to differentiate between groups if it is inside the aes()
ggplot(data=inflation, aes(x=pceInfl)) +
  geom_histogram(aes(color=decade)) # NOTE the color is inside the aes()

# for histograms fill= is better than color=
ggplot(data=inflation, aes(x=pceInfl)) +
  geom_histogram(aes(fill=decade))


# The color can also be missing or "transparent"
ggplot(data=inflation, aes(x=pceInfl)) +
  geom_histogram(aes(color=decade), fill="transparent")

# Histogram might be less visually appealing than the density plot
ggplot(data=inflation90s, aes(x=pceInfl)) +
  geom_histogram() +
  geom_density() # the plots can be layered 

# The axis of counts and density are not the same
ggplot(data=inflation90s, aes(x=pceInfl)) +
  geom_histogram(aes(y =..density..)) +
  geom_density()

# We can use the density calls for the decades
ggplot(data=inflation, aes(x=pceInfl)) +
  geom_density(aes(color=decade))

# To make the density plots smoother the adjust= can be adjusted
ggplot(data=subset(inflation, decade!="NA"), 
       aes(x=pceInfl)) +
  geom_density(aes(color=decade), adjust=2) +
  labs(
    x="Inflation, monthly change in PCE",
    y="",
    color="Decade",
    title="Distribution of inflation rates over three decades"
  )

# What conclusion about the means and standard deviations of inflation rates would you make from this graph? Confirm it with a table by completing the following command
inflation %>% group_by(decade) %>% summarize( mean(pceInfl, na.rm=T), sd(pceInfl, na.rm=T))

# See this tutorial for more steps and options https://www.datacamp.com/community/tutorials/make-histogram-ggplot2

# ----
# Consider graphs that show information about a relationship between two variables

# ----
# Download data from the WDI
WDIdata <- WDI(country = "all", indicator =c(taxrevenue2GDP="GC.TAX.TOTL.GD.ZS", 
                                         rGDPpc="NY.GDP.PCAP.PP.KD",
                                         rGDP="NY.GDP.MKTP.PP.KD"), 
           start = 2000, end = 2019, extra = TRUE, cache = NULL) %>%
  filter(region!="Aggregates") %>% 
  select(-longitude, -latitude, -income, -lending, -region, -iso2c) %>%
  group_by(country, iso3c) %>%
  summarize( ave.tax = mean(taxrevenue2GDP, na.rm=TRUE) , 
             ave.rGDPpc=mean(rGDPpc, na.rm=TRUE),
             ave.rGDP=mean(rGDP, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(income.group=cut(rank(-ave.rGDPpc),
                          5, 
                          labels=c("1st","2nd","3rd","4th","5th")))

# Scatter plot
ggplot(data=WDIdata, aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() 

ggplot(data=WDIdata, aes(x=ave.rGDPpc)) +
  geom_density() 


# Rescaling axes may help make the graph better
ggplot(data=WDIdata, aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() +
  scale_x_log10()

# An outlier can be removed by subsetting the data
ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() +
  scale_x_log10()

# We can also add the best fit line "lm" stands for "linear model"
ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_x_log10()

# We can also explore a more flexible relationship by omitting the method="lm". What are other method= options?
ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() +
  geom_smooth( se=FALSE) +
  scale_x_log10()

# Set the se=TRUE to see the effect

# ---- Color, shape, alpha, and size

# The appearance of every geom can be changed

ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point( size=2.5,
              shape="square",
              color="maroon",
              alpha=.5) +
  geom_smooth(method="lm", se=FALSE, 
              size=.5, 
              linetype="dashed",
              color="navy") +
  scale_x_log10()

# The attributes inside the aes() can be adjusted by value
ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point(aes(size=ave.rGDP), # note the use of size=
             alpha=.9,
             shape="square",
             color="maroon") +
  geom_smooth( se=FALSE) +
  scale_x_log10() +
  labs(x="Real GDP per capita, mln USD",
       y="Tax to GDP, %",
       title="GDP per capita and tax burden",
       size="Real GDP")


ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point(aes(color=log10(ave.rGDP))) +
  geom_smooth( se=FALSE) +
  scale_x_log10() +
  labs(x="Real GDP per capita, log10",
       y="Tax to GDP, %",
       title="GDP per capita and tax burden",
       color="Real GDP")


# ---- Labeling using the "text" geom

ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_text(aes(label=iso3c), hjust=0, vjust=0, color="salmon") +
  scale_x_log10()

# The labels can be added to the scatter
ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() +
  geom_text(aes(label=iso3c), hjust=0, vjust=0) +
  scale_x_log10()

# The labels and points can also be added selectively 
ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() +
  geom_point(data=subset( WDIdata, ave.tax<60 & ave.tax>30), color="red") +
  geom_text(data=subset( WDIdata, ave.tax<60 & ave.tax>30), aes(label=iso3c), hjust=0.5, vjust=-0.5, color="red") +
  scale_x_log10()

# Labels
ggplot(data=subset(WDIdata, ave.tax<60), aes(x=ave.rGDPpc, y=ave.tax)) +
  geom_point() +
  geom_point(data=subset( WDIdata, ave.tax<60 & ave.tax>30), color="red") +
  geom_text(data=subset( WDIdata, ave.tax<60 & ave.tax>30), aes(label=iso3c), hjust=0.5, vjust=-0.5, color="red") +
  scale_x_log10() +
  labs(x="PCE inflation",
       y="CPI inflation",
       title="Path diagram of the PCE and CPI inflations")


# ----
# Two graphs on the same plot

# To show two time series on the same plot
ggplot(data=inflation, aes(x=date)) +
  geom_point(aes(y=pceInfl), color="red") +
  geom_point(aes(y=cpiInfl), color="blue")

# The line geom might work better
ggplot(data=inflation, aes(x=date)) +
  geom_line(aes(y=pceInfl), color="red") +
  geom_line(aes(y=cpiInfl), color="blue")

# Limit the data
ggplot(data=subset(inflation, date>as.Date("2010-01-01")), aes(x=date)) +
  geom_line(aes(y=pceInfl), color="red") +
  geom_line(aes(y=cpiInfl), color="blue")

# ----
# The path of co-movememnt between two time series

# For the relationship between time series there is information in the path of change

inflation %>% group_by(year(date)) %>%
  mutate(ave.pceInfl=mean(pceInfl, na.rm=TRUE), ave.cpiInfl=mean(cpiInfl, na.rm=TRUE)) %>%
  ggplot(aes(x=ave.pceInfl, y=ave.cpiInfl)) +
  geom_point() # points conceal information about the order

inflation %>% group_by(year(date)) %>%
  mutate(ave.pceInfl=mean(pceInfl, na.rm=TRUE), ave.cpiInfl=mean(cpiInfl, na.rm=TRUE)) %>%
  ggplot(aes(x=ave.pceInfl, y=ave.cpiInfl)) +
  geom_line() # line creates a false impression of order

inflation %>% group_by(year(date)) %>%
  mutate(ave.pceInfl=mean(pceInfl, na.rm=TRUE), ave.cpiInfl=mean(cpiInfl, na.rm=TRUE)) %>%
  ggplot(aes(x=ave.pceInfl, y=ave.cpiInfl)) +
  geom_path() # path recreates the order

#Homework hint:
inflation %>% group_by(year(date)) %>%
  filter(decade!="1980s") %>%
  mutate(ave.pceInfl=mean(pceInfl, na.rm=TRUE), ave.cpiInfl=mean(cpiInfl, na.rm=TRUE)) %>%
  ggplot(aes(x=ave.pceInfl, y=ave.cpiInfl)) +
  geom_path(aes(color=decade)) # Color can be used to separate the decades

# Selectively marking observations is done similarly. You will need to create a data frame so you can subset it later.
plot_data <-  inflation %>% group_by(year(date), decade) %>%
  filter(decade!="1980s") %>%
  summarise(ave.pceInfl=mean(pceInfl, na.rm=TRUE), 
         ave.cpiInfl=mean(cpiInfl, na.rm=TRUE))
ggplot(data=plot_data, aes(x=ave.pceInfl, y=ave.cpiInfl)) +
  geom_path(aes(color=decade)) +
  geom_text(data=subset(plot_data, ave.cpiInfl>.4 | ave.pceInfl<.05), 
            aes(label=`year(date)`))


# For the homework you might want to create a separate variable for the year and month using something like:
# mutate(year_month=paste(year(inflation$date),"/", month(inflation$date), sep="") )

# For the homework, to mark the first or last month of some group you can use something like this:
inflation %>% arrange(decade, date) %>% mutate(first=1:n()==1)

# ---- DO NOT RUN --- random piece below to show how to label different geoms on the same graph
ggplot(wage1, aes(x=educ)) +
  geom_jitter(aes(y=lwage), color="gray") +
  geom_line(aes(y=predict(lm.linear), color="Linear")) +
  geom_line(aes(y=predict(lm.level), color="Four levels")) +
  geom_line(aes(y=predict(lm.factor), color="All levels"))+
  geom_point(aes(x=12,y=1.56), color="red", shape=1, size=10)+
  labs(x="Years of education",
       y="Wage, log",
       title="Three different functional forms of the effect of education on wage") +
  scale_color_manual(name = "Functional form", 
                     values = c("Linear" = "black", 
                                "Four levels" = "maroon",
                                "All levels"="darkgreen")) 



