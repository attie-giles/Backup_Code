#Clear the environment, optional
remove(list=ls())
#====================================================================================
# Module 1 : Data frames
#====================================================================================

# Install required packages

install.packages("wooldridge")
install.packages("foreign")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")

#====================================================================================
library(ggplot2) # used to create graphs
library(dplyr) # used to manipulate the data
library(wooldridge) # used to load data

# == Data frames 
# ---- Variables vs observations
# Load data sets from the Wooldridge package

data("wine")
?wine
ncol(wine) # columns are variables
nrow(wine) # rows are observations
names(wine) # names of the columns, names of the variables
  #this vector can also be used to rename variables
names(wine)[names(wine)=="LIVER"] <- "liver"

#Note that everything we learned about vectors applies to variables (because they are named vectors inside a data frame)
any(wine$liver<wine$heart) # is there any country that has higher rate of liver disease than heart disease?
all(wine$liver<wine$heart) # is it true for all countries

any(duplicated(wine$country)) # does any country occur more than once? In other words, does country identify observations

  # duplicated() returns TRUE in second and all subsequent instances
  duplicated(c(1,3,4,5,4,9,4))

# Let's load a few more data sets
?approval

data("econmath")
?econmath # general help command describes the data

data("countymurders") # this one is used in the homework 
?countymurders

# TRY THIS: Explore the data: open the data in the browser, how many observations are in each dataset, 
# how many variables, what are the types of variables are in the data.

# ======== Data Structures ========

# ---- Identifying variables

# Variables contain data. Is 4.99 data?

# Variables contain information. Some variables describe characteristics, others tell us where those characteristics belong.
# Those variables are identifying variables or the variables that describe the dimensions of the data

# The set of "identifying variables" identifies unique or not duplicated observations
# Q: What are the identifying variables in each data set we opened?

# Identifying observations are often categorical. The values of categorical variables contain information only about the category 
# or group: country, gender (categorical and binary). Categorical variables are usefully stored as factor variables.

# ---- Data structures

# Data types and identifying variables:
# - in time series data the identifying variables are time periods: year, day, month
# - in cross-sectional data the identifying variables are units/regions/individuals. These are often implied by the number of observation.
# - in panels or longitudinal data the identifying variables are time AND cross-sectional identifiers
# - in pooled panels the cross-sectional identifiers can differ by time period. For example, we can conduct an annual survey asking a different group of individuals every year.

# Q: Which variables are identifying in each data set?

# ...... Verifying that the identifying variables are unique and not duplicated

unique(wine$country) #list unique values

any(duplicated(wine$country)) #check if any observations are duplicates

table(wine$country) #we can also check how many observations per group or category

# Does each country occur exactly one time
all(
  table(
    wine$country
    )==1
  ) #note how the output of table() is a vector that can be manipulated

# TRY THIS: Is "countyid" the identifying variable in the "countrymurders" data?

# ...... Detecting duplicates and determining dimensions of data

# do years identify observations?
any(duplicated(countymurders$year)) #do years identify observations?
table(countymurders$year) # show number of observations per year.
  # Q: How would you check if every year has the same number of observations without looking at the table? Hints:
  all( table(countymurders$year) == mean(table(countymurders$year)))
  any( != )
  
range( # shows the range of years
  unique(countymurders$year)
  ) # what is the range of years in the data?

#How many years are in the data?
length(
  unique(
    countymurders$year
    )
  ) 

# Q: How to check if any year is missing?
length(unique(countymurders$year))  ==  max(countymurders$year) - min(countymurders$year) +1


# If it is not year then: Do states identify observations?
any(duplicated(countymurders$statefips))
table(countymurders$statefips)

# list of fips codes https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696 what is texas?


# Does the combination of year, state, and county identify observations?

any(
  duplicated(
    subset(countymurders, # subset(dataframe, select=c(varialbe1, variable2)) picks out variables
           select=c(year, statefips, countyfips)
    )
  )
)
# TRY THIS: Remove statefips above. Does the command produced the expected result? What does it mean?

# ======== Types of descriptive numerical variables ========

# ...... Categorical variables
# Does the state code have a meaning?

# ...... Continuous variables
# Which variables are continuous? How do you know?
# How do we think about integers?

# ======== Manipulating variables in a frame using base R ========

# ...... Creating a new variable
wine$hl <- wine$heart+wine$liver

# ...... Removing a variable
wine$hl.temp <- wine$heart+wine$liver
wine$hl.temp <- NULL

# ...... Creating a new variable the dplyr way mutate() verb
# ......................
# DPLYR offers a set of tools that make data manipulation easier
# dplyr is simplifying data work in a two important ways. First, it
# provides a set of functions it calls "verbs" and second it allows to "pipe" between those verbs.
# dplyr "piping" works by applying a sequence of functions (verbs) where the output of a 
# previous function (verb) is "piped" into the subsequent function (verb). This
# makes the syntax fairly intuitive to read.

# Please begin by reading the following introduction to the dplyr package from the creators:
#   https://dplyr.tidyverse.org/ 
# Take a look at the explanations of the dplyr "verbs"
#  https://dplyr.tidyverse.org/articles/dplyr.html
# Finally go over the following tutorial
#   https://www.sharpsightlabs.com/blog/dplyr-intro-data-manipulation-with-r/
# ......................

wine <- wine %>% mutate(hl=heart+liver)

# ...... Transforming a variable
wine <- wine %>% mutate(deaths = deaths/1000) #re-scaling a variable

# ...... Renaming a variable (again)
names(wine)[which(names(wine)=="hl")] <- "heartandliver"

wine <- wine %>% rename(hANDl=heartandliver)

# ...... Removing a variable
wine$hANDl <- NULL

# ---- Sorting a data frame
# ...... order() in base
order(econmath$age)
econmath[order(econmath$age),] #note, we need the comma in the square brackets to make sure we are indexing the rows
head(econmath[order(-econmath$age),])

# ...... arrange() verb in dplyr

econmath %>% arrange(age) %>% head
econmath %>% arrange(-age) %>% head

# ===== Subsetting data frames ==================

# ----- The subset() way 
# consumption of alcohol seems to be higher in some countries
qplot(data=wine,x=alcohol, geom="histogram")

# what are those countries
subset(wine,alcohol>5,select=c(country))

# when was the approval the highest
qplot(data=approval,x=approve, geom="histogram")
subset(approval,approve>85, select=c(month, year))
qplot(data=approval,y=approve, x=id, geom="point")

# ----- The dplyr way
wine %>% 
  filter(alcohol>5) %>% 
  arrange(-alcohol) %>% 
  select(country)

approval %>% 
  filter(approve>85) %>% 
  select(month, year)

approval %>% 
  select(month, year, approve) %>% 
  filter(approve==max(approve)) #when was the approval rate the highest?


# ===== Combining data frames =====

# The data can be combined two fundamentally different ways. First, we can stack
# two or more data frames on top of each other to combine observations. 
# For example, if we have two data 
# frames with GDP for two different time periods. In R jargon we call it "binding" 
# and use command like "bind_rows". In other words we combines rows or observations
# not the variables.

# Second, we can add variables (not rows or observations)
# from one data frame to another. For example, we have a trade data set and we 
# need to add information on GDP of the exporter. In R jargon we call it "joining"
# and use commands like "left_join" or "inner_join"

#Here is a simple illustration how these commands work
x1 <- data.frame(isoexp = c("USA", "GBR"), totalexports=c(24,7))
x1 # total exports of two countries
x2 <- data.frame(isoexp = c("EST", "UKR"), totalexports=c(2,3))
x2 # total exports of another two countries
x3 <- data.frame(isoexp = c("USA", "GBR", "EST", "MEX"), gdp=c(101,32,12,23)) 
x3 # GDP's of four countries, not necessarily the same ones as in the previous data

# Stack the observations, i.e. bind data frames
x_stacked <- bind_rows(x1, x2)
x_stacked
# Add the variables, i.e. join data frames
full_join(x_stacked, x3, by=c("isoexp"))

# Piping syntax is again very convenient
bind_rows(x1, x2) %>% left_join(x3)
bind_rows(x1, x2) %>% right_join(x3)
bind_rows(x1, x2) %>% inner_join(x3)
bind_rows(x1, x2) %>% full_join(x3)

# ===== Dealing with missing observations =====

# what if we need to change the NA's to zeros ( not a good idea here)

# NA is not a typical value. For example you can't do this
NA==NA
# you need to use a special command is.na() to evaluated a value, vector, or data frame
is.na(5)
is.na(NA)

# In the following example we will create a file that contains NA observations because the files that we joined do not have information on one of the variables
xNA <- bind_rows(x1, x2) %>% full_join(x3)
is.na(xNA) # this produces a matrix of the same size as the data frame with TRUE for NA values. This matrix can be used to index elements of the original data frame
xNA[is.na(xNA)] <- 0  
xNA
# Sometimes NA obsrevations do not mean that they are zeros and other times they are true zeros
xNA <- bind_rows(x1, x2) %>% full_join(x3)
xNA$totalexports[is.na(xNA$totalexports)] <- 0
xNA

# is.na() is.infinite() is.null()

# na.omit()

# ===== Loading data and pivoting the data ==============

library(foreign)
library(tidyr)
# install.packages("cli")
library(cli)

# In general read.___ () commands are used to input data into R
tradedata <- read.dta("https://www.dropbox.com/s/7cn5hdhd09hsbdi/trade_data_small.dta?dl=1") 

# Subsample of US exports to China, Canada, and Mexico from 2000 to 2004
sample <- subset(tradedata, 
                 isoexp=="USA" & 
                   isoimp %in% c("CAN", "MEX", "CHN") & 
                   between(year,2000,2004), 
                 select=c(isoexp, isoimp, year, tradeflow))
sample

# The data in sample are not easy to eyeball because for example if you need to compare trade with Mexico to trade with Canada in 2002 we will have to compare two distant rows. 
# A solution for improved data layout is to turn the table wider.

sample_wide <- pivot_wider(sample, 
                           names_from=year, 
                           values_from=tradeflow) # the names in the new wide table will come from the "year" column, the values in the columns with "year" will come from the tradeflow collumn
sample_wide

# we can also add a prefix to the column names to make them more readable
sample_wide <- pivot_wider(sample, 
                           names_from=year, 
                           names_glue="year_{year}", # the prefix for the year
                           values_from=tradeflow)
sample_wide

# this process can be reversed. this is especially useful if we try to transform the data that is created for visual consumption into the form that is useful for data analysis
sample_long <- pivot_longer(sample_wide, cols = starts_with("year_"), values_to="tradeflow", names_to="year", names_prefix="year_")
sample_long

# TRY THIS: Show imports of BRAZIL (BRA) from three of its neighbors in 2000-2004 
# where years are the rows and the names of the countries are the columns.
