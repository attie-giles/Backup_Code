#Clear the environemnt, optional
remove(list=ls())
#====================================================================================
# Module 2 : Loading and exploring data
#====================================================================================

# Install required packages

#install.packages("wooldridge")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
install.packages("comptradr")
install.packages("quantmod")
install.packages("WDI")
install.packages("zoo") # Makes working with time series easier
install.packages("lubridate") # Makes working with dates easier

#====================================================================================
# ===== Downloading several basic data sets 
library(quantmod) #https://www.quantmod.com/ describes the package and https://fred.stlouisfed.org/ contains information about the Federal Reserve data depository
library(WDI) # Please see https://cran.r-project.org/web/packages/WDI/WDI.pdf for documentation about the package and https://data.worldbank.org/indicator for general information about the data

library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)

#====================================================================================
# ----- Cross sectional trade data

# Load the entire "gravity" data set from BACI (change the path)
# The data came from "download" http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
# It can be downloaded directly as well.
Gravity_V202211 <- readRDS("~/Dropbox/_TEACH/PhDbootcamp/2024_bootcamp/Data/Gravity_V202211.rds")
# Check dimensions of the data, variable names

USexports <- Gravity_V202211 %>%  filter(year %in% 2015:2019) %>%
                                  filter(iso3_o=="USA") %>% 
                                  select(partner_iso=iso3_d, year, 
                                         exports=tradeflow_baci)
  
USimports <- Gravity_V202211 %>%  filter(year %in% 2015:2019) %>%
                                  filter(iso3_d=="USA") %>% 
                                  select(partner_iso=iso3_o, year, 
                                         imports=tradeflow_baci) 


# What are the dimensions of the data above?

# What should we do with the World as a partner?

# Join both data frames. Should this be the left, right, full, or inner?

UStrade <- full_join(USexports, USimports, by=c("partner_iso","year"))
# Why did I choose to do full_join()?

# Select a subset of observations for one year
UStrade.2018 <- subset(UStrade, 
                      year==2018)

# TRY THIS: Repeat this using the dplyr verb filter():


# Explore and describe the data:
# -- what are the names of the variables?
  names(UStrade.2018)
# -- how many ___ are there?
  ncol(UStrade.2018)
# -- how many ___ are there?
  nrow(UStrade.2018)
# -- are there missing values? Why do you think they exist? Does it make sense to turn them into zeros?
  any(
    is.na(UStrade.2018$imports))
  any(
    is.na(UStrade.2018$exports))
# 
  UStrade.2018 %>% filter( is.na(imports) )%>% select(partner)
  
# -- are there countries where the US imports from but does not export to?
  any((is.na(UStrade.2018$exports) & !is.na(UStrade.2018$imports)) | 
      (is.na(UStrade.2018$imports) & !is.na(UStrade.2018$imports)) )

# -- what are the identifying variables in UStrade.2018? are there duplicates?
  any(duplicated(UStrade.2018$partner_iso))
  any(duplicated(subset(UStrade.2018, select=c(partner_iso, year))))
 
# -- TRY THIS: repeat the same for UStrade, what are the identifying variables?
  any(duplicated(UStrade$partner_iso))
  any(duplicated(subset(UStrade, select=c(partner_iso, year))))

# Why do you think the above output happened?  
UStrade %>% filter(duplicated(subset(UStrade, select=c(partner_iso, year))))

# Above does not paint a full picture because it only identifies occurences after the first oen

UStrade.duplicates <- UStrade %>% group_by(partner_iso, year) %>%
            mutate(n=n()) %>%
            arrange(partner_iso, year) %>%
            filter(n>1)
# What would happen if we used summarize (n=n())  replace and explain

table(UStrade.duplicates$n)

# How would you proceed with the data?

UStrade <- UStrade %>% group_by(partner_iso, year) %>%
  mutate(n=n(), .groups="drop") %>%
  filter(n==1)
  

# -- Some exploratory data calculations
  
# Calculate a share of each partner in US imports and arrange the data frame in ascending order of the import share
UStrade.2018 <- UStrade.2018 %>% 
  mutate(
    imp_share=round(imports/sum(imports)*100, digits=2)) %>%
  arrange(-imp_share) 

head(UStrade.2018, n=10)

# Q: What do you think is wrong with the code above? Hint: sum() operates on numbers.

# After we fix it, what do we need to do to make the data frame make sense?

# Produce the table of US imports from the top 10 partners
UStrade.2018 %>% 
  mutate(imp_share = round(imports/sum(imports, na.rm=TRUE)*100 , digits=1)) %>%
  filter(rank(-imp_share)<=10) %>% #Make sure to understand what "rank" does
  select(partner_iso, imp_share) %>% 
  arrange(-imp_share)

UStrade.2018 %>% 
  mutate(imp_share = round(imports/sum(imports, na.rm=TRUE)*100 , digits=10)) %>%
  filter(rank(imp_share)<=10) %>% #Make sure to understand what "rank" does
  select(partner_iso, imp_share) %>% 
  arrange(-imp_share)


# TRY THIS: Produce the table of US exports from the top 10 partners


# Calculate concentration of the imports vs exports? Which one is more concentrated? How sensitive is the comparison to inclusion of the very small coutries?
UStrade.2018 %>% 
  summarise(sum( (imports/sum(imports, na.rm=TRUE) * 100)^2, na.rm=TRUE), 
            sum( (exports/sum(exports)*100)^2 )
            )
# What is wrong with the output above? Hint: Does na.rm=TRUE fix the problem with the missing variable?

# Another measure of concentration is the share of top 4 countries. How would you go about calculating it?

# ---- A powerful function of the dplyr is the ability to perform calculations for subset of the data

# The verb group_by(year) breaks the data into the separate groups for each year
# For an example let's perform the familiar calculation and display top 3 sources 
# of imports for each year (not just for 2018):
UStrade %>% 
  group_by(year) %>% # <- instructs R to break up the data for execution by group 
  mutate(imp_share = round(imports/sum(imports , na.rm=T)*100 , digits=1)) %>%
  filter(rank(-imp_share)<=3)

# Show a table with countries, their import shares up to some cumulative threshold for each year
table <- UStrade %>% 
  mutate(imports=replace(imports,is.na(imports),0)) %>% # replace imports when imports are missing with 0
  group_by(year) %>% # break up the data frame in years for all subsequent commands
  mutate(imp_share = round(imports/sum(imports)*100 , digits=1)) %>% # create the import share in percentages rounded to 1 digit
  arrange(year, -imp_share) %>% # sort the data, this is important because cumsum() adds up values from the top to the bottom of the data frame
  mutate(sum.imp_share=cumsum(imp_share)) %>% # calculate cumulative share 
  filter(sum.imp_share<70) %>% # filter in only the top origins of imports up to some cumulative threshold
  select(year, partner_iso, imp_share) %>% # select to keep only variables that needed for the table
  pivot_wider(names_from=year, values_from=imp_share) # "un-stack" the table for presentation
table

# Let's do the same for the rank
table <- UStrade %>% group_by(year) %>%
  mutate(imports=replace(imports,is.na(imports),0)) %>%
  mutate(imp_share = round(imports/sum(imports , na.rm=T)*100 , digits=1)) %>%
  mutate(rank.imp_share = rank(-imp_share)) %>%
  arrange(year, -imp_share) %>%
  mutate(sum.imp_share=cumsum(imp_share)) %>% 
  filter(sum.imp_share<70) %>%
  select(year, partner_iso, rank.imp_share) %>%
  pivot_wider(names_from=year, values_from=c(rank.imp_share)) # what happens if you remove rank.imp_share?
table

# For the homework I am asking you to swap the rank (names of rows) and partner_iso (content of the year columns)

#====================================================================================
# ---- Time series for the US

# Download time series of inflation based on the CPI and on PCE
getSymbols('CPIAUCNS',src='FRED') #
getSymbols('PCEPI',src='FRED') 
df1 <- data.frame(PCEPI, date=index(PCEPI))
df2 <- data.frame(CPIAUCNS, date=index(CPIAUCNS))

# Combine two data frames using date
inflation <- full_join(df1, df2, by="date") #what would inner_join() do?

# Calculate inflation rates (data frame needs to be sorted by date for lag())
inflation$pceInfl <- round(
  (inflation$PCEPI-lag(inflation$PCEPI))/(lag(inflation$PCEPI))*100, 
  digits=2)
inflation$cpiInfl <- round(
  (inflation$CPIAUCNS-lag(inflation$CPIAUCNS))/(lag(inflation$CPIAUCNS))*100, 
  digits=2)

# Why is the inflation rate not available for the first period?

# Plot the time series of both
qplot(data=inflation,y=cpiInfl,x=date,geom="line") # something is wrong
qplot(data=inflation,y=pceInfl,x=date,geom="line")

# TRY THIS: Identify the issue with the data see how to fix it:

# Show the date of the problem observation:
inflation$date[which(inflation$cpiInfl < -75)]
inflation %>% arrange(date) %>% head


# Are there are missing values for either of the inflation measures?
any(is.na(inflation$cpiInfl))
# How many are missing?
sum(is.na(inflation$cpiInfl))

# TRY THIS: Do the same for the other measure of inflation

# Remove entire observations(rows) with missing observations
inflation <- inflation %>% filter(!is.na(cpiInfl)) 
  # How is the presence of the NA observations related to the full_join() vs inner_join()?

# Filter data for recent recession based on dates
inflation[inflation$date>"2020-02-01",]
inflation %>% filter(date>"2020-02-01")

# How many periods of data do we have following the beginning of the COVID recession?
nrow(inflation)

#--------------------------------------------------
# Selecting time period between two dates
# How would you select a time period between two dates? Use between() and specify the number of months

 inflation %>% 
                filter(between(date, 
                               as.Date("2020-02-01"), 
                               as.Date("2020-04-01") )
                       )   

# You can also get fancier and add 5 months to the start using library(libridate) functions dmonths(5) and floor_date(,"months")
 inflation %>% 
   filter( (date>=as.Date("2020-02-01") & 
            date <= floor_date(as.Date("2020-01-01")+dmonths(5), unit="month")) |
           (date>=as.Date("2021-02-01") & 
            date <= floor_date(as.Date("2021-01-01")+dmonths(5), unit="month"))
   ) 
  
# To see what floor_date( , "month") does compare the following two
floor_date( as.Date("2020-02-01") + dmonths(5), "month")
            as.Date("2020-02-01") + dmonths(5)


#--------------------------------------------------
# Creating categorical variable to denote intervals

# Create a variable "period" that takes different values for different periods use function cut() # Create a variable "recession" in a similar fashion for your homework
inflation <- inflation %>% 
              mutate(
                recession=cut(date,
                              breaks=c(as.Date("2020-04-01"),
                                       as.Date("2020-06-01")), 
                              labels=c("Period 1"))) #Note that 2 cutoff points create one interval. The number of elements passed to labels= needs to match the number of intervals.

# You can have more than one interval
inflation <- inflation %>% 
                mutate(period=cut(date,
                                  breaks=c(   as.Date("2020-02-01"),
                                              as.Date("2020-04-01"),
                                              as.Date("2020-06-01"),
                                              as.Date("2020-08-01")), 
                                  labels=c(   "First 2 months", 
                                              "Second 2 months", 
                                              "Third 2 months")))
  # Note: the value of the lower cutoff is included in the next interval. This can be changed in options

# If your data has equal numbers of observations for each of the periods (as it should because the homeowork asks for 6 months from the start of each recession) then 
# you can specify the number of breaks instead of the cutoff points. breaks=3 means that the vector is broken into 3 equal parts. Note the result is the same.
inflation <- inflation %>% 
  mutate(period=cut(
    1:n(),
    breaks=3, 
    labels=c("First 2 months", "Second 2 months", "Third 2 months")))
# 1:n() is a vector of sequential integers from 1 to the number of observations n()


# Calculate averages for each measure (this is useful for the homework, 
# all you will need to do is to add the appropriate group_by() to calculate the averages for each of the periods)
inflation %>% summarize(mean(cpiInfl), mean(pceInfl))
inflation %>% summarize(mean(cpiInfl, na.rm=T),mean(pceInfl, na.rm=T))

# Calculate averages for each period (just like what I am asking for the homework)
inflation %>% group_by(period) %>% summarize(mean(cpiInfl),mean(pceInfl))

# --- Some additional typical but advanced calculations

# Calculate 3 months rolling average
rollapply(inflation$pceInfl,3,mean) 
#rolapply() is a very useful command that allows to calculate rolling averages
numbers<-sample(1:10)
numbers
rollapply(numbers,3,mean)

# Why can't we create a variable for rolling average and store it in the original data frame?
inflation$pceInfl.3m<-rollapply(inflation$pceInfl,3,mean)
# What is a solution?
inflation$n<-c(1:nrow(inflation))
# ifelse() is a useful function ifelse(condition,value if true, value if false)
ifelse(inflation$n<3,NA,rollapply(inflation$pceInfl,3,mean))
# Now the assignment of the new variable works
inflation$pceInfl.3m <- ifelse(inflation$n<3,NA,rollapply(inflation$pceInfl,3,mean))

# Now calculate the rolling mean in a way that we have values for the entire period of the recession without missing months.

#====================================================================================
# ---- Panel data for multiple countries and multiple years

# Download data from the WDI
WDIraw <- WDI(country = "all", 
              indicator =c(taxrevenue2GDP="GC.TAX.TOTL.GD.ZS", 
                           rGDPpc="NY.GDP.PCAP.PP.KD"), 
              start = 2000, end = 2019, extra = TRUE, cache = NULL)

# TRY THIS: Explore the data. Check dimensions. Which observations are missing? What are the units of the variables?

# Filter out aggregates
WDIdata <- WDIraw[WDIraw$region!="Aggregates",]

# How many observations have missing values for each variable?
mean(is.na(WDIdata$taxrevenue2GDP)) # Calculates percentage

# TRY THIS: How many observations are missing for the other variable?

# Select out variables that are not very useful
WDIdata <- subset(WDIdata, select=-c(longitude,latitude,income, lending, region, iso2c))

# TRY THIS: How many observations do we have for each year?
table()


# Count missing observations by country ("by country" = "for each country" = "one country at a time")
WDIdata %>% group_by(country) %>% 
  summarize(nmissing=sum(is.na(taxrevenue2GDP))) %>%
  arrange(-nmissing)

# TRY THIS: Modify the code above to see if any observations have zero or few missing observations.

# TRY THIS: How would you create a file that has only countries and their capitals? Use the unique() function 
WDIdata %>% select(country, capital) %>% unique

# Calculate growth in rGDPpc for each country (recall that the data needs to be in the right order to use lag())
WDIdata <- WDIdata %>% 
  arrange(country, year) %>% # this step is very-very important for lag() because lag() uses previous obsrvation in order
  group_by(country)  %>% 
  mutate(rGDPpc.growth=((rGDPpc-lag(rGDPpc))/lag(rGDPpc)))

# Calculate the average growth rate by country
avegrowth <- WDIdata %>% 
              group_by(country) %>% 
              summarize ( ave.growth = mean(rGDPpc.growth, na.rm=TRUE),   #note the buse of na.rm=TRUE, why wouldn't we just drop observations with missing values?
                          ave.tax = mean(taxrevenue2GDP, na.rm=TRUE) , 
                          ave.rGDPpc=mean(rGDPpc), na.rm=TRUE) 

# Define income quintile using only observations with the non-missing values for the GDP per capita
avegrowth <- avegrowth %>% filter(!is.na(ave.rGDPpc)) %>% 
  mutate(inc.quintile=ceiling(rank(ave.rGDPpc)/(n()/5)))
# How would you do the same using cut() function?
table(avegrowth$inc.quintile)

# Present average tax revenue by income quintile
avegrowth %>% group_by(inc.quintile) %>% summarize(mean(ave.tax, na.rm=TRUE))

