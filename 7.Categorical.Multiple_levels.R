# -------------------------------------------------------------------------
# Categorical variables with multiple levels
# -------------------------------------------------------------------------
#
# - Setup of the model
# - Dummy variable trap (either constant or all dummies, colinearity)
#   - omitting constant invalidates R-squared
# - Interpretation
#   - expected values (means in case of SLR, or h.o.t.c. for MLR)
#   - differences in expected values
# - Factor variables implementation
#   - test for joint significance
# - Partialling out equivalence to mean differencing
# - Turning continuous variables into categorical (what is it? Advantages and disadvantages?)

remove(list=ls())

#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
# install.packages("stargazer")
library(stargazer)
# install.packages("car")
library(car)

# -----------------
# Categorical variable with multiple levels

data(wage1)
?wage1
# Which variables define region?
# Which region is missing? Let's recreate the missing one (you will see why we need it)
wage1 %>% mutate(east=1-(northcen+south+west)) %>% summarize(mean(northcen+south+west+east))

# Create a dummy for "east"
wage1 <- wage1%>%
  mutate(east=1-(northcen+south+west))

# -----------------
# Interpretation

# How is wage different by regions?

# Why "east" is automatically excluded? 
lm.noeast <- lm(lwage ~ northcen + south + west        , wage1)
lm.all <- lm(lwage ~ northcen + south + west + east , wage1)
lm.noconstant <- lm(lwage ~ 0 + northcen + south + west + east , wage1) # Force no constant and overall significance test
lm.nosouth <- lm(lwage ~ northcen + west + east , wage1)
stargazer(
    lm.noeast,
    lm.all,
    lm.noconstant,
    lm.nosouth,
    type="text"
)

# What is the difference in lwage between "south" and "east"?

# What is the difference in lwage between "east" and "west"?

# What is the expected value of lwage for "northcen"?

# -----------------
# Factor variables

# Create one factor variable that contains regions
wage1 <- wage1%>%
  mutate(region=case_when( #Note the use of case_when() to code variables
    northcen==1 ~ "northcen",
    south   ==1 ~ "south",
    west    ==1 ~ "west",
    east    ==1 ~ "east"
  ))
# Note qualitative information often comes in the form of a single variable with 
# multiple levels. Like the "region" variable that takes on 4 values.

# Check the values of the "region" variable
table(wage1$region)

# factor(region) does the same as a set of dummies for all regions

# Compare the two columns of the following table:
lm.dummies <- lm(lwage ~ northcen + south + west, wage1)
lm.factors    <- lm(lwage ~ factor(region)         , wage1)

stargazer(
  lm.dummies,
  lm.factors,
  type="text"
)

# -----------------
# Changing the base level

# One of the categories is always designated as a "base" level to avoid the 
# collinearity in the presense of constant. That level is arbitrary and can be
# changed. Let's select "south" region as the base level

# relevel( , ref = )

lm.base.east  <- lm(lwage ~ factor(region) , wage1)
lm.base.south <- lm(lwage ~ relevel(factor(region), ref="south") , wage1)
lm.base.west  <- lm(lwage ~ relevel(factor(region), ref="west") , wage1)

stargazer(
  lm.base.east, 
  lm.base.south,
  lm.base.west,
  type="text"
)

# What is the difference in lwage between "south" and "east" based on the table above?
# What is the difference in lwage between "east" and "west"?

# Compare your answers to the table where "east was the base category.

# -----------------
# Visualizing the difference between regions

ggplot(wage1, aes(x=lwage)) +
  geom_density(color="gray", adjust=1.5, fill="gray", alpha=.5) +
  geom_density(aes(color=region), adjust=1.5) +
  labs(x="Wage, log",
       y="",
       title="Distribution by region and overall (gray) distribution")

# -----------------
# Partialling out of a multiple level categorical variable

# Partialling out of the differences across groups is important in the presence
# of additional explanatory variables.

# Partialling out is equivalent to mean-differencing with respect to 
# means for each level of categorical variable. The reason it works is
#
# because E(y| group = j) = mean(y | group = j) = constant + dummy(group=j)
#               ^                  ^                     ^
#           theoretical          sample                model               
#
# So the residual is r_hat = y-mean(y | group=j)
#
# this can be applied to all y and x variables.


# Is effect of education on lwage different across regions?

mdiff <- function(x) { #function for mean-differencing
  result <- x-mean(x)
}

# Apply mean differencing to all variables separately for each region
wage1.md <- wage1 %>% 
  select(region, lwage, educ, female, exper) %>% 
  group_by(region) %>%
  mutate(across(where(is.numeric),      # condition for application of a function to columns
                mdiff,                  # function to apply, can be a list or a standard function like sum
                .names = "{.col}.md"))  # how to name new variables

# Compare the effect of education with factor() and .md
lm.pooled <-  lm(lwage    ~ educ   , 
                      data=wage1.md)
lm.factors<-  lm(lwage    ~ educ    + factor(region),
                      data=wage1.md)
lm.md     <-  lm(lwage.md ~ educ.md,   
                      data=wage1.md)


stargazer(lm.pooled, lm.factors, lm.md,
  type="text"
)


# Visualizing regional differences in the relationship
ggplot(wage1.md, aes(x=educ, y=lwage, color=region))+
  geom_jitter() +
  geom_smooth(method="lm",se=F, alpha=.25, size=.25 )

# Display the effect of mean-differencing
ggplot(wage1.md )+
  geom_smooth(aes(x=educ, y=lwage), method="lm",se=F, color="gray" ) +
  geom_jitter(aes(x=educ, y=lwage, color=region)) +
  geom_line(aes(x=educ, y=predict(lm.factors), color=region)) 
  
# -----------------
# Testing for joint significance of a categorical set of dummies.

linearHypothesis( lm.factors,               # model to be tested
                  matchCoefs(lm.factors,    # model to get coefficient names (same in this case)
                             "region"))    # word to match


# -----------------
# Partialling out can apply to many variables

lm.pooled <- lm(lwage    ~ educ    + female +    exper
   , data=wage1.md)
lm.factors<- lm(lwage    ~ educ    + female +    exper + factor(region)
   , data=wage1.md)
lm.md <- lm(lwage.md ~ educ.md + female.md + exper.md
   , data=wage1.md)

stargazer(lm.pooled, lm.factors, lm.md,
  type="text"
)


# ------------------------------------------------------------
# Repeat the example for occupations

# Create a factor
wage1 <- wage1%>%
  mutate(occupation=case_when( 
    profocc==1 ~ "prof",
    clerocc==1 ~ "cler",
    servocc==1 ~ "serv",
    TRUE       ~ "manu"    #missing occupation, manual labor
  ))

table(wage1$occupation)

# Mean difference
wage1.md <- wage1 %>% 
  select(occupation, lwage, educ, female, exper) %>% 
  group_by(occupation) %>%
  mutate(across(where(is.numeric),      # condition for application of a function to columns
                mdiff,                  # function to apply, can be a list or a standard function like sum
                .names = "{.col}.md"))  # how to name new variables

# Compare estimation result
lm.pooled <- lm(lwage    ~ educ    + female +    exper
   , data=wage1.md)
lm.factors<-  lm(lwage    ~ educ    + female +    exper + factor(occupation)
     , data=wage1.md)
lm.md     <- lm(lwage.md ~ educ.md + female.md + exper.md
   , data=wage1.md)
stargazer(lm.pooled, lm.factors, lm.md,
  type="text"
)

# Which occupation is the "base"?
# Rank occupations in terms of expected lwage holding other things constant (this is 
# not the same as ranking the occupations in terms of expected lwage). Is it the same?
stargazer(
  lm(lwage~ factor(occupation), data=wage1.md),
  type="text"
)
stargazer(
  lm(exper~ factor(occupation), data=wage1.md)
)
# Visualizing the difference across occupations
ggplot(wage1, aes(x=lwage)) +
  geom_density(color="gray", adjust=1.5, fill="gray") +
  geom_density(aes(color=occupation), adjust=1.5) +
  labs(x="Wage, log",
       y="",
       title="Distribution by occupation and overall (gray) distribution")

# Display the effect of mean-differencing. It averages the slopes
lm.educ.factors <-  lm(lwage    ~ educ   + factor(occupation), data=wage1)
ggplot(wage1 )+
  geom_smooth(aes(x=educ, y=lwage), method="lm",se=F, color="gray" ) +
  geom_jitter(aes(x=educ, y=lwage, color=occupation), alpha=.5) +
  geom_line(aes(x=educ, y=predict(lm.educ.factors), color=occupation)) 

# Can you think of the bias as an omitted variable bias?

# Visualizing regional differences in the relationship
ggplot(wage1.md, aes(x=educ, y=lwage, color=occupation))+
  geom_jitter() +
  geom_smooth(method="lm",se=F, alpha=.25 )
# This is often a good first glance at the data.
# PREVIEW: What is the advantage of estimating an interaction model instead of actually
# estimating a different model for each group?


# Test for joint significance
linearHypothesis( lm.factors,               # model to be tested
                  matchCoefs(lm.factors,    # model to get coefficient names (same in this case)
                             "occupation")) # word to match
linearHypothesis( lm.factors,  matchCoefs(lm.factors, "occupation"))

# -----------------
# Categorical variables can be coded to represent continuous information
#


# Turning continuous variable into a factor has advantages and disadvantages
#
# - it allows for a more flexible (non-parametric) functional form
# - it makes interpretation more difficult
# - it costs degrees of freedom
# - some levels might be too close to each other to detect differences 
#


# Turn education into education levels
wage1 <- wage1%>%
  mutate(educlevel=case_when( 
    educ< 12 ~ "1.Less than HS",
    educ==12 ~ "2.HS",
    educ<=15 ~ "3.Some college",
    educ<=16 ~ "4.College",
    TRUE     ~ "5.Above college"   
  ))

table(wage1$educlevel)

# Compare estimates
lm.linear  <- lm(lwage ~ educ, wage1)
lm.level  <- lm(lwage ~ factor(educlevel), wage1)
lm.factor <- lm(lwage ~ factor(educ), wage1)

stargazer(
  lm.linear,
  lm.level,
  lm.factor,
  type="text"
)
# Compare magnitudes: What is the effect of the move from HS (12 years) to College (16 years)?
# Why some estimates are not significant in the last column? If the dummy for 12 years is not significant, 
# does that mean that HS offers not gain?
# Make a comment about R-squared and about adjusted R-squared.

ggplot(wage1, aes(x=educ)) +
  geom_jitter(aes(y=lwage), color="gray") +
  geom_line(aes(y=predict(lm.linear), color="Linear")) +
  geom_line(aes(y=predict(lm.level), color="Five levels")) +
  geom_line(aes(y=predict(lm.factor), color="All levels"))+
  geom_point(aes(x=12,y=1.56), color="red", shape=1, size=10)+
  labs(x="Years of education",
       y="Wage, log",
       title="Three different functional forms of the effect of education on wage") +
  scale_color_manual(name = "Functional form", 
                     values = c("Linear" = "chocolate2", 
                                "Five levels" = "maroon",
                                "All levels"="darkgreen")) 
  
# Why do the two lines intersect inside the red circle even though the estimated coefficients for 12 years are different?

# What changes in a model with more variables
lm.linear <- lm(lwage ~ educ + exper + female, wage1)
lm.level  <- lm(lwage ~ factor(educlevel) + exper + female, wage1)
lm.factor <- lm(lwage ~ factor(educ) + exper + female, wage1)

stargazer(
  lm.linear,
  lm.level,
  lm.factor,
  type="text"
)

