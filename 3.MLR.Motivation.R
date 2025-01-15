# ------------------------------------------
# This scripts is used to illustrate a motivating example for multiple regression model
# ------------------------------------------
remove(list=ls())
#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
# install.packages("stargazer")
library(stargazer)


data("jtrain98")
?jtrain98
stargazer(lm(earn98 ~ train, jtrain98),
          lm(earn98 ~ train + married + educ + age, jtrain98),
          lm(earn98 ~ train + earn96, jtrain98)
          , type="text")

# ====================================================================================
# Intuition and motivation

# The goal of this script is to show that addition of variables to our analysis
# allows for "ceteris paribus" interpreation of the coefficients


#====================================================================================
# The simple case of a dummy variable
#



# The simplest case is when one of the variables is binary. Recall, a binary variable picks
# up the difference in means.

# Here is what we know
data(wage1)

wage1<-wage1%>%mutate(male=1-female) # create a male dummy

table(wage1$male)
table(wage1$female)

# Consider the familiar problem of finding out the difference in wages across two groups
stargazer(
  lm(lwage ~ male, data=wage1),
  type="text"
)
wage1%>%group_by(male)%>%summarize(mean(lwage))

# The problem with interpreting the simple model to mean that the differences are due to gender 
# is that the two groups differ
# with respect to many variables. To illustrate the intuition of what changes with 
# the addition of more variables, let's start with one, the familiar education.

# Let's add education as an additional variable
ggplot(data=wage1, aes(x=educ, y=lwage)) +
  geom_jitter(aes(color=factor(male)), width = .5, height = 0.5) +
  geom_vline(xintercept=mean(wage1$educ[wage1$male==1]), color="cyan4", linetype=2)+ # mean education for males
  geom_hline(yintercept=mean(wage1$lwage[wage1$male==1]), color="cyan4", linetype=2)+ # mean lwage for males
  geom_vline(xintercept=mean(wage1$educ[wage1$male==0]), color="coral3", linetype=2)+ # mean education for females
  geom_hline(yintercept=mean(wage1$lwage[wage1$male==0]), color="coral3", linetype=2) # mean lwage for females

# The ADDITION of education CHANGES the difference between the estimated groups by about 10% 
stargazer(
  lm(lwage ~ male, data=wage1),
  lm(lwage ~ male + educ, data=wage1),
  type="text"
)

# How is the interpretation of the coefficient on "male" differ between the two columns?

# Hint: We hold something constant in the second columns. So, the interpretation in 
# second column needs to start with "holding education constant ..." 
# or "for any level of education ..."

# Why are the coefficients on "male" different across the columns? This is because the two groups differ in their average education:

wage1%>%group_by(male)%>%summarize(mean(lwage), mean(educ))

# As you can see males have higher wages and higher education in this sample.
# So, the "male" dummy captures the differences in the wages due to to everything 
# that varies by gender. "male"=1 corresponds to higher education and higher wages.
# So the coefficient on "male" dummy in the SLR captures the positive effect of 
# both variables. This is one case of the omitted variable bias.

# So, how DO you interpret the estimates on the dummies?
# 1. The estimated coefficients are now intercepts (not the averages)
# 2. Beta_1 still captures the difference between the two groups.
# 3. Beta_0 and Beta_1 by themselves do not tell us anything about the means for the groups.

# To illustrate the three points above let's add the lines of best fit
ggplot(data=wage1, aes(x=educ)) +
  geom_jitter (aes( y= lwage, 
                        color=factor(male)), width = .5, height = 0.5) +
  geom_line  (aes( y= predict(lm(lwage ~ male + educ, data=wage1)), # Note the use of "predict" to get the lwage_hat
                        color=factor(male))) +
  geom_vline(xintercept=mean(wage1$educ[wage1$male==1]), color="cyan4", linetype=2)+ # mean education for males
  geom_hline(yintercept=mean(wage1$lwage[wage1$male==1]), color="cyan4", linetype=2)+ # mean lwage for males
  geom_vline(xintercept=mean(wage1$educ[wage1$male==0]), color="coral3", linetype=2)+ # mean education for females
  geom_hline(yintercept=mean(wage1$lwage[wage1$male==0]), color="coral3", linetype=2) # mean lwage for females


# To points for a deeper discussion of the MLR:
# 1. Are the results without education still meaningful? In what sense?
# 2. What are the limitations of the "holding other things constant" interpretation? In this case we just hold the education constant.

#====================================================================================
# The logic applies both ways

# Combing "male" and education changes the effect of either of those variables
stargazer(
  lm(lwage ~ male, data=wage1),
  lm(lwage ~ educ, data=wage1),
  lm(lwage ~ male + educ, data=wage1),
  type="text"
)
# Why do you think R-squared is not a sum of the two R-squared's?

# Starting with the education, we can see that the line of best fit on the overall data ignores differences
# across groups and is therefore "biased". Can this bias be explained by E(cov(x,u)/var(x))? Hint: Yes.

# First, reconstruct the usual line of best fit || R comment: use predict(lm()) instead of geom_smooth(, method="lm")
ggplot(data=wage1, aes(x=educ)) +
  geom_jitter(aes(y=lwage, color=factor(male)), width = .5, height = 0.5) +
  geom_line(aes(y=predict(lm(lwage ~ educ, data=wage1))))

# Second, add "male" to the education model

# What if the best fit includes "male? Why does it look weird? || R comment: add male to lm() formula
ggplot(data=wage1, aes(x=educ)) +
  geom_jitter(aes(y=lwage, color=factor(male)), width = .5, height = 0.5) +
  geom_line(aes(y=predict(lm(lwage ~ male + educ, data=wage1))))

# It is because there predicted line is different for males and females || R comment: add color=factor(male) to aes()
ggplot(data=wage1, aes(x=educ)) +
  geom_jitter(
    aes(y=lwage, 
        color=factor(male)),
    width = .5, height = 0.5) +
  geom_line(
    aes(y=predict(lm(lwage ~ male + educ, data=wage1)), 
        color=factor(male)))

# How does the line compare to the line of the overall fit?
ggplot(data=wage1, aes(x=educ)) +
  geom_jitter(aes(y=lwage, color=factor(male)), width = .5, height = 0.5) +
  geom_line(aes(y=predict(lm(lwage ~ male  +  educ, data=wage1)), color=factor(female)))+
  geom_line(aes(y=predict(lm(lwage ~          educ, data=wage1)))) 

# The overall fit is steeper because when we ignore the gender, the information contained in the 
# female variable is captured by the educ variable because the two are correlated? Omitted variable bias.

# Repeating the table we see that the addition of "male" flattens the education effect
stargazer(
  lm(lwage ~ educ, data=wage1),
  lm(lwage ~ male + educ, data=wage1),
  type="text"
)

# What is the correlation between education, lwage, and female
wage1%>%summarize(round(cor(lwage, male), digits=3), 
                  round(cor(educ,  male), digits=3))

#====================================================================================
# To understand deeper what MLR does relative to SLR, let's try to covert the MLR
# logic to the SLR logic. In other words, let's try to take out the effect of gender
# differences from the relationship between education and earnings.

# How do we do it? Well, since the addition of male revealed differences in means, we can subtract those means

wage1 <- wage1%>%
  group_by(male)%>%
  mutate(                     # recall that mutate() adds a column or a variable
    lwage.md=lwage-mean(lwage),
    educ.md=educ-mean(educ)
  )

# Did it work? Let's plot the mean-differenced data and find the line of best fit:

ggplot(data=wage1, aes(x=educ.md)) +
  geom_jitter(aes(y=lwage.md, color=factor(male)), width = .5, height = 0.5) +
  geom_line(aes(y=predict(lm(lwage.md ~ educ.md, data=wage1)))) +
  geom_vline(xintercept=mean(wage1$educ.md[wage1$male==1])+0.025, color="cyan4")+ # mean education for males
  geom_hline(yintercept=mean(wage1$lwage.md[wage1$male==1]+0.005), color="cyan4")+ # mean lwage for females
  geom_vline(xintercept=mean(wage1$educ.md[wage1$male==0]), color="coral3", linetype=2)+ # mean education for males
  geom_hline(yintercept=mean(wage1$lwage.md[wage1$male==0]), color="coral3", linetype=2) # mean lwage for males

# What is the slope of that line?

stargazer(
  lm(lwage   ~ male, data=wage1),
  lm(lwage   ~ educ, data=wage1),
  lm(lwage   ~ male + educ, data=wage1),
  lm(lwage.md~ male + educ.md, data=wage1),
  type="text"
)
# Turns out the slope is EXACTLY the same as as what we had before. 
# In the MLR lingo we "partialled out" the effect of gender on "lwage" and "educ"


# ----------------------
data(beauty)

ggplot(data=beauty, aes(x=looks, y=lwage)) +
  geom_jitter(aes(color=factor(female)), width = .5, height = 0.5) +
  geom_line  (aes( y= predict(  lm(lwage ~ female + looks, data=beauty)), # Note the use of "predict" to get the lwage_hat
                   color=factor(female))) +
  geom_smooth(method="lm", se=F, size=.5) +
  geom_vline(xintercept=mean(beauty$looks[beauty$female==1]), color="cyan4", linetype=2)+ # mean  for males
  geom_hline(yintercept=mean(beauty$lwage[beauty$female==1]), color="cyan4", linetype=2)+ # mean  for females
  geom_vline(xintercept=mean(beauty$looks[beauty$female==0]), color="coral3", linetype=2)+ # mean  for males
  geom_hline(yintercept=mean(beauty$lwage[beauty$female==0]), color="coral3", linetype=2) # mean  for males

stargazer(
  lm(lwage ~ female, data=beauty),
  lm(lwage ~ looks, data=beauty),
  lm(lwage ~ female + looks, data=beauty),
  type="text"
)

# ----------------------
data("jtrain98")
?jtrain98
jtrain98$lnearn98<-log(jtrain98$earn98)
jtrain98$lnearn96<-log(jtrain98$earn96)

m1 <- lm(earn98 ~ train, jtrain98)
m2 <- lm(earn96 ~ train, jtrain98)
m3 <- lm(earn98 ~ train + earn96, jtrain98)
m4 <- lm(earn98 ~ train + earn96 + age + educ, jtrain98)

stargazer( m1, m2, m3, m4,
  type="text"
)


# ----------------------
data(hprice1)

ggplot(data=hprice1, aes(x=lsqrft, y=lprice)) +
  geom_jitter(aes(color=factor(colonial)), width = .5, height = 0.5) +
  geom_line  (aes( y= predict(lm(lprice ~ lsqrft + colonial, data=hprice1)), # Note the use of "predict" to get the lwage_hat
                   color=factor(colonial))) +
  geom_smooth(method="lm", se=F, size=.5) +
  geom_vline(xintercept=mean(hprice1$lsqrft[hprice1$colonial==1]), color="cyan4", linetype=2)+ 
  geom_hline(yintercept=mean(hprice1$lprice[hprice1$colonial==1]), color="cyan4", linetype=2)+ 
  geom_vline(xintercept=mean(hprice1$lsqrft[hprice1$colonial==0]), color="coral3", linetype=2)+ 
  geom_hline(yintercept=mean(hprice1$lprice[hprice1$colonial==0]), color="coral3", linetype=2) 

stargazer(
  lm(lprice ~ lsqrft, data=hprice1),
  lm(lprice ~ colonial, data=hprice1),
  lm(lprice ~ lsqrft + colonial, data=hprice1),
  type="text"
)


