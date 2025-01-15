#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
library(stargazer)

#Question 6
data(hprice2) 
?hprice2

hprice2 <- hprice2 %>% #using dplyer to combine the 4 variables into 2 variable sets 
  mutate(X1 = dist + stratio)

hprice2 <- hprice2  %>%
  mutate(X2 = crime + radial)

model.ovb <-lm(lprice ~X1,data=hprice2) # model with omitted variable
model.full<-lm(lprice ~ X1+X2,data=hprice2) # full model
model.vars<-lm(                X2 ~ X1,data=hprice2) # relationship between the variables

stargazer( # NOTE storing of the regression outcomes INSIDE the stargazer call
  model.ovb , # model with omitted variable
  model.full, # full model
  model.vars, # relationship between the variables
  type="text"
)

#Question 7
data("alcohol")
?alcohol
#Partial out from the educ and the status
educ.status.res   <- residuals(lm(educ ~ unemrate + age,
                                    alcohol))
status.res   <- residuals(lm(status ~ unemrate + age,
                             alcohol))
#Partial out from the educ and the unemployment
educ.unemrate.res    <- residuals(lm(educ ~ status + age,
                                    alcohol))
unemrate.res    <- residuals(lm(unemrate ~ status + age,
                             alcohol))
#Partial out from the educ and the age
educ.age.res <- residuals(lm(educ ~ status + unemrate, 
                                    alcohol))
age.res <- residuals(lm(age ~ status + unemrate ,
                             alcohol))

# Show that the coefficients of MLR are identical to the partialled out SLRs
m1 <-    lm(educ ~ status + unemrate + age, alcohol)
m2 <-    lm(educ.status.res ~ status.res)
m3 <-    lm(educ.unemrate.res ~ unemrate.res)
m4 <-    lm(educ.age.res ~ age.res)
stargazer(m1, m2, m3, m4, 
          type="latex"
)

m1 <-    lm(educ ~ status + unemrate + age, alcohol)
m2 <-    lm(educ ~ status.res, alcohol)
m3 <-    lm(educ ~ unemrate.res, alcohol)
m4 <-    lm(educ ~ age.res, alcohol)

stargazer(m1, m2, m3, m4, 
          type="latex"
)