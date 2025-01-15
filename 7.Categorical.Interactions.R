# -------------------------------------------------------------------------
# Interactions with categorical variables
# -------------------------------------------------------------------------
remove(list=ls())

#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
# install.packages("stargazer")
library(stargazer)
# install.packages("car")
library(car)


#---------------------
# Model of wage as a function of education for two groups

data('wage1')
?wage1

model.pooled  <- lm(lwage ~ educ, wage1)          # Pooled model
model.control <- lm(lwage ~ educ + female, wage1) # Controlling for gender
model.inter   <- lm(lwage ~ educ*female, wage1)   # Interacting with gender

stargazer(model.pooled,  # Pooled model
          model.control, # Controlling for gender
          model.inter,   # Interacting with gender
          type="text")

# Controlling for gender answers the question: what is the effect of education on wage holding gender constant? In other words: No matter what the gender is, what is the effect?
# Interacting with gender answers the question: how does the effect of eduction on wage differ across genders? In other words: Pick a gender, what is the effecct for that gender?

# ---
# What does the coefficient on female tell us in the specification with interactions?
#
# the p-value on female in column (2) tests if there is a difference in averages
# the p-value on female in column (3) tests if there is a difference in intercepts (not necessarily the averages)

# What does the coefficient on education interacted with female tell us?
#
# the p-value on educ:female in column (3) tests if the effect of educatation is different for males and females

# The joint test of the variable and it's interaction term tests if the model is different
#
linearHypothesis( model.inter, 
                  matchCoefs(model.inter, "female"))

#---------------------
# Model of wage as a function of experience for two groups

model.pooled  <- lm(lwage ~ exper, wage1)        # Pooled model
model.control <- lm(lwage ~ exper +female, wage1) # Controlling for gender
model.inter   <- lm(lwage ~ exper*female, wage1)  # Interacting with gender

stargazer(model.pooled, # Pooled model
          model.control, # Controlling for gender
          model.inter, # Interacting with gender
          type="text")

linearHypothesis( model.inter, 
                  matchCoefs(model.inter, "female"))


# Scatterplot of the estimation results with intercept and the interaction 
# allowing for difference in BOTH intercept and the interaction is equivalent
# to allowing for a different MODEL for male and female. 
ggplot(wage1) + 
  geom_smooth(aes(y=lwage,x=exper), color="orange", size=.5, method="lm", se=FALSE) +
  geom_line(aes(y=predict(model.inter),x=exper, color=factor(female))) +
  geom_point(aes(y=lwage,x=exper, color=factor(female)), alpha=.5) +
  scale_color_manual(values=c("maroon","navyblue"))

#Scatterplot of the estimation results with and without the interaction 
ggplot(wage1) + 
  geom_line(aes(y=predict(model.inter),x=exper, color=factor(female))) +
  geom_line(aes(y=predict(model.control),x=exper, color=factor(female)), alpha=.5) +
  geom_jitter(aes(y=lwage,x=exper, color=factor(female)), alpha=.5) +
  scale_color_manual(values=c("maroon","navyblue")) +
  labs(title="Effect of interaction model in comparison to the dummy control model (faded)",
       x="Experience",
       y="Wage, log", 
       color="Female")

#---------------------
# More complex interactions
model.pooled <-  lm(lwage ~ exper, wage1) 
model.control <- lm(lwage ~ exper +female + married, wage1) 
model.inter <- lm(lwage ~ exper*(female+married), wage1)
model.inter.all <- lm(lwage ~ exper*female*married, wage1)

stargazer(model.pooled, 
          model.control, 
          model.inter,
          model.inter.all,
          type="text")

# How do we interpret the results?

# We can test for some interactions
linearHypothesis( model.inter, 
                  matchCoefs(model.inter, "married"))

linearHypothesis( model.inter, 
                  matchCoefs(model.inter, "female"))

# Or all interactions
linearHypothesis( model.inter.all, 
                  matchCoefs(model.inter, "female|married"))

linearHypothesis( model.inter.all, 
                  matchCoefs(model.inter.all, "female|married"))

# -----------------
# interactions where we will use
# interactions to test for difference in models (Chow test)

data(wage1)
?wage1

# Create a factor
wage1 <- wage1%>%
  mutate(occupation=case_when( 
    profocc==1 ~ "prof",
    clerocc==1 ~ "cler",
    servocc==1 ~ "serv",
    TRUE       ~ "manu"    #missing occupation, manual labor
  ))

table(wage1$occupation)

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


# Visualizing occupational differences in the relationship
ggplot(wage1, aes(x=educ, y=lwage, color=occupation))+
  geom_jitter() +
  geom_smooth(method="lm",se=F, alpha=.25 )
# This is often a good first glance at the data.
# What is the advantage of estimating an interaction model instead of actually
# estimating a different model for each group?

lm.base          <- lm(lwage ~ educ                    , data=wage1)
lm.inter         <- lm(lwage ~ educ*factor(occupation) , data=wage1) 
lm.inter.relevel <- lm(lwage ~ educ*relevel(factor(occupation), ref="serv") , data=wage1)  

# Compare base with interactions
stargazer(lm.base,
          lm.inter,
          type="text"
)

# Compare two different base levels
stargazer(lm.inter,
          lm.inter.relevel,
          type="text"
)

linearHypothesis( lm.inter,               # model to be tested
                  matchCoefs(lm.inter,    # model to get coefficient names (same in this case)
                             "occupation"))# word to match
#Test of the occupation specific slopes
linearHypothesis( lm.inter, "educ+educ:factor(occupation)manu")
linearHypothesis( lm.inter, "educ+educ:factor(occupation)prof")
linearHypothesis( lm.inter, "educ+educ:factor(occupation)serv")
