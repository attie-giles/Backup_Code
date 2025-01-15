# ----------------
# Quadratic functions: non-linearity, testing, convexity
# ------------------
remove(list=ls())

#Load packages
library(wooldridge) #for more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(dplyr)
library(ggplot2)
# install.packages("stargazer")
library(stargazer)
# install.packages("car")
library(car)


data('wage1')
?wage1

# Linear model with the quadratic term

# Show the results of the estimation 
model.lin  <- lm(lwage ~ exper           , wage1)
model.quad <- lm(lwage ~ exper+I(exper^2), wage1)
stargazer(model.lin  ,
          model.quad ,
          type="text")

# Plot the predicted relationship between X and Y
qplot(data=wage1, 
      x=exper, 
      y=predict(model.quad), 
      geom=c("line","point"))

# Save coefficients
B1<-model.quad$coefficients["exper"]
B2<-model.quad$coefficients["I(exper^2)"]

linearHypothesis(model.quad, "I(exper^2)=0")
# This plot is important because just knowing the shape does not tell you if the function is convex, concave, U-shape, or inverse U-shape.

# Does the maximum fall into the range of explanatory variables?
maxexper <- - B1 / ( 2*B2)
maxexper

# The slope is different for every point

# Show the marginal effect
ggplot(data=wage1, aes(x=exper, y= (B1 + 2*B2*exper))) +
  geom_line(color="maroon") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=maxexper, color="navy")+ 
  labs(y="Marginal effect")

# ----
# Average marginal effect

# Since the effect is different at every point we can calculate the average marginal effect. 
mean( model.quad$coefficients["exper"] + 
        2*(model.quad$coefficients["I(exper^2)"] ) * wage1$exper
)

# How is it different from the estimated coefficient in the model without the quadratic term? What do you think explains the difference?

# Distribution of "exper" affects the distribution of the marginal effect
ggplot(data=wage1, aes(x=exper))+
    geom_histogram()

# There are more individuals with higher positive effect of experience
ggplot(data=wage1) +
  geom_histogram(aes(x= model.quad$coefficients["exper"] 
                        + 2*(model.quad$coefficients["I(exper^2)"] ) * wage1$exper
                     )
                )


# The following produces the same number for the average marginal effect. Why?
model.quad$coefficients["exper"] + 
  2*(model.quad$coefficients["I(exper^2)"] ) * mean(wage1$exper)

# ----
# A linear model with two quadratic terms 
model.lin  <- lm(lwage ~ exper + tenure , 
                 wage1)
model.quad <- lm(lwage ~ exper+I(exper^2) + tenure + I(tenure^2), 
                 wage1)
stargazer(model.lin ,
          model.quad ,
          type="text")

# We can test for non-linearities in both variables
linearHypothesis( model.quad, c("I(exper^2)=0", "I(tenure^2)=0"))
# Why do we need to an F-test? How is it different from the two separate t-tests?

# Here again, the above test might detect non-linearity in the model
# but the correction might be to introduce only one, not both, variables
# in quadratic terms because only one of the terms might be significant.

# Plotting effect of one variable requires "holding other things constant"
# Here is what happens if we do not "h.o.t.c."
qplot(data=wage1, x=exper, y=predict(model.quad), geom="point")

# We can "h.o.t.c." by setting values to a certain level for the predictions
predict_data <- wage1 %>% mutate(tenure=0) # set tenure to some value 
predict_data <- wage1 %>% mutate(tenure=mean(tenure)) # set tenure to the sample average
predict_data <- wage1 %>% mutate(tenure=7) # set tenure to the sample average

# This is the meaning of h.o.t.c.! the other variables can be ANYTHING not kept constant
wage1$y_hat <- predict(model.quad,predict_data) 
qplot(data=wage1, x=exper, y=y_hat, geom="point")

# Calculate the maxima
- model.quad$coefficients["exper"] / ( 2*model.quad$coefficients["I(exper^2)"] )
- model.quad$coefficients["tenure"] / ( 2*model.quad$coefficients["I(tenure^2)"] )

# Calculate the range
range(wage1$exper)
range(wage1$tenure)

# Combine the information on the range and the maxima to decide if the relationship is U shaped
# concave or convex.

# We can also calculate the average effect of experience at mean
model.quad$coefficients["exper"] + 2*model.quad$coefficients["I(exper^2)"]*mean(wage1$exper)
model.quad$coefficients["tenure"] + 2*model.quad$coefficients["I(tenure^2)"]*mean(wage1$tenure)

# ----
data(hprice1)
  model.ln  <-lm(lprice ~ lsqrft + llotsize + bdrms, hprice1)
  
  model.q   <-lm(price ~ sqrft    + I(sqrft^2) +
                   lotsize  + I(lotsize^2) +
                   bdrms, hprice1)
  
  model.ln.q<-lm(lprice ~ lsqrft   + I(lsqrft^2) +
                   llotsize + I(llotsize^2) +
                   bdrms, hprice1)

stargazer(
  model.ln,
  model.q,
  model.ln.q,
  type="text"
)

# test for non-linearity
linearHypothesis( model.q, c("I(sqrft^2)=0", "I(lotsize^2)"))
linearHypothesis( model.ln.q, c("I(lsqrft^2)=0", "I(llotsize^2)"))

# calculate the min/max (which one is it) and use the range to make conclusion
model.q$coefficients["sqrft"] + 2*model.q$coefficients["I(sqrft^2)"]*mean(hprice1$sqrft)
range(hprice1$sqrft)

model.q$coefficients["lotsize"] + 2*model.q$coefficients["I(lotsize^2)"]*mean(hprice1$lotsize)
range(hprice1$lotsize)

# do the same for the log model
model.ln.q$coefficients["lsqrft"] + 2*model.ln.q$coefficients["I(lsqrft^2)"]*mean(hprice1$lsqrft)
range(hprice1$lsqrft)

model.ln.q$coefficients["llotsize"] + 2*model.ln.q$coefficients["I(llotsize^2)"]*mean(hprice1$llotsize)
range(hprice1$llotsize)
