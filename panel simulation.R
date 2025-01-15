remove(list=ls()) #Clear out the environment


# Load required packages
library(lfe)
library(ggplot2)
library(dplyr)
library(stargazer)
library(plm)
library(lmtest)

# Set seed for reproducibility
#set.seed(123)

# Simulation parameters
n <- 5  # Number of individuals
t <- 15   # Time periods
beta <- 1 # True slope coefficient

# Generate a covariate and error terms

eps <- rnorm(n * t)

# Three scenarios for individual effects (alphas)
rel_var <- 1 # var(alpha)=var(eps)*rel_var^2
cor_strength <- 1 # cor(alpha, x)

alpha <- (rel_var)^2*rnorm(n) #Relative variance of alpha_i
alpha <- rep(alpha, each=t)

x_uncorrelated <- rnorm(n * t)
x_positive <- cor_strength * alpha + rnorm(n)
x_negative <- -cor_strength * alpha + rnorm(n)

# Data generation
generate_panel <- function(x) {
  id <- as.factor(rep(1:n, each = t))
  time <- rep(1:t, times = n)
  y <- alpha + beta * x + eps
  data.frame(id, time, x, y, alpha) %>% 
    group_by(id) %>%
    mutate(y.gr.mean=mean(y),
           x.gr.mean=mean(x))
}

# ====== Create the three panels
panel_uncorrelated <- generate_panel(x_uncorrelated)
panel_positive <- generate_panel(x_positive)
panel_negative <- generate_panel(x_negative)

# ====== Estimation of fixed effects alpha
#pols.uncor <-   lm(y~x,                 data=panel_uncorrelated)
pols.uncor   <-  plm(y ~ x, data = panel_uncorrelated, 
                   index = c("id", "time"), model = "pooling")
be.uncor   <-   lm(y.gr.mean~x.gr.mean, data=panel_uncorrelated)
#fe.uncor   <- felm(y ~ x | id | 0 | 0,  data = panel_uncorrelated)
fe.uncor      <-  plm(y ~ x, data = panel_uncorrelated, 
                    index = c("id", "time"), model = "within")
re.uncor   <-  plm(y ~ x, data = panel_uncorrelated, 
                   index = c("id", "time"), 
                   model = "random")

#pols.pos   <-   lm(y~x,                 data=panel_positive)
pols.pos   <-  plm(y ~ x, data = panel_positive, 
                   index = c("id", "time"), model = "pooling")
be.pos     <-   lm(y.gr.mean~x.gr.mean, data=panel_positive)
#fe.pos     <- felm(y ~ x | id | 0 | 0,  data = panel_positive)
fe.pos      <-  plm(y ~ x, data = panel_positive, 
                    index = c("id", "time"), model = "within")
re.pos    <- plm(y ~ x, data = panel_positive, 
                   index = c("id", "time"), model = "random")

  
#pols.neg   <-   lm(y~x,                 data=panel_negative)
pols.neg   <-  plm(y ~ x, data = panel_negative, 
  index = c("id", "time"), model = "pooling")
be.neg     <-   lm(y.gr.mean~x.gr.mean, data=panel_negative)
#fe.neg     <- felm(y ~ x | id | 0 | 0,  data = panel_negative)
fe.neg      <-  plm(y ~ x, data = panel_negative, 
                   index = c("id", "time"), model = "within")
re.neg     <-  plm(y ~ x, data = panel_negative, 
                   index = c("id", "time"), model = "random")


# ======  Present the results
# Verify that b_total=F_within * b_within + F_between * b_between
stargazer(pols.uncor, be.uncor, fe.uncor, re.uncor, type="text",
          column.labels = c("POLS", "Between", "Within/FE", "RE"),
          omit.table.layout = "ldm")
# Tests

# F-test for fixed effects.
# "Tests of poolability" 
# POLS vs FE based on fit
# H0: u_i=0 for all i
# F-distribution
# low p-val -> fixed effects improve model fit -> FE is preferred
# Assmpt: POLS and RE are both consistent,cov(u_i, x_i)=0
pFtest( fe.uncor, pols.uncor)

# Hausman test for systematic differences between FE and RE
# "Test for unobserved heterogeneity"
# FE vs RE
# H0: cov(u_i, x_i)=0
# Chi-squared
# low p-val -> FE and RE are different -> FE is preferred
# Assmpt: RE efficient and consistent under Null, inconsistent under Alt
# Assmpt: FE consistent under Null and Alt, inefficient under Null
phtest(fe.uncor, re.uncor)

# Breusch-Pagan test for individual effects in POLS errors.
# "Test for random effects"
# RE vs POLS
# H0: var(u_i)=0
# Chi-squared with 1 degree of freedom because it evaluates 1 parameter var(u_i)
# low p-val -> RE is preferred
# Assmpt: POLS and RE are both consistent,cov(u_i, x_i)=0
plmtest(pols.uncor, type = "bp", effect = "individual")


#------------------------------------------------------------------------------
stargazer(pols.pos,  be.pos,   fe.pos, re.pos,   type="text",
          column.labels = c("POLS", "Between", "Within/FE", "RE"),
          omit.table.layout = "ldm")
# Tests
pFtest(fe.pos, pols.pos)
phtest(fe.pos, re.pos)
plmtest(pols.pos, type = "bp", effect = "individual")

stargazer(pols.neg,   be.neg,   fe.neg, re.neg,  type="text",
          column.labels = c("POLS", "Between", "Within/FE", "RE"),
          omit.table.layout = "ldm")
# Tests
pFtest(fe.neg, pols.neg)
phtest(fe.neg, re.neg)
plmtest(pols.neg, type = "bp", effect = "individual")




# ====== Plotting panels
panel_uncorrelated$scenario <- "Uncorrelated"
panel_positive$scenario <- "Positive Correlation"
panel_negative$scenario <- "Negative Correlation"


# Separate plots

# Uncorrelated: cov(alpha,x)=0
ggplot(panel_uncorrelated, aes(x = x, y = y)) +
  geom_point(aes(color=id), alpha = 0.5) +
  stat_ellipse(aes(fill=id), geom="polygon", alpha=.08) +  # Add 95% confidence ellipse
  geom_point(aes(x=x.gr.mean, y=y.gr.mean), color="maroon", size=3) +
  geom_smooth(aes(color="POLS"), method = "lm", se = FALSE) +
  geom_smooth(aes(x=x.gr.mean,y=y.gr.mean, color="Between"), method="lm", fullrange=TRUE, se=FALSE) +
  geom_line(aes(y=predict(fe.uncor), x=x, group=id, color="FE"), linewidth=.75, alpha=.5) +
  geom_line(aes(y=predict(re.uncor), x=x, color="RE"), linewidth=.9, linetype = "dashed")+
  labs(title = "Fixed Effects Estimation vs POLS vs BE vs RE \nfor uncorrelated effects",
       x = "Covariate (x)", color="Model", y = "Outcome (y)") +
  scale_color_manual(values = c("POLS" = "navy", "FE" = "black", "RE"="darkorange", "Between"="maroon")) +
  theme_minimal()

# Positive: cov(alpha,x)>0
ggplot(panel_positive, aes(x = x, y = y)) +
  geom_point(aes(color=id), alpha = 0.5) +
  stat_ellipse(aes(fill=id), geom="polygon", alpha=.08) +  # Add 95% confidence ellipse
  geom_point(aes(x=x.gr.mean,y=y.gr.mean), color="maroon", size=3) +
  geom_smooth(aes(color="POLS"), method = "lm", se = FALSE) +
  geom_smooth(aes(x=x.gr.mean,y=y.gr.mean, color="Between"), method="lm", fullrange=F, se=FALSE) +
  geom_line(aes(y=predict(fe.pos), x=x, group=id, color="FE"), linewidth=.75, alpha=.5) +
  geom_line(aes(y=predict(re.pos), x=x, color="RE"), linewidth=.9, linetype = "dashed")+
  labs(title = "Fixed Effects Estimation vs POLS vs BE vs RE \nfor positively correlated effects",
       x = "Covariate (x)", color="Model", y = "Outcome (y)") +
  scale_color_manual(values = c("POLS" = "navy", "FE" = "black", "RE"="darkorange", "Between"="maroon")) +
  theme_minimal()

# Negative: cov(alpha,x)<0
ggplot(panel_negative, aes(x = x, y = y)) +
  geom_point(aes(color=id), alpha = 0.5) +
  stat_ellipse(aes(fill=id), geom="polygon", alpha=.08, level=.9) +  # Add 95% confidence ellipse
  geom_point(aes(x=x.gr.mean,y=y.gr.mean), color="maroon", size=3) +
  geom_smooth(aes(color="POLS"), method = "lm", se = FALSE) +
  geom_smooth(aes(x=x.gr.mean,y=y.gr.mean, color="Between"), method="lm", fullrange=F, se=FALSE) +
  geom_line(aes(y=predict(fe.neg), x=x, group=id, color="FE"), linewidth=.75, alpha=.5) +
  geom_line(aes(y=predict(re.neg), x=x, color="RE"), linetype = "dashed", linewidth=.9)+
  labs(title = "Fixed Effects Estimation vs POLS vs BE vs RE \nfor negatively correlated effects",
       x = "Covariate (x)", color="Model", y = "Outcome (y)") +
  scale_color_manual(values = c("POLS" = "navy", "FE" = "black", "RE"="darkorange", "Between"="maroon")) +
  theme_minimal()

# ====== Calculating and plotting alpha_hat
# Uncorrelated
panel_uncorrelated <-  panel_uncorrelated %>% 
  group_by(id) %>% 
  mutate(alpha.hat=mean(y-fe.uncor$coefficients[1]*x)) %>% 
  ungroup()
ggplot(data= panel_uncorrelated)+geom_point(aes(alpha.hat, alpha)) + geom_line(aes(alpha, alpha)) + theme_minimal()
ggplot(data= panel_uncorrelated, aes(alpha.hat, x.gr.mean))+geom_point() + geom_smooth(method="lm", se=FALSE) + theme_minimal()

# Positive
panel_positive <-  panel_positive %>% group_by(id) %>% mutate(alpha.hat=mean(y-fe.pos$coefficients[1]*x)) %>% ungroup()
ggplot(data= panel_positive)+geom_point(aes(alpha.hat, alpha)) + geom_line(aes(alpha, alpha)) + theme_minimal()
ggplot(data= panel_positive, aes(alpha.hat, x.gr.mean))+geom_point() + geom_smooth(method="lm", se=FALSE) + theme_minimal()

# Negative
panel_negative <-  panel_negative %>% group_by(id) %>% mutate(alpha.hat=mean(y-fe.neg$coefficients[1]*x)) %>% ungroup()
ggplot(data= panel_negative)+geom_point(aes(alpha.hat, alpha)) + geom_line(aes(alpha, alpha)) + theme_minimal()
ggplot(data= panel_negative, aes(alpha.hat, x.gr.mean))+geom_point() + geom_smooth(method="lm", se=FALSE) + theme_minimal()



# combined plot

combined_panel <- bind_rows(panel_uncorrelated, panel_positive, panel_negative)

ggplot(combined_panel, aes(x = x, y = y, color = scenario)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, fullrange=TRUE) +
  labs(title = "POLS Estimation of Panel Data Across Scenarios",
       x = "Covariate (x)", y = "Outcome (y)", color= "cov(u,X) scenarios") +
  theme_minimal()


