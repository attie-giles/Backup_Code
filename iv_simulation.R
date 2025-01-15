# Clear workspace and set working directory
rm(list = ls())
#setwd("~/Dropbox/_TEACH/(X′X) Adv Metrics/Adv Econometrics I shared/R examples/")
# Install and load required packages
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if(!require(stargazer)) install.packages("stargazer", dependencies = TRUE)
if(!require(patchwork)) install.packages("patchwork")
if(!require(ggtext)) install.packages("ggtext")
if(!require(plm)) install.packages("plm")

library(patchwork)
library(ggtext)
library(ggplot2)
library(stargazer)
library(plm)

# General setup of the supply and demand with one exogenous variable:
# Supply equation: q_s = a_0 + a_1 * p_s + e_s
# Inverse demand equation: p_d = d_0 + d_1 * q_d + d_2 * income + e_d
# In equilibrium: p_d = p_s = p, q_d = q_s = q

# Initialize dataset
#set.seed(5360) # Set seed for reproducibility
n <- 100        # Number of observations

# Generate residuals and exogenous variable
e_d <- rnorm(n) * 10
e_s <- rnorm(n) * 1
income <- 5 * runif(n)

# Supply parameters
a_0 <- 1
a_1 <- 1

# Demand parameters
d_0 <- 15
d_1 <- -1
d_2 <- 5  # You can try different values of d_2

# Solve for prices and quantity
q <- (a_0 + a_1 * (d_0 + d_2 * income + e_d) + e_s) / (1 - a_1 * d_1)
p <- (q - a_0 - e_s) / a_1

# Extract the minimum and maximum of the price for graphs
p_min <- min(p)
p_max <- max(p)

# Create a data frame for use with plm
data <- data.frame(id=as.numeric(1:n), q, p, income)

# Convert data to a pdata.frame for panel data models
pdata <- pdata.frame(data, index="id")

# 1. Fit OLS model using plm
ols_model <- plm(q ~ p, data = pdata, model = "pooling")
summary(ols_model)

# 2. Fit IV model using plm (instrumenting p with income)
iv_model <- plm(q ~ p | income, data = pdata, model = "pooling")
summary(iv_model)

# 3. Hausman test comparing IV and OLS models
phtest(iv_model, ols_model)



# Print OLS and IV results
stargazer(ols_model, iv_model, type="text")

# First stage regression
first_stage <- lm(p ~ income, data=data)
stargazer(first_stage, type="text")


# Extract fitted values
p_hat <- fitted(first_stage)

# Draw oro pick an illustrative observation
N <-  sample(1:n, 1)
q_N <- q[N]
p_N <- p[N]
p_hat_N <- p_hat[N]
income_N <- income[N]

# Plot 1: Actual and predicted data
plot1 <- ggplot() + 
  stat_ellipse(aes(x = p, y = q), geom="polygon", fill="navy", alpha=.08) +  # Add 95% confidence ellipse
  stat_ellipse(aes(x = p_hat, y = q), geom="polygon", fill="maroon", alpha=.2) +  # Add 95% confidence ellipse
  geom_point(aes(x = p, y = q), color = 'navy', shape = 4) +   # Observed prices
  geom_point(aes(x = p_hat, y = q), color = 'maroon', shape = 1) + # Predicted prices
  geom_point(aes(x = p[N], y = q[N]), color = 'navy', size = 2) +  # Highlight observation N
  geom_point(aes(x = p_hat[N], y = q[N]), color = 'maroon', size = 2) +
  geom_vline(xintercept= p[N], color="navy", linetype = "dashed") +
  geom_vline(xintercept= p_hat[N], color="maroon", linetype = "dashed") +
  geom_smooth(aes(x = p, y = q), method = "lm", se = FALSE, color = "navy") + # OLS fitted line
  geom_smooth(aes(x = p_hat, y = q), fullrange=TRUE, method = "lm", se = FALSE, color = "maroon", linetype = "solid") + # 2SLS fitted line
  labs(x = NULL, 
       y = "**Second Stage**<br><br>Quantity",
       title = "<span style='color:maroon;'>IV fitted to predicted prices</span> <br>
                <span style='color:navy;'>OLS fitted to actual data</span>") +
  theme_minimal() +
  theme(
    axis.title.y = element_markdown(size = 12),  # Enable markdown for y-axis
    plot.title = element_markdown(size = 16, hjust = 0.5)  # Use markdown for title
  ) 
  
# Plot 2: First stage
plot2 <- ggplot() + 
  geom_point(aes(y = income, x = p), color="darkgreen", shape = 1) +
  geom_smooth(aes(y = income, x = p_hat), color="darkgreen", method = "lm", se = FALSE) +
  geom_point(aes(y = income[N], x = p[N]), color = 'navy', size = 2) + 
  geom_point(aes(y = income[N], x = p_hat[N]), color = 'maroon', size = 2) +
  geom_hline(yintercept = income[N], color="darkgreen", linetype = "dashed" ) +
  geom_vline(xintercept= p[N], color="navy", linetype = "dashed") +
  geom_vline(xintercept= p_hat[N], color="maroon", linetype = "dashed") +
  labs(y = "**First Stage**<br><br>Income", x = "Price") +
  scale_x_continuous(position = "top") +
  theme_minimal() +
  theme(
    axis.title.y = element_markdown(size = 12),  # Enable markdown for y-axis
   ) 

# Plot 3: Histogram of prices
plot3 <- ggplot() + 
  geom_density(aes(x = p), fill = "navy", alpha = 0.5) + 
  geom_density(aes(x = p_hat), fill = "maroon", alpha = 0.5) +
  labs( x=NULL, y = "") +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  theme_minimal()

# Combine the three plots using patchwork
combined_plot <-  plot1 / 
                  plot3 / 
                  plot2 + 
    plot_layout(ncol = 1, 
                heights = c(4, 1, 4))

# Print combined plot
print(combined_plot)

