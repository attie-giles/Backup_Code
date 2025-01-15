rm(list = ls())

# Ensure required packages are installed
if(!require(wooldridge)) install.packages("wooldridge")
if(!require(car)) install.packages("car")
if(!require(sandwich)) install.packages("sandwich")
if(!require(stargazer)) install.packages("stargazer")

# Load necessary libraries
library(wooldridge)
library(car)
library(sandwich)
library(stargazer)

### Example 1: Testing if a coefficient equals 1 (lassess = 1)

# This example parallels the t-test

# Load data and fit the model
data("hprice1")
model1 <- lm(lprice ~ lassess + llotsize + lsqrft + bdrms, data = hprice1)
stargazer(model1, type = "text")

# Null hypothesis: H0: lassess = 1

# By hand (Wald test)
R <- matrix(c(0, 1, 0, 0, 0), nrow = 1, byrow = TRUE)  # Restricting lassess coefficient
r <- c(1)  # Hypothesized value r is q
vcov_matrix <- vcov(model1)
wald_stat <- t(R %*% coef(model1) - r) %*% 
  solve(R %*% vcov_matrix %*% t(R)) %*% 
  (R %*% coef(model1) - r)
df <- nrow(R)
p_value_hand <- 1 - pchisq(wald_stat, df)

cat("\nExample 1 - By Hand - Wald Test Statistic:", wald_stat, "\n")
cat("Example 1 - By Hand - p-value:", p_value_hand, "\n")

# Using linearHypothesis() with and without hypothesis.matrix
(linearHypothesis(model1, hypothesis.matrix = R, rhs = r, vcov. = vcov_matrix, test = "Chisq"))
(linearHypothesis(model1, "lassess = 1", vcov. = vcov_matrix, test = "Chisq"))

# T-test for lassess = 1
beta_hat <- coef(model1)["lassess"]
se_beta <- sqrt(vcov_matrix["lassess", "lassess"])
t_stat <- (beta_hat - 1) / se_beta
df_t <- df.residual(model1)
p_value_t <- 2 * (1 - pt(abs(t_stat), df_t))

cat("\nExample 1 - T-test Statistic:", t_stat, "\n")
cat("Example 1 - T-test p-value:", p_value_t, "\n")

### Example 2: Testing if two coefficients are equal (lrgasprice = lcpifood)

# This example shows a test about two coefficients

# Load data and fit the model
data("approval")
model2 <- lm(approve / 100 ~ lrgasprice + lsp500 + lcpifood, data = approval)
stargazer(model2, type="text")
# Null hypothesis: H0: lrgasprice = lcpifood

# By hand (Wald test)

R <- matrix(c(0, 1, 0, -1), nrow = 1, byrow = TRUE)  # Restricting coefficients
r <- c(0)
vcov_matrix <- vcov(model2)
wald_stat <- t(R %*% coef(model2) - r) %*% 
  solve(R %*% vcov_matrix %*% t(R)) %*% 
  (R %*% coef(model2) - r)
df <- nrow(R)
p_value_hand <- 1 - pchisq(wald_stat, df)

cat("\nExample 2 - By Hand - Wald Test Statistic:", wald_stat, "\n")
cat("Example 2 - By Hand - p-value:", p_value_hand, "\n")

# Using linearHypothesis() with and without hypothesis.matrix
(linearHypothesis(model2, hypothesis.matrix = R, rhs = r, vcov. = vcov_matrix, test = "Chisq"))
(linearHypothesis(model2, "lrgasprice = lcpifood", vcov. = vcov_matrix, test = "Chisq"))

### Example 3: Testing linear combination of coefficients with robust variance-covariance matrix

# This example a variation on the above example but with robust
# variance-covariance matrix. This showcases differences with F-test.

# Load data and fit the model
data("vote1")
model3 <- lm(voteA ~ expendA + expendB + democA, data = vote1)
stargazer(model3, type="text")

# Null hypothesis: H0: expendA = -expendB

# By hand (Wald test) using robust variance-covariance matrix
R <- matrix(c(0, 1, 1, 0), nrow = 1, byrow = TRUE)  # Restricting coefficients
r <- c(0)
vcov_matrix <- vcovHC(model3, type = "HC1")  # Robust variance-covariance matrix
wald_stat <- t(R %*% coef(model3) - r) %*% 
  solve(R %*% vcov_matrix %*% t(R)) %*% 
  (R %*% coef(model3) - r)
df <- nrow(R)
p_value_hand <- 1 - pchisq(wald_stat, df)

cat("\nExample 3 - By Hand - Wald Test Statistic with Robust VCov:", wald_stat, "\n")
cat("Example 3 - By Hand - p-value:", p_value_hand, "\n")

# Using linearHypothesis() with robust variance-covariance matrix
(linearHypothesis(model3, hypothesis.matrix = R, rhs = r, vcov. = vcov_matrix, test = "Chisq"))
(linearHypothesis(model3, "expendA = -expendB", vcov. = vcov_matrix, test = "Chisq"))

### Example 4: Testing joint significance of first 4 coefficients in a model with 5 explanatory variables

# This example parallels F-test of restricted model

# Load data and fit the model
data("hprice2")
model4 <- lm(lprice ~ crime + nox + rooms + dist + stratio, data = hprice2)
stargazer(model4, type = "text")


# Null hypothesis: H0: coefficients of crime, nox, rooms, dist are all zero

# By hand (Wald test)

R <- matrix(c(0, 1, 0, 0, 0, 0,  # crime
              0, 0, 1, 0, 0, 0,  # nox
              0, 0, 0, 1, 0, 0,  # rooms
              0, 0, 0, 0, 1, 0), nrow = 4, byrow = TRUE)
r <- c(0, 0, 0, 0)
vcov_matrix <- vcov(model4)
wald_stat <- t(R %*% coef(model4) - r) %*% 
  solve(R %*% vcov_matrix %*% t(R)) %*% 
  (R %*% coef(model4) - r)
df <- nrow(R)
p_value_hand <- 1 - pchisq(wald_stat, df)

cat("\nExample 4 - By Hand - Wald Test Statistic:", wald_stat, "\n")
cat("Example 4 - By Hand - p-value:", p_value_hand, "\n")

# Using linearHypothesis() with and without hypothesis.matrix
(linearHypothesis(model4, hypothesis.matrix = R, rhs = r, vcov. = vcov_matrix, test = "Chisq"))
(linearHypothesis(model4, c("crime = 0", "nox = 0", "rooms = 0", "dist = 0"), vcov. = vcov_matrix, test = "Chisq"))
