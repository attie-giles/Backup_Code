# Load necessary packages
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install the wooldridge package from GitHub
remotes::install_github("JustinMShea/wooldridge")

#if (!require(wooldridge)) install.packages("wooldridge", dependencies = TRUE)
if (!require(plm)) install.packages("plm", dependencies = TRUE)
if (!require(stargazer)) install.packages("stargazer", dependencies = TRUE)
if (!require(lmtest)) install.packages("lmtest", dependencies = TRUE)
if (!require(sandwich)) install.packages("sandwich", dependencies = TRUE)
if (!require(car)) install.packages("car", dependencies = TRUE)

library(wooldridge)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)
library(car)

# Load the data
data("card", package = "wooldridge")

# Define variables for the model
dep.var <- "lwage"         # Dependent variable (log of wage)
end.var <- "educ"          # Endogenous variable (years of education)
inc.inst <- c("exper", "I(exper^2)", "smsa")  # Included instruments: experience, experience squared, smsa (urban indicator)
excl.inst <- c("nearc2", "nearc4")      # Excluded instruments: proximity to two- or four-year colleges

# Create formulae using the defined variables
ols_formula <- as.formula(paste(dep.var, "~", paste(c(end.var, inc.inst), collapse = " + ")))
iv_formula <- as.formula(paste(dep.var, "~", paste(c(end.var, inc.inst), collapse = " + "), 
                               "|", paste(c(excl.inst, inc.inst), collapse = " + ")))

# Estimate the model using OLS
ols_model <- plm(ols_formula, data = card, model = "pooling")

# Estimate the model using 2SLS with plm
iv_model <- plm(iv_formula, data = card, model = "pooling", inst.method = "baltagi")

# First stage regression for weak instrument testing
first_stage_formula <- as.formula(paste(end.var, "~", paste(c(excl.inst, inc.inst), collapse = " + ")))
first_stage <- lm(first_stage_formula, data = card)

# Test for weak instruments: F-test on excluded instruments only
weak_inst_test <- linearHypothesis(first_stage, excl.inst, test = "F")
cat("\nWeak instruments F-statistic (excluded instruments only):\n")
print(weak_inst_test)

# Display OLS and IV results using Stargazer
stargazer(ols_model, iv_model, first_stage, type = "text", title = "OLS and 2SLS Estimation Results",
          model.names = TRUE, column.labels = c("OLS", "2SLS", "1st stage"))

# Hausman test (Method 1): Using phtest
hausman_test1 <- phtest( iv_model, ols_model)
cat("\nHausman Test (Single Command):\n")
print(hausman_test1)

# =============================================================================
# Hausman test (Method 2): Regression-based test
# Step 1: Generate predicted values pred_educ from the first stage
card$pred_educ <- predict(first_stage)

# Step 2: Estimate second stage regression with predicted values included as regressor
hausman_reg_formula <- as.formula(paste(dep.var, "~", end.var, "+ pred_educ +", paste(inc.inst, collapse = " + ")))
hausman_reg <- lm(hausman_reg_formula, data = card)

# Step 3: Test the significance of the coefficient on pred_educ
hausman_test2 <- coeftest(hausman_reg, vcov = vcovHC(hausman_reg, type = "HC0"))
hausman_test2 <- linearHypothesis(hausman_reg, 
                                  "pred_educ", 
                                  vcov. = vcovHC(hausman_reg, type = "HC0"),
                                  test = "Chisq")

cat("\nHausman Test (Regression-Based):\n")
print(hausman_test2)

#===============================================================================
# Overidentification test: Regression-based test for overidentifying restrictions

# Step 1: Obtain residuals from the 2SLS model
card$residuals_iv <- residuals(iv_model)

# Step 2: Regress the residuals on all instruments (excluded and included)
overid_formula <- as.formula(paste("residuals_iv ~", paste(c(excl.inst, inc.inst), collapse = " + ")))
overid_test <- lm(overid_formula, data = card)

# Step 3: Obtain the R-squared from this regression and compute the test statistic
overid_rsq <- summary(overid_test)$r.squared
overid_stat <- nrow(card) * overid_rsq  # Test statistic for overidentification
overid_pval <- 1 - pchisq(overid_stat, length(excl.inst) - 1)  # p-value

cat("\nOveridentification Test:\n")
cat("Test Statistic:", overid_stat, "\n")
cat("p-value:", overid_pval, "\n")
cat("Interpretation: A high p-value suggests that the instruments are valid (uncorrelated with the error term).\n")

