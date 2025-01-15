# Load necessary libraries ---------------------------------------------
library(wooldridge) # For datasets
library(stargazer)  # For table output
library(plm)        # For fixed effects models
library(dplyr)

# Example 1: Overall significance of the model -------------------------
data("hprice2")

# Unrestricted model
model_1 <- lm(lprice ~ dist + rooms + crime + lnox + stratio, data = hprice2)
stargazer(model_1, type="text")
summary(model_1)
# Manual F-statistic calculation
n <- nrow(hprice2)                     # Number of observations
k <- length(coef(model_1))             # Number of parameters (including intercept)
ssr_ur <- sum(resid(model_1)^2)        # Sum of squared residuals for unrestricted model
ssr_r <- sum((hprice2$lprice - mean(hprice2$lprice))^2) # Restricted model: only intercept
f_stat_manual_1 <- ((ssr_r - ssr_ur) / (k - 1)) / (ssr_ur / (n - k))
p_value_manual_1 <- pf(f_stat_manual_1, df1 = k - 1, df2 = n - k, lower.tail = FALSE)

# Display manual results
stargazer(model_1, type = "text", title = "Overall Significance of the Model")
cat("\nManual F-statistic: ", f_stat_manual_1, "\n")
cat("Manual p-value: ", p_value_manual_1, "\n")

# Example 2: Significance of a single variable -------------------------
# Unrestricted model
model_2_ur <- lm(lprice ~ dist + rooms + crime + lnox + stratio, data = hprice2)
# Restricted model (without lnox)
model_2_r  <- lm(lprice ~ dist + rooms + crime +        stratio, data = hprice2)

# Manual F-statistic calculation
ssr_ur_2 <- sum(resid(model_2_ur)^2)
ssr_r_2 <- sum(resid(model_2_r)^2)
f_stat_manual_2 <- ((ssr_r_2 - ssr_ur_2) / 1) / (ssr_ur_2 / (n - k))
p_value_manual_2 <- pf(f_stat_manual_2, df1 = 1, df2 = n - k, lower.tail = FALSE)

# F-statistic from package using anova()
anova(model_2_r, model_2_ur)

# Display manual results
stargazer(model_2_ur, type = "text", title = "Significance of lnox")
cat("\nManual F-statistic: ", f_stat_manual_2, "\n")
cat("Manual p-value: ", p_value_manual_2, "\n")

# Homework: Do the same for another variable. Compare p-value to the p-value from the t-stat.

# Example 3: Joint significance of a subset of variables ---------------
# Unrestricted model
model_3_ur <- lm(lprice ~ dist + rooms + crime + lnox + stratio, data = hprice2)
# Restricted model (without crime and lnox)
model_3_r  <- lm(lprice ~ dist + rooms                + stratio, data = hprice2)

# Manual F-statistic calculation
ssr_ur_3 <- sum(resid(model_3_ur)^2)
ssr_r_3 <- sum(resid(model_3_r)^2)
f_stat_manual_3 <- ((ssr_r_3 - ssr_ur_3) / 2) / (ssr_ur_3 / (n - k))
p_value_manual_3 <- pf(f_stat_manual_3, df1 = 2, df2 = n - k, lower.tail = FALSE)

# F-statistic from package using anova()
anova(model_3_r, model_3_ur)

# Display results
stargazer(model_3_ur, type = "text", title = "Joint Significance of crime and lnox")
cat("\nManual F-statistic: ", f_stat_manual_3, "\n")
cat("Manual p-value: ", p_value_manual_3, "\n")

# Example 4: Significance of fixed effects -----------------------------
data("wage1")

# Create an occupation factor variable
wage1 <- wage1%>%
  mutate(occupation=case_when( 
    profocc==1 ~ "prof",
    clerocc==1 ~ "cler",
    servocc==1 ~ "serv",
    TRUE       ~ "manu"    #missing occupation, manual labor
  ))


# Restricted model: pooled OLS
ols_model_4 <- plm(lwage ~ exper + as.numeric(educ), 
                   data = wage1, 
                   model = "pooling")

# Unrestricted fixed effects model
fe_model_4 <- plm(lwage ~ as.numeric(educ) + exper, 
                  data = wage1, 
                  model = "within", 
                  index = "occupation")

stargazer(
  fe_model_4,
  ols_model_4,
  type="text"
)
# Manual F-statistic using SSR
ssr_ur_4 <- sum(resid(fe_model_4)^2) # SSR for unrestricted (within) model
ssr_r_4 <- sum(resid(ols_model_4)^2) # SSR for restricted (pooled OLS) model

n <- nrow(wage1) # Number of observations
k <- length(coef(ols_model_4)) # Number of parameters (including intercept)
# Degrees of freedom
n <- nrow(wage1)                           # Number of observations
k <- length(coef(ols_model_4))             # Number of regressors (including intercept)
g <- length(unique(wage1$occupation))              # Number of groups (fixed effects)

df2 <- n - k - g + 1                       # Residual degrees of freedom for unrestricted model
df_resid_r <- n - k                        # Residual degrees of freedom for restricted model
df1 <- df_resid_r - df2                    # Numerator degrees of freedom

f_stat_manual_4_ssr <- ((ssr_r_4 - ssr_ur_4) / df1) / (ssr_ur_4 / df2)
p_value_manual_4_ssr <- pf(f_stat_manual_4_ssr, df1 = df1, df2 = df2, lower.tail = FALSE)

# Display results
cat("\nF-test for Fixed Effects:\n")
cat("Manual F-statistic using SSR: ", f_stat_manual_4_ssr, "\n")
cat("Manual p-value using SSR: ", p_value_manual_4_ssr, "\n")

#DO NOT RUN BELOW THIS IT IS WRONG (something with educ read in as factor)

# Example 5: Significance of interactions with fixed effects ----------
# Unrestricted model with interaction
fe_model_5 <- plm(lwage ~ as.numeric(educ) * occupation, data = wage1, model = "within", index = "occupation")
# Restricted model without interaction
fe_model_5_r <- plm(lwage ~ as.numeric(educ) , data = wage1, model = "within", index = "occupation")

# F-test for interaction
f_test_interaction <- pFtest(fe_model_5, fe_model_5_r)

# Extract p-value
f_stat_manual_5 <- f_test_interaction$statistic[1]
p_value_manual_5 <- f_test_interaction$p.value

# Display results
cat("\nF-test for Significance of Interactions with Fixed Effects:\n")
cat("Manual F-statistic: ", f_stat_manual_5, "\n")
cat("Manual p-value: ", p_value_manual_5, "\n")
print(f_test_interaction)
