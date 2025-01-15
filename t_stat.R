remove(list=ls()) #Clear out the environment

# Load necessary libraries ---------------------------------------------
library(wooldridge) # For the dataset
library(ggplot2)    # For plotting
library(stargazer)  # For the table
library(sandwich)   # For robust standard errors
library(lmtest)     # For coeftest with robust SE

# ================================================================
# Load the dataset 
data("wage1") # Wooldridge's wage1 dataset
# Fit a linear regression model -
model <- lm(lwage ~ educ + exper + tenure  + married, data = wage1)
# ================================================================

# Homework: Try a different data set. hprice2


# Extract coefficients and standard errors -----------------------------
coefficients <- coef(model)
std_errors <- sqrt(diag(vcov(model)))
# List the var-cov matrix var(b)
round(vcov(model),digits=6)
# Calculate robust standard errors -------------------------------------
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))

# Save results using coeftest (robust SE) ---------------------------
model_robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
# Present results in a stargazer table ---------------------------------
stargazer(model, 
          model_robust, 
          type = "text", 
          column.labels = c("OLS", "Robust"),
          title = "Regression Results")


# Calculate t-statistics and p-values ----------------------------------
t_values <- coefficients / std_errors
t_values_robust <- coefficients / robust_se
degrees_of_freedom <- df.residual(model)
p_values <- 2 * pt(-abs(t_values), df = degrees_of_freedom)
p_values_robust <- 2 * pt(-abs(t_values_robust), df = degrees_of_freedom)

# Testing coefficients against specific values (e.g., 1 or 1.5) --------
test_values <- c(1, -1) # Example test values
t_values_against <- sapply(test_values, function(value) {
  (coefficients - value) / std_errors
})
t_values_against_robust <- sapply(test_values, function(value) {
  (coefficients - value) / robust_se
})
p_values_against <- sapply(seq_along(test_values), function(i) {
  2 * pt(-abs(t_values_against[, i]), df = degrees_of_freedom)
})
p_values_against_robust <- sapply(seq_along(test_values), function(i) {
  2 * pt(-abs(t_values_against_robust[, i]), df = degrees_of_freedom)
})

# Combine results into a dataframe -------------------------------------
results <- data.frame(
  Term = names(coefficients),
  Coefficient = coefficients,
  Std_Error = std_errors,
  Robust_SE = robust_se,
  T_Value = t_values,
  T_Value_Robust = t_values_robust,
  P_Value = p_values,
  P_Value_Robust = p_values_robust
)

print(results)

# Add tests against specific values to the results
for (i in seq_along(test_values)) {
  results[paste0("T_Value_Test_", test_values[i])] <- t_values_against[, i]
  results[paste0("P_Value_Test_", test_values[i])] <- p_values_against[, i]
  results[paste0("T_Value_Test_Robust_", test_values[i])] <- t_values_against_robust[, i]
  results[paste0("P_Value_Test_Robust_", test_values[i])] <- p_values_against_robust[, i]
}

print(results)


# Visualize t-distribution for one coefficient -------------------------
# List names of variables used in the model
names(coefficients(model))[-1]
# Example for "educ"
term_to_plot <- "educ"
t_value <- t_values[term_to_plot]
t_value_robust <- t_values_robust[term_to_plot]
critical_region <- qt(0.975, df = degrees_of_freedom) # 95% two-tailed

# Create the t-distribution plot
t_dist_plot <- ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dt, args = list(df = degrees_of_freedom), color = "navy") +
  geom_vline(xintercept = t_value, color = "maroon", linetype = "dashed", size = 1) +
  geom_vline(xintercept = t_value_robust, color = "darkgreen", linetype = "dashed", size = 1) +
  geom_vline(xintercept = -critical_region, color = "darkred", linetype = "dotted") +
  geom_vline(xintercept = critical_region, color = "darkred", linetype = "dotted") +
  annotate("text", x = t_value, y = 0.1, label = paste0("t (OLS) = ", round(t_value, 2)),
           hjust = 1.05, color = "maroon") +
  annotate("text", x = t_value_robust, y = 0.15, label = paste0("t (Robust) = ", round(t_value_robust, 2)),
           hjust = 1.05, color = "darkgreen") +
  labs(
    title = paste("T-Distribution for Term:", term_to_plot),
    x = "t-value",
    y = "Density"
  ) +
  theme_minimal()

# Print the plot
print(t_dist_plot)

# Plot all t-values for estimated coefficients -------------------------
t_values_df <- data.frame(
  Term = names(t_values),
  T_Value = t_values,
  T_Value_Robust = t_values_robust
)

# Create the plot
all_t_values_plot <- ggplot(t_values_df, aes(x = Term)) +
  geom_bar(aes(y = T_Value), stat = "identity", fill = "maroon", alpha = 0.7) +
  geom_bar(aes(y = T_Value_Robust), stat = "identity", fill = "darkgreen", alpha = 0.3) +
  geom_hline(yintercept = qt(0.975, df = degrees_of_freedom), color = "red", linetype = "dashed") +
  geom_hline(yintercept = -qt(0.975, df = degrees_of_freedom), color = "red", linetype = "dashed") +
  labs(
    title = "T-Values (OLS and Robust) for All Estimated Coefficients",
    x = "Coefficient",
    y = "T-Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(all_t_values_plot)

