# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Define degrees of freedom for chi-squared distributions
dfs <- c(5, 10, 20, 50)

# Define a range of x values for the chi-squared distributions
x_vals <- seq(0, 100, length.out = 200)

# Generate data for chi-squared distributions with varying df
chi_data <- data.frame(
  x = x_vals
) %>%
  expand_grid(df = dfs) %>%
  mutate(density = dchisq(x, df))

# Calculate critical values for 95% confidence
crit_values <- data.frame(
  df = dfs,
  crit_value = qchisq(0.95, dfs) # 95% critical value
)

# Plot 1: Chi-squared distributions with varying df
ggplot(chi_data, aes(x = x, y = density, color = factor(df))) +
  geom_line(size = .7) +
  # Add vertical lines for critical values
  geom_vline(
    data = crit_values,
    aes(xintercept = crit_value, color = factor(df)),
    linetype = "dashed", size = 0.5
  ) +
  scale_color_brewer(palette = "Set1", name = "Degrees\nof\nFreedom") +
  labs(
    title = "Chi-squared Distributions with\nVarying Degrees of Freedom",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)


