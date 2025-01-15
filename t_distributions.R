remove(list=ls()) #Clear out the environment

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Define degrees of freedom
dfs <- c(3, 10, 30, 1000000)

# Generate data for t-distributions
t_data <- data.frame(
  x = seq(-5, 5, length.out = 100)
) %>%
  expand_grid(df = dfs) %>%
  mutate(density = dt(x, df))

# Calculate critical t-values for 95% confidence intervals
crit_values <- data.frame(
  df = dfs,
  t_critical = qt(0.975, dfs) # 95% confidence level
)

# Plot the t-distributions
ggplot(t_data, aes(x = x, y = density, color = factor(df))) +
  geom_line(size = .6) +
  # Add vertical lines for critical t-values
  geom_vline(
    data = crit_values,
    aes(xintercept = t_critical, color = factor(df)),
    linetype = "dashed", size = 0.5
  ) +
  geom_vline(
    data = crit_values,
    aes(xintercept = -t_critical, color = factor(df)),
    linetype = "dashed", size = 0.5
  ) +
  scale_color_brewer(palette = "Set1", name = "Degrees\nof\nFreedom") +
  labs(
    title = "T-Distributions with\n95% Confidence Intervals",
    x = "t-value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

