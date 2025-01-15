remove(list=ls()) #Clear out the environment

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Define degrees of freedom for numerator and denominator
num_dfs <- c(3, 5, 10, 20) # For the second graph
den_dfs <- c(5, 10, 30, 1000) # For the first graph

# Define a range of x values for the F-distributions
x_vals <- seq(0, 6, length.out = 200)

# Generate data for varying denominator degrees of freedom
f_data_den <- data.frame(
  x = x_vals
) %>%
  expand_grid(den_df = den_dfs, num_df = 5) %>% # Fix numerator df = 5
  mutate(density = df(x, num_df, den_df))

# Generate data for varying numerator degrees of freedom
f_data_num <- data.frame(
  x = x_vals
) %>%
  expand_grid(num_df = num_dfs, den_df = 10) %>% # Fix denominator df = 10
  mutate(density = df(x, num_df, den_df))

# Calculate critical values for 95% confidence
crit_values_den <- data.frame(
  den_df = den_dfs,
  num_df = 5,
  crit_value = qf(0.95, df1 = 5, df2 = den_dfs) # 95% critical value
)

crit_values_num <- data.frame(
  num_df = num_dfs,
  den_df = 10,
  crit_value = qf(0.95, df1 = num_dfs, df2 = 10) # 95% critical value
)

# Plot 1: Varying Denominator Degrees of Freedom
plot_den <- ggplot(f_data_den, aes(x = x, y = density, color = factor(den_df))) +
  geom_line(size = .7) +
  # Add vertical lines for critical values
  geom_vline(
    data = crit_values_den,
    aes(xintercept = crit_value, color = factor(den_df)),
    linetype = "dashed", size = 0.5
  ) +
  scale_color_brewer(palette = "Set1", name = "Denom.\ndf") +
  labs(
    title = "F-Distributions\n(Varying Denominator df)",
    x = "F-value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

# Plot 2: Varying Numerator Degrees of Freedom
plot_num <- ggplot(f_data_num, aes(x = x, y = density, color = factor(num_df))) +
  geom_line(size = .7) +
  # Add vertical lines for critical values
  geom_vline(
    data = crit_values_num,
    aes(xintercept = crit_value, color = factor(num_df)),
    linetype = "dashed", size = 0.5
  ) +
  scale_color_brewer(palette = "Set1", name = "Numer.\ndf") +
  labs(
    title = "F-Distributions\n(Varying Numerator df)",
    x = "F-value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

# Print the plots
print(plot_den)
print(plot_num)
