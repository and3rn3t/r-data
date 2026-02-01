# Visualization Script
# This script demonstrates common data visualizations with ggplot2

library(tidyverse)
library(patchwork)
library(here)

# Example: Load data
# data <- read_csv(here("data/processed/cleaned_data.csv"))

# ============================================================
# Univariate Plots
# ============================================================

# Histogram
# p1 <- ggplot(data, aes(x = numeric_var)) +
#   geom_histogram(bins = 30, fill = "steelblue", color = "white") +
#   labs(title = "Distribution of Variable",
#        x = "Variable Name",
#        y = "Count")

# Density plot
# p2 <- ggplot(data, aes(x = numeric_var)) +
#   geom_density(fill = "steelblue", alpha = 0.5) +
#   labs(title = "Density Plot",
#        x = "Variable Name",
#        y = "Density")

# Bar plot (categorical variable)
# p3 <- data %>%
#   count(category_var) %>%
#   ggplot(aes(x = reorder(category_var, n), y = n)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(title = "Frequency of Categories",
#        x = "Category",
#        y = "Count")

# ============================================================
# Bivariate Plots
# ============================================================

# Scatter plot
# p4 <- ggplot(data, aes(x = var1, y = var2)) +
#   geom_point(alpha = 0.6, color = "steelblue") +
#   geom_smooth(method = "lm", color = "red") +
#   labs(title = "Relationship between Variables",
#        x = "Variable 1",
#        y = "Variable 2")

# Box plot
# p5 <- ggplot(data, aes(x = category_var, y = numeric_var)) +
#   geom_boxplot(fill = "steelblue", alpha = 0.7) +
#   labs(title = "Distribution by Category",
#        x = "Category",
#        y = "Value")

# Violin plot
# p6 <- ggplot(data, aes(x = category_var, y = numeric_var)) +
#   geom_violin(fill = "steelblue", alpha = 0.7) +
#   geom_boxplot(width = 0.1, fill = "white") +
#   labs(title = "Distribution by Category",
#        x = "Category",
#        y = "Value")

# Line plot (time series or trends)
# p7 <- ggplot(data, aes(x = time_var, y = value_var)) +
#   geom_line(color = "steelblue", size = 1) +
#   geom_point(color = "steelblue") +
#   labs(title = "Trend Over Time",
#        x = "Time",
#        y = "Value")

# ============================================================
# Multivariate Plots
# ============================================================

# Grouped bar plot
# p8 <- data %>%
#   count(var1, var2) %>%
#   ggplot(aes(x = var1, y = n, fill = var2)) +
#   geom_col(position = "dodge") +
#   labs(title = "Grouped Bar Plot",
#        x = "Category 1",
#        y = "Count",
#        fill = "Category 2")

# Faceted plot
# p9 <- ggplot(data, aes(x = var1, y = var2)) +
#   geom_point(color = "steelblue") +
#   facet_wrap(~category_var) +
#   labs(title = "Relationship by Category",
#        x = "Variable 1",
#        y = "Variable 2")

# ============================================================
# Combine Multiple Plots
# ============================================================

# Using patchwork to combine plots
# combined_plot <- (p1 | p2) / (p4 | p5)

# ============================================================
# Save Plots
# ============================================================

# Save individual plot
# ggsave(here("outputs/figures/plot1.png"), 
#        plot = p1, 
#        width = 8, 
#        height = 6, 
#        dpi = 300)

# Save combined plot
# ggsave(here("outputs/figures/combined_plot.png"), 
#        plot = combined_plot, 
#        width = 12, 
#        height = 8, 
#        dpi = 300)

cat("Visualizations complete!\n")
