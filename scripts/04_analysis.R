# Main Analysis Script
# This script demonstrates common statistical analyses

library(tidyverse)
library(broom)
library(here)

# Example: Load processed data
# data <- read_csv(here("data/processed/cleaned_data.csv"))

# ============================================================
# Descriptive Statistics
# ============================================================

# Summary statistics by group
# summary_stats <- data %>%
#   group_by(group_var) %>%
#   summarise(
#     n = n(),
#     mean = mean(outcome_var, na.rm = TRUE),
#     sd = sd(outcome_var, na.rm = TRUE),
#     median = median(outcome_var, na.rm = TRUE),
#     q25 = quantile(outcome_var, 0.25, na.rm = TRUE),
#     q75 = quantile(outcome_var, 0.75, na.rm = TRUE)
#   )

# ============================================================
# Hypothesis Testing
# ============================================================

# T-test (comparing two groups)
# t_test_result <- t.test(outcome_var ~ group_var, data = data)
# tidy(t_test_result)

# ANOVA (comparing multiple groups)
# anova_result <- aov(outcome_var ~ group_var, data = data)
# summary(anova_result)
# tidy(anova_result)

# Chi-square test (categorical variables)
# chisq_result <- chisq.test(data$var1, data$var2)
# tidy(chisq_result)

# ============================================================
# Correlation Analysis
# ============================================================

# Pearson correlation
# cor_result <- cor.test(data$var1, data$var2)
# tidy(cor_result)

# ============================================================
# Regression Analysis
# ============================================================

# Simple linear regression
# model_simple <- lm(outcome_var ~ predictor_var, data = data)
# summary(model_simple)
# tidy(model_simple)
# glance(model_simple)

# Multiple linear regression
# model_multiple <- lm(outcome_var ~ var1 + var2 + var3, data = data)
# summary(model_multiple)
# tidy(model_multiple)

# Logistic regression (binary outcome)
# model_logistic <- glm(binary_outcome ~ var1 + var2, 
#                       data = data, 
#                       family = binomial)
# summary(model_logistic)
# tidy(model_logistic, exponentiate = TRUE)  # Get odds ratios

# ============================================================
# Model Diagnostics
# ============================================================

# Check residuals
# plot(model_simple)

# Get model predictions
# data_with_predictions <- data %>%
#   mutate(
#     predicted = predict(model_simple, newdata = .),
#     residuals = residuals(model_simple)
#   )

# Save results
# write_csv(summary_stats, here("outputs/tables/summary_statistics.csv"))

cat("Analysis complete!\n")
