# Exploratory Data Analysis Script
# This script demonstrates common EDA techniques

library(tidyverse)
library(skimr)
library(here)

# Example: Load cleaned data
# data <- read_csv(here("data/processed/cleaned_data.csv"))

# Get a quick summary of the data
# skim(data)

# Basic statistics
# summary(data)

# Check data structure
# glimpse(data)
# str(data)

# Check for missing values
# colSums(is.na(data))
# data %>% summarise(across(everything(), ~sum(is.na(.))))

# Frequency tables for categorical variables
# table(data$category_var)
# data %>% count(category_var, sort = TRUE)

# Cross-tabulation
# table(data$var1, data$var2)
# data %>% count(var1, var2)

# Descriptive statistics for numeric variables
# data %>%
#   summarise(
#     mean_val = mean(numeric_var, na.rm = TRUE),
#     median_val = median(numeric_var, na.rm = TRUE),
#     sd_val = sd(numeric_var, na.rm = TRUE),
#     min_val = min(numeric_var, na.rm = TRUE),
#     max_val = max(numeric_var, na.rm = TRUE)
#   )

# Group-wise summaries
# data %>%
#   group_by(category_var) %>%
#   summarise(
#     count = n(),
#     mean_val = mean(numeric_var, na.rm = TRUE),
#     median_val = median(numeric_var, na.rm = TRUE)
#   )

# Correlation matrix for numeric variables
# data %>%
#   select(where(is.numeric)) %>%
#   cor(use = "complete.obs")

cat("Exploratory data analysis complete!\n")
