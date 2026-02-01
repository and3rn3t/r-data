# Data Cleaning Script
# This script demonstrates common data cleaning operations

library(tidyverse)
library(janitor)
library(here)

# Example: Load data (uncomment and modify as needed)
# data_raw <- read_csv(here("data/raw/your_file.csv"))

# Clean column names (remove spaces, special characters, etc.)
# data_clean <- data_raw %>%
#   clean_names()

# Remove duplicates
# data_clean <- data_clean %>%
#   distinct()

# Handle missing values
# Option 1: Remove rows with any missing values
# data_clean <- data_clean %>%
#   drop_na()

# Option 2: Remove rows with missing values in specific columns
# data_clean <- data_clean %>%
#   drop_na(column1, column2)

# Option 3: Impute missing values
# data_clean <- data_clean %>%
#   mutate(column1 = replace_na(column1, mean(column1, na.rm = TRUE)))

# Filter rows
# data_clean <- data_clean %>%
#   filter(condition)

# Select specific columns
# data_clean <- data_clean %>%
#   select(col1, col2, col3)

# Create new variables
# data_clean <- data_clean %>%
#   mutate(
#     new_col = existing_col * 2,
#     category = case_when(
#       value < 10 ~ "Low",
#       value < 20 ~ "Medium",
#       TRUE ~ "High"
#     )
#   )

# Convert data types
# data_clean <- data_clean %>%
#   mutate(
#     date_col = as.Date(date_col),
#     factor_col = as.factor(factor_col),
#     numeric_col = as.numeric(numeric_col)
#   )

# Save cleaned data
# write_csv(data_clean, here("data/processed/cleaned_data.csv"))

cat("Data cleaning complete!\n")
# cat("Original rows:", nrow(data_raw), "\n")
# cat("Cleaned rows:", nrow(data_clean), "\n")
