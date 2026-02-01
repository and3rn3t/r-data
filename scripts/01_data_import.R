# Data Import Script
# This script demonstrates how to import various data formats

# Load required packages
library(tidyverse)
library(readxl)
library(haven)
library(here)

# Set working directory to project root
# The here package ensures paths work regardless of where R is run from
project_root <- here()

# Example: Import CSV file
# data_csv <- read_csv(here("data/raw/your_file.csv"))

# Example: Import Excel file
# data_excel <- read_excel(here("data/raw/your_file.xlsx"), sheet = 1)

# Example: Import SPSS/Stata/SAS files
# data_spss <- read_spss(here("data/raw/your_file.sav"))
# data_stata <- read_stata(here("data/raw/your_file.dta"))
# data_sas <- read_sas(here("data/raw/your_file.sas7bdat"))

# Example: Import from URL
# data_url <- read_csv("https://example.com/data.csv")

# Example: Import multiple files
# file_list <- list.files(here("data/raw"), pattern = "*.csv", full.names = TRUE)
# data_list <- lapply(file_list, read_csv)
# combined_data <- bind_rows(data_list)

# Print summary
cat("Data import complete!\n")
# cat("Dimensions:", dim(data_csv), "\n")
# glimpse(data_csv)
