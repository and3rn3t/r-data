# Data Validation Script
# Validates data quality and integrity using pointblank
# ======================================================

library(tidyverse)
library(here)

# Install pointblank if not available
if (!require("pointblank")) {
  install.packages("pointblank")
  library(pointblank)
}

# =============================================================================
# Validation Functions
# =============================================================================

#' Validate a data frame and return a report
#' 
#' @param df Data frame to validate
#' @param name Name for the validation report
#' @return pointblank agent with validation results
validate_iowa_cities <- function(df, name = "Iowa Cities Data") {
  
  agent <- df %>%
    create_agent(
      tbl_name = name,
      actions = action_levels(warn_at = 0.05, stop_at = 0.10)
    ) %>%
    # Check for required columns
    col_exists(columns = vars(city)) %>%
    col_exists(columns = vars(population)) %>%
    
    # Check data types
    col_is_character(columns = vars(city)) %>%
    col_is_numeric(columns = vars(population)) %>%
    
    # Check value constraints
    col_vals_gt(columns = vars(population), value = 0) %>%
    col_vals_lt(columns = vars(population), value = 500000) %>%
    
    # Check for duplicates
    rows_distinct() %>%
    
    # Check for missing values in key columns
    col_vals_not_null(columns = vars(city)) %>%
    
    interrogate()
  
  return(agent)
}

#' Quick data quality summary
#' 
#' @param df Data frame to summarize
#' @return Tibble with quality metrics
data_quality_summary <- function(df) {
  tibble(
    metric = c(
      "Total Rows",
      "Total Columns",
      "Complete Rows",
      "Complete Row %",
      "Duplicate Rows",
      "Columns with NA"
    ),
    value = c(
      nrow(df),
      ncol(df),
      sum(complete.cases(df)),
      round(sum(complete.cases(df)) / nrow(df) * 100, 1),
      nrow(df) - nrow(distinct(df)),
      sum(colSums(is.na(df)) > 0)
    )
  )
}

# =============================================================================
# Run Validations
# =============================================================================

if (interactive()) {
  cat("Data Validation Script\n")
  cat("======================\n\n")
  
  # Validate clean Iowa cities data
  if (file.exists(here("data/processed/iowa_cities_clean.csv"))) {
    cat("Validating iowa_cities_clean.csv...\n")
    
    iowa_cities <- read_csv(
      here("data/processed/iowa_cities_clean.csv"),
      show_col_types = FALSE
    )
    
    # Quick quality summary
    cat("\nData Quality Summary:\n")
    print(data_quality_summary(iowa_cities))
    
    # Detailed validation if city and population columns exist
    if (all(c("city", "population") %in% tolower(names(iowa_cities)))) {
      names(iowa_cities) <- tolower(names(iowa_cities))
      agent <- validate_iowa_cities(iowa_cities)
      
      cat("\nValidation Results:\n")
      print(agent)
    }
  } else {
    cat("No processed data found. Run cleaning scripts first.\n")
  }
}

cat("\nValidation functions loaded. Use:\n")
cat("  - validate_iowa_cities(df) for full validation\n")
cat("  - data_quality_summary(df) for quick summary\n")
