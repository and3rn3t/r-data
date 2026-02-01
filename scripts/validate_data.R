# Data Validation Script
# Validates data quality and integrity using pointblank
# ======================================================

library(tidyverse)
library(here)
library(pointblank)

source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

# =============================================================================
# Validation Functions for Each Dataset
# =============================================================================

#' Validate major cities base data
#' @param data Data frame to validate
#' @return pointblank agent with validation results
validate_major_cities <- function(data) {
  create_agent(data, tbl_name = "iowa_major_cities") %>%
    col_exists(columns = c("city", "county", "population_2020", "latitude", "longitude", "region")) %>%
    rows_distinct(columns = c("city")) %>%
    col_vals_gt(columns = population_2020, value = 0) %>%
    col_vals_between(columns = latitude, left = 40.4, right = 43.5) %>%
    col_vals_between(columns = longitude, left = -96.6, right = -90.1) %>%
    col_vals_in_set(columns = region, set = c("Central", "Eastern", "Western", "Northern", "Southern", "Metro")) %>%
    col_vals_not_null(columns = c("city", "county", "population_2020")) %>%
    interrogate()
}

#' Validate crime data
#' @param data Data frame to validate
#' @return pointblank agent with validation results
validate_crime_data <- function(data) {
  create_agent(data, tbl_name = "iowa_crime_data") %>%
    col_exists(columns = c("city", "violent_crime_rate", "property_crime_rate")) %>%
    col_vals_gte(columns = violent_crime_rate, value = 0) %>%
    col_vals_gte(columns = property_crime_rate, value = 0) %>%
    col_vals_lt(columns = violent_crime_rate, value = 5000) %>%
    col_vals_lt(columns = property_crime_rate, value = 20000) %>%
    col_vals_not_null(columns = c("city", "violent_crime_rate", "property_crime_rate")) %>%
    interrogate()
}

#' Validate housing data
#' @param data Data frame to validate
#' @return pointblank agent with validation results
validate_housing_data <- function(data) {
  create_agent(data, tbl_name = "iowa_housing_data") %>%
    col_exists(columns = c("city", "median_home_value", "median_rent", "owner_occupied_pct")) %>%
    col_vals_between(columns = median_home_value, left = 50000, right = 1000000) %>%
    col_vals_between(columns = median_rent, left = 300, right = 3000) %>%
    col_vals_between(columns = owner_occupied_pct, left = 0, right = 100) %>%
    col_vals_not_null(columns = c("city", "median_home_value")) %>%
    interrogate()
}

#' Validate education data
#' @param data Data frame to validate
#' @return pointblank agent with validation results
validate_education_data <- function(data) {
  create_agent(data, tbl_name = "iowa_education_data") %>%
    col_exists(columns = c("city", "graduation_rate", "college_readiness_pct", "pct_bachelors")) %>%
    col_vals_between(columns = graduation_rate, left = 0, right = 100) %>%
    col_vals_between(columns = college_readiness_pct, left = 0, right = 100) %>%
    col_vals_between(columns = pct_bachelors, left = 0, right = 100) %>%
    col_vals_not_null(columns = c("city", "graduation_rate")) %>%
    interrogate()
}

#' Validate economic data
#' @param data Data frame to validate
#' @return pointblank agent with validation results
validate_economic_data <- function(data) {
  create_agent(data, tbl_name = "iowa_economic_data") %>%
    col_exists(columns = c("city", "median_household_income", "unemployment_rate", "poverty_rate")) %>%
    col_vals_gt(columns = median_household_income, value = 0) %>%
    col_vals_between(columns = unemployment_rate, left = 0, right = 50) %>%
    col_vals_between(columns = poverty_rate, left = 0, right = 100) %>%
    col_vals_not_null(columns = c("city", "median_household_income")) %>%
    interrogate()
}

#' Validate healthcare data
#' @param data Data frame to validate
#' @return pointblank agent with validation results
validate_healthcare_data <- function(data) {
  create_agent(data, tbl_name = "iowa_healthcare_data") %>%
    col_exists(columns = c("city", "life_expectancy", "health_insurance_coverage_pct")) %>%
    col_vals_between(columns = life_expectancy, left = 60, right = 95) %>%
    col_vals_between(columns = health_insurance_coverage_pct, left = 0, right = 100) %>%
    col_vals_not_null(columns = c("city", "life_expectancy")) %>%
    interrogate()
}

#' Legacy validation for processed data
#' @param df Data frame to validate
#' @param name Name for the validation report
#' @return pointblank agent with validation results
validate_iowa_cities <- function(df, name = "Iowa Cities Data") {
  
  agent <- df %>%
    create_agent(
      tbl_name = name,
      actions = action_levels(warn_at = 0.05, stop_at = 0.10)
    ) %>%
    col_exists(columns = vars(city)) %>%
    col_exists(columns = vars(population)) %>%
    col_is_character(columns = vars(city)) %>%
    col_is_numeric(columns = vars(population)) %>%
    col_vals_gt(columns = vars(population), value = 0) %>%
    col_vals_lt(columns = vars(population), value = 500000) %>%
    rows_distinct() %>%
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
# Main Validation Runner
# =============================================================================

#' Run all data validations and generate report
#' @param output_dir Directory to save validation reports
#' @return List of validation results
run_all_validations <- function(output_dir = here("outputs/validation")) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Starting data validation...\n")
  cat("==========================\n\n")
  
  results <- list()
  
  datasets <- list(
    major_cities = list(file = "iowa_major_cities.csv", validator = validate_major_cities),
    crime = list(file = "iowa_crime_data.csv", validator = validate_crime_data),
    housing = list(file = "iowa_housing_data.csv", validator = validate_housing_data),
    education = list(file = "iowa_education_data.csv", validator = validate_education_data),
    economic = list(file = "iowa_economic_data.csv", validator = validate_economic_data),
    healthcare = list(file = "iowa_healthcare_data.csv", validator = validate_healthcare_data)
  )
  
  for (name in names(datasets)) {
    file_path <- here("data/raw", datasets[[name]]$file)
    
    if (file.exists(file_path)) {
      cat(sprintf("Validating %s...\n", name))
      data <- safe_read_csv(file_path)
      
      if (!is.null(data)) {
        agent <- datasets[[name]]$validator(data)
        results[[name]] <- agent
        
        report <- get_agent_report(agent, display_table = FALSE)
        pass_rate <- mean(report$all_passed, na.rm = TRUE) * 100
        cat(sprintf("  ✓ %s: %.1f%% tests passed\n", name, pass_rate))
        
        tryCatch({
          export_report(agent, filename = file.path(output_dir, paste0(name, "_validation.html")))
        }, error = function(e) {
          cat(sprintf("  ⚠ Could not export report: %s\n", e$message))
        })
      }
    } else {
      cat(sprintf("  ⚠ %s: File not found\n", name))
    }
  }
  
  cat("\n==========================\n")
  cat("Validation complete!\n")
  cat(sprintf("Reports saved to: %s\n", output_dir))
  
  return(results)
}

# =============================================================================
# Run Validations
# =============================================================================

if (interactive()) {
  cat("Data Validation Script\n")
  cat("======================\n\n")
  
  # Run comprehensive validations if raw data exists
  if (file.exists(here("data/raw/iowa_major_cities.csv"))) {
    results <- run_all_validations()
  } else if (file.exists(here("data/processed/iowa_cities_clean.csv"))) {
    cat("Validating iowa_cities_clean.csv...\n")
    
    iowa_cities <- read_csv(
      here("data/processed/iowa_cities_clean.csv"),
      show_col_types = FALSE
    )
    
    cat("\nData Quality Summary:\n")
    print(data_quality_summary(iowa_cities))
    
    if (all(c("city", "population") %in% tolower(names(iowa_cities)))) {
      names(iowa_cities) <- tolower(names(iowa_cities))
      agent <- validate_iowa_cities(iowa_cities)
      cat("\nValidation Results:\n")
      print(agent)
    }
  } else {
    cat("No data found. Run data import scripts first.\n")
  }
}

cat("\nValidation functions loaded. Use:\n")
cat("  - run_all_validations() for comprehensive validation\n")
cat("  - validate_iowa_cities(df) for single dataset\n")
cat("  - data_quality_summary(df) for quick summary\n")
