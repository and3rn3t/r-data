# Test Helper Functions
# Shared fixtures and utilities for testthat tests
# =================================================

library(tidyverse)

# =============================================================================
# MOCK DATA GENERATORS
# =============================================================================

#' Create mock city data for testing
#' 
#' @param n Number of cities to generate
#' @param seed Random seed for reproducibility
#' @return Data frame with mock city data
create_mock_city_data <- function(n = 5, seed = 42) {
  set.seed(seed)
  tibble(
    city = paste0("TestCity", seq_len(n)),
    county = paste0("County", seq_len(n)),
    population = sample(10000:200000, n),
    population_2020 = sample(10000:200000, n),
    latitude = runif(n, 40.5, 43.5),
    longitude = runif(n, -96.5, -90.5),
    region = sample(c("Central", "Eastern", "Western", "Northern"), n, replace = TRUE)
  )
}

#' Create mock crime data for testing
#' 
#' @param cities Vector of city names
#' @param seed Random seed
#' @return Data frame with mock crime data
create_mock_crime_data <- function(cities, seed = 42) {
  set.seed(seed)
  n <- length(cities)
  tibble(
    city = cities,
    violent_crime_rate = runif(n, 50, 600),
    property_crime_rate = runif(n, 500, 3000),
    officers_per_1000 = runif(n, 1.5, 4.0)
  )
}

#' Create mock housing data for testing
#' 
#' @param cities Vector of city names
#' @param seed Random seed
#' @return Data frame with mock housing data
create_mock_housing_data <- function(cities, seed = 42) {
  set.seed(seed)
  n <- length(cities)
  tibble(
    city = cities,
    median_home_value = sample(80000:350000, n),
    median_rent = sample(700:1500, n),
    owner_occupied_pct = runif(n, 55, 80)
  )
}

#' Create mock scores for testing
#' 
#' @param n Number of cities
#' @param seed Random seed
#' @return Data frame with mock scores
create_mock_scores <- function(n = 5, seed = 42) {
  set.seed(seed)
  tibble(
    city = paste0("TestCity", seq_len(n)),
    safety_score = runif(n, 30, 100),
    housing_score = runif(n, 30, 100),
    education_score = runif(n, 30, 100),
    economic_score = runif(n, 30, 100),
    healthcare_score = runif(n, 30, 100),
    overall_score = runif(n, 40, 90),
    rank = seq_len(n)
  )
}

#' Create complete mock dataset list for testing
#' 
#' @param n Number of cities
#' @param seed Random seed
#' @return Named list of mock data frames
create_mock_datasets <- function(n = 5, seed = 42) {
  cities_df <- create_mock_city_data(n, seed)
  cities <- cities_df$city
  
  list(
    major_cities = cities_df,
    crime = create_mock_crime_data(cities, seed),
    housing = create_mock_housing_data(cities, seed),
    education = tibble(
      city = cities,
      graduation_rate = runif(n, 80, 98),
      pct_bachelors = runif(n, 20, 50)
    ),
    economic = tibble(
      city = cities,
      median_household_income = sample(40000:100000, n),
      unemployment_rate = runif(n, 2, 8)
    ),
    healthcare = tibble(
      city = cities,
      life_expectancy = runif(n, 75, 82),
      health_insurance_coverage_pct = runif(n, 85, 98)
    ),
    amenities = tibble(
      city = cities,
      livability_score = runif(n, 50, 90),
      cost_of_living_index = runif(n, 85, 115)
    )
  )
}

# =============================================================================
# TEST UTILITIES
# =============================================================================

#' Check if a data frame has expected columns
#' 
#' @param df Data frame to check
#' @param expected_cols Vector of expected column names
#' @return TRUE if all columns present, FALSE otherwise
has_expected_columns <- function(df, expected_cols) {
  all(expected_cols %in% names(df))
}

#' Check if scores are in valid range (0-100)
#' 
#' @param scores Numeric vector of scores
#' @return TRUE if all scores in range
scores_in_range <- function(scores) {
  all(scores >= 0 & scores <= 100, na.rm = TRUE)
}

#' Create a temporary test directory
#' 
#' @return Path to temporary directory
create_test_dir <- function() {
  dir <- tempfile("test_")
  dir.create(dir)
  dir
}

#' Clean up temporary test files
#' 
#' @param path Path to clean up
cleanup_test_dir <- function(path) {
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
  }
}
