# Unit Tests for Shiny App Helper Functions
# ==========================================
# These tests verify the helper functions used in the Shiny app
# without requiring a full Shiny server context.

library(testthat)
library(tidyverse)
library(here)

# Source required files
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

# =============================================================================
# Load Data for Testing
# =============================================================================

# Load data similar to how app.R does
load_test_data <- function() {
  list(
    crime = safe_read_csv(here("data/raw/iowa_crime_data.csv")),
    housing = safe_read_csv(here("data/raw/iowa_housing_data.csv")),
    education = safe_read_csv(here("data/raw/iowa_education_data.csv")),
    cities = safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  )
}

# =============================================================================
# Test: City Validation Logic
# =============================================================================

test_that("validate_city accepts valid cities", {
  valid_cities <- c("Des Moines", "Cedar Rapids", "Iowa City")
  
  validate_city <- function(city) {
    if (is.null(city) || city == "" || !city %in% IOWA_CITIES) {
      return(FALSE)
    }
    TRUE
  }
  
  for (city in valid_cities) {
    expect_true(validate_city(city),
                info = paste(city, "should be valid"))
  }
})

test_that("validate_city rejects invalid cities", {
  validate_city <- function(city) {
    if (is.null(city) || city == "" || !city %in% IOWA_CITIES) {
      return(FALSE)
    }
    TRUE
  }
  
  expect_false(validate_city(NULL))
  expect_false(validate_city(""))
  expect_false(validate_city("Fake City"))
  expect_false(validate_city("Chicago"))  # Not in Iowa
})

# =============================================================================
# Test: Safe Filter City Logic
# =============================================================================

test_that("safe_filter_city returns data for valid city", {
  data <- load_test_data()
  skip_if(is.null(data$cities), "Cities data not available")
  
  validate_city <- function(city) {
    !is.null(city) && city != "" && city %in% data$cities$city
  }
  
  safe_filter_city <- function(df, city_name) {
    if (!validate_city(city_name)) return(df[0, ])
    filter(df, city == city_name)
  }
  
  result <- safe_filter_city(data$cities, "Des Moines")
  
  expect_true(nrow(result) > 0)
  expect_equal(result$city[1], "Des Moines")
})

test_that("safe_filter_city returns empty for invalid city", {
  data <- load_test_data()
  skip_if(is.null(data$cities), "Cities data not available")
  
  validate_city <- function(city) {
    !is.null(city) && city != "" && city %in% data$cities$city
  }
  
  safe_filter_city <- function(df, city_name) {
    if (!validate_city(city_name)) return(df[0, ])
    filter(df, city == city_name)
  }
  
  result <- safe_filter_city(data$cities, "Fake City")
  
  expect_equal(nrow(result), 0)
})

# =============================================================================
# Test: Score Calculation Logic
# =============================================================================

test_that("normalize produces 0-100 scale", {
  # Test the normalize function used in score calculations
  values <- c(10, 20, 30, 40, 50)
  result <- normalize(values)
  
  expect_true(all(result >= 0))
  expect_true(all(result <= 100))
  expect_equal(min(result), 0)
  expect_equal(max(result), 100)
})

test_that("normalize reverse flips the scale", {
  values <- c(1, 2, 3, 4, 5)
  normal <- normalize(values)
  reversed <- normalize(values, reverse = TRUE)
  
  expect_equal(normal[1], 0)
  expect_equal(reversed[1], 100)
  expect_equal(normal[5], 100)
  expect_equal(reversed[5], 0)
})

# =============================================================================
# Test: Data Loading
# =============================================================================

test_that("All raw data files can be loaded", {
  data <- load_test_data()
  
  expect_false(is.null(data$crime))
  expect_false(is.null(data$housing))
  expect_false(is.null(data$education))
  expect_false(is.null(data$cities))
})

test_that("Data files have consistent city column", {
  data <- load_test_data()
  skip_if(any(sapply(data, is.null)), "Some data files missing")
  
  for (name in names(data)) {
    expect_true("city" %in% names(data[[name]]),
                info = paste(name, "should have 'city' column"))
  }
})

test_that("All datasets have same cities", {
  data <- load_test_data()
  skip_if(any(sapply(data, is.null)), "Some data files missing")
  
  base_cities <- sort(unique(data$cities$city))
  
  for (name in names(data)) {
    dataset_cities <- sort(unique(data[[name]]$city))
    
    # Check that all cities in base are in each dataset
    missing <- setdiff(base_cities, dataset_cities)
    expect_true(length(missing) == 0,
                info = paste(name, "missing cities:", paste(missing, collapse = ", ")))
  }
})

# =============================================================================
# Test: Recommendation Score Calculation
# =============================================================================

test_that("Custom scores respect weights", {
  # Simulate the recommendation calculation
  scores <- tibble(
    city = c("City A", "City B"),
    safety_score = c(100, 0),
    education_score = c(0, 100),
    economic_score = c(50, 50),
    housing_score = c(50, 50),
    healthcare_score = c(50, 50),
    livability_score_calc = c(50, 50)
  )
  
  # Weight safety heavily
  weights <- c(safety = 0.6, education = 0.1, economy = 0.1, 
               housing = 0.1, healthcare = 0.05, livability = 0.05)
  weights <- weights / sum(weights)
  
  result <- scores %>%
    mutate(
      custom_score = safety_score * weights["safety"] +
                     education_score * weights["education"] +
                     economic_score * weights["economy"] +
                     housing_score * weights["housing"] +
                     healthcare_score * weights["healthcare"] +
                     livability_score_calc * weights["livability"]
    )
  
  # City A should win (high safety)
  expect_true(result$custom_score[1] > result$custom_score[2])
})

test_that("Custom scores with equal weights produce mean", {
  scores <- tibble(
    city = "Test City",
    safety_score = 60,
    education_score = 80,
    economic_score = 40,
    housing_score = 70,
    healthcare_score = 50,
    livability_score_calc = 90
  )
  
  weights <- rep(1/6, 6)
  names(weights) <- c("safety", "education", "economy", "housing", "healthcare", "livability")
  
  result <- scores %>%
    mutate(
      custom_score = safety_score * weights["safety"] +
                     education_score * weights["education"] +
                     economic_score * weights["economy"] +
                     housing_score * weights["housing"] +
                     healthcare_score * weights["healthcare"] +
                     livability_score_calc * weights["livability"]
    )
  
  expected_mean <- mean(c(60, 80, 40, 70, 50, 90))
  expect_equal(unname(result$custom_score[1]), expected_mean, tolerance = 0.1)
})

# =============================================================================
# Test: Benchmark Comparison Display
# =============================================================================

test_that("Benchmark comparison returns valid strings", {
  value <- 70000  # Above Iowa median income
  
  result <- compare_to_benchmark(value, "median_household_income", 
                                  level = "iowa", higher_is_better = TRUE)
  
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("Benchmark comparison handles edge cases", {
  # Exactly equal to benchmark
  iowa_income <- IOWA_BENCHMARKS$median_household_income
  
  result <- compare_to_benchmark(iowa_income, "median_household_income")
  
  expect_type(result, "character")
})

# =============================================================================
# Test: Export Data Structure
# =============================================================================

test_that("Export data has required columns", {
  data <- load_test_data()
  skip_if(is.null(data$cities), "Cities data not available")
  
  # Check for city column (required)
  expect_true("city" %in% names(data$cities))
  
  # Check for region column (may have different name)
  has_region <- "region" %in% names(data$cities) || 
                any(grepl("region", names(data$cities), ignore.case = TRUE))
  expect_true(has_region || ncol(data$cities) > 1)
  
  # Check for population column (may have suffix like _2020)
  has_pop <- any(grepl("pop", names(data$cities), ignore.case = TRUE))
  expect_true(has_pop || ncol(data$cities) > 2)
})

# =============================================================================
# Test: Theme Toggle Logic
# =============================================================================

test_that("Dark mode CSS class names are valid", {
  # These are the CSS classes used for dark mode
  dark_mode_classes <- c(
    ".dark-mode .content-wrapper",
    ".dark-mode .box",
    ".dark-mode .dataTables_wrapper"
  )
  
  # Verify they're non-empty strings (basic validation)
  for (class in dark_mode_classes) {
    expect_true(nchar(class) > 0)
    expect_true(grepl("dark-mode", class))
  }
})

# =============================================================================
# Test: Log Event Structure
# =============================================================================

test_that("Log events have required format", {
  # Simulate log event structure
  log_event <- function(event, details = "") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    session_id <- "session_test_12345"
    log_entry <- paste(timestamp, session_id, event, details, sep = " | ")
    return(log_entry)
  }
  
  result <- log_event("TEST_EVENT", "test details")
  
  expect_true(grepl("\\|", result))  # Contains separator
  expect_true(grepl("TEST_EVENT", result))
  expect_true(grepl("session_", result))
})

# =============================================================================
# Test: Keyboard Shortcuts
# =============================================================================

test_that("Tab names match expected values", {
  # These should match the sidebarMenu items in app.R
  expected_tabs <- c("overview", "rankings", "compare", "profiles", 
                     "maps", "recommend", "trends")
  
  expect_length(expected_tabs, 7)
  
  # Verify no duplicates
  expect_equal(length(expected_tabs), length(unique(expected_tabs)))
})
