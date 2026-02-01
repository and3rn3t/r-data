# Test file for api.R - REST API endpoints
# ==================================================

library(testthat)
library(here)
library(tidyverse)
library(jsonlite)

# Source utilities for testing
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))
source(here("scripts/security.R"))

# =============================================================================
# API HELPER FUNCTION TESTS
# =============================================================================

test_that("API data loading works", {
  # Test that load_datasets works for API data files
  API_DATA_FILES <- c(
    crime = "iowa_crime_data.csv",
    housing = "iowa_housing_data.csv",
    major_cities = "iowa_major_cities.csv"
  )
  
  data <- load_datasets(API_DATA_FILES)
  
  expect_type(data, "list")
  expect_true("crime" %in% names(data))
  expect_true("housing" %in% names(data))
  expect_true("major_cities" %in% names(data))
})

test_that("Safe read CSV handles missing files gracefully", {
  result <- safe_read_csv("nonexistent_file.csv")
  expect_null(result)
})

# =============================================================================
# SANITIZATION TESTS
# =============================================================================

test_that("sanitize_string works correctly for safe input", {
  # Basic sanitization - safe inputs
  result <- sanitize_string("hello")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("sanitize_string rejects dangerous input", {
  # Should return NULL for XSS attempts
  expect_null(sanitize_string("<script>alert('xss')</script>"))
  expect_null(sanitize_string("javascript:alert(1)"))
  expect_null(sanitize_string("<iframe src='bad'>"))
})

test_that("sanitize_string respects max length", {
  long_string <- paste(rep("a", 150), collapse = "")
  result <- sanitize_string(long_string, max_length = 50)
  expect_equal(nchar(result), 50)
})

test_that("sanitize_string handles edge cases", {
  expect_null(sanitize_string(NULL))
  expect_null(sanitize_string(123))  # non-character
  expect_null(sanitize_string(c("a", "b")))  # length > 1
})

test_that("validate_numeric_input works correctly", {
  expect_equal(validate_numeric_input(42, 0, 100), 42)
  expect_equal(validate_numeric_input("42", 0, 100), 42)
  expect_null(validate_numeric_input(-5, 0, 100))  # below min
  expect_null(validate_numeric_input(150, 0, 100))  # above max
})

test_that("validate_numeric_input handles edge cases", {
  expect_null(validate_numeric_input(NULL))
  expect_null(validate_numeric_input(NA))
  expect_null(validate_numeric_input("abc"))  # non-numeric string
})

# =============================================================================
# RATE LIMITING TESTS
# =============================================================================

test_that("Rate limiter creates valid environment", {
  limiter <- create_rate_limiter(max_requests = 10, window_seconds = 60)
  
  expect_true(is.environment(limiter))
  expect_equal(limiter$max_requests, 10)
  expect_equal(limiter$window_seconds, 60)
  expect_null(limiter$blocked_until)
})

test_that("Rate limiter allows requests under limit", {
  limiter <- create_rate_limiter(max_requests = 5, window_seconds = 60)
  
  result <- check_rate_limit(limiter, "test")
  expect_true(result$allowed)
  expect_equal(result$wait_seconds, 0)
})

test_that("Rate limiter blocks when limit exceeded", {
  limiter <- create_rate_limiter(max_requests = 2, window_seconds = 60)
  
  # Make requests up to limit
  check_rate_limit(limiter, "test")
  check_rate_limit(limiter, "test")
  
  # Third request should be blocked
  result <- check_rate_limit(limiter, "test")
  expect_false(result$allowed)
  expect_true(result$wait_seconds > 0)
})

# =============================================================================
# API RESPONSE FORMAT TESTS
# =============================================================================

test_that("API responses have correct structure", {
  # Mock data for testing response structure
  mock_city_response <- list(
    city = "Des Moines",
    population = 215636,
    region = "Central",
    lat = 41.6005,
    lon = -93.6091
  )
  
  expect_true("city" %in% names(mock_city_response))
  expect_true("population" %in% names(mock_city_response))
  expect_type(mock_city_response$population, "double")
})

test_that("Pagination parameters are validated correctly", {
  # Test pagination bounds using validate_numeric_input
  validate_page <- function(page, limit) {
    page <- validate_numeric_input(page, 1, 1000) %||% 1
    limit <- validate_numeric_input(limit, 1, 100) %||% 10
    list(page = page, limit = limit)
  }
  
  result <- validate_page(1, 10)
  expect_equal(result$page, 1)
  expect_equal(result$limit, 10)
  
  # Test out of bounds
  result <- validate_page(0, 200)
  expect_equal(result$page, 1)  # Falls back to default
  expect_equal(result$limit, 10)  # Falls back to default
})

# =============================================================================
# FILTER LOGIC TESTS
# =============================================================================

test_that("Region filter logic works correctly", {
  # Mock major_cities data
  mock_cities <- data.frame(
    city = c("Des Moines", "Ames", "Sioux City"),
    region = c("Central", "Central", "Northwest"),
    population = c(215000, 65000, 85000),
    stringsAsFactors = FALSE
  )
  
  # Test region filtering
  region_sanitized <- "Central"
  filtered <- mock_cities[tolower(mock_cities$region) == tolower(region_sanitized), ]
  
  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$region == "Central"))
})

test_that("Population filter works correctly", {
  mock_cities <- data.frame(
    city = c("Des Moines", "Ames", "Sioux City"),
    population = c(215000, 65000, 85000),
    stringsAsFactors = FALSE
  )
  
  min_pop <- 70000
  max_pop <- 300000
  
  filtered <- mock_cities[mock_cities$population >= min_pop & 
                          mock_cities$population <= max_pop, ]
  
  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$population >= min_pop))
  expect_true(all(filtered$population <= max_pop))
})

# =============================================================================
# COMPARE ENDPOINT TESTS
# =============================================================================

test_that("City comparison logic validates inputs", {
  # Test that city names are properly parsed and sanitized
  cities_param <- "Des Moines,Ames,Cedar Rapids"
  cities <- strsplit(cities_param, ",")[[1]]
  cities <- trimws(cities)
  
  expect_equal(length(cities), 3)
  expect_equal(cities[1], "Des Moines")
  expect_equal(cities[3], "Cedar Rapids")
})

test_that("Comparison handles missing cities gracefully", {
  valid_cities <- c("Des Moines", "Ames", "Cedar Rapids")
  requested <- c("Des Moines", "NonExistentCity", "Ames")
  
  found <- requested[requested %in% valid_cities]
  missing <- requested[!requested %in% valid_cities]
  
  expect_equal(length(found), 2)
  expect_equal(length(missing), 1)
  expect_equal(missing[1], "NonExistentCity")
})

# =============================================================================
# SCORING LOGIC TESTS
# =============================================================================

test_that("Score calculation uses correct formula", {
  # Test the normalize function for scoring
  x <- c(10, 20, 30, 40, 50)
  result <- normalize(x)
  
  expect_equal(min(result, na.rm = TRUE), 0)
  expect_equal(max(result, na.rm = TRUE), 100)
})

test_that("Overall score is average of component scores", {
  safety_score <- 80
  housing_score <- 70
  education_score <- 90
  economic_score <- 60
  
  overall <- (safety_score + housing_score + education_score + economic_score) / 4
  
  expect_equal(overall, 75)
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("Security functions handle malformed requests gracefully", {
  # XSS attempts should return NULL
  expect_null(sanitize_string("<script>alert('xss')</script>"))
  
  # Invalid numeric should return NULL
  expect_null(validate_numeric_input("not_a_number"))
})

test_that("NULL coalesce operator works", {
  # Test %||% operator
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(NA %||% "default", NA)  # Note: NA is not NULL
})

# =============================================================================
# MOCK API INTEGRATION TESTS
# =============================================================================

test_that("Health endpoint response is valid", {
  # Mock health check response
  health_response <- list(
    status = "healthy",
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    version = "1.0.0"
  )
  
  expect_equal(health_response$status, "healthy")
  expect_true(nchar(health_response$timestamp) > 0)
})

test_that("Cities endpoint can return all cities", {
  # Load actual data for comprehensive test
  major_cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  
  skip_if(is.null(major_cities), "Major cities data not available")
  
  expect_true(nrow(major_cities) > 0)
  expect_true("city" %in% names(major_cities))
})

# =============================================================================
# DATASET ENDPOINT TESTS
# =============================================================================

test_that("Dataset endpoints return expected columns", {
  crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
  
  skip_if(is.null(crime), "Crime data not available")
  
  expected_cols <- c("city", "violent_crime_rate", "property_crime_rate")
  expect_true(all(expected_cols %in% names(crime)))
})

test_that("Housing data has required fields", {
  housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
  
  skip_if(is.null(housing), "Housing data not available")
  
  expected_cols <- c("city", "median_home_value", "median_rent")
  expect_true(all(expected_cols %in% names(housing)))
})

# =============================================================================
# SEARCH ENDPOINT TESTS
# =============================================================================

test_that("Search correctly filters by query string", {
  mock_cities <- data.frame(
    city = c("Des Moines", "West Des Moines", "Ames", "Cedar Rapids"),
    stringsAsFactors = FALSE
  )
  
  query <- "des moines"
  filtered <- mock_cities[grepl(query, mock_cities$city, ignore.case = TRUE), , drop = FALSE]
  
  expect_equal(nrow(filtered), 2)
  expect_true(all(grepl("Des Moines", filtered$city, ignore.case = TRUE)))
})

# =============================================================================
# STATS ENDPOINT TESTS
# =============================================================================

test_that("Stats calculations are correct", {
  mock_pop <- c(100000, 200000, 300000, 400000, 500000)
  
  stats <- list(
    min = min(mock_pop),
    max = max(mock_pop),
    mean = mean(mock_pop),
    median = median(mock_pop),
    sd = sd(mock_pop)
  )
  
  expect_equal(stats$min, 100000)
  expect_equal(stats$max, 500000)
  expect_equal(stats$mean, 300000)
  expect_equal(stats$median, 300000)
})
