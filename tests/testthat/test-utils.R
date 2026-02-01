# Unit Tests for Utility Functions
# =================================

library(testthat)
library(tidyverse)

# Source the utils file
source(here::here("scripts/utils.R"))

# Test: calculate_ci ----
test_that("calculate_ci returns correct structure", {
  result <- calculate_ci(c(1, 2, 3, 4, 5))
  
  expect_named(result, c("mean", "lower", "upper"))
  expect_length(result, 3)
  expect_true(result["lower"] < result["mean"])
  expect_true(result["upper"] > result["mean"])
})

test_that("calculate_ci handles NA values", {
  result <- calculate_ci(c(1, 2, NA, 4, 5))
  
  expect_false(any(is.na(result)))
})

test_that("calculate_ci respects confidence level", {
  result_95 <- calculate_ci(1:100, conf.level = 0.95)
  result_99 <- calculate_ci(1:100, conf.level = 0.99)
  
  # 99% CI should be wider than 95% CI
  width_95 <- result_95["upper"] - result_95["lower"]
  width_99 <- result_99["upper"] - result_99["lower"]
  

  expect_true(width_99 > width_95)
})

# Test: standardize_names ----
test_that("standardize_names cleans column names", {
  df <- data.frame(
    "Column Name" = 1,
    "Another.Column" = 2,
    "UPPERCASE" = 3,
    check.names = FALSE
  )
  
  result <- standardize_names(df)
  
  expect_true(all(names(result) == tolower(names(result))))
  expect_false(any(grepl(" ", names(result))))
})

# Test: count_missing ----
test_that("count_missing counts correctly", {
  df <- data.frame(
    a = c(1, 2, NA, 4),
    b = c(NA, NA, 3, 4),
    c = c(1, 2, 3, 4)
  )
  
  result <- count_missing(df)
  
  expect_equal(result$missing_count[result$variable == "a"], 1)
  expect_equal(result$missing_count[result$variable == "b"], 2)
  expect_equal(result$missing_count[result$variable == "c"], 0)
})

test_that("count_missing calculates percentages", {
  df <- data.frame(
    a = c(1, NA, NA, NA, 5)  # 60% missing
  )
  
  result <- count_missing(df)
  
  expect_equal(result$missing_pct[result$variable == "a"], 60)
})

# Test: summarise_numeric ----
test_that("summarise_numeric returns expected columns", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5)
  )
  
  result <- summarise_numeric(df)
  
  expect_true("value_mean" %in% names(result))
  expect_true("value_sd" %in% names(result))
  expect_true("value_median" %in% names(result))
})

test_that("summarise_numeric works with grouping", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 10, 20)
  )
  
  result <- summarise_numeric(df, "group")
  
  expect_equal(nrow(result), 2)
})

# =============================================================================
# NEW TESTS: normalize function
# =============================================================================

test_that("normalize scales values to 0-100", {
  result <- normalize(c(0, 50, 100))
  
  expect_equal(result[1], 0)
  expect_equal(result[2], 50)
  expect_equal(result[3], 100)
})

test_that("normalize handles reverse parameter", {
  result <- normalize(c(0, 50, 100), reverse = TRUE)
  
  expect_equal(result[1], 100)
  expect_equal(result[2], 50)
  expect_equal(result[3], 0)
})

test_that("normalize handles NA values", {
  result <- normalize(c(0, NA, 100))
  
  expect_equal(result[1], 0)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 100)
})

test_that("normalize handles all-same values", {
  result <- normalize(c(5, 5, 5))
  
  # Should return 50 for all when values are identical

  expect_true(all(result == 50))
})

test_that("normalize handles all-NA values", {
  result <- normalize(c(NA, NA, NA))
  
  expect_true(all(is.na(result)))
})

test_that("normalize returns rounded values", {
  result <- normalize(c(0, 33, 100))
  
  # Should be rounded to 1 decimal place

  expect_equal(result[2], 33.0)
})

# =============================================================================
# NEW TESTS: weighted_score function
# =============================================================================

test_that("weighted_score calculates correctly", {
  df <- data.frame(
    a = c(100, 0),
    b = c(0, 100)
  )
  
  result <- weighted_score(a = 0.5, b = 0.5, data = df)
  
  expect_equal(result[1], 50)
  expect_equal(result[2], 50)
})

test_that("weighted_score warns when weights don't sum to 1", {
  df <- data.frame(a = c(100), b = c(100))
  
  expect_warning(
    weighted_score(a = 0.3, b = 0.3, data = df),
    "Weights do not sum to 1"
  )
})

test_that("weighted_score handles missing columns gracefully", {
  df <- data.frame(a = c(100))
  
  # Should not error, missing column treated as 0
  result <- weighted_score(a = 0.5, missing_col = 0.5, data = df)
  
  expect_equal(result[1], 50)
})

# =============================================================================
# NEW TESTS: safe_read_csv function
# =============================================================================

test_that("safe_read_csv returns NULL for non-existent file", {
  result <- safe_read_csv("nonexistent_file.csv")
  
  expect_null(result)
})

test_that("safe_read_csv tries fallback path", {
  # Create a temp file for testing
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:3), temp_file, row.names = FALSE)
  
  result <- safe_read_csv("nonexistent.csv", fallback_path = temp_file)
  
  expect_false(is.null(result))
  expect_equal(nrow(result), 3)
  
  unlink(temp_file)
})

test_that("safe_read_csv errors when required file missing", {
  expect_error(
    safe_read_csv("nonexistent.csv", required = TRUE),
    "Required file not found"
  )
})

# =============================================================================
# NEW TESTS: validate_iowa_data function
# =============================================================================

test_that("validate_iowa_data catches missing columns", {
  df <- data.frame(population = 1:5)
  
  result <- validate_iowa_data(df, required_cols = c("city", "population"))
  
  expect_false(result$is_valid)
  expect_true(any(grepl("Missing columns", result$issues)))
})

test_that("validate_iowa_data catches empty data", {
  df <- data.frame(city = character(0))
  
  result <- validate_iowa_data(df)
  
  expect_false(result$is_valid)
  expect_true(any(grepl("no rows", result$issues)))
})

test_that("validate_iowa_data catches unknown cities", {
  df <- data.frame(city = c("Des Moines", "Fake City"))
  
  result <- validate_iowa_data(df, check_cities = TRUE)
  
  expect_false(result$is_valid)
  expect_true(any(grepl("Unknown cities", result$issues)))
})

test_that("validate_iowa_data passes valid data", {
  df <- data.frame(
    city = c("Des Moines", "Cedar Rapids", "Iowa City"),
    population = c(1, 2, 3)
  )
  
  result <- validate_iowa_data(df, check_cities = TRUE)
  
  expect_true(result$is_valid)
  expect_length(result$issues, 0)
})

# =============================================================================
# NEW TESTS: iowa_colors function
# =============================================================================

test_that("iowa_colors returns correct number of colors", {
  result <- iowa_colors(n = 5)
  
  expect_length(result, 5)
})
  
test_that("iowa_colors returns hex colors", {
  result <- iowa_colors(n = 3)
  
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("iowa_colors warns for unknown palette", {
  expect_warning(
    iowa_colors(palette = "unknown_palette"),
    "Unknown palette"
  )
})

# =============================================================================
# NEW TESTS: format_elapsed function
# =============================================================================

test_that("format_elapsed formats seconds correctly", {
  start <- Sys.time() - 30  # 30 seconds ago
  
  result <- format_elapsed(start)
  
  expect_true(grepl("seconds", result))
})

test_that("format_elapsed formats minutes correctly", {
  start <- Sys.time() - 120  # 2 minutes ago
  
  result <- format_elapsed(start)
  
  expect_true(grepl("minutes", result))
})

# =============================================================================
# NEW TESTS: safe_left_join function
# =============================================================================

test_that("safe_left_join handles null input", {
  base <- data.frame(city = c("A", "B"), pop = c(100, 200))
  
  result <- safe_left_join(base, NULL, by = "city")
  
  expect_equal(result, base)
})

test_that("safe_left_join removes duplicate columns", {
  base <- data.frame(city = c("A", "B"), pop = c(100, 200))
  new_data <- data.frame(city = c("A", "B"), pop = c(999, 999), score = c(50, 60))
  
  result <- safe_left_join(base, new_data, by = "city")
  
  expect_equal(result$pop, c(100, 200))  # Keep original
  expect_true("score" %in% names(result))
})

test_that("safe_left_join handles empty data frame", {
  base <- data.frame(city = c("A", "B"), pop = c(100, 200))
  empty <- data.frame(city = character(0), score = numeric(0))
  
  result <- safe_left_join(base, empty, by = "city")
  
  expect_equal(nrow(result), 2)
})

# =============================================================================
# NEW TESTS: safe_read_csv function
# =============================================================================

test_that("safe_read_csv returns NULL for missing file", {
  result <- safe_read_csv("nonexistent_file_12345.csv")
  
  expect_null(result)
})

test_that("safe_read_csv uses fallback path", {
  # This test requires an existing file - skip if not in project
  skip_if_not(file.exists(here::here("data/raw/iowa_major_cities.csv")))
  
  result <- safe_read_csv("missing.csv", here::here("data/raw/iowa_major_cities.csv"))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("safe_read_csv stops on required missing file", {
  expect_error(
    safe_read_csv("nonexistent.csv", required = TRUE),
    "Required file not found"
  )
})

# =============================================================================
# NEW TESTS: weighted_score function
# =============================================================================

test_that("weighted_score calculates correctly with equal weights", {
  df <- data.frame(a = c(0, 100), b = c(100, 0))
  
  result <- weighted_score(a = 0.5, b = 0.5, data = df)
  
  expect_equal(result, c(50, 50))
})

test_that("weighted_score warns when weights don't sum to 1", {
  df <- data.frame(a = c(50, 100), b = c(100, 50))
  
  expect_warning(
    weighted_score(a = 0.3, b = 0.3, data = df),
    "Weights do not sum to 1"
  )
})

# =============================================================================
# NEW TESTS: calculate_city_scores function
# =============================================================================

test_that("calculate_city_scores returns expected structure", {
  # Create minimal mock data
  mock_data <- list(
    major_cities = data.frame(
      city = c("CityA", "CityB"),
      county = c("CountyA", "CountyB"),
      population_2020 = c(50000, 100000),
      latitude = c(41.5, 42.0),
      longitude = c(-93.5, -94.0),
      region = c("Central", "Eastern")
    ),
    crime = data.frame(
      city = c("CityA", "CityB"),
      violent_crime_rate = c(200, 400),
      property_crime_rate = c(1000, 2000)
    ),
    housing = data.frame(
      city = c("CityA", "CityB"),
      median_home_value = c(150000, 200000),
      owner_occupied_pct = c(65, 70)
    ),
    education = NULL,
    economic = NULL,
    healthcare = NULL,
    amenities = NULL
  )
  
  result <- calculate_city_scores(mock_data)
  
  expect_s3_class(result, "data.frame")
  expect_true("overall_score" %in% names(result))
  expect_true("safety_score" %in% names(result))
  expect_true("rank" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("calculate_city_scores ranks correctly", {
  mock_data <- list(
    major_cities = data.frame(
      city = c("HighScore", "LowScore"),
      population_2020 = c(50000, 50000),
      latitude = c(41.5, 42.0),
      longitude = c(-93.5, -94.0)
    ),
    crime = data.frame(
      city = c("HighScore", "LowScore"),
      violent_crime_rate = c(50, 500),   # Lower is better
      property_crime_rate = c(100, 1000)
    ),
    housing = NULL,
    education = NULL,
    economic = NULL,
    healthcare = NULL,
    amenities = NULL
  )
  
  result <- calculate_city_scores(mock_data)
  
  # HighScore should rank 1 (lower crime = higher score)
  expect_equal(result$city[result$rank == 1], "HighScore")
})
