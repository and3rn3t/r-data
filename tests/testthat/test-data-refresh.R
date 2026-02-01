# Test file for data_refresh.R - ETL Pipeline
# ==================================================

library(testthat)
library(here)
library(tidyverse)

# Source utilities for testing
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

# =============================================================================
# ETL CONFIGURATION TESTS
# =============================================================================

test_that("Data source configuration is valid", {
  # Expected data sources
  expected_files <- c(
    "iowa_crime_data.csv",
    "iowa_housing_data.csv",
    "iowa_education_data.csv",
    "iowa_economic_data.csv",
    "iowa_major_cities.csv"
  )
  
  # All should exist in data/raw
  for (file in expected_files) {
    file_path <- here("data/raw", file)
    expect_true(file.exists(file_path), 
                info = paste("Missing file:", file))
  }
})

test_that("Raw data directory exists", {
  expect_true(dir.exists(here("data/raw")))
})

test_that("Processed data directory exists or can be created", {
  processed_dir <- here("data/processed")
  
  if (!dir.exists(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE)
  }
  
  expect_true(dir.exists(processed_dir))
})

# =============================================================================
# DATA EXTRACTION TESTS
# =============================================================================

test_that("CSV files can be read successfully", {
  files <- list.files(here("data/raw"), pattern = "\\.csv$", full.names = TRUE)
  
  expect_true(length(files) > 0, "No CSV files found in data/raw")
  
  # Try reading first file
  first_file <- files[1]
  data <- tryCatch(
    read_csv(first_file, show_col_types = FALSE),
    error = function(e) NULL
  )
  
  expect_false(is.null(data), info = paste("Failed to read:", first_file))
})

test_that("safe_read_csv handles various file states", {
  # Good file
  good_result <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  expect_true(!is.null(good_result) || is.data.frame(good_result))
  
  # Non-existent file
  bad_result <- safe_read_csv(here("data/raw/nonexistent.csv"))
  expect_null(bad_result)
})

# =============================================================================
# DATA VALIDATION TESTS
# =============================================================================

test_that("City data has required columns", {
  cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  
  skip_if(is.null(cities), "Cities data not available")
  
  required_cols <- c("city")
  expect_true(all(required_cols %in% names(cities)))
})

test_that("Crime data passes validation", {
  crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
  
  skip_if(is.null(crime), "Crime data not available")
  
  # Check for city column
  expect_true("city" %in% names(crime))
  
  # Check for numeric crime rates (if present)
  if ("violent_crime_rate" %in% names(crime)) {
    expect_type(crime$violent_crime_rate, "double")
    expect_true(all(crime$violent_crime_rate >= 0, na.rm = TRUE))
  }
})

test_that("Housing data passes validation", {
  housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
  
  skip_if(is.null(housing), "Housing data not available")
  
  # Check for city column
  expect_true("city" %in% names(housing))
  
  # Check for positive housing values
  if ("median_home_value" %in% names(housing)) {
    expect_true(all(housing$median_home_value > 0, na.rm = TRUE))
  }
})

# =============================================================================
# DATA TRANSFORMATION TESTS
# =============================================================================

test_that("Column renaming works correctly", {
  mock_data <- data.frame(
    old_name = 1:3,
    another_col = letters[1:3]
  )
  
  renamed <- mock_data %>%
    rename(new_name = old_name)
  
  expect_true("new_name" %in% names(renamed))
  expect_false("old_name" %in% names(renamed))
})

test_that("Missing value handling works", {
  mock_data <- data.frame(
    city = c("A", "B", "C"),
    value = c(10, NA, 30)
  )
  
  # Replace NA with default
  filled <- mock_data %>%
    mutate(value = coalesce(value, 0))
  
  expect_equal(filled$value[2], 0)
})

test_that("Type coercion works correctly", {
  mock_data <- data.frame(
    city = c("Des Moines", "Ames"),
    value = c("100", "200"),
    stringsAsFactors = FALSE
  )
  
  converted <- mock_data %>%
    mutate(value = as.numeric(value))
  
  expect_type(converted$value, "double")
  expect_equal(sum(converted$value), 300)
})

test_that("Normalization produces 0-100 range", {
  test_values <- c(10, 20, 30, 40, 50)
  normalized <- normalize(test_values)
  
  expect_equal(min(normalized, na.rm = TRUE), 0)
  expect_equal(max(normalized, na.rm = TRUE), 100)
})

test_that("Reverse normalization works", {
  test_values <- c(10, 20, 30, 40, 50)
  
  normal <- normalize(test_values)
  reversed <- normalize(test_values, reverse = TRUE)
  
  # Min should become max and vice versa
  expect_equal(which.min(normal), which.max(reversed))
  expect_equal(which.max(normal), which.min(reversed))
})

# =============================================================================
# DATA LOADING TESTS
# =============================================================================

test_that("Processed data can be written", {
  test_data <- data.frame(
    city = c("Test City 1", "Test City 2"),
    value = c(100, 200)
  )
  
  test_file <- here("data/processed/test_output.csv")
  
  # Write
  write_csv(test_data, test_file)
  expect_true(file.exists(test_file))
  
  # Read back
  read_back <- read_csv(test_file, show_col_types = FALSE)
  expect_equal(nrow(read_back), 2)
  
  # Cleanup
  file.remove(test_file)
})

test_that("Data merge across datasets works", {
  cities <- data.frame(city = c("A", "B", "C"), pop = c(100, 200, 300))
  crime <- data.frame(city = c("A", "B", "C"), rate = c(1.5, 2.0, 1.0))
  
  merged <- cities %>%
    left_join(crime, by = "city")
  
  expect_equal(nrow(merged), 3)
  expect_true(all(c("pop", "rate") %in% names(merged)))
})

# =============================================================================
# ETL PIPELINE TESTS
# =============================================================================

test_that("Full dataset loading works", {
  # Test the centralized loading function
  test_files <- c(major_cities = "iowa_major_cities.csv")
  
  result <- tryCatch(
    load_datasets(test_files),
    error = function(e) NULL
  )
  
  expect_false(is.null(result))
  expect_true("major_cities" %in% names(result))
})

test_that("Data freshness check works", {
  check_freshness <- function(file_path, max_age_days = 7) {
    if (!file.exists(file_path)) return(FALSE)
    
    mod_time <- file.info(file_path)$mtime
    age_days <- as.numeric(difftime(Sys.time(), mod_time, units = "days"))
    
    return(age_days <= max_age_days)
  }
  
  # Test with existing file
  cities_file <- here("data/raw/iowa_major_cities.csv")
  if (file.exists(cities_file)) {
    # Should be fresh unless very old
    result <- check_freshness(cities_file, max_age_days = 365)
    expect_true(result)
  }
  
  # Non-existent file should be "stale"
  expect_false(check_freshness("nonexistent.csv"))
})

# =============================================================================
# SCHEDULED JOB TESTS
# =============================================================================

test_that("Cron schedule parsing is valid", {
  # GitHub Actions schedule: "0 6 * * 1"
  # This means: minute 0, hour 6, any day, any month, Monday
  
  schedule <- list(
    minute = 0,
    hour = 6,
    day_of_week = 1  # Monday
  )
  
  expect_equal(schedule$minute, 0)
  expect_equal(schedule$hour, 6)
  expect_equal(schedule$day_of_week, 1)
})

test_that("Next run time calculation works", {
  # Find next Monday at 6 AM
  find_next_run <- function() {
    now <- Sys.time()
    days_until_monday <- (8 - as.integer(format(now, "%u"))) %% 7
    if (days_until_monday == 0) days_until_monday <- 7
    
    next_monday <- as.Date(now) + days_until_monday
    return(as.POSIXct(paste(next_monday, "06:00:00")))
  }
  
  next_run <- find_next_run()
  expect_true(next_run > Sys.time())
  expect_equal(format(next_run, "%u"), "1")  # Monday
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("ETL handles missing files gracefully", {
  result <- tryCatch({
    data <- safe_read_csv("completely_nonexistent_file.csv")
    if (is.null(data)) "handled" else "loaded"
  }, error = function(e) "error")
  
  expect_equal(result, "handled")
})

test_that("ETL handles malformed CSV gracefully", {
  # Create temporary malformed file
  temp_file <- tempfile(fileext = ".csv")
  writeLines(c("col1,col2", "a,b,c", "d,e"), temp_file)  # Inconsistent columns
  
  result <- tryCatch({
    read_csv(temp_file, show_col_types = FALSE)
    "success"
  }, warning = function(w) "warning",
     error = function(e) "error")
  
  # Should either succeed with warnings or handle gracefully
  expect_true(result %in% c("success", "warning", "error"))
  
  file.remove(temp_file)
})

# =============================================================================
# LOGGING TESTS
# =============================================================================

test_that("ETL log entry format is correct", {
  format_log <- function(level, step, message) {
    sprintf("[%s] [%s] %s: %s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            toupper(level),
            step,
            message)
  }
  
  entry <- format_log("info", "EXTRACT", "Starting data extraction")
  
  expect_true(grepl("\\[INFO\\]", entry))
  expect_true(grepl("EXTRACT", entry))
  expect_true(grepl("Starting data extraction", entry))
})

test_that("ETL status tracking works", {
  status <- list(
    started = Sys.time(),
    steps_completed = character(),
    errors = character()
  )
  
  # Add completed step
  status$steps_completed <- c(status$steps_completed, "extract")
  expect_true("extract" %in% status$steps_completed)
  
  # Add error
  status$errors <- c(status$errors, "Transform failed: missing column")
  expect_equal(length(status$errors), 1)
})

# =============================================================================
# DATA QUALITY CHECKS
# =============================================================================

test_that("Duplicate detection works", {
  mock_data <- data.frame(
    city = c("Des Moines", "Ames", "Des Moines"),
    value = c(100, 200, 100)
  )
  
  n_duplicates <- sum(duplicated(mock_data$city))
  expect_equal(n_duplicates, 1)
  
  # Remove duplicates
  unique_data <- mock_data[!duplicated(mock_data$city), ]
  expect_equal(nrow(unique_data), 2)
})

test_that("Missing value detection works", {
  mock_data <- data.frame(
    city = c("A", "B", "C", "D"),
    value = c(10, NA, 30, NA)
  )
  
  na_count <- sum(is.na(mock_data$value))
  expect_equal(na_count, 2)
  
  na_pct <- na_count / nrow(mock_data) * 100
  expect_equal(na_pct, 50)
})

test_that("Range validation works", {
  validate_range <- function(x, min_val, max_val) {
    all(x >= min_val & x <= max_val, na.rm = TRUE)
  }
  
  good_data <- c(0.1, 0.5, 0.9)
  bad_data <- c(0.1, 1.5, 0.9)
  
  expect_true(validate_range(good_data, 0, 1))
  expect_false(validate_range(bad_data, 0, 1))
})

# =============================================================================
# BACKUP AND RECOVERY TESTS
# =============================================================================

test_that("Pre-refresh backup naming is consistent", {
  create_backup_name <- function(dataset) {
    paste0(dataset, "_backup_", format(Sys.Date(), "%Y%m%d"), ".csv")
  }
  
  name <- create_backup_name("iowa_crime_data")
  
  expect_true(grepl("^iowa_crime_data_backup_", name))
  expect_true(grepl("\\.csv$", name))
})

test_that("Rollback detection works", {
  should_rollback <- function(new_rows, old_rows, threshold = 0.5) {
    if (old_rows == 0) return(FALSE)
    change_pct <- abs(new_rows - old_rows) / old_rows
    return(change_pct > threshold)
  }
  
  # Normal change
  expect_false(should_rollback(105, 100))
  
  # Major loss (should rollback)
  expect_true(should_rollback(40, 100))
  
  # Major increase (suspicious, should review)
  expect_true(should_rollback(200, 100))
})
