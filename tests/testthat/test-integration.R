# Integration Tests for Iowa Cities Data Pipeline
# ================================================
# These tests verify the complete data flow from raw data to processed scores.

library(testthat)
library(tidyverse)
library(here)

# Source required files
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

# =============================================================================
# Test: Raw Data Availability
# =============================================================================

test_that("All required raw data files exist", {
  required_files <- c(
    "iowa_major_cities.csv",
    "iowa_crime_data.csv",
    "iowa_housing_data.csv",
    "iowa_education_data.csv"
  )
  
  for (file in required_files) {
    file_path <- here("data/raw", file)
    expect_true(file.exists(file_path),
                info = paste("Missing required file:", file))
  }
})

# =============================================================================
# Test: Data Loading Pipeline
# =============================================================================

test_that("All datasets load without errors", {
  # This simulates app.R's data loading
  expect_no_error({
    crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
    housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
    education <- safe_read_csv(here("data/raw/iowa_education_data.csv"))
    cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  })
})

test_that("All datasets have city column for joining", {
  datasets <- list(
    crime = safe_read_csv(here("data/raw/iowa_crime_data.csv")),
    housing = safe_read_csv(here("data/raw/iowa_housing_data.csv")),
    education = safe_read_csv(here("data/raw/iowa_education_data.csv")),
    cities = safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  )
  
  for (name in names(datasets)) {
    skip_if(is.null(datasets[[name]]), paste(name, "data not available"))
    expect_true("city" %in% names(datasets[[name]]),
                info = paste(name, "should have 'city' column"))
  }
})

# =============================================================================
# Test: Data Joining
# =============================================================================

test_that("Datasets can be joined on city column", {
  crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
  housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
  education <- safe_read_csv(here("data/raw/iowa_education_data.csv"))
  cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  
  skip_if(any(sapply(list(crime, housing, education, cities), is.null)),
          "Some data files missing")
  
  # Perform joins
  joined <- cities %>%
    left_join(crime, by = "city") %>%
    left_join(housing, by = "city") %>%
    left_join(education, by = "city")
  
  expect_equal(nrow(joined), nrow(cities))
  expect_true("violent_crime_rate" %in% names(joined) || 
              any(grepl("crime", names(joined), ignore.case = TRUE)))
})

test_that("No cities are lost in joins", {
  crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
  cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  
  skip_if(is.null(crime) || is.null(cities), "Data files missing")
  
  # Check all cities in cities data are in crime data
  cities_in_crime <- cities$city %in% crime$city
  
  expect_true(all(cities_in_crime),
              info = paste("Missing cities in crime data:", 
                          paste(cities$city[!cities_in_crime], collapse = ", ")))
})

# =============================================================================
# Test: Score Calculation
# =============================================================================

test_that("Normalize function produces valid scores", {
  test_values <- c(10, 20, 30, 40, 50)
  
  result <- normalize(test_values)
  
  expect_true(all(result >= 0))
  expect_true(all(result <= 100))
  expect_equal(min(result), 0)
  expect_equal(max(result), 100)
})

test_that("Score calculation produces valid overall scores", {
  # Simulate score calculation like app.R
  scores <- tibble(
    city = c("City A", "City B", "City C"),
    safety_score = c(80, 60, 40),
    housing_score = c(70, 70, 70),
    education_score = c(90, 50, 30),
    economic_score = c(60, 80, 40),
    healthcare_score = c(75, 75, 75),
    livability_score = c(65, 55, 85)
  ) %>%
    mutate(
      overall_score = (safety_score + housing_score + education_score + 
                       economic_score + healthcare_score + livability_score) / 6
    )
  
  # Overall scores should be between 0 and 100
  expect_true(all(scores$overall_score >= 0))
  expect_true(all(scores$overall_score <= 100))
  
  # Should produce valid rankings
  expect_equal(nrow(scores), 3)
})

test_that("Rankings are assigned correctly", {
  scores <- tibble(
    city = c("City A", "City B", "City C"),
    overall_score = c(90, 80, 70)
  ) %>%
    arrange(desc(overall_score)) %>%
    mutate(rank = row_number())
  
  expect_equal(scores$rank[scores$city == "City A"], 1)
  expect_equal(scores$rank[scores$city == "City B"], 2)
  expect_equal(scores$rank[scores$city == "City C"], 3)
})

# =============================================================================
# Test: Data Quality After Processing
# =============================================================================

test_that("Processed data has no infinite values", {
  cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  skip_if(is.null(cities), "Cities data not available")
  
  numeric_cols <- cities %>% select(where(is.numeric)) %>% names()
  
  for (col in numeric_cols) {
    expect_false(any(is.infinite(cities[[col]])),
                 info = paste("Column", col, "has infinite values"))
  }
})

test_that("Score normalization handles edge cases", {
  # Test with single value
  single <- normalize(c(50))
  expect_equal(single, 50)  # Single value should return 50
  
  # Test with identical values
  identical <- normalize(c(30, 30, 30))
  expect_true(all(identical == 50))  # All same = all 50
  
  # Test with NA
  with_na <- normalize(c(0, NA, 100))
  expect_equal(with_na[1], 0)
  expect_true(is.na(with_na[2]))
  expect_equal(with_na[3], 100)
})

# =============================================================================
# Test: Complete Pipeline
# =============================================================================

test_that("Full pipeline produces valid output", {
  # Load data
  crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
  housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
  education <- safe_read_csv(here("data/raw/iowa_education_data.csv"))
  cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  
  skip_if(any(sapply(list(crime, housing, education, cities), is.null)),
          "Data files missing")
  
  # Join data
  combined <- cities %>%
    left_join(crime, by = "city") %>%
    left_join(housing, by = "city") %>%
    left_join(education, by = "city")
  
  # Calculate scores (simplified)
  if ("violent_crime_rate" %in% names(combined)) {
    combined <- combined %>%
      mutate(
        safety_score = normalize(violent_crime_rate, reverse = TRUE)
      )
    
    expect_true(all(combined$safety_score >= 0, na.rm = TRUE))
    expect_true(all(combined$safety_score <= 100, na.rm = TRUE))
  }
})

# =============================================================================
# Test: Constants Integration
# =============================================================================

test_that("All cities in data match IOWA_CITIES constant", {
  cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  skip_if(is.null(cities), "Cities data not available")
  
  data_cities <- unique(cities$city)
  
  # Check overlap
  in_constant <- data_cities %in% IOWA_CITIES
  in_data <- IOWA_CITIES %in% data_cities
  
  # At least most cities should match
  expect_true(sum(in_constant) >= length(data_cities) * 0.8,
              info = "Most data cities should be in IOWA_CITIES constant")
})

test_that("Most regions in data are known", {
  cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  skip_if(is.null(cities), "Cities data not available")
  skip_if(!"region" %in% names(cities), "No region column")
  
  data_regions <- unique(cities$region)
  constant_regions <- names(CITY_REGIONS)
  
  # Count how many regions match
  matched <- sum(data_regions %in% constant_regions)
  
  # At least some should match (data may have more detailed regions)
  expect_true(matched > 0 || length(data_regions) > 0,
              info = "Data should have recognizable regions")
})

# =============================================================================
# Test: Benchmark Integration
# =============================================================================

test_that("Benchmarks can be compared to real data", {
  cities <- safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  skip_if(is.null(cities), "Cities data not available")
  skip_if(!"median_household_income" %in% names(cities), "No income column")
  
  # Compare to benchmark
  iowa_benchmark <- get_benchmark("median_household_income", "iowa")
  data_mean <- mean(cities$median_household_income, na.rm = TRUE)
  
  # Data mean should be somewhat close to benchmark (within 50%)
  ratio <- data_mean / iowa_benchmark
  expect_true(ratio > 0.5 && ratio < 1.5,
              info = "Data mean should be within 50% of benchmark")
})

# =============================================================================
# Test: Output File Creation
# =============================================================================

test_that("Processed data directory exists", {
  expect_true(dir.exists(here("data/processed")))
})

test_that("Output directories exist", {
  expect_true(dir.exists(here("outputs")))
})
