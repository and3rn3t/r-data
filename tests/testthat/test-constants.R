# Unit Tests for Constants
# =========================

library(testthat)
library(tidyverse)

# Source the constants file
source(here::here("scripts/constants.R"))

# =============================================================================
# IOWA_CITIES Tests
# =============================================================================

test_that("IOWA_CITIES contains expected cities", {
  expect_true("Des Moines" %in% IOWA_CITIES)
  expect_true("Cedar Rapids" %in% IOWA_CITIES)
  expect_true("Iowa City" %in% IOWA_CITIES)
  expect_true("Ames" %in% IOWA_CITIES)
})

test_that("IOWA_CITIES has 20 cities", {
  expect_length(IOWA_CITIES, 20)
})

test_that("IOWA_CITIES has no duplicates", {
  expect_equal(length(IOWA_CITIES), length(unique(IOWA_CITIES)))
})

# =============================================================================
# CITY_REGIONS Tests
# =============================================================================

test_that("CITY_REGIONS covers all regions", {
  expect_true("Central" %in% names(CITY_REGIONS))
  expect_true("Eastern" %in% names(CITY_REGIONS))
  expect_true("Western" %in% names(CITY_REGIONS))
  expect_true("Northern" %in% names(CITY_REGIONS))
})

test_that("All cities are assigned to a region", {
  all_regional_cities <- unlist(CITY_REGIONS)
  
  for (city in IOWA_CITIES) {
    expect_true(city %in% all_regional_cities,
                info = paste(city, "should be in a region"))
  }
})

# =============================================================================
# get_region() Tests
# =============================================================================

test_that("get_region returns correct region for Des Moines", {
  expect_equal(get_region("Des Moines"), "Central")
})

test_that("get_region returns correct region for Cedar Rapids", {
  expect_equal(get_region("Cedar Rapids"), "Eastern")
})

test_that("get_region returns correct region for Sioux City", {
  expect_equal(get_region("Sioux City"), "Western")
})

test_that("get_region returns correct region for Waterloo", {
  expect_equal(get_region("Waterloo"), "Northern")
})
  
test_that("get_region returns NA for unknown city", {
  expect_true(is.na(get_region("Fake City")))
})

test_that("get_region handles empty string", {
  expect_true(is.na(get_region("")))
})

# =============================================================================
# CITY_COUNTIES Tests
# =============================================================================

test_that("CITY_COUNTIES maps Des Moines to Polk", {
  expect_equal(CITY_COUNTIES["Des Moines"], c("Des Moines" = "Polk"))
})

test_that("CITY_COUNTIES maps Cedar Rapids to Linn", {
  expect_equal(CITY_COUNTIES["Cedar Rapids"], c("Cedar Rapids" = "Linn"))
})

test_that("All 20 cities have county mappings", {
  expect_gte(length(CITY_COUNTIES), 20)
})

# =============================================================================
# REGION_COLORS Tests
# =============================================================================

test_that("REGION_COLORS has required regions", {
  expect_true("Central" %in% names(REGION_COLORS))
  expect_true("Eastern" %in% names(REGION_COLORS))
  expect_true("Western" %in% names(REGION_COLORS))
})

test_that("REGION_COLORS contains valid hex colors", {
  for (region_name in names(REGION_COLORS)) {
    color <- REGION_COLORS[[region_name]]
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", color),
                info = paste("Invalid color for", region_name, ":", color))
  }
})

# =============================================================================
# OVERALL_WEIGHTS Tests
# =============================================================================

test_that("OVERALL_WEIGHTS sum to 1", {
  expect_equal(sum(unlist(OVERALL_WEIGHTS)), 1)
})

test_that("OVERALL_WEIGHTS has required categories", {
  expect_true("safety" %in% names(OVERALL_WEIGHTS))
  expect_true("education" %in% names(OVERALL_WEIGHTS))
  expect_true("housing" %in% names(OVERALL_WEIGHTS))
})

test_that("All weight values are positive", {
  for (weight in OVERALL_WEIGHTS) {
    expect_true(weight >= 0,
                info = paste("Weight should be non-negative:", weight))
  }
})

# =============================================================================
# IOWA_BENCHMARKS Tests
# =============================================================================

test_that("IOWA_BENCHMARKS contains expected metrics", {
  expect_true("median_household_income" %in% names(IOWA_BENCHMARKS))
  expect_true("median_home_value" %in% names(IOWA_BENCHMARKS))
  expect_true("violent_crime_rate" %in% names(IOWA_BENCHMARKS))
  expect_true("high_school_graduation_rate" %in% names(IOWA_BENCHMARKS))
})

test_that("IOWA_BENCHMARKS values are reasonable", {
  # Income should be reasonable (between $40k and $100k)
  expect_true(IOWA_BENCHMARKS$median_household_income > 40000)
  expect_true(IOWA_BENCHMARKS$median_household_income < 100000)
  
  # Home value should be reasonable (between $100k and $300k)
  expect_true(IOWA_BENCHMARKS$median_home_value > 100000)
  expect_true(IOWA_BENCHMARKS$median_home_value < 300000)
  
  # Graduation rate should be percentage (0-100)
  expect_true(IOWA_BENCHMARKS$high_school_graduation_rate > 0)
  expect_true(IOWA_BENCHMARKS$high_school_graduation_rate <= 100)
})

# =============================================================================
# US_BENCHMARKS Tests
# =============================================================================

test_that("US_BENCHMARKS contains expected metrics", {
  expect_true("median_household_income" %in% names(US_BENCHMARKS))
  expect_true("median_home_value" %in% names(US_BENCHMARKS))
})

test_that("US benchmarks differ from Iowa benchmarks", {
  # These should be different values
  expect_false(US_BENCHMARKS$median_household_income == IOWA_BENCHMARKS$median_household_income)
  expect_false(US_BENCHMARKS$median_home_value == IOWA_BENCHMARKS$median_home_value)
})

# =============================================================================
# get_benchmark() Tests
# =============================================================================

test_that("get_benchmark returns Iowa benchmark by default", {
  result <- get_benchmark("median_household_income")
  expect_equal(result, IOWA_BENCHMARKS$median_household_income)
})

test_that("get_benchmark returns US benchmark when specified", {
  result <- get_benchmark("median_household_income", level = "us")
  expect_equal(result, US_BENCHMARKS$median_household_income)
})

test_that("get_benchmark returns Midwest benchmark when specified", {
  result <- get_benchmark("median_household_income", level = "midwest")
  expect_equal(result, MIDWEST_BENCHMARKS$median_household_income)
})

test_that("get_benchmark returns NA for unknown metric", {
  result <- get_benchmark("fake_metric")
  expect_true(is.na(result))
})

test_that("get_benchmark returns NA for unknown level", {
  result <- get_benchmark("median_household_income", level = "fake_level")
  # Should default to iowa benchmarks
  expect_false(is.na(result))
})

# =============================================================================
# compare_to_benchmark() Tests
# =============================================================================

test_that("compare_to_benchmark identifies above average", {
  # Value significantly above benchmark (more than 10% above)
  iowa_income <- IOWA_BENCHMARKS$median_household_income
  high_value <- iowa_income * 1.2
  
  result <- compare_to_benchmark(high_value, "median_household_income", 
                                  level = "iowa", higher_is_better = TRUE)
  
  expect_true(grepl("Above|above", result))
})

test_that("compare_to_benchmark identifies below average", {
  iowa_income <- IOWA_BENCHMARKS$median_household_income
  low_value <- iowa_income * 0.8
  
  result <- compare_to_benchmark(low_value, "median_household_income", 
                                  level = "iowa", higher_is_better = TRUE)
  
  expect_true(grepl("Below|below", result))
})

test_that("compare_to_benchmark identifies near average", {
  iowa_income <- IOWA_BENCHMARKS$median_household_income
  similar_value <- iowa_income * 1.05  # Only 5% above
  
  result <- compare_to_benchmark(similar_value, "median_household_income", 
                                  level = "iowa", higher_is_better = TRUE)
  
  expect_true(grepl("Near|near|average", result))
})

test_that("compare_to_benchmark handles lower-is-better metrics", {
  iowa_crime <- IOWA_BENCHMARKS$violent_crime_rate
  low_crime <- iowa_crime * 0.7  # 30% lower crime
  
  result <- compare_to_benchmark(low_crime, "violent_crime_rate", 
                                  level = "iowa", higher_is_better = FALSE)
  
  expect_true(grepl("Better|better|lower", result))
})

test_that("compare_to_benchmark handles unknown metric", {
  result <- compare_to_benchmark(100, "fake_metric")
  
  expect_true(grepl("No benchmark", result))
})

# =============================================================================
# USER_PROFILES Tests
# =============================================================================

test_that("USER_PROFILES has expected profiles", {
  expect_true("family" %in% names(USER_PROFILES))
  expect_true("young_professional" %in% names(USER_PROFILES))
  expect_true("retiree" %in% names(USER_PROFILES))
})

test_that("USER_PROFILES have descriptions", {
  for (profile_name in names(USER_PROFILES)) {
    expect_true("description" %in% names(USER_PROFILES[[profile_name]]),
                info = paste(profile_name, "should have a description"))
  }
})

test_that("USER_PROFILES weights sum to 1", {
  for (profile_name in names(USER_PROFILES)) {
    weights <- USER_PROFILES[[profile_name]]$weights
    expect_equal(sum(weights), 1, tolerance = 0.01,
                 info = paste(profile_name, "weights should sum to 1"))
  }
})

# =============================================================================
# USER_PROFILES Tests Complete
# =============================================================================
