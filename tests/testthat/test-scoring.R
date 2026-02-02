# Unit Tests for Scoring Functions
# =================================

library(testthat)
library(tidyverse)

# Source the scoring module
source(here::here("R/scoring.R"))

# =============================================================================
# Test: SCORE_CATEGORIES
# =============================================================================

test_that("SCORE_CATEGORIES has required structure", {
  expect_type(SCORE_CATEGORIES, "list")
  expect_true(length(SCORE_CATEGORIES) >= 7)  # At least 7 core + 4 optional
  
  # Check each category has required fields
  for (cat_name in names(SCORE_CATEGORIES)) {
    cat <- SCORE_CATEGORIES[[cat_name]]
    expect_true("name" %in% names(cat), 
                info = paste("Missing name in", cat_name))
    expect_true("icon" %in% names(cat), 
                info = paste("Missing icon in", cat_name))
    expect_true("description" %in% names(cat), 
                info = paste("Missing description in", cat_name))
  }
})

test_that("SCORE_CATEGORIES has expected categories", {
  expected_core <- c("safety", "housing", "education", "economic", "healthcare")
  expected_optional <- c("family", "climate", "senior", "pet")
  
  for (cat in expected_core) {
    expect_true(cat %in% names(SCORE_CATEGORIES), 
                info = paste("Missing core category:", cat))
  }
  
  for (cat in expected_optional) {
    expect_true(cat %in% names(SCORE_CATEGORIES), 
                info = paste("Missing optional category:", cat))
    expect_true(isTRUE(SCORE_CATEGORIES[[cat]]$optional),
                info = paste(cat, "should be optional"))
  }
})

# =============================================================================
# Test: LIFESTYLE_MODES
# =============================================================================

test_that("LIFESTYLE_MODES has required presets", {
  expected_modes <- c("balanced", "family_focus", "career_focus", 
                      "retirement_focus", "outdoor_pet_lover", "custom")
  
  for (mode in expected_modes) {
    expect_true(mode %in% names(LIFESTYLE_MODES), 
                info = paste("Missing lifestyle mode:", mode))
  }
})

test_that("LIFESTYLE_MODES have valid weights", {
  for (mode_name in names(LIFESTYLE_MODES)) {
    mode <- LIFESTYLE_MODES[[mode_name]]
    
    expect_true("name" %in% names(mode), 
                info = paste("Missing name in mode:", mode_name))
    
    # Custom mode may not have weights defined
    if (mode_name != "custom") {
      expect_true("weights" %in% names(mode), 
                  info = paste("Missing weights in mode:", mode_name))
      
      weights <- mode$weights
      expect_type(weights, "list")
      
      # All weights should be numeric and non-negative
      for (w_name in names(weights)) {
        expect_true(is.numeric(weights[[w_name]]),
                    info = paste("Non-numeric weight in", mode_name, ":", w_name))
        expect_true(weights[[w_name]] >= 0,
                    info = paste("Negative weight in", mode_name, ":", w_name))
      }
    }
  }
})

# =============================================================================
# Test: get_lifestyle_weights
# =============================================================================

test_that("get_lifestyle_weights returns valid weights", {
  weights <- get_lifestyle_weights("balanced")
  
  # Returns named numeric vector
  expect_type(weights, "double")
  expect_true(length(weights) >= 7)
  
  # Check core categories have weights
  expect_true("safety" %in% names(weights))
  expect_true("housing" %in% names(weights))
  expect_true("education" %in% names(weights))
})

test_that("get_lifestyle_weights handles invalid mode", {
  weights <- get_lifestyle_weights("nonexistent_mode")
  
  # Should return balanced weights as fallback
  expect_type(weights, "double")
  expect_true(length(weights) >= 7)
})

# =============================================================================
# Test: normalize_weights
# =============================================================================

test_that("normalize_weights sums to 1", {
  test_weights <- c(a = 10, b = 20, c = 30, d = 40)
  
  normalized <- normalize_weights(test_weights)
  
  total <- sum(normalized)
  expect_equal(total, 1, tolerance = 0.001)
})
  
test_that("normalize_weights preserves relative proportions", {
  test_weights <- c(a = 10, b = 20)
  
  normalized <- normalize_weights(test_weights)
  
  # b should be 2x the weight of a
  expect_equal(unname(normalized["b"] / normalized["a"]), 2, tolerance = 0.001)
})

test_that("normalize_weights handles zero weights", {
  test_weights <- c(a = 0, b = 0, c = 100)
  
  normalized <- normalize_weights(test_weights)
  
  expect_equal(unname(normalized["a"]), 0)
  expect_equal(unname(normalized["b"]), 0)
  expect_equal(unname(normalized["c"]), 1, tolerance = 0.001)
})

test_that("normalize_weights handles all zeros gracefully", {
  test_weights <- c(a = 0, b = 0, c = 0)
  
  # Should return default weights when all are zero
  normalized <- suppressWarnings(normalize_weights(test_weights))
  
  expect_type(normalized, "double")
})

# =============================================================================
# Test: sliders_to_weights
# =============================================================================

test_that("sliders_to_weights converts slider values", {
  slider_values <- c(safety = 50, housing = 75, education = 25)
  
  weights <- sliders_to_weights(slider_values)
  
  expect_type(weights, "double")
  expect_true(all(c("safety", "housing", "education") %in% names(weights)))
  
  # Weights should be proportional to slider values
  expect_true(weights["housing"] > weights["safety"])
  expect_true(weights["safety"] > weights["education"])
})

# =============================================================================
# Test: calculate_custom_score
# =============================================================================

test_that("calculate_custom_score computes correctly with data frame", {
  # calculate_custom_score expects a data frame with score columns
  test_data <- tibble(
    safety_score = c(80, 60),
    housing_score = c(70, 75),
    education_score = c(65, 70)
  )
  weights <- c(safety = 0.5, housing = 0.3, education = 0.2)
  
  result <- calculate_custom_score(test_data, weights)
  
  expect_type(result, "double")
  expect_length(result, 2)
  
  # First row: 80*0.5 + 70*0.3 + 65*0.2 = 40 + 21 + 13 = 74
  expect_equal(result[1], 74, tolerance = 1)
})

test_that("calculate_custom_score handles missing columns", {
  test_data <- tibble(
    safety_score = c(80, 60),
    housing_score = c(70, 75)
    # Missing education_score
  )
  weights <- c(safety = 0.5, housing = 0.3, education = 0.2)
  
  # Should handle gracefully (skip missing columns)
  result <- calculate_custom_score(test_data, weights)
  expect_type(result, "double")
  expect_length(result, 2)
})

test_that("calculate_custom_score handles zero weights", {
  test_data <- tibble(
    safety_score = c(100, 50),
    housing_score = c(0, 100),
    education_score = c(50, 50)
  )
  weights <- c(safety = 1, housing = 0, education = 0)
  
  result <- calculate_custom_score(test_data, weights)
  
  # Only safety should contribute
  expect_equal(result[1], 100, tolerance = 0.1)
  expect_equal(result[2], 50, tolerance = 0.1)
})

# =============================================================================
# Test: Helper Functions
# =============================================================================

test_that("get_core_categories returns non-optional categories", {
  core <- get_core_categories()
  
  expect_type(core, "character")
  expect_true(length(core) >= 5)
  expect_true("safety" %in% core)
  expect_true("housing" %in% core)
})

test_that("get_optional_categories returns optional categories", {
  optional <- get_optional_categories()
  
  expect_type(optional, "character")
  expect_true(length(optional) >= 4)
  expect_true("family" %in% optional)
  expect_true("climate" %in% optional)
  expect_true("senior" %in% optional)
  expect_true("pet" %in% optional)
})

test_that("get_default_weights returns valid weights", {
  weights <- get_default_weights()
  
  expect_type(weights, "double")
  
  # Core categories should have positive weights
  core <- get_core_categories()
  for (cat in core) {
    expect_true(weights[cat] > 0, info = paste(cat, "should have positive weight"))
  }
  
  # Optional categories should have zero weights by default
  optional <- get_optional_categories()
  for (cat in optional) {
    expect_equal(unname(weights[cat]), 0, info = paste(cat, "should have zero weight"))
  }
  
  # Weights should sum to 1
  expect_equal(sum(weights), 1, tolerance = 0.001)
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("different lifestyle modes have different weights", {
  balanced <- get_lifestyle_weights("balanced")
  family <- get_lifestyle_weights("family_focus")
  career <- get_lifestyle_weights("career_focus")
  
  # Family focus should have higher education weight than career
  expect_true(family["education"] > career["education"])
  
  # Career focus should have higher economic weight than family
  expect_true(career["economic"] > family["economic"])
  
  # Family focus should have positive family weight
  expect_true(family["family"] > 0)
})
