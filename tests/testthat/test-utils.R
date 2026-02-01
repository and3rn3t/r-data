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
