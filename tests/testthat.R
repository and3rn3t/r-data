# Test Runner for R Data Analysis Workspace
# Run all tests with: testthat::test_dir("tests/testthat")
# =========================================================

library(testthat)

# Source utility functions to test
source(here::here("scripts/utils.R"))

# Run all tests
test_dir(here::here("tests/testthat"))
