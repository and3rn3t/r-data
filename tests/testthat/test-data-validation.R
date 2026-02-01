# Data Validation Tests
# Run these tests to ensure data integrity after processing
# =========================================================

library(testthat)
library(tidyverse)
library(here)

# Test: Iowa Cities Clean Data ----
test_that("iowa_cities_clean.csv has expected structure", {
  skip_if_not(file.exists(here("data/processed/iowa_cities_clean.csv")),
              "Clean data file not found - run cleaning script first")
  
  df <- read_csv(here("data/processed/iowa_cities_clean.csv"), 
                 show_col_types = FALSE)
  
  # Check minimum expected columns
  expect_true("city" %in% tolower(names(df)) | 
              any(grepl("city|name", names(df), ignore.case = TRUE)),
              info = "Should have a city/name column")
  
  # Check no completely empty rows
  complete_rows <- complete.cases(df)
  expect_true(sum(complete_rows) > 0, 
              info = "Should have at least some complete rows")
  
  # Check reasonable row count

  expect_gt(nrow(df), 0, info = "Should have at least one row")
})

# Test: No duplicate cities ----
test_that("iowa_cities_clean.csv has no duplicate cities",
 {
  skip_if_not(file.exists(here("data/processed/iowa_cities_clean.csv")),
              "Clean data file not found")
  
  df <- read_csv(here("data/processed/iowa_cities_clean.csv"),
                 show_col_types = FALSE)
  
  city_col <- names(df)[grepl("city|name", names(df), ignore.case = TRUE)][1]
  
  skip_if(is.na(city_col), "No city column found")
  
  n_unique <- n_distinct(df[[city_col]])
  n_total <- nrow(df)
  
  expect_equal(n_unique, n_total,
               info = "Each city should appear only once")
})

# Test: Numeric columns are valid ----
test_that("Numeric columns contain valid values", {
  skip_if_not(file.exists(here("data/processed/iowa_cities_clean.csv")),
              "Clean data file not found")
  
  df <- read_csv(here("data/processed/iowa_cities_clean.csv"),
                 show_col_types = FALSE)
  
  numeric_cols <- df %>% select(where(is.numeric)) %>% names()
  
  for (col in numeric_cols) {
    # Check no Inf values
    expect_false(any(is.infinite(df[[col]])),
                 info = paste("Column", col, "should not contain Inf values"))
  }
})

# Test: Population values are reasonable ----
test_that("Population values are reasonable", {
  skip_if_not(file.exists(here("data/processed/iowa_cities_clean.csv")),
              "Clean data file not found")
  
  df <- read_csv(here("data/processed/iowa_cities_clean.csv"),
                 show_col_types = FALSE)
  
  pop_col <- names(df)[grepl("pop", names(df), ignore.case = TRUE)][1]
  
  skip_if(is.na(pop_col), "No population column found")
  
  # Population should be positive
  expect_true(all(df[[pop_col]] > 0, na.rm = TRUE),
              info = "Population should be positive")
  
  # Iowa's largest city (Des Moines) is ~215k, total state ~3.2M
  expect_true(all(df[[pop_col]] < 500000, na.rm = TRUE),
              info = "No Iowa city has population > 500k")
})
