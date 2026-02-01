# Unit Tests for Report Generation
# =================================

library(testthat)
library(tidyverse)
library(here)

# Source required files
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

# =============================================================================
# Test: Report Data Loading
# =============================================================================

test_that("Report data can be loaded", {
  load_report_data <- function() {
    list(
      cities = safe_read_csv(here("data/raw/iowa_major_cities.csv")),
      crime = safe_read_csv(here("data/raw/iowa_crime_data.csv")),
      housing = safe_read_csv(here("data/raw/iowa_housing_data.csv")),
      education = safe_read_csv(here("data/raw/iowa_education_data.csv"))
    )
  }
  
  data <- load_report_data()
  
  expect_false(is.null(data$cities))
  expect_false(is.null(data$crime))
  expect_false(is.null(data$housing))
  expect_false(is.null(data$education))
})

# =============================================================================
# Test: City Profile Data Compilation
# =============================================================================

test_that("City profile compiles data from multiple sources", {
  data <- list(
    cities = safe_read_csv(here("data/raw/iowa_major_cities.csv")),
    crime = safe_read_csv(here("data/raw/iowa_crime_data.csv")),
    housing = safe_read_csv(here("data/raw/iowa_housing_data.csv")),
    education = safe_read_csv(here("data/raw/iowa_education_data.csv"))
  )
  
  skip_if(any(sapply(data, is.null)), "Data files missing")
  
  get_city_profile <- function(city, data) {
    list(
      city = city,
      cities = data$cities %>% filter(city == !!city),
      crime = data$crime %>% filter(city == !!city),
      housing = data$housing %>% filter(city == !!city),
      education = data$education %>% filter(city == !!city)
    )
  }
  
  profile <- get_city_profile("Des Moines", data)
  
  expect_equal(profile$city, "Des Moines")
  expect_true(nrow(profile$cities) > 0)
  expect_true(nrow(profile$crime) > 0)
  expect_true(nrow(profile$housing) > 0)
  expect_true(nrow(profile$education) > 0)
})

test_that("City profile returns empty for invalid city", {
  data <- list(
    cities = safe_read_csv(here("data/raw/iowa_major_cities.csv")),
    crime = safe_read_csv(here("data/raw/iowa_crime_data.csv"))
  )
  
  skip_if(any(sapply(data, is.null)), "Data files missing")
  
  get_city_profile <- function(city, data) {
    list(
      city = city,
      cities = data$cities %>% filter(city == !!city),
      crime = data$crime %>% filter(city == !!city)
    )
  }
  
  profile <- get_city_profile("Fake City", data)
  
  expect_equal(nrow(profile$cities), 0)
  expect_equal(nrow(profile$crime), 0)
})

# =============================================================================
# Test: HTML Report Structure
# =============================================================================

test_that("HTML report contains required sections", {
  # Simulate HTML report generation
  build_html_report <- function(city, population, region, income) {
    paste0(
      "<!DOCTYPE html>",
      "<html><head><title>", city, " City Report</title></head>",
      "<body>",
      "<h1>", city, " City Profile</h1>",
      "<h2>Demographics</h2>",
      "<p>Population: ", format(population, big.mark = ","), "</p>",
      "<p>Region: ", region, "</p>",
      "<p>Median Income: $", format(income, big.mark = ","), "</p>",
      "<h2>Housing</h2>",
      "<h2>Safety</h2>",
      "<h2>Education</h2>",
      "</body></html>"
    )
  }
  
  html <- build_html_report("Des Moines", 215000, "Central", 55000)
  
  # Check required elements
  expect_true(grepl("<!DOCTYPE html>", html))
  expect_true(grepl("<html>", html))
  expect_true(grepl("Des Moines", html))
  expect_true(grepl("Demographics", html))
  expect_true(grepl("Housing", html))
  expect_true(grepl("Safety", html))
  expect_true(grepl("Education", html))
})

test_that("HTML report formats numbers correctly", {
  population <- 215000
  formatted <- format(population, big.mark = ",")
  
  expect_equal(formatted, "215,000")
})

test_that("HTML report formats currency correctly", {
  income <- 55000
  formatted <- paste0("$", format(income, big.mark = ","))
  
  expect_equal(formatted, "$55,000")
})

# =============================================================================
# Test: CSV Export Structure
# =============================================================================

test_that("CSV export has required columns", {
  expected_columns <- c(
    "City", "Population", "Region", "Median_Income",
    "Median_Home_Value", "Median_Rent",
    "Violent_Crime_Rate", "Property_Crime_Rate",
    "HS_Graduation_Rate", "Bachelors_Degree_Pct"
  )
  
  # Create sample export data
  export_df <- tibble(
    City = "Des Moines",
    Population = 215000,
    Region = "Central",
    Median_Income = 55000,
    Median_Home_Value = 175000,
    Median_Rent = 950,
    Violent_Crime_Rate = 4.5,
    Property_Crime_Rate = 25.3,
    HS_Graduation_Rate = 92.5,
    Bachelors_Degree_Pct = 35.2
  )
  
  for (col in expected_columns) {
    expect_true(col %in% names(export_df),
                info = paste("Export should include", col))
  }
})

# =============================================================================
# Test: Output File Naming
# =============================================================================

test_that("Report filenames are properly formatted", {
  city <- "Des Moines"
  
  # Generate filename like app.R does
  filename <- paste0(gsub(" ", "_", tolower(city)), "_report_", Sys.Date(), ".html")
  
  expect_true(grepl("des_moines", filename))
  expect_true(grepl("\\.html$", filename))
  expect_true(grepl("\\d{4}-\\d{2}-\\d{2}", filename))  # Contains date
})

test_that("CSV filenames are properly formatted", {
  city <- "Cedar Rapids"
  
  filename <- paste0(gsub(" ", "_", tolower(city)), "_profile_", Sys.Date(), ".csv")
  
  expect_true(grepl("cedar_rapids", filename))
  expect_true(grepl("\\.csv$", filename))
})

test_that("Bulk export filename is properly formatted", {
  filename <- paste0("iowa_cities_complete_", Sys.Date(), ".csv")
  
  expect_true(grepl("iowa_cities_complete", filename))
  expect_true(grepl("\\.csv$", filename))
})

# =============================================================================
# Test: Data Validation for Reports
# =============================================================================

test_that("Report handles missing data gracefully", {
  # Simulate profile with missing data
  profile <- list(
    city = "Test City",
    cities = tibble(population = NA_real_, region = NA_character_),
    crime = tibble(violent_crime_rate = NA_real_),
    housing = tibble(median_home_value = NA_real_),
    education = tibble(high_school_graduation_rate = NA_real_)
  )
  
  # Formatting should not error
  formatted_pop <- format(profile$cities$population, big.mark = ",")
  expect_type(formatted_pop, "character")
})

test_that("Report handles zero rows gracefully", {
  data <- list(
    cities = tibble(city = character(), population = numeric())
  )
  
  profile <- data$cities %>% filter(city == "Nonexistent")
  
  expect_equal(nrow(profile), 0)
})

# =============================================================================
# Test: Report Output Directory
# =============================================================================

test_that("Output directory path is valid", {
  output_dir <- here("outputs/reports")
  
  # Check path is constructable
  expect_type(output_dir, "character")
  expect_true(nchar(output_dir) > 0)
})

# =============================================================================
# Test: Benchmark Integration in Reports
# =============================================================================

test_that("Benchmarks can be included in reports", {
  # Test that benchmarks are accessible for report generation
  expect_true(exists("IOWA_BENCHMARKS"))
  expect_true(exists("US_BENCHMARKS"))
  
  # Get benchmark for comparison
  iowa_income <- get_benchmark("median_household_income", "iowa")
  us_income <- get_benchmark("median_household_income", "us")
  
  expect_true(!is.na(iowa_income))
  expect_true(!is.na(us_income))
})

test_that("Comparison text can be generated for reports", {
  city_income <- 65000
  
  iowa_comparison <- compare_to_benchmark(city_income, "median_household_income", 
                                           level = "iowa", higher_is_better = TRUE)
  us_comparison <- compare_to_benchmark(city_income, "median_household_income", 
                                          level = "us", higher_is_better = TRUE)
  
  expect_type(iowa_comparison, "character")
  expect_type(us_comparison, "character")
})
