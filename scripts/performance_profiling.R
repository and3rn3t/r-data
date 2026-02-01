# Performance Profiling for Iowa Cities Dashboard
# Analyze bottlenecks and optimize performance
# ========================================

library(here)
library(profvis)  # For profiling
library(bench)    # For benchmarking

#' Profile data loading performance
#' @return A profvis object with profiling results
profile_data_loading <- function() {
  source(here("scripts/utils.R"))
  
  profvis::profvis({
    # Profile each data load
    crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
    housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
    education <- safe_read_csv(here("data/raw/iowa_education_data.csv"))
    economic <- safe_read_csv(here("data/raw/iowa_economic_data.csv"))
  })
}

#' Benchmark key operations
#' @return A tibble with benchmark results
benchmark_operations <- function() {
  source(here("scripts/utils.R"))
  source(here("scripts/constants.R"))
  
  # Load test data
  crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
  housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
  
  bench::mark(
    # Benchmark normalization
    normalize_small = normalize(1:100),
    normalize_large = normalize(1:10000),
    
    # Benchmark region lookups
    region_lookup = get_region("Des Moines"),
    
    # Benchmark benchmark lookups
    benchmark_lookup = get_benchmark("housing", "median_home_value"),
    
    # Benchmark data filtering
    filter_city = filter(crime, city == "Des Moines"),
    
    iterations = 100,
    check = FALSE
  )
}

#' Profile Shiny reactive calculations
#' @description Must be run while app is running
profile_shiny_reactives <- function() {
  message("To profile Shiny reactives, add this to your app.R server function:")
  message("")
  message('  shiny::shinyOptions(profile = TRUE)')
  message('  shiny::setShinyOption("profile", TRUE)')
  message("")
  message("Then run the app and check the 'Profiler' tab in RStudio.")
}

#' Analyze cached vs uncached performance
#' @return Comparison data frame
compare_cache_performance <- function() {
  source(here("scripts/utils.R"))
  
  cache_file <- here("data/cache/city_scores.rds")
  
  # Time cached load
  cached_time <- if (file.exists(cache_file)) {
    system.time(readRDS(cache_file))[[3]]
  } else {
    NA
  }
  
  # Time fresh computation (approximate)
  fresh_time <- system.time({
    crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
    housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
    # Add more data loads as needed
  })[[3]]
  
  data.frame(
    operation = c("Cached Load", "Fresh Computation"),
    time_seconds = c(cached_time, fresh_time),
    speedup = c(fresh_time / cached_time, 1)
  )
}

#' Memory usage analysis
#' @return Memory usage summary
analyze_memory_usage <- function() {
  source(here("scripts/utils.R"))
  
  initial_mem <- pryr::mem_used()
  
  # Load all data
  crime <- safe_read_csv(here("data/raw/iowa_crime_data.csv"))
  after_crime <- pryr::mem_used()
  
  housing <- safe_read_csv(here("data/raw/iowa_housing_data.csv"))
  after_housing <- pryr::mem_used()
  
  education <- safe_read_csv(here("data/raw/iowa_education_data.csv"))
  after_education <- pryr::mem_used()
  
  data.frame(
    stage = c("Initial", "After Crime", "After Housing", "After Education"),
    memory_mb = c(initial_mem, after_crime, after_housing, after_education) / 1e6,
    delta_mb = c(0, 
                 (after_crime - initial_mem) / 1e6,
                 (after_housing - after_crime) / 1e6,
                 (after_education - after_housing) / 1e6)
  )
}

#' Generate performance report
#' @param output_file Path to save report
generate_performance_report <- function(output_file = here("outputs/reports/performance_report.html")) {
  # Ensure output directory exists
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  
  cat("Performance Profiling Report\n")
  cat("============================\n\n")
  
  cat("1. Benchmark Results:\n")
  print(benchmark_operations())
  
  cat("\n2. Cache Performance:\n")
  print(compare_cache_performance())
  
  cat("\nReport generation complete.\n")
  cat("For detailed profiling, run profile_data_loading() in RStudio.\n")
}

# Run if executed directly
if (sys.nframe() == 0) {
  cat("Iowa Cities Dashboard - Performance Profiling\n")
  cat("==============================================\n\n")
  
  if (!requireNamespace("bench", quietly = TRUE)) {
    cat("Installing bench package...\n")
    install.packages("bench")
  }
  
  cat("Running benchmarks...\n\n")
  results <- benchmark_operations()
  print(results)
}
