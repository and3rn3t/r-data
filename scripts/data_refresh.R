# Data Refresh Pipeline - ETL Automation
# Scheduled data updates for Iowa Cities project
# ========================================

library(here)
library(tidyverse)
library(httr2)
library(jsonlite)

# Source utilities
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

# =============================================================================
# CONFIGURATION
# =============================================================================

# Data source URLs (placeholder - replace with actual API endpoints)
DATA_SOURCES <- list(
  census = list(
    url = "https://api.census.gov/data/2020/acs/acs5",
    api_key_env = "CENSUS_API_KEY"
  ),
  fbi_crime = list(
    url = "https://api.usa.gov/crime/fbi/cde",
    api_key_env = "FBI_API_KEY"
  ),
  bls = list(
    url = "https://api.bls.gov/publicAPI/v2/timeseries/data/",
    api_key_env = "BLS_API_KEY"
  )
)

# Refresh schedule (cron expressions)
REFRESH_SCHEDULE <- list(
  daily = "0 6 * * *",      # 6 AM daily
  weekly = "0 6 * * 0",     # 6 AM Sunday
  monthly = "0 6 1 * *"     # 6 AM first of month
)

# =============================================================================
# ETL LOGGING
# =============================================================================

#' Initialize ETL log
#' @return Path to log file
init_etl_log <- function() {
  log_dir <- here("outputs/logs/etl")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  log_file <- file.path(log_dir, paste0("etl_", format(Sys.Date(), "%Y%m%d"), ".log"))
  return(log_file)
}

#' Log ETL event
#' @param event Event type
#' @param message Event message
#' @param status success/warning/error
log_etl <- function(event, message, status = "info") {
  log_file <- init_etl_log()
  
  tryCatch({
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    entry <- paste(timestamp, toupper(status), event, message, sep = " | ")
    cat(entry, "\n", file = log_file, append = TRUE)
    
    if (status == "error") {
      message(crayon::red(entry))
    } else if (status == "warning") {
      message(crayon::yellow(entry))
    } else {
      message(entry)
    }
  }, error = function(e) NULL)
}

# =============================================================================
# EXTRACT FUNCTIONS
# =============================================================================

#' Fetch data from Census API
#' @param variables Vector of variable codes
#' @param geography Geography level (place, county, state)
#' @return Data frame with census data
extract_census_data <- function(variables, geography = "place") {
  log_etl("EXTRACT", "Starting Census API fetch", "info")
  
  api_key <- Sys.getenv("CENSUS_API_KEY")
  if (api_key == "") {
    log_etl("EXTRACT", "Census API key not found", "error")
    return(NULL)
  }
  
  tryCatch({
    # Build request
    response <- request(DATA_SOURCES$census$url) |>
      req_url_query(
        get = paste(variables, collapse = ","),
        `for` = paste0(geography, ":*"),
        `in` = "state:19",  # Iowa FIPS code
        key = api_key
      ) |>
      req_perform()
    
    if (resp_status(response) == 200) {
      data <- resp_body_json(response)
      log_etl("EXTRACT", paste("Census: Retrieved", length(data) - 1, "records"), "success")
      
      # Convert to data frame
      df <- as.data.frame(do.call(rbind, data[-1]))
      names(df) <- unlist(data[[1]])
      return(df)
    } else {
      log_etl("EXTRACT", paste("Census API error:", resp_status(response)), "error")
      return(NULL)
    }
  }, error = function(e) {
    log_etl("EXTRACT", paste("Census extract failed:", e$message), "error")
    return(NULL)
  })
}

#' Fetch data from local backup files
#' @param dataset Dataset name
#' @return Data frame
extract_local_backup <- function(dataset) {
  backup_dir <- here("data/backup")
  file_path <- file.path(backup_dir, paste0(dataset, ".csv"))
  
  if (file.exists(file_path)) {
    log_etl("EXTRACT", paste("Loading backup for", dataset), "info")
    return(safe_read_csv(file_path))
  }
  
  log_etl("EXTRACT", paste("No backup found for", dataset), "warning")
  return(NULL)
}

# =============================================================================
# TRANSFORM FUNCTIONS
# =============================================================================

#' Validate and clean extracted data
#' @param data Raw data frame
#' @param schema Expected column types
#' @return Cleaned data frame
transform_validate <- function(data, schema = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    log_etl("TRANSFORM", "No data to validate", "warning")
    return(data)
  }
  
  log_etl("TRANSFORM", paste("Validating", nrow(data), "rows"), "info")
  
  # Remove duplicates
  original_rows <- nrow(data)
  data <- distinct(data)
  if (nrow(data) < original_rows) {
    log_etl("TRANSFORM", paste("Removed", original_rows - nrow(data), "duplicates"), "info")
  }
  
  # Check for required columns
  if (!is.null(schema)) {
    missing_cols <- setdiff(names(schema), names(data))
    if (length(missing_cols) > 0) {
      log_etl("TRANSFORM", paste("Missing columns:", paste(missing_cols, collapse = ", ")), "warning")
    }
  }
  
  return(data)
}

#' Normalize city names to match project standards
#' @param data Data frame with city column
#' @return Data frame with normalized city names
transform_city_names <- function(data) {
  if (!"city" %in% names(data)) return(data)
  
  # Common name variations
  name_mapping <- c(
    "Des Moines city" = "Des Moines",
    "Cedar Rapids city" = "Cedar Rapids",
    "Davenport city" = "Davenport",
    "Sioux City city" = "Sioux City",
    "Iowa City city" = "Iowa City"
  )
  
  data <- data %>%
    mutate(city = str_remove(city, " city$")) %>%
    mutate(city = str_remove(city, ", Iowa$"))
  
  log_etl("TRANSFORM", "City names normalized", "info")
  return(data)
}

#' Calculate derived metrics
#' @param data Transformed data
#' @return Data with derived columns
transform_calculate_metrics <- function(data) {
  # Add any calculated fields
  if ("violent_crime_rate" %in% names(data) && "property_crime_rate" %in% names(data)) {
    data <- data %>%
      mutate(total_crime_rate = violent_crime_rate + property_crime_rate)
    log_etl("TRANSFORM", "Calculated total_crime_rate", "info")
  }
  
  return(data)
}

# =============================================================================
# LOAD FUNCTIONS
# =============================================================================

#' Create backup of existing data
#' @param dataset Dataset name
backup_existing <- function(dataset) {
  backup_dir <- here("data/backup")
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
  
  raw_file <- here("data/raw", paste0(dataset, ".csv"))
  if (file.exists(raw_file)) {
    backup_file <- file.path(backup_dir, paste0(dataset, "_", format(Sys.Date(), "%Y%m%d"), ".csv"))
    file.copy(raw_file, backup_file, overwrite = TRUE)
    log_etl("LOAD", paste("Backed up", dataset), "info")
  }
}

#' Load transformed data to destination
#' @param data Transformed data frame
#' @param dataset Dataset name
#' @param dry_run If TRUE, don't actually write
load_to_raw <- function(data, dataset, dry_run = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    log_etl("LOAD", paste("No data to load for", dataset), "warning")
    return(FALSE)
  }
  
  dest_file <- here("data/raw", paste0(dataset, ".csv"))
  
  if (dry_run) {
    log_etl("LOAD", paste("DRY RUN: Would write", nrow(data), "rows to", dataset), "info")
    return(TRUE)
  }
  
  # Backup existing
  backup_existing(dataset)
  
  # Write new data
  tryCatch({
    write_csv(data, dest_file)
    log_etl("LOAD", paste("Wrote", nrow(data), "rows to", dataset), "success")
    return(TRUE)
  }, error = function(e) {
    log_etl("LOAD", paste("Failed to write", dataset, ":", e$message), "error")
    return(FALSE)
  })
}

#' Update cache after data refresh
update_cache <- function() {
  cache_script <- here("scripts/update_cache.R")
  if (file.exists(cache_script)) {
    log_etl("LOAD", "Updating data cache", "info")
    tryCatch({
      source(cache_script)
      log_etl("LOAD", "Cache updated successfully", "success")
    }, error = function(e) {
      log_etl("LOAD", paste("Cache update failed:", e$message), "error")
    })
  }
}

# =============================================================================
# ETL ORCHESTRATION
# =============================================================================

#' Run full ETL pipeline for a dataset
#' @param dataset Dataset name
#' @param extract_fn Custom extract function
#' @param transform_fn Custom transform function
#' @param dry_run If TRUE, validate without writing
run_etl_pipeline <- function(dataset, extract_fn = NULL, transform_fn = NULL, dry_run = FALSE) {
  log_etl("PIPELINE", paste("Starting ETL for", dataset), "info")
  start_time <- Sys.time()
  
  # Extract
  if (!is.null(extract_fn)) {
    data <- extract_fn()
  } else {
    data <- extract_local_backup(dataset)
  }
  
  if (is.null(data)) {
    log_etl("PIPELINE", paste("Extract failed for", dataset), "error")
    return(FALSE)
  }
  
  # Transform
  data <- transform_validate(data)
  data <- transform_city_names(data)
  
  if (!is.null(transform_fn)) {
    data <- transform_fn(data)
  }
  
  data <- transform_calculate_metrics(data)
  
  # Load
  success <- load_to_raw(data, dataset, dry_run)
  
  elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
  log_etl("PIPELINE", paste("ETL for", dataset, "completed in", elapsed, "seconds"), 
          if (success) "success" else "error")
  
  return(success)
}

#' Run all ETL pipelines
#' @param dry_run If TRUE, validate without writing
run_all_etl <- function(dry_run = FALSE) {
  log_etl("PIPELINE", "Starting full data refresh", "info")
  
  datasets <- c(
    "iowa_crime_data",
    "iowa_housing_data",
    "iowa_education_data",
    "iowa_economic_data",
    "iowa_healthcare_data",
    "iowa_demographics_data"
  )
  
  results <- list()
  for (ds in datasets) {
    results[[ds]] <- run_etl_pipeline(ds, dry_run = dry_run)
  }
  
  # Update cache if any successful loads
  if (any(unlist(results)) && !dry_run) {
    update_cache()
  }
  
  # Summary
  success_count <- sum(unlist(results))
  log_etl("PIPELINE", paste("Completed:", success_count, "/", length(datasets), "datasets"), 
          if (success_count == length(datasets)) "success" else "warning")
  
  return(results)
}

# =============================================================================
# SCHEDULED EXECUTION
# =============================================================================

#' Check if data needs refresh
#' @param dataset Dataset name
#' @param max_age_days Maximum age in days before refresh needed
#' @return TRUE if refresh needed
needs_refresh <- function(dataset, max_age_days = 7) {
  file_path <- here("data/raw", paste0(dataset, ".csv"))
  
  if (!file.exists(file_path)) return(TRUE)
  
  file_age <- difftime(Sys.time(), file.mtime(file_path), units = "days")
  return(as.numeric(file_age) > max_age_days)
}

#' Run scheduled refresh
scheduled_refresh <- function() {
  log_etl("SCHEDULE", "Running scheduled data refresh check", "info")
  
  datasets_to_refresh <- c()
  
  # Check each dataset
  datasets <- c("iowa_crime_data", "iowa_housing_data", "iowa_education_data",
                "iowa_economic_data", "iowa_healthcare_data")
  
  for (ds in datasets) {
    if (needs_refresh(ds, max_age_days = 7)) {
      datasets_to_refresh <- c(datasets_to_refresh, ds)
    }
  }
  
  if (length(datasets_to_refresh) == 0) {
    log_etl("SCHEDULE", "All datasets are up to date", "info")
    return(invisible(NULL))
  }
  
  log_etl("SCHEDULE", paste("Refreshing:", paste(datasets_to_refresh, collapse = ", ")), "info")
  
  for (ds in datasets_to_refresh) {
    run_etl_pipeline(ds)
  }
}

# =============================================================================
# CLI INTERFACE
# =============================================================================

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    cat("Iowa Cities ETL Pipeline\n")
    cat("========================\n")
    cat("\nUsage:\n")
    cat("  Rscript data_refresh.R all         - Refresh all datasets\n")
    cat("  Rscript data_refresh.R check       - Check which datasets need refresh\n")
    cat("  Rscript data_refresh.R <dataset>   - Refresh specific dataset\n")
    cat("  Rscript data_refresh.R --dry-run   - Validate without writing\n")
  } else if (args[1] == "all") {
    dry_run <- "--dry-run" %in% args
    run_all_etl(dry_run = dry_run)
  } else if (args[1] == "check") {
    scheduled_refresh()
  } else {
    run_etl_pipeline(args[1], dry_run = "--dry-run" %in% args)
  }
}
