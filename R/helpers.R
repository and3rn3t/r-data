# Helper Functions for Iowa Cities Dashboard
# Utility functions for data loading, validation, and rendering
# =============================================================

library(dplyr)
library(readr)
library(here)

# =============================================================================
# DATA LOADING
# =============================================================================

#' Load all raw datasets
#'
#' @param data_files Named vector of file names
#' @param data_dir Directory containing data files
#' @return Named list of data frames
#' @export
load_all_data <- function(data_files = NULL, data_dir = here("data/raw")) {
  if (is.null(data_files)) {
    data_files <- c(
      major_cities = "iowa_major_cities.csv",
      crime = "iowa_crime_data.csv",
      housing = "iowa_housing_data.csv",
      education = "iowa_education_data.csv",
      economic = "iowa_economic_data.csv",
      healthcare = "iowa_healthcare_data.csv",
      demographics = "iowa_demographics_data.csv",
      environment = "iowa_environment_data.csv",
      amenities = "iowa_amenities_data.csv",
      infrastructure = "iowa_infrastructure_data.csv",
      historical = "iowa_historical_data.csv",
      # New datasets
      family = "iowa_family_data.csv",
      climate = "iowa_climate_data.csv",
      senior = "iowa_senior_data.csv",
      pets = "iowa_pets_data.csv"
    )
  }

  datasets <- list()
  for (name in names(data_files)) {
    path <- file.path(data_dir, data_files[[name]])
    if (file.exists(path)) {
      datasets[[name]] <- tryCatch(
        read_csv(path, show_col_types = FALSE),
        error = function(e) {
          message("Failed to load ", name, ": ", e$message)
          NULL
        }
      )
    }
  }

  # Rename 'pets' to 'pet' for consistency with scoring
  if (!is.null(datasets$pets)) {
    datasets$pet <- datasets$pets
    datasets$pets <- NULL
  }

  datasets
}

#' Load cached data or compute from raw files
#'
#' @param cache_hours Maximum cache age in hours
#' @return List with scores and data
#' @export
load_cached_or_compute <- function(cache_hours = 24) {
  cache_file <- here("data/cache/city_scores.rds")
  cache_info_file <- here("data/cache/cache_info.rds")

  if (file.exists(cache_file) && file.exists(cache_info_file)) {
    cache_info <- tryCatch(readRDS(cache_info_file), error = function(e) NULL)

    if (!is.null(cache_info) &&
          difftime(Sys.time(), cache_info$created, units = "hours") < cache_hours) {
      message("Loading cached city scores...")
      return(list(
        scores = readRDS(cache_file),
        data = readRDS(here("data/cache/all_datasets.rds")),
        cache_time = cache_info$created
      ))
    }
  }

  message("Computing city scores (run cache_city_data.R to speed up)...")
  data <- load_all_data()

  # Source scoring module if not already loaded
  if (!exists("calculate_city_scores")) {
    source(here("R/scoring.R"))
  }

  scores <- calculate_city_scores(data)

  list(
    scores = scores,
    data = data,
    cache_time = NULL
  )
}

#' Get cache freshness message
#'
#' @param cache_time POSIXct cache time or NULL
#' @return Character string describing data freshness
#' @export
get_cache_freshness <- function(cache_time) {
  if (is.null(cache_time)) {
    "Live data"
  } else {
    hours_ago <- as.numeric(difftime(Sys.time(), cache_time, units = "hours"))

    if (hours_ago < 1) {
      "Updated < 1 hour ago"
    } else if (hours_ago < 24) {
      paste0("Updated ", round(hours_ago), " hours ago")
    } else {
      days_ago <- round(hours_ago / 24)
      paste0("Updated ", days_ago, " days ago")
    }
  }
}

# =============================================================================
# VALIDATION HELPERS
# =============================================================================

#' Validate city input against known cities
#'
#' @param city City name to validate
#' @param valid_cities Vector of valid city names
#' @return TRUE if valid, FALSE otherwise
#' @export
validate_city <- function(city, valid_cities) {
  !is.null(city) && city %in% valid_cities
}

#' Safely filter data by city
#'
#' @param data Data frame to filter
#' @param city City name
#' @param valid_cities Optional vector of valid cities
#' @return Filtered data frame or empty data frame
#' @export
safe_filter_city <- function(data, city, valid_cities = NULL) {
  if (!is.null(valid_cities) && !validate_city(city, valid_cities)) {
    return(data[0, ])
  }
  filter(data, city == !!city)
}

# Note: validate_category() is defined in scripts/security.R with additional
# input validation (length checks, type checks). Use that version for user input.
# This simple wrapper is kept for internal use where input is already validated.

# =============================================================================
# RENDERING HELPERS
# =============================================================================

#' Safe render wrapper with error handling
#'
#' @param expr Expression to evaluate
#' @param default Default value on error
#' @param error_msg Error message to log
#' @return Result of expression or default
#' @export
safe_render <- function(expr, default = NULL, error_msg = "Error loading data") {
  tryCatch(
    expr,
    error = function(e) {
      message("Shiny error: ", e$message)
      default
    }
  )
}

#' Format number with commas
#'
#' @param x Numeric value
#' @return Formatted string
#' @export
format_number <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

#' Format currency
#'
#' @param x Numeric value
#' @param prefix Currency prefix (default "$")
#' @return Formatted string
#' @export
format_currency <- function(x, prefix = "$") {
  paste0(prefix, format(round(x), big.mark = ","))
}

#' Format percentage
#'
#' @param x Numeric value
#' @param digits Decimal places
#' @return Formatted string
#' @export
format_pct <- function(x, digits = 1) {
  paste0(round(x, digits), "%")
}

# =============================================================================
# SESSION LOGGING
# =============================================================================

#' Create session logger
#'
#' @param session Shiny session object
#' @param log_dir Directory for log files
#' @return List with log functions
#' @export
create_session_logger <- function(session, log_dir = here("outputs/logs")) {
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  session_id <- paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1))
  log_file <- file.path(log_dir, paste0("usage_", format(Sys.Date(), "%Y%m%d"), ".log"))

  log_event <- function(event, details = "") {
    tryCatch({
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- paste(timestamp, session_id, event, details, sep = " | ")
      cat(log_entry, "\n", file = log_file, append = TRUE)
    }, error = function(e) NULL)
  }

  list(
    session_id = session_id,
    log_file = log_file,
    log = log_event
  )
}

# =============================================================================
# URL SHARING
# =============================================================================

#' Generate shareable URL with query parameters
#'
#' @param base_url Base URL of the app
#' @param weights Named list of weight values
#' @param lifestyle_mode Lifestyle mode name
#' @return URL string
#' @export
generate_share_url <- function(base_url, weights, lifestyle_mode = "custom") {
  params <- list(mode = lifestyle_mode)

  # Add non-zero weights
  for (name in names(weights)) {
    if (weights[[name]] > 0) {
      params[[paste0("w_", name)]] <- weights[[name]]
    }
  }

  query <- paste(names(params), params, sep = "=", collapse = "&")
  paste0(base_url, "?", query)
}

#' Parse weights from URL query parameters
#'
#' @param query Named list from parseQueryString
#' @return Named list of weights
#' @export
parse_url_weights <- function(query) {
  weights <- list(
    safety = 5, housing = 5, education = 5, economy = 5,
    healthcare = 5, livability = 5, connectivity = 3,
    family = 0, climate = 0, senior = 0, pet = 0
  )

  for (name in names(query)) {
    if (startsWith(name, "w_")) {
      weight_name <- sub("w_", "", name)
      if (weight_name %in% names(weights)) {
        weights[[weight_name]] <- as.numeric(query[[name]])
      }
    }
  }

  weights
}

# =============================================================================
# STATE AVERAGE CALCULATIONS
# =============================================================================

#' Calculate Iowa state averages for comparison
#'
#' @param scores Data frame of city scores
#' @return Single row data frame with state averages
#' @export
calculate_state_average <- function(scores) {
  tibble(
    city = "Iowa State Average",
    region = "Statewide",
    population = sum(scores$population, na.rm = TRUE),
    overall_score = mean(scores$overall_score, na.rm = TRUE),
    safety_score = mean(scores$safety_score, na.rm = TRUE),
    housing_score = mean(scores$housing_score, na.rm = TRUE),
    education_score = mean(scores$education_score, na.rm = TRUE),
    economic_score = mean(scores$economic_score, na.rm = TRUE),
    healthcare_score = mean(scores$healthcare_score, na.rm = TRUE),
    livability_score_calc = mean(scores$livability_score_calc, na.rm = TRUE),
    connectivity_score = mean(scores$connectivity_score, na.rm = TRUE),
    family_score = mean(scores$family_score, na.rm = TRUE),
    climate_score = mean(scores$climate_score, na.rm = TRUE),
    senior_score = mean(scores$senior_score, na.rm = TRUE),
    pet_score = mean(scores$pet_score, na.rm = TRUE)
  )
}

cat("✓ Helper functions loaded\n")
