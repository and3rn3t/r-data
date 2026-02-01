# Iowa Cities API - Plumber REST Endpoints
# Provides programmatic access to Iowa cities data
# ========================================
# 
# Run with: plumber::plumb("api.R")$run(port = 8000)
# Or: Rscript -e "plumber::plumb('api.R')\$run(host='0.0.0.0', port=8000)"

library(plumber)
library(tidyverse)
library(here)
library(jsonlite)

# Load utilities and data
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))
source(here("scripts/security.R"))

# =============================================================================
# LOAD DATA AT STARTUP
# =============================================================================

load_api_data <- function() {
  list(
    crime = safe_read_csv(here("data/raw/iowa_crime_data.csv")),
    housing = safe_read_csv(here("data/raw/iowa_housing_data.csv")),
    education = safe_read_csv(here("data/raw/iowa_education_data.csv")),
    economic = safe_read_csv(here("data/raw/iowa_economic_data.csv")),
    healthcare = safe_read_csv(here("data/raw/iowa_healthcare_data.csv")),
    demographics = safe_read_csv(here("data/raw/iowa_demographics_data.csv")),
    major_cities = safe_read_csv(here("data/raw/iowa_major_cities.csv"))
  )
}

api_data <- load_api_data()

# Pre-compute city scores
compute_scores <- function() {
  tryCatch({
    # Simplified score computation
    api_data$major_cities %>%
      left_join(api_data$crime, by = "city") %>%
      left_join(api_data$housing, by = "city") %>%
      left_join(api_data$education, by = "city") %>%
      left_join(api_data$economic, by = "city") %>%
      mutate(
        safety_score = 100 - normalize(violent_crime_rate %||% 0),
        housing_score = 100 - normalize(median_home_value %||% 200000),
        education_score = normalize(graduation_rate %||% 85),
        economic_score = normalize(median_household_income %||% 50000)
      ) %>%
      mutate(
        overall_score = (safety_score + housing_score + education_score + economic_score) / 4
      ) %>%
      arrange(desc(overall_score))
  }, error = function(e) {
    message("Error computing scores: ", e$message)
    data.frame()
  })
}

city_scores <- compute_scores()

# =============================================================================
# API CONFIGURATION
# =============================================================================

#* @apiTitle Iowa Cities Data API
#* @apiDescription REST API for accessing Iowa cities data and analytics
#* @apiVersion 1.0.0
#* @apiContact list(name = "API Support", email = "support@example.com")
#* @apiLicense list(name = "MIT", url = "https://opensource.org/licenses/MIT")

# =============================================================================
# FILTERS (Middleware)
# =============================================================================

#* Add CORS headers
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, X-API-Key")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

#* Add security headers
#* @filter security
function(req, res) {
  headers <- get_security_headers()
  for (name in names(headers)) {
    res$setHeader(name, headers[[name]])
  }
  plumber::forward()
}

#* Log API requests
#* @filter logger
function(req, res) {
  log_dir <- here("outputs/logs/api")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  log_file <- file.path(log_dir, paste0("api_", format(Sys.Date(), "%Y%m%d"), ".log"))
  
  tryCatch({
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- paste(
      timestamp,
      req$REQUEST_METHOD,
      req$PATH_INFO,
      req$REMOTE_ADDR %||% "unknown",
      sep = " | "
    )
    cat(log_entry, "\n", file = log_file, append = TRUE)
  }, error = function(e) NULL)
  
  plumber::forward()
}

# =============================================================================
# HEALTH & INFO ENDPOINTS
# =============================================================================

#* API health check
#* @get /health
#* @tag System
#* @response 200 API is healthy
function() {
  list(
    status = "healthy",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    version = "1.0.0"
  )
}

#* Get API information
#* @get /info
#* @tag System
function() {
  list(
    name = "Iowa Cities Data API",
    version = "1.0.0",
    description = "REST API for Iowa cities demographics, economics, and quality of life data",
    endpoints = list(
      "/health" = "Health check",
      "/cities" = "List all cities",
      "/cities/{city}" = "Get city details",
      "/scores" = "Get city rankings",
      "/compare" = "Compare cities",
      "/benchmarks" = "Get Iowa/US benchmarks"
    ),
    data_updated = format(file.mtime(here("data/raw/iowa_major_cities.csv")), "%Y-%m-%d")
  )
}

# =============================================================================
# CITIES ENDPOINTS
# =============================================================================

#* List all Iowa cities
#* @get /cities
#* @tag Cities
#* @param region Filter by region (optional)
#* @param limit Maximum number of results (default 50)
#* @response 200 List of cities
function(region = NULL, limit = 50) {
  result <- api_data$major_cities
  
  # Validate and apply region filter
  if (!is.null(region) && region != "") {
    region <- sanitize_string(region, 50)
    if (!is.null(region)) {
      result <- filter(result, tolower(region) == tolower(!!region))
    }
  }
  
  # Validate limit
  limit <- validate_numeric_input(limit, 1, 100) %||% 50
  
  result %>%
    head(limit) %>%
    select(city, region, population, lat, lon) %>%
    as.list()
}

#* Get details for a specific city
#* @get /cities/<city>
#* @tag Cities
#* @param city City name
#* @response 200 City details
#* @response 404 City not found
function(city, res) {
  # Validate city input
  if (!validate_city_input(city, api_data$major_cities$city)) {
    res$status <- 404
    return(list(error = "City not found", city = city))
  }
  
  # Gather all data for city
  city_data <- list(
    basic = filter(api_data$major_cities, city == !!city),
    crime = filter(api_data$crime, city == !!city),
    housing = filter(api_data$housing, city == !!city),
    education = filter(api_data$education, city == !!city),
    economic = filter(api_data$economic, city == !!city)
  )
  
  # Combine into single response
  list(
    city = city,
    region = city_data$basic$region[1],
    population = city_data$basic$population[1],
    coordinates = list(
      lat = city_data$basic$lat[1],
      lon = city_data$basic$lon[1]
    ),
    crime = as.list(city_data$crime[1, ]),
    housing = as.list(city_data$housing[1, ]),
    education = as.list(city_data$education[1, ]),
    economic = as.list(city_data$economic[1, ])
  )
}

# =============================================================================
# SCORES & RANKINGS ENDPOINTS
# =============================================================================

#* Get city rankings and scores
#* @get /scores
#* @tag Rankings
#* @param category Score category (overall, safety, housing, education, economic)
#* @param order Sort order (desc or asc)
#* @param limit Maximum results (default 20)
#* @response 200 City scores
function(category = "overall", order = "desc", limit = 20) {
  # Validate category
  valid_categories <- c("overall", "safety", "housing", "education", "economic")
  category <- if (category %in% valid_categories) category else "overall"
  
  score_col <- paste0(category, "_score")
  
  # Validate limit
  limit <- validate_numeric_input(limit, 1, 50) %||% 20
  
  result <- city_scores
  
  # Sort
  if (order == "asc") {
    result <- arrange(result, .data[[score_col]])
  } else {
    result <- arrange(result, desc(.data[[score_col]]))
  }
  
  result %>%
    head(limit) %>%
    select(city, region, population, 
           overall_score, safety_score, housing_score, 
           education_score, economic_score) %>%
    mutate(across(ends_with("_score"), ~round(., 1)))
}

#* Get score for a specific city
#* @get /scores/<city>
#* @tag Rankings
#* @param city City name
#* @response 200 City score details
#* @response 404 City not found
function(city, res) {
  if (!validate_city_input(city, city_scores$city)) {
    res$status <- 404
    return(list(error = "City not found", city = city))
  }
  
  score_data <- filter(city_scores, city == !!city)
  
  list(
    city = city,
    rank = which(city_scores$city == city),
    total_cities = nrow(city_scores),
    scores = list(
      overall = round(score_data$overall_score[1], 1),
      safety = round(score_data$safety_score[1], 1),
      housing = round(score_data$housing_score[1], 1),
      education = round(score_data$education_score[1], 1),
      economic = round(score_data$economic_score[1], 1)
    )
  )
}

# =============================================================================
# COMPARISON ENDPOINTS
# =============================================================================

#* Compare multiple cities
#* @get /compare
#* @tag Comparison
#* @param cities Comma-separated list of city names (2-5 cities)
#* @response 200 Comparison data
#* @response 400 Invalid request
function(cities, res) {
  if (is.null(cities) || cities == "") {
    res$status <- 400
    return(list(error = "cities parameter is required"))
  }
  
  city_list <- strsplit(cities, ",")[[1]] %>% trimws()
  
  if (length(city_list) < 2 || length(city_list) > 5) {
    res$status <- 400
    return(list(error = "Provide 2-5 cities to compare"))
  }
  
  # Validate all cities
  valid_cities <- city_list[city_list %in% city_scores$city]
  
  if (length(valid_cities) < 2) {
    res$status <- 400
    return(list(
      error = "At least 2 valid cities required",
      valid_cities = city_scores$city
    ))
  }
  
  comparison <- city_scores %>%
    filter(city %in% valid_cities) %>%
    select(city, region, population,
           overall_score, safety_score, housing_score,
           education_score, economic_score) %>%
    mutate(across(ends_with("_score"), ~round(., 1)))
  
  # Determine winners per category
  categories <- c("overall", "safety", "housing", "education", "economic")
  winners <- lapply(categories, function(cat) {
    col <- paste0(cat, "_score")
    comparison$city[which.max(comparison[[col]])]
  })
  names(winners) <- categories
  
  list(
    cities = as.list(comparison),
    winners = winners
  )
}

# =============================================================================
# BENCHMARK ENDPOINTS
# =============================================================================

#* Get Iowa and US benchmarks
#* @get /benchmarks
#* @tag Benchmarks
#* @param category Optional category filter
#* @response 200 Benchmark data
function(category = NULL) {
  iowa <- IOWA_BENCHMARKS
  us <- US_BENCHMARKS
  
  if (!is.null(category) && category != "") {
    category <- sanitize_string(category, 50)
    if (!is.null(category) && category %in% names(iowa)) {
      iowa <- iowa[category]
      us <- us[category]
    }
  }
  
  list(
    iowa = iowa,
    us = us
  )
}

# =============================================================================
# DATA EXPORT ENDPOINTS
# =============================================================================

#* Export all city data as CSV
#* @get /export/csv
#* @tag Export
#* @serializer csv
#* @response 200 CSV file
function() {
  city_scores %>%
    select(city, region, population,
           overall_score, safety_score, housing_score,
           education_score, economic_score)
}

#* Export city data as JSON
#* @get /export/json
#* @tag Export
#* @response 200 JSON data
function() {
  city_scores %>%
    select(city, region, population,
           overall_score, safety_score, housing_score,
           education_score, economic_score) %>%
    mutate(across(ends_with("_score"), ~round(., 1)))
}
