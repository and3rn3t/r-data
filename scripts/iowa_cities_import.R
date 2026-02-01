# Iowa Cities Data Import Script
# This script imports city data for the state of Iowa from various sources

# Load required packages
library(tidyverse)
library(here)
library(janitor)

# Source setup and utilities
source(here("scripts/00_setup.R"))
source(here("scripts/utils.R"))

# =============================================================================
# Iowa Cities Data Sources
# =============================================================================

# Option 1: Iowa Data Portal - City Population Data
# The Iowa Data Portal provides various datasets about Iowa cities
iowa_cities_url <- "https://data.iowa.gov/api/views/qtnr-zsrc/rows.csv?accessType=DOWNLOAD"

# Try to download Iowa city data
tryCatch({
  iowa_cities <- read_csv(iowa_cities_url) %>%
    clean_names()
  
  cat("Successfully downloaded Iowa cities data from Iowa Data Portal\n")
  cat("Dimensions:", nrow(iowa_cities), "rows x", ncol(iowa_cities), "columns\n")
  
  # Save raw data
  write_csv(iowa_cities, here("data/raw/iowa_cities_raw.csv"))
  cat("Saved raw data to: data/raw/iowa_cities_raw.csv\n")
  
}, error = function(e) {
  cat("Could not download from Iowa Data Portal. Error:", e$message, "\n")
  cat("Please download data manually or use alternative source below.\n")
})

# Option 2: US Census Bureau - Iowa Places
# Census data for incorporated places in Iowa
census_iowa_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/cities/totals/sub-est2023.csv"

tryCatch({
  # Download full census places data
  census_places <- read_csv(census_iowa_url) %>%
    clean_names()
  
  # Filter to Iowa only (FIPS state code 19)
  iowa_census <- census_places %>%
    filter(state == 19) %>%
    filter(sumlev == 162)  # 162 = incorporated places
  
  cat("\nSuccessfully downloaded Census Bureau Iowa places data\n")
  cat("Found", nrow(iowa_census), "incorporated places in Iowa\n")
  
  # Save Census data
  write_csv(iowa_census, here("data/raw/iowa_cities_census.csv"))
  cat("Saved Census data to: data/raw/iowa_cities_census.csv\n")
  
}, error = function(e) {
  cat("Could not download Census data. Error:", e$message, "\n")
})

# =============================================================================
# Manual Data Entry - Top Iowa Cities
# =============================================================================

# If online sources are unavailable, here's a starter dataset of major Iowa cities
iowa_major_cities <- tibble(
  city = c("Des Moines", "Cedar Rapids", "Davenport", "Sioux City", "Iowa City",
           "Waterloo", "Ames", "West Des Moines", "Ankeny", "Council Bluffs",
           "Dubuque", "Urbandale", "Cedar Falls", "Marion", "Bettendorf",
           "Mason City", "Marshalltown", "Clinton", "Burlington", "Fort Dodge"),
  county = c("Polk", "Linn", "Scott", "Woodbury", "Johnson",
             "Black Hawk", "Story", "Polk", "Polk", "Pottawattamie",
             "Dubuque", "Polk", "Black Hawk", "Linn", "Scott",
             "Cerro Gordo", "Marshall", "Clinton", "Des Moines", "Webster"),
  population_2020 = c(214237, 137710, 101724, 85797, 74828,
                      67314, 66427, 68723, 67355, 62230,
                      59667, 45580, 40713, 40359, 39102,
                      27088, 26486, 24462, 24505, 24264),
  latitude = c(41.5868, 41.9779, 41.5236, 42.4963, 41.6611,
               42.4928, 42.0308, 41.5772, 41.7318, 41.2619,
               42.5006, 41.6267, 42.5349, 42.0341, 41.5246,
               43.1536, 42.0494, 41.8445, 40.8075, 42.4974),
  longitude = c(-93.6250, -91.6656, -90.5776, -96.4003, -91.5302,
                -92.3426, -93.6319, -93.7113, -93.6050, -95.8608,
                -90.6646, -93.7122, -92.4535, -91.5946, -90.5160,
                -93.2010, -92.9080, -90.1887, -91.1129, -94.1680),
  region = c("Central", "East Central", "East", "Northwest", "East Central",
             "Northeast", "Central", "Central", "Central", "Southwest",
             "Northeast", "Central", "Northeast", "East Central", "East",
             "North Central", "Central", "East", "Southeast", "North Central")
)

# Save the manual dataset as a backup
write_csv(iowa_major_cities, here("data/raw/iowa_major_cities.csv"))
cat("\nSaved major Iowa cities reference data to: data/raw/iowa_major_cities.csv\n")

# =============================================================================
# Summary of Available Data
# =============================================================================

cat("\n=== Iowa Cities Data Import Summary ===\n")
cat("Check the data/raw folder for available datasets.\n")
cat("Available data files:\n")

raw_files <- list.files(here("data/raw"), pattern = "\\.csv$")
if (length(raw_files) > 0) {
  for (f in raw_files) {
    cat("  -", f, "\n")
  }
} else {
  cat("  No CSV files found in data/raw/\n")
}

cat("\nNext steps:\n")
cat("1. Run scripts/02_data_cleaning.R to clean the data\n")
cat("2. Run scripts/iowa_cities_analysis.R to analyze Iowa city data\n")
