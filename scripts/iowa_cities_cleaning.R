# Iowa Cities Data Cleaning Script
# This script cleans and prepares Iowa city data for analysis

# Load required packages
library(tidyverse)
library(here)
library(janitor)

source(here("scripts/00_setup.R"))
source(here("scripts/utils.R"))

# =============================================================================
# Load Raw Data
# =============================================================================

cat("=== Iowa Cities Data Cleaning ===\n\n")

# Check which data files are available
raw_files <- list.files(here("data/raw"), pattern = "\\.csv$", full.names = TRUE)

if (length(raw_files) == 0) {
  stop("No raw data files found. Please run iowa_cities_import.R first.")
}

cat("Available raw data files:\n")
for (f in raw_files) {
  cat("  -", basename(f), "\n")
}

# Load the primary dataset (prefer Census data if available)
if (file.exists(here("data/raw/iowa_cities_census.csv"))) {
  iowa_raw <- read_csv(here("data/raw/iowa_cities_census.csv"))
  data_source <- "Census"
  cat("\nLoading Census Bureau data...\n")
} else if (file.exists(here("data/raw/iowa_cities_raw.csv"))) {
  iowa_raw <- read_csv(here("data/raw/iowa_cities_raw.csv"))
  data_source <- "Iowa Data Portal"
  cat("\nLoading Iowa Data Portal data...\n")
} else {
  iowa_raw <- read_csv(here("data/raw/iowa_major_cities.csv"))
  data_source <- "Manual Entry"
  cat("\nLoading manual entry data...\n")
}

cat("Source:", data_source, "\n")
cat("Initial dimensions:", nrow(iowa_raw), "rows x", ncol(iowa_raw), "columns\n\n")

# =============================================================================
# Data Cleaning Steps
# =============================================================================

# Step 1: Standardize column names
iowa_clean <- iowa_raw %>%
  clean_names()

cat("Step 1: Standardized column names\n")

# Step 2: Handle different data sources
if (data_source == "Census") {
  # Census data has specific column names
  iowa_clean <- iowa_clean %>%
    rename(
      city = name,
      population_2020 = popestimate2020
    ) %>%
    mutate(
      # Remove ", city" suffix from city names
      city = str_remove(city, " city$"),
      city = str_remove(city, " town$"),
      city = str_trim(city)
    ) %>%
    select(city, population_2020, everything())
    
} else if (data_source == "Iowa Data Portal") {
  # Iowa Data Portal may have different column names
  # Adjust based on actual column names
  iowa_clean <- iowa_clean %>%
    mutate(
      city = str_trim(city),
      city = str_to_title(city)
    )
}

cat("Step 2: Handled source-specific formatting\n")

# Step 3: Remove duplicates
n_before <- nrow(iowa_clean)
iowa_clean <- iowa_clean %>%
  distinct()
n_after <- nrow(iowa_clean)
cat("Step 3: Removed", n_before - n_after, "duplicate rows\n")

# Step 4: Handle missing values
missing_summary <- count_missing(iowa_clean)
cat("Step 4: Missing value summary:\n")
print(missing_summary %>% filter(missing_count > 0))

# Step 5: Data type conversions
iowa_clean <- iowa_clean %>%
  mutate(
    across(where(is.character), str_trim),
    across(contains("population"), as.numeric),
    across(contains("latitude") | contains("longitude"), as.numeric)
  )

cat("Step 5: Converted data types\n")

# Step 6: Add derived columns
iowa_clean <- iowa_clean %>%
  mutate(
    # Population size category
    size_category = case_when(
      population_2020 >= 100000 ~ "Large City",
      population_2020 >= 50000  ~ "Medium City",
      population_2020 >= 25000  ~ "Small City",
      population_2020 >= 10000  ~ "Town",
      population_2020 >= 5000   ~ "Small Town",
      TRUE                      ~ "Village"
    ),
    # Population rank
    population_rank = dense_rank(desc(population_2020))
  )

cat("Step 6: Added derived columns (size_category, population_rank)\n")

# Step 7: Add Iowa regions if not present
if (!"region" %in% names(iowa_clean) && "county" %in% names(iowa_clean)) {
  # Define Iowa regions by county
  iowa_regions <- tribble(
    ~county, ~region,
    "Polk", "Central",
    "Story", "Central",
    "Dallas", "Central",
    "Warren", "Central",
    "Jasper", "Central",
    "Marshall", "Central",
    "Boone", "Central",
    "Linn", "East Central",
    "Johnson", "East Central",
    "Benton", "East Central",
    "Iowa", "East Central",
    "Scott", "East",
    "Clinton", "East",
    "Muscatine", "East",
    "Woodbury", "Northwest",
    "Plymouth", "Northwest",
    "Sioux", "Northwest",
    "Black Hawk", "Northeast",
    "Dubuque", "Northeast",
    "Bremer", "Northeast",
    "Fayette", "Northeast",
    "Cerro Gordo", "North Central",
    "Webster", "North Central",
    "Hancock", "North Central",
    "Pottawattamie", "Southwest",
    "Mills", "Southwest",
    "Des Moines", "Southeast",
    "Lee", "Southeast",
    "Henry", "Southeast"
  )
  
  iowa_clean <- iowa_clean %>%
    left_join(iowa_regions, by = "county") %>%
    mutate(region = replace_na(region, "Other"))
  
  cat("Step 7: Added regional classifications\n")
}

# =============================================================================
# Data Validation
# =============================================================================

cat("\n=== Data Validation ===\n")

# Check for valid population values
invalid_pop <- iowa_clean %>%
  filter(is.na(population_2020) | population_2020 <= 0)

if (nrow(invalid_pop) > 0) {
  cat("Warning:", nrow(invalid_pop), "cities with invalid population values\n")
} else {
  cat("✓ All population values are valid\n")
}

# Check for valid coordinates (if present)
if (all(c("latitude", "longitude") %in% names(iowa_clean))) {
  invalid_coords <- iowa_clean %>%
    filter(
      latitude < 40 | latitude > 44 |  # Iowa latitude range
      longitude < -97 | longitude > -90  # Iowa longitude range
    )
  
  if (nrow(invalid_coords) > 0) {
    cat("Warning:", nrow(invalid_coords), "cities with coordinates outside Iowa\n")
  } else {
    cat("✓ All coordinates are within Iowa boundaries\n")
  }
}

# =============================================================================
# Save Cleaned Data
# =============================================================================

write_csv(iowa_clean, here("data/processed/iowa_cities_clean.csv"))
cat("\n✓ Cleaned data saved to: data/processed/iowa_cities_clean.csv\n")

# Save a summary report
sink(here("data/processed/cleaning_log.txt"))
cat("Iowa Cities Data Cleaning Log\n")
cat("=============================\n")
cat("Date:", as.character(Sys.time()), "\n")
cat("Source:", data_source, "\n")
cat("Original rows:", nrow(iowa_raw), "\n")
cat("Cleaned rows:", nrow(iowa_clean), "\n")
cat("Columns:", ncol(iowa_clean), "\n")
cat("\nColumn names:\n")
cat(paste("-", names(iowa_clean)), sep = "\n")
sink()

cat("✓ Cleaning log saved to: data/processed/cleaning_log.txt\n")

# =============================================================================
# Final Summary
# =============================================================================

cat("\n=== Cleaning Complete ===\n")
cat("Final dataset:", nrow(iowa_clean), "rows x", ncol(iowa_clean), "columns\n")
cat("\nColumn summary:\n")
glimpse(iowa_clean)
