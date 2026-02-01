# Pre-compute City Scores for Fast Shiny Loading
# Run this script to cache computed data for the dashboard
# ===========================================================

library(tidyverse)
library(here)

source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

cat("
╔═══════════════════════════════════════════════════════════════╗
║         PRE-COMPUTING CITY SCORES FOR SHINY                  ║
╚═══════════════════════════════════════════════════════════════╝
\n")

start_time <- Sys.time()

# =============================================================================
# 1. LOAD ALL DATA
# =============================================================================

cat("[1/4] Loading all datasets...\n")

datasets <- load_datasets(RAW_DATA_FILES)

# Validate required datasets loaded
required <- c("major_cities", "crime", "housing", "education", "economic")
loaded <- names(datasets[!sapply(datasets, is.null)])
if (!all(required %in% loaded)) {
  stop("Missing required datasets: ", paste(setdiff(required, loaded), collapse = ", "))
}

# =============================================================================
# 2. BUILD COMPREHENSIVE CITY DATA
# =============================================================================

cat("\n[2/4] Building comprehensive city dataset...\n")

# Start with major cities
city_data <- datasets$major_cities %>%
  select(city, county, population = population_2020, latitude, longitude, region)

# Safe join helper
safe_left_join <- function(base, new, by = "city") {
  if (is.null(new)) return(base)
  cols_to_keep <- setdiff(names(new), setdiff(names(base), by))
  left_join(base, new %>% select(all_of(cols_to_keep)), by = by)
}

# Join all datasets
city_data <- city_data %>%
  safe_left_join(datasets$crime) %>%
  safe_left_join(datasets$housing) %>%
  safe_left_join(datasets$education) %>%
  safe_left_join(datasets$economic) %>%
  safe_left_join(datasets$healthcare) %>%
  safe_left_join(datasets$demographics) %>%
  safe_left_join(datasets$environment) %>%
  safe_left_join(datasets$amenities)

cat("  ✓ Merged", ncol(city_data), "variables for", nrow(city_data), "cities\n")

# =============================================================================
# 3. CALCULATE SCORES
# =============================================================================

cat("\n[3/4] Calculating composite scores...\n")

# Safe normalize helper
safe_norm <- function(x, reverse = FALSE) {
  if (is.null(x) || all(is.na(x))) return(rep(50, length(x)))
  normalize(x, reverse = reverse)
}

city_scores <- city_data %>%
  mutate(
    # Safety Score
    safety_score = (
      safe_norm(violent_crime_rate, reverse = TRUE) * SAFETY_WEIGHTS$violent_crime +
      safe_norm(property_crime_rate, reverse = TRUE) * SAFETY_WEIGHTS$property_crime +
      safe_norm(officers_per_1000) * SAFETY_WEIGHTS$officers +
      safe_norm(avg_response_time_min, reverse = TRUE) * SAFETY_WEIGHTS$response_time
    ),
    
    # Housing Score
    housing_score = (
      safe_norm(median_home_value, reverse = TRUE) * HOUSING_WEIGHTS$home_value +
      safe_norm(median_rent, reverse = TRUE) * HOUSING_WEIGHTS$rent +
      safe_norm(owner_occupied_pct) * HOUSING_WEIGHTS$ownership +
      safe_norm(vacancy_rate, reverse = TRUE) * HOUSING_WEIGHTS$vacancy +
      safe_norm(home_value_change_1yr) * HOUSING_WEIGHTS$appreciation
    ),
    
    # Education Score
    education_score = (
      safe_norm(graduation_rate) * EDUCATION_WEIGHTS$graduation +
      safe_norm(college_readiness_pct) * EDUCATION_WEIGHTS$college_ready +
      safe_norm(pct_bachelors) * EDUCATION_WEIGHTS$bachelors +
      safe_norm(per_pupil_spending) * EDUCATION_WEIGHTS$spending +
      safe_norm(student_teacher_ratio, reverse = TRUE) * EDUCATION_WEIGHTS$student_teacher
    ),
    
    # Economic Score
    economic_score = (
      safe_norm(median_household_income) * ECONOMIC_WEIGHTS$income +
      safe_norm(unemployment_rate, reverse = TRUE) * ECONOMIC_WEIGHTS$unemployment +
      safe_norm(job_growth_rate) * ECONOMIC_WEIGHTS$job_growth +
      safe_norm(poverty_rate, reverse = TRUE) * ECONOMIC_WEIGHTS$poverty +
      safe_norm(businesses_per_1000) * ECONOMIC_WEIGHTS$businesses
    ),
    
    # Healthcare Score (simplified)
    healthcare_score = (
      safe_norm(life_expectancy) * 0.4 +
      safe_norm(health_insurance_coverage_pct) * 0.4 +
      safe_norm(primary_care_physicians_per_1000) * 0.2
    ),
    
    # Environment Score (simplified)
    environment_score = (
      safe_norm(air_quality_index, reverse = TRUE) * 0.3 +
      safe_norm(green_space_pct) * 0.3 +
      safe_norm(walkability_score) * 0.2 +
      safe_norm(tree_canopy_pct) * 0.2
    ),
    
    # Amenities Score (simplified)
    amenities_score = (
      safe_norm(livability_score) * 0.4 +
      safe_norm(restaurants_per_1000) * 0.3 +
      safe_norm(parks_per_10000) * 0.3
    ),
    
    # Overall Score
    overall_score = (
      safety_score * OVERALL_WEIGHTS$safety +
      housing_score * OVERALL_WEIGHTS$housing +
      education_score * OVERALL_WEIGHTS$education +
      economic_score * OVERALL_WEIGHTS$economic +
      healthcare_score * OVERALL_WEIGHTS$healthcare +
      environment_score * 0.05 +
      amenities_score * OVERALL_WEIGHTS$amenities
    ),
    
    # Rank
    overall_rank = dense_rank(desc(overall_score))
  )

cat("  ✓ Calculated 8 composite scores\n")

# =============================================================================
# 4. SAVE CACHED DATA
# =============================================================================

cat("\n[4/4] Saving cached data...\n")

# Create cache directory
cache_dir <- here("data/cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Save as RDS for fast loading
saveRDS(city_data, file.path(cache_dir, "city_data.rds"))
saveRDS(city_scores, file.path(cache_dir, "city_scores.rds"))
saveRDS(datasets, file.path(cache_dir, "all_datasets.rds"))

# Also save timestamp
cache_info <- list(
  created = Sys.time(),
  n_cities = nrow(city_data),
  n_variables = ncol(city_data),
  datasets = names(datasets[!sapply(datasets, is.null)])
)
saveRDS(cache_info, file.path(cache_dir, "cache_info.rds"))

cat("  ✓ Saved city_data.rds\n")
cat("  ✓ Saved city_scores.rds\n")
cat("  ✓ Saved all_datasets.rds\n")
cat("  ✓ Saved cache_info.rds\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat(rep("═", 60), "\n", sep = "")
cat("                    CACHE COMPLETE\n")
cat(rep("═", 60), "\n", sep = "")
cat(sprintf("  Cities cached:       %d\n", nrow(city_scores)))
cat(sprintf("  Variables:           %d\n", ncol(city_data)))
cat(sprintf("  Scores calculated:   %d\n", 8))
cat(sprintf("  Time elapsed:        %s\n", format_elapsed(start_time)))
cat(sprintf("  Cache location:      %s\n", cache_dir))
cat(rep("═", 60), "\n", sep = "")

cat("\n📊 To use cached data in Shiny:\n")
cat("  city_scores <- readRDS(here('data/cache/city_scores.rds'))\n")
