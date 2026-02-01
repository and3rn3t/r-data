# Iowa Comprehensive City Analysis
# Analyzes all datasets to create city rankings and insights
# ============================================================

library(tidyverse)
library(here)
library(scales)

# Load utility functions (includes normalize, safe_read_csv, validate_iowa_data, etc.)
source(here("scripts/utils.R"))

cat("
╔═══════════════════════════════════════════════════════════════╗
║         IOWA COMPREHENSIVE CITY ANALYSIS                      ║
║                                                               ║
║   Combining all datasets for holistic city rankings           ║
╚═══════════════════════════════════════════════════════════════╝
\n")

start_time <- Sys.time()

# =============================================================================
# 1. LOAD ALL DATASETS
# =============================================================================

cat("[1/6] Loading all datasets...\n")

# Define datasets to load with their file names
dataset_files <- c(
  crime = "iowa_crime_data.csv",
  housing = "iowa_housing_data.csv",
  education = "iowa_education_data.csv",
  major_cities = "iowa_major_cities.csv",
  economic = "iowa_economic_data.csv",
  healthcare = "iowa_healthcare_data.csv",
  infrastructure = "iowa_infrastructure_data.csv",
  amenities = "iowa_amenities_data.csv",
  demographics = "iowa_demographics_data.csv",
  environment = "iowa_environment_data.csv",
  historical = "iowa_historical_data.csv"
)

# Load all datasets with error handling
datasets <- load_datasets(dataset_files)

# Validate required datasets
required <- c("major_cities", "crime", "housing", "education", "economic")
missing_required <- setdiff(required, names(datasets[!sapply(datasets, is.null)]))
if (length(missing_required) > 0) {
  stop("Missing required datasets: ", paste(missing_required, collapse = ", "))
}

# Extract to individual variables for easier access
crime <- datasets$crime
housing <- datasets$housing
education <- datasets$education
major_cities <- datasets$major_cities
economic <- datasets$economic
healthcare <- datasets$healthcare
infrastructure <- datasets$infrastructure
amenities <- datasets$amenities
demographics <- datasets$demographics
environment <- datasets$environment
historical <- datasets$historical

cat("  ✓ Loaded", sum(!sapply(datasets, is.null)), "of", length(dataset_files), "datasets\n")

# =============================================================================
# 2. MERGE ALL DATA
# =============================================================================

cat("\n[2/6] Merging datasets...\n")

#' Safely join a dataset, removing duplicate columns
#' 
#' @param base Base data frame
#' @param new New data frame to join
#' @param by Join key
safe_join <- function(base, new, by = "city") {
  if (is.null(new)) return(base)
  
  # Remove columns that already exist in base (except join key)
  existing_cols <- intersect(names(new), names(base))
  cols_to_remove <- setdiff(existing_cols, by)
  new_clean <- new %>% select(-all_of(cols_to_remove))
  
  left_join(base, new_clean, by = by)
}

# Start with major cities for geographic info
iowa_complete <- major_cities %>%
  select(city, county, population_2020, latitude, longitude, region) %>%
  rename(population = population_2020)

# Join all datasets safely
iowa_complete <- iowa_complete %>%
  safe_join(crime) %>%
  safe_join(housing) %>%
  safe_join(education) %>%
  safe_join(economic) %>%
  safe_join(healthcare) %>%
  safe_join(infrastructure) %>%
  safe_join(amenities) %>%
  safe_join(demographics) %>%
  safe_join(environment) %>%
  safe_join(historical %>% select(-any_of(c("population_2020", "county"))))

cat("  ✓ Merged dataset has", nrow(iowa_complete), "cities and", 
    ncol(iowa_complete), "variables\n")

# =============================================================================
# 3. CREATE COMPOSITE SCORES
# =============================================================================

cat("\n[3/6] Calculating composite scores...\n")

#' Safely normalize a column, returning 50 if column doesn't exist
#' 
#' @param data Data frame
#' @param col_name Column name as string
#' @param reverse Whether to reverse the scale
safe_normalize <- function(data, col_name, reverse = FALSE) {
  if (!col_name %in% names(data)) {
    return(rep(50, nrow(data)))  # Neutral score if column missing
  }
  normalize(data[[col_name]], reverse = reverse)
}

# Calculate domain scores with safe column access
iowa_scores <- iowa_complete %>%
  mutate(
    # Safety Score (lower crime = higher score)
    safety_score = (
      safe_normalize(., "violent_crime_rate", reverse = TRUE) * 0.4 +
      safe_normalize(., "property_crime_rate", reverse = TRUE) * 0.3 +
      safe_normalize(., "officers_per_1000") * 0.15 +
      safe_normalize(., "avg_response_time_min", reverse = TRUE) * 0.15
    ),
    
    # Housing Affordability Score
    housing_score = (
      safe_normalize(., "median_home_value", reverse = TRUE) * 0.3 +
      safe_normalize(., "median_rent", reverse = TRUE) * 0.3 +
      safe_normalize(., "owner_occupied_pct") * 0.2 +
      safe_normalize(., "vacancy_rate", reverse = TRUE) * 0.1 +
      safe_normalize(., "home_value_change_1yr") * 0.1
    ),
    
    # Education Score
    education_score = (
      safe_normalize(., "graduation_rate") * 0.25 +
      safe_normalize(., "college_readiness_pct") * 0.25 +
      safe_normalize(., "pct_bachelors") * 0.2 +
      safe_normalize(., "per_pupil_spending") * 0.15 +
      safe_normalize(., "student_teacher_ratio", reverse = TRUE) * 0.15
    ),
    
    # Economic Opportunity Score
    economic_score = (
      safe_normalize(., "median_household_income") * 0.3 +
      safe_normalize(., "unemployment_rate", reverse = TRUE) * 0.25 +
      safe_normalize(., "job_growth_rate") * 0.2 +
      safe_normalize(., "poverty_rate", reverse = TRUE) * 0.15 +
      safe_normalize(., "businesses_per_1000") * 0.1
    ),
    
    # Healthcare Score
    healthcare_score = (
      safe_normalize(., "health_insurance_coverage_pct") * 0.25 +
      safe_normalize(., "primary_care_physicians_per_1000") * 0.25 +
      safe_normalize(., "life_expectancy") * 0.2 +
      safe_normalize(., "preventable_hospital_stays", reverse = TRUE) * 0.15 +
      safe_normalize(., "exercise_access_pct") * 0.15
    ),
    
    # Connectivity Score
    connectivity_score = (
      safe_normalize(., "broadband_coverage_pct") * 0.3 +
      safe_normalize(., "avg_internet_speed_mbps") * 0.25 +
      safe_normalize(., "ev_charging_stations") * 0.15 +
      safe_normalize(., "avg_commute_time_min", reverse = TRUE) * 0.15 +
      safe_normalize(., "work_from_home_pct") * 0.15
    ),
    
    # Livability/Amenities Score
    amenities_score = (
      safe_normalize(., "livability_score") * 0.25 +
      safe_normalize(., "restaurants_per_1000") * 0.15 +
      safe_normalize(., "parks_per_10000") * 0.15 +
      safe_normalize(., "fitness_centers_per_1000") * 0.15 +
      safe_normalize(., "annual_events_count") * 0.15 +
      safe_normalize(., "cost_of_living_index", reverse = TRUE) * 0.15
    ),
    
    # Overall Composite Score
    overall_score = (
      safety_score * 0.15 +
      housing_score * 0.15 +
      education_score * 0.15 +
      economic_score * 0.20 +
      healthcare_score * 0.15 +
      connectivity_score * 0.10 +
      amenities_score * 0.10
    )
  )

cat("  ✓ Calculated 8 composite scores\n")

# =============================================================================
# 4. GENERATE RANKINGS
# =============================================================================

cat("\n[4/6] Generating city rankings...\n")

# Overall rankings
overall_rankings <- iowa_scores %>%
  select(city, region, population, overall_score, 
         safety_score, housing_score, education_score, 
         economic_score, healthcare_score, connectivity_score, 
         amenities_score) %>%
  arrange(desc(overall_score)) %>%
  mutate(overall_rank = row_number())

cat("\n📊 TOP 10 IOWA CITIES (Overall Score):\n")
cat(rep("─", 60), "\n", sep = "")
overall_rankings %>%
  head(10) %>%
  mutate(
    display = sprintf("%2d. %-20s │ Score: %5.1f │ Pop: %,d", 
                      overall_rank, city, overall_score, population)
  ) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# Best in each category
cat("\n\n🏆 CATEGORY LEADERS:\n")
cat(rep("─", 60), "\n", sep = "")

categories <- c("safety_score", "housing_score", "education_score", 
                "economic_score", "healthcare_score", "connectivity_score", 
                "amenities_score")

category_names <- c("Safety", "Housing Affordability", "Education", 
                    "Economic Opportunity", "Healthcare", "Connectivity", 
                    "Livability")

for (i in seq_along(categories)) {
  leader <- iowa_scores %>%
    slice_max(!!sym(categories[i]), n = 1) %>%
    select(city, score = !!sym(categories[i]))
  
  cat(sprintf("  %-22s: %-18s (%.1f)\n", 
              category_names[i], leader$city, leader$score))
}

# =============================================================================
# 5. CLUSTER ANALYSIS
# =============================================================================

cat("\n\n[5/6] Performing cluster analysis...\n")

# Prepare data for clustering
cluster_data <- iowa_scores %>%
  select(city, safety_score, housing_score, education_score,
         economic_score, healthcare_score, connectivity_score,
         amenities_score) %>%
  column_to_rownames("city")

# K-means clustering
set.seed(42)
kmeans_result <- kmeans(cluster_data, centers = 4, nstart = 25)

iowa_scores$cluster <- kmeans_result$cluster

# Describe clusters
cluster_summary <- iowa_scores %>%
  group_by(cluster) %>%
  summarise(
    n_cities = n(),
    avg_population = mean(population),
    avg_overall = mean(overall_score),
    cities = paste(city, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_overall))

cat("\n📍 CITY CLUSTERS:\n")
cat(rep("─", 60), "\n", sep = "")

cluster_labels <- c("Premium", "Growing", "Stable", "Developing")
for (i in 1:nrow(cluster_summary)) {
  cat(sprintf("\n  Cluster %d - %s Cities (Avg Score: %.1f)\n", 
              cluster_summary$cluster[i],
              cluster_labels[i],
              cluster_summary$avg_overall[i]))
  cat(sprintf("    Cities: %s\n", cluster_summary$cities[i]))
}

# =============================================================================
# 6. SAVE RESULTS
# =============================================================================

cat("\n\n[6/6] Saving results...\n")

# Ensure output directory exists
dir.create(here("data/processed"), recursive = TRUE, showWarnings = FALSE)

# Save comprehensive dataset
safe_write_csv(iowa_complete, here("data/processed/iowa_cities_comprehensive.csv"))

# Save scores and rankings
safe_write_csv(overall_rankings, here("data/processed/iowa_cities_rankings.csv"))

# Save for visualization
iowa_viz_data <- iowa_scores %>%
  select(city, region, population, 
         any_of(c("latitude", "longitude")),
         overall_score, safety_score, housing_score, education_score,
         economic_score, healthcare_score, connectivity_score,
         amenities_score, cluster)

safe_write_csv(iowa_viz_data, here("data/processed/iowa_cities_scores.csv"))

# =============================================================================
# SUMMARY
# =============================================================================

elapsed <- format_elapsed(start_time)

cat("\n")
cat(rep("═", 60), "\n", sep = "")
cat("                    ANALYSIS COMPLETE\n")
cat(rep("═", 60), "\n", sep = "")
cat(sprintf("  Cities analyzed:     %d\n", nrow(iowa_complete)))
cat(sprintf("  Variables tracked:   %d\n", ncol(iowa_complete)))
cat(sprintf("  Composite scores:    %d\n", 8))
cat(sprintf("  City clusters:       %d\n", 4))
cat(sprintf("  Time elapsed:        %s\n", elapsed))
cat(rep("═", 60), "\n", sep = "")
