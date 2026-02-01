# Iowa Comprehensive City Analysis
# Analyzes all datasets to create city rankings and insights
# ============================================================

library(tidyverse)
library(here)
library(scales)

# Load utility functions
source(here("scripts/utils.R"))

cat("
╔═══════════════════════════════════════════════════════════════╗
║         IOWA COMPREHENSIVE CITY ANALYSIS                      ║
║                                                               ║
║   Combining all datasets for holistic city rankings           ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# 1. LOAD ALL DATASETS
# =============================================================================

cat("[1/6] Loading all datasets...\n")

# Core datasets
crime <- read_csv(here("data/raw/iowa_crime_data.csv"), show_col_types = FALSE)
housing <- read_csv(here("data/raw/iowa_housing_data.csv"), show_col_types = FALSE)
education <- read_csv(here("data/raw/iowa_education_data.csv"), show_col_types = FALSE)
major_cities <- read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)

# Complementary datasets
economic <- read_csv(here("data/raw/iowa_economic_data.csv"), show_col_types = FALSE)
healthcare <- read_csv(here("data/raw/iowa_healthcare_data.csv"), show_col_types = FALSE)
infrastructure <- read_csv(here("data/raw/iowa_infrastructure_data.csv"), show_col_types = FALSE)
amenities <- read_csv(here("data/raw/iowa_amenities_data.csv"), show_col_types = FALSE)

# New: Demographics, Environment, Historical
demographics <- read_csv(here("data/raw/iowa_demographics_data.csv"), show_col_types = FALSE)
environment <- read_csv(here("data/raw/iowa_environment_data.csv"), show_col_types = FALSE)
historical <- read_csv(here("data/raw/iowa_historical_data.csv"), show_col_types = FALSE)

cat("  ✓ Loaded 11 datasets\n")

# =============================================================================
# 2. MERGE ALL DATA
# =============================================================================

cat("\n[2/6] Merging datasets...\n")

# Start with major cities for geographic info
iowa_complete <- major_cities %>%
  select(city, county, population_2020, latitude, longitude, region) %>%
  rename(population = population_2020)

# Join all datasets
iowa_complete <- iowa_complete %>%
  left_join(
    crime %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    housing %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    education %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    economic %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    healthcare %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    infrastructure %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    amenities %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    demographics %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    environment %>% select(-population, -county),
    by = "city"
  ) %>%
  left_join(
    historical %>% select(-population_2020, -county),
    by = "city"
  )

cat("  ✓ Merged dataset has", nrow(iowa_complete), "cities and", 
    ncol(iowa_complete), "variables\n")

# =============================================================================
# 3. CREATE COMPOSITE SCORES
# =============================================================================

cat("\n[3/6] Calculating composite scores...\n")

# Helper function to normalize scores (0-100 scale)
normalize <- function(x, reverse = FALSE) {
  if (all(is.na(x))) return(x)
  scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  if (reverse) scaled <- 100 - scaled
  return(scaled)
}

# Calculate domain scores
iowa_scores <- iowa_complete %>%
  mutate(
    # Safety Score (lower crime = higher score)
    safety_score = (
      normalize(violent_crime_rate, reverse = TRUE) * 0.4 +
      normalize(property_crime_rate, reverse = TRUE) * 0.3 +
      normalize(officers_per_1000) * 0.15 +
      normalize(avg_response_time_min, reverse = TRUE) * 0.15
    ),
    
    # Housing Affordability Score
    housing_score = (
      normalize(median_home_value, reverse = TRUE) * 0.3 +  # Lower = more affordable
      normalize(median_rent, reverse = TRUE) * 0.3 +
      normalize(owner_occupied_pct) * 0.2 +
      normalize(vacancy_rate, reverse = TRUE) * 0.1 +
      normalize(home_value_change_1yr) * 0.1  # Growth potential
    ),
    
    # Education Score
    education_score = (
      normalize(graduation_rate) * 0.25 +
      normalize(college_readiness_pct) * 0.25 +
      normalize(pct_bachelors) * 0.2 +
      normalize(per_pupil_spending) * 0.15 +
      normalize(student_teacher_ratio, reverse = TRUE) * 0.15
    ),
    
    # Economic Opportunity Score
    economic_score = (
      normalize(median_household_income) * 0.3 +
      normalize(unemployment_rate, reverse = TRUE) * 0.25 +
      normalize(job_growth_rate) * 0.2 +
      normalize(poverty_rate, reverse = TRUE) * 0.15 +
      normalize(businesses_per_1000) * 0.1
    ),
    
    # Healthcare Score
    healthcare_score = (
      normalize(health_insurance_coverage_pct) * 0.25 +
      normalize(primary_care_physicians_per_1000) * 0.25 +
      normalize(life_expectancy) * 0.2 +
      normalize(preventable_hospital_stays, reverse = TRUE) * 0.15 +
      normalize(exercise_access_pct) * 0.15
    ),
    
    # Connectivity Score
    connectivity_score = (
      normalize(broadband_coverage_pct) * 0.3 +
      normalize(avg_internet_speed_mbps) * 0.25 +
      normalize(ev_charging_stations) * 0.15 +
      normalize(avg_commute_time_min, reverse = TRUE) * 0.15 +
      normalize(work_from_home_pct) * 0.15
    ),
    
    # Livability/Amenities Score
    amenities_score = (
      normalize(livability_score) * 0.25 +
      normalize(restaurants_per_1000) * 0.15 +
      normalize(parks_per_10000) * 0.15 +
      normalize(fitness_centers_per_1000) * 0.15 +
      normalize(annual_events_count) * 0.15 +
      normalize(cost_of_living_index, reverse = TRUE) * 0.15
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

# Save comprehensive dataset
write_csv(iowa_complete, here("data/processed/iowa_cities_comprehensive.csv"))

# Save scores and rankings
write_csv(overall_rankings, here("data/processed/iowa_cities_rankings.csv"))

# Save for visualization
iowa_viz_data <- iowa_scores %>%
  select(city, region, population, latitude, longitude,
         overall_score, safety_score, housing_score, education_score,
         economic_score, healthcare_score, connectivity_score,
         amenities_score, cluster)

write_csv(iowa_viz_data, here("data/processed/iowa_cities_scores.csv"))

cat("  ✓ Saved iowa_cities_comprehensive.csv\n")
cat("  ✓ Saved iowa_cities_rankings.csv\n")
cat("  ✓ Saved iowa_cities_scores.csv\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat(rep("═", 60), "\n", sep = "")
cat("                    ANALYSIS COMPLETE\n")
cat(rep("═", 60), "\n", sep = "")
cat(sprintf("  Cities analyzed:     %d\n", nrow(iowa_complete)))
cat(sprintf("  Variables tracked:   %d\n", ncol(iowa_complete)))
cat(sprintf("  Composite scores:    %d\n", 8))
cat(sprintf("  City clusters:       %d\n", 4))
cat(rep("═", 60), "\n", sep = "")
