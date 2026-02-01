# Iowa Cities Recommendation Engine
# Personalized city recommendations based on user preferences
# =============================================================

library(tidyverse)
library(here)

cat("
╔═══════════════════════════════════════════════════════════════╗
║              IOWA CITY RECOMMENDATION ENGINE                  ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("[1/3] Loading and preparing data...\n")

crime <- read_csv(here("data/raw/iowa_crime_data.csv"), show_col_types = FALSE)
housing <- read_csv(here("data/raw/iowa_housing_data.csv"), show_col_types = FALSE)
education <- read_csv(here("data/raw/iowa_education_data.csv"), show_col_types = FALSE)
economic <- read_csv(here("data/raw/iowa_economic_data.csv"), show_col_types = FALSE)
healthcare <- read_csv(here("data/raw/iowa_healthcare_data.csv"), show_col_types = FALSE)
amenities <- read_csv(here("data/raw/iowa_amenities_data.csv"), show_col_types = FALSE)
demographics <- read_csv(here("data/raw/iowa_demographics_data.csv"), show_col_types = FALSE)
environment <- read_csv(here("data/raw/iowa_environment_data.csv"), show_col_types = FALSE)
major_cities <- read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)

# Normalization function
normalize <- function(x, reverse = FALSE) {
  scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  if (reverse) scaled <- 100 - scaled
  return(round(scaled, 1))
}

# =============================================================================
# BUILD MASTER CITY DATABASE
# =============================================================================

cat("[2/3] Building comprehensive city database...\n")

city_db <- major_cities %>%
  select(city, region, population = population_2020, latitude, longitude) %>%
  # Crime/Safety
  left_join(crime %>% select(city, violent_crime_rate, property_crime_rate), by = "city") %>%
  # Housing
  left_join(housing %>% select(city, median_home_value, median_rent, owner_occupied_pct), by = "city") %>%
  # Education
  left_join(education %>% select(city, graduation_rate, college_readiness_pct, pct_bachelors, 
                                  student_teacher_ratio), by = "city") %>%
  # Economic
  left_join(economic %>% select(city, median_household_income, unemployment_rate, poverty_rate,
                                 job_growth_rate, commute_time_minutes), by = "city") %>%
  # Healthcare
  left_join(healthcare %>% select(city, life_expectancy, health_insurance_coverage_pct, 
                                   physicians_per_10000, hospitals_nearby), by = "city") %>%
  # Amenities
  left_join(amenities %>% select(city, livability_score, cost_of_living_index, 
                                  parks_per_10000, restaurants_per_1000, walkability_score), by = "city") %>%
  # Demographics
  left_join(demographics %>% select(city, median_age, pct_families_with_children,
                                     diversity_index), by = "city") %>%
  # Environment
  left_join(environment %>% select(city, air_quality_index, water_quality_score,
                                    green_space_pct, tree_canopy_pct), by = "city") %>%
  # Calculate normalized scores
  mutate(
    # Individual normalized metrics
    safety_score = (normalize(violent_crime_rate, TRUE) * 0.6 + 
                   normalize(property_crime_rate, TRUE) * 0.4),
    affordability_score = (normalize(median_home_value, TRUE) * 0.5 + 
                          normalize(median_rent, TRUE) * 0.3 +
                          normalize(cost_of_living_index, TRUE) * 0.2),
    education_score = (normalize(graduation_rate) * 0.4 + 
                      normalize(college_readiness_pct) * 0.3 +
                      normalize(student_teacher_ratio, TRUE) * 0.3),
    job_market_score = (normalize(median_household_income) * 0.4 + 
                       normalize(unemployment_rate, TRUE) * 0.3 +
                       normalize(job_growth_rate) * 0.3),
    healthcare_score = (normalize(life_expectancy) * 0.3 + 
                       normalize(health_insurance_coverage_pct) * 0.3 +
                       normalize(physicians_per_10000) * 0.4),
    outdoor_score = (normalize(parks_per_10000) * 0.3 + 
                    normalize(green_space_pct) * 0.4 +
                    normalize(tree_canopy_pct) * 0.3),
    urban_amenities_score = (normalize(restaurants_per_1000) * 0.4 + 
                            normalize(walkability_score) * 0.3 +
                            normalize(livability_score) * 0.3),
    family_friendly_score = (normalize(graduation_rate) * 0.3 +
                            normalize(pct_families_with_children) * 0.3 +
                            normalize(violent_crime_rate, TRUE) * 0.4),
    commute_score = normalize(commute_time_minutes, TRUE),
    environment_score = (normalize(air_quality_index, TRUE) * 0.4 + 
                        normalize(water_quality_score) * 0.3 +
                        normalize(green_space_pct) * 0.3)
  )

cat("  ✓ Database built with", nrow(city_db), "cities\n")

# =============================================================================
# RECOMMENDATION FUNCTION
# =============================================================================

cat("[3/3] Creating recommendation engine...\n\n")

#' Get personalized city recommendations
#' 
#' @param safety_weight Weight for safety (0-10)
#' @param affordability_weight Weight for housing affordability (0-10)
#' @param education_weight Weight for schools (0-10)
#' @param jobs_weight Weight for job market (0-10)
#' @param healthcare_weight Weight for healthcare access (0-10)
#' @param outdoor_weight Weight for outdoor recreation (0-10)
#' @param urban_weight Weight for urban amenities (0-10)
#' @param family_weight Weight for family-friendliness (0-10)
#' @param commute_weight Weight for short commute (0-10)
#' @param environment_weight Weight for environment quality (0-10)
#' @param max_budget Maximum home budget (optional)
#' @param min_population Minimum population (optional)
#' @param region_preference Preferred region (optional)
#' @param n_results Number of results to return
#' @return Data frame with recommended cities
recommend_city <- function(
    safety_weight = 5,
    affordability_weight = 5,
    education_weight = 5,
    jobs_weight = 5,
    healthcare_weight = 5,
    outdoor_weight = 5,
    urban_weight = 5,
    family_weight = 5,
    commute_weight = 5,
    environment_weight = 5,
    max_budget = Inf,
    min_population = 0,
    region_preference = NULL,
    n_results = 5
) {
  
  # Filter cities
  filtered <- city_db %>%
    filter(
      median_home_value <= max_budget,
      population >= min_population
    )
  
  if (!is.null(region_preference)) {
    filtered <- filtered %>% filter(region == region_preference)
  }
  
  if (nrow(filtered) == 0) {
    cat("No cities match your criteria. Try relaxing constraints.\n")
    return(NULL)
  }
  
  # Normalize weights
  total_weight <- safety_weight + affordability_weight + education_weight + 
                  jobs_weight + healthcare_weight + outdoor_weight + urban_weight +
                  family_weight + commute_weight + environment_weight
  
  if (total_weight == 0) total_weight <- 1
  
  # Calculate personalized score
  results <- filtered %>%
    mutate(
      match_score = (
        safety_score * safety_weight +
        affordability_score * affordability_weight +
        education_score * education_weight +
        job_market_score * jobs_weight +
        healthcare_score * healthcare_weight +
        outdoor_score * outdoor_weight +
        urban_amenities_score * urban_weight +
        family_friendly_score * family_weight +
        commute_score * commute_weight +
        environment_score * environment_weight
      ) / total_weight
    ) %>%
    arrange(desc(match_score)) %>%
    head(n_results)
  
  # Print results
  cat("\n")
  cat(rep("═", 70), "\n", sep = "")
  cat("  YOUR TOP RECOMMENDED CITIES\n")
  cat(rep("═", 70), "\n", sep = "")
  
  for (i in 1:nrow(results)) {
    city <- results[i, ]
    
    cat(sprintf("\n  #%d  %s (Match: %.1f%%)\n", i, city$city, city$match_score))
    cat(rep("─", 50), "\n", sep = "")
    cat(sprintf("      Region: %s │ Population: %s\n", 
                city$region, format(city$population, big.mark = ",")))
    cat(sprintf("      Home Value: $%s │ Income: $%s\n",
                format(city$median_home_value, big.mark = ","),
                format(city$median_household_income, big.mark = ",")))
    
    # Show top strengths
    scores <- c(
      Safety = city$safety_score,
      Affordability = city$affordability_score,
      Education = city$education_score,
      Jobs = city$job_market_score,
      Healthcare = city$healthcare_score,
      Outdoors = city$outdoor_score,
      `Urban Life` = city$urban_amenities_score,
      `Family` = city$family_friendly_score,
      Commute = city$commute_score,
      Environment = city$environment_score
    )
    top_3 <- names(sort(unlist(scores), decreasing = TRUE)[1:3])
    cat(sprintf("      Strengths: %s\n", paste(top_3, collapse = ", ")))
  }
  
  cat("\n")
  cat(rep("═", 70), "\n", sep = "")
  
  invisible(results)
}

# =============================================================================
# PREDEFINED PROFILES
# =============================================================================

#' Get recommendations for common profiles
recommend_for_profile <- function(profile = c("young_professional", "family", 
                                               "retiree", "remote_worker", 
                                               "outdoor_enthusiast", "budget_conscious")) {
  profile <- match.arg(profile)
  
  cat(sprintf("\n🎯 Recommendations for: %s\n", gsub("_", " ", toupper(profile))))
  
  switch(profile,
    young_professional = recommend_city(
      safety_weight = 6,
      affordability_weight = 7,
      education_weight = 3,
      jobs_weight = 9,
      healthcare_weight = 4,
      outdoor_weight = 5,
      urban_weight = 9,
      family_weight = 2,
      commute_weight = 7,
      environment_weight = 4
    ),
    family = recommend_city(
      safety_weight = 10,
      affordability_weight = 7,
      education_weight = 10,
      jobs_weight = 7,
      healthcare_weight = 7,
      outdoor_weight = 6,
      urban_weight = 4,
      family_weight = 10,
      commute_weight = 6,
      environment_weight = 7
    ),
    retiree = recommend_city(
      safety_weight = 9,
      affordability_weight = 8,
      education_weight = 2,
      jobs_weight = 2,
      healthcare_weight = 10,
      outdoor_weight = 7,
      urban_weight = 5,
      family_weight = 3,
      commute_weight = 3,
      environment_weight = 8
    ),
    remote_worker = recommend_city(
      safety_weight = 6,
      affordability_weight = 9,
      education_weight = 4,
      jobs_weight = 4,
      healthcare_weight = 5,
      outdoor_weight = 8,
      urban_weight = 7,
      family_weight = 4,
      commute_weight = 2,
      environment_weight = 8
    ),
    outdoor_enthusiast = recommend_city(
      safety_weight = 5,
      affordability_weight = 5,
      education_weight = 3,
      jobs_weight = 5,
      healthcare_weight = 4,
      outdoor_weight = 10,
      urban_weight = 3,
      family_weight = 3,
      commute_weight = 4,
      environment_weight = 10
    ),
    budget_conscious = recommend_city(
      safety_weight = 5,
      affordability_weight = 10,
      education_weight = 5,
      jobs_weight = 7,
      healthcare_weight = 5,
      outdoor_weight = 4,
      urban_weight = 4,
      family_weight = 5,
      commute_weight = 5,
      environment_weight = 4,
      max_budget = 200000
    )
  )
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

cat(rep("═", 70), "\n", sep = "")
cat("RECOMMENDATION ENGINE READY\n")
cat(rep("═", 70), "\n", sep = "")

cat("\nUsage:\n")
cat("  # Custom recommendation with weights (0-10):\n")
cat("  recommend_city(\n")
cat("    safety_weight = 8,\n")
cat("    education_weight = 10,\n")
cat("    affordability_weight = 6\n")
cat("  )\n")

cat("\n  # With budget constraint:\n")
cat("  recommend_city(max_budget = 250000, min_population = 20000)\n")

cat("\n  # Use predefined profile:\n")
cat("  recommend_for_profile('family')\n")
cat("  recommend_for_profile('young_professional')\n")
cat("  recommend_for_profile('retiree')\n")
cat("  recommend_for_profile('remote_worker')\n")
cat("  recommend_for_profile('outdoor_enthusiast')\n")
cat("  recommend_for_profile('budget_conscious')\n")

# =============================================================================
# DEMO: RUN ALL PROFILES
# =============================================================================

cat("\n\n")
cat(rep("═", 70), "\n", sep = "")
cat("DEMO: TOP PICKS FOR EACH PROFILE\n")
cat(rep("═", 70), "\n", sep = "")

recommend_for_profile("family")
recommend_for_profile("young_professional")
recommend_for_profile("retiree")
