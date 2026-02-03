# Scoring System for Iowa Cities Dashboard
# Lifestyle presets and composite score calculations
# ==================================================

library(dplyr)

# =============================================================================
# SCORE CATEGORIES
# =============================================================================

#' All scoring categories with metadata
SCORE_CATEGORIES <- list(
  # Original categories
  safety = list(
    name = "Safety",
    icon = "shield-alt",
    description = "Crime rates, police coverage, response times",
    columns = c("safety_score"),
    default_weight = 0.12
  ),
  housing = list(
    name = "Housing",
    icon = "home",
    description = "Home values, rent, affordability",
    columns = c("housing_score"),
    default_weight = 0.12
  ),
  education = list(
    name = "Education",
    icon = "graduation-cap",
    description = "Schools, graduation rates, college attainment",
    columns = c("education_score"),
    default_weight = 0.12
  ),
  economic = list(
    name = "Economy",
    icon = "briefcase",
    description = "Jobs, income, business activity",
    columns = c("economic_score"),
    default_weight = 0.15
  ),
  healthcare = list(
    name = "Healthcare",
    icon = "heartbeat",
    description = "Medical access, health outcomes",
    columns = c("healthcare_score"),
    default_weight = 0.12
  ),
  livability = list(
    name = "Livability",
    icon = "tree",
    description = "Amenities, environment, quality of life",
    columns = c("livability_score_calc"),
    default_weight = 0.10
  ),
  connectivity = list(
    name = "Connectivity",
    icon = "wifi",
    description = "Internet, transit, infrastructure",
    columns = c("connectivity_score"),
    default_weight = 0.07
  ),
  
  # New categories
  family = list(
    name = "Family",
    icon = "users",
    description = "Childcare, pediatrics, youth programs",
    columns = c("family_score"),
    default_weight = 0.00,  # Optional category
    optional = TRUE
  ),
  climate = list(
    name = "Climate",
    icon = "cloud-sun",
    description = "Weather, sunny days, severe weather risk",
    columns = c("climate_score"),
    default_weight = 0.00,  # Optional category
    optional = TRUE
  ),
  senior = list(
    name = "Senior Living",
    icon = "user-clock",
    description = "Assisted living, senior services, Medicare",
    columns = c("senior_score"),
    default_weight = 0.00,  # Optional category
    optional = TRUE
  ),
  pet = list(
    name = "Pet-Friendly",
    icon = "paw",
    description = "Dog parks, vets, pet-friendly housing",
    columns = c("pet_score"),
    default_weight = 0.00,  # Optional category
    optional = TRUE
  )
)

#' Get list of core (non-optional) categories
get_core_categories <- function() {
  names(Filter(function(x) !isTRUE(x$optional), SCORE_CATEGORIES))
}

#' Get list of optional categories
get_optional_categories <- function() {
  names(Filter(function(x) isTRUE(x$optional), SCORE_CATEGORIES))
}

# =============================================================================
# LIFESTYLE PRESETS
# =============================================================================

#' Lifestyle mode definitions
#' Weights should sum to 1.0 within each mode
LIFESTYLE_MODES <- list(
  balanced = list(
    name = "Balanced",
    description = "Equal emphasis on all core factors",
    icon = "balance-scale",
    weights = list(
      safety = 0.15,
      housing = 0.15,
      education = 0.15,
      economic = 0.15,
      healthcare = 0.15,
      livability = 0.15,
      connectivity = 0.10,
      family = 0.00,
      climate = 0.00,
      senior = 0.00,
      pet = 0.00
    ),
    enabled_optional = c()
  ),
  
  family_focus = list(
    name = "Family Focus",
    description = "Prioritizes safety, schools, and family amenities",
    icon = "users",
    weights = list(
      safety = 0.20,
      housing = 0.10,
      education = 0.25,
      economic = 0.10,
      healthcare = 0.10,
      livability = 0.05,
      connectivity = 0.05,
      family = 0.15,
      climate = 0.00,
      senior = 0.00,
      pet = 0.00
    ),
    enabled_optional = c("family")
  ),
  
  career_focus = list(
    name = "Career Focus",
    description = "Emphasizes jobs, economy, and connectivity",
    icon = "briefcase",
    weights = list(
      safety = 0.10,
      housing = 0.15,
      education = 0.10,
      economic = 0.30,
      healthcare = 0.10,
      livability = 0.10,
      connectivity = 0.15,
      family = 0.00,
      climate = 0.00,
      senior = 0.00,
      pet = 0.00
    ),
    enabled_optional = c()
  ),
  
  retirement_focus = list(
    name = "Retirement Focus",
    description = "Prioritizes healthcare, safety, and senior services",
    icon = "user-clock",
    weights = list(
      safety = 0.20,
      housing = 0.10,
      education = 0.00,
      economic = 0.05,
      healthcare = 0.25,
      livability = 0.10,
      connectivity = 0.05,
      family = 0.00,
      climate = 0.10,
      senior = 0.15,
      pet = 0.00
    ),
    enabled_optional = c("climate", "senior")
  ),
  
  outdoor_pet_lover = list(
    name = "Outdoor & Pet Lover",
    description = "Emphasizes environment, climate, and pet-friendliness",
    icon = "paw",
    weights = list(
      safety = 0.10,
      housing = 0.10,
      education = 0.05,
      economic = 0.10,
      healthcare = 0.10,
      livability = 0.20,
      connectivity = 0.05,
      family = 0.00,
      climate = 0.15,
      senior = 0.00,
      pet = 0.15
    ),
    enabled_optional = c("climate", "pet")
  ),
  
  custom = list(
    name = "Custom",
    description = "Set your own priorities with the sliders below",
    icon = "sliders-h",
    weights = NULL,  # User-defined
    enabled_optional = NULL  # User-defined
  )
)

# =============================================================================
# LIFESTYLE FUNCTIONS
# =============================================================================

#' Get lifestyle mode configuration
#' 
#' @param mode_name Character. One of the lifestyle mode names
#' @return List with mode configuration
#' @export
get_lifestyle_mode <- function(mode_name = "balanced") {
  if (!mode_name %in% names(LIFESTYLE_MODES)) {
    warning("Unknown lifestyle mode '", mode_name, "'. Using 'balanced'.")
    mode_name <- "balanced"
  }
  LIFESTYLE_MODES[[mode_name]]
}

#' Get lifestyle mode names for selectInput
#' 
#' @return Named vector for use in selectInput choices
#' @export
get_lifestyle_choices <- function() {
  choices <- sapply(LIFESTYLE_MODES, function(m) m$name)
  setNames(names(LIFESTYLE_MODES), choices)
}

#' Get weights for a lifestyle mode
#' 
#' @param mode_name Character. Lifestyle mode name
#' @return Named numeric vector of weights
#' @export
get_lifestyle_weights <- function(mode_name = "balanced") {
  mode <- get_lifestyle_mode(mode_name)
  
  if (is.null(mode$weights)) {
    # Return default weights for custom mode
    return(get_default_weights())
  }
  
  unlist(mode$weights)
}

#' Get default weights (equal for core, zero for optional)
#' 
#' @return Named numeric vector of weights
#' @export
get_default_weights <- function() {
  core <- get_core_categories()
  optional <- get_optional_categories()
  n_core <- length(core)
  
  weights <- c(
    setNames(rep(1 / n_core, n_core), core),
    setNames(rep(0, length(optional)), optional)
  )
  
  weights
}

#' Normalize weights to sum to 1
#' 
#' @param weights Named numeric vector of weights
#' @return Normalized weights
#' @export
normalize_weights <- function(weights) {
  total <- sum(weights, na.rm = TRUE)
  if (total == 0) {
    warning("All weights are zero. Using default weights.")
    return(get_default_weights())
  }
  weights / total
}

#' Convert slider values (1-10) to normalized weights
#' 
#' @param slider_values Named list of slider values
#' @return Normalized weight vector
#' @export
sliders_to_weights <- function(slider_values) {
  weights <- unlist(slider_values)
  normalize_weights(weights)
}

# =============================================================================
# SCORING FUNCTIONS
# =============================================================================

#' Calculate custom score based on weights
#' 
#' @param data Data frame with score columns
#' @param weights Named numeric vector of weights
#' @return Numeric vector of custom scores
#' @export
calculate_custom_score <- function(data, weights) {
  weights <- normalize_weights(weights)
  
  # Map category names to actual column names
  column_map <- list(
    safety = "safety_score",
    housing = "housing_score",
    education = "education_score",
    economic = "economic_score",
    healthcare = "healthcare_score",
    livability = "livability_score_calc",
    connectivity = "connectivity_score",
    family = "family_score",
    climate = "climate_score",
    senior = "senior_score",
    pet = "pet_score"
  )
  
  score <- rep(0, nrow(data))
  
  for (cat in names(weights)) {
    if (weights[cat] > 0) {
      col <- column_map[[cat]]
      if (!is.null(col) && col %in% names(data)) {
        score <- score + (data[[col]] * weights[cat])
      }
    }
  }
  
  round(score, 1)
}

#' Calculate all category scores for a dataset
#' 
#' @param data List of data frames from load_datasets
#' @return Data frame with city scores
#' @export
calculate_city_scores <- function(data) {
  # Start with major cities as base
  # Use population_2020 and rename to population for consistency
  scores <- data$major_cities %>%
    select(city, region, county, population = population_2020, latitude, longitude)
  
  # Add economic data (includes median_household_income)
  if (!is.null(data$economic)) {
    econ_base <- data$economic %>%
      select(city, median_household_income)
    scores <- left_join(scores, econ_base, by = "city")
  }
  
  # Add crime/safety scores
  if (!is.null(data$crime)) {
    crime <- data$crime %>%
      mutate(
        safety_score = normalize(violent_crime_rate, reverse = TRUE) * 0.4 +
                       normalize(property_crime_rate, reverse = TRUE) * 0.3 +
                       normalize(officers_per_1000) * 0.15 +
                       normalize(avg_response_time_min, reverse = TRUE) * 0.15
      ) %>%
      select(city, safety_score, violent_crime_rate, property_crime_rate)
    scores <- left_join(scores, crime, by = "city")
  }
  
  # Add housing scores
  if (!is.null(data$housing)) {
    housing <- data$housing %>%
      mutate(
        housing_score = normalize(median_home_value, reverse = TRUE) * 0.3 +
                        normalize(median_rent, reverse = TRUE) * 0.3 +
                        normalize(owner_occupied_pct) * 0.2 +
                        normalize(vacancy_rate, reverse = TRUE) * 0.1 +
                        normalize(home_value_change_1yr) * 0.1
      ) %>%
      select(city, housing_score, median_home_value, median_rent)
    scores <- left_join(scores, housing, by = "city")
  }
  
  # Add education scores
  if (!is.null(data$education)) {
    education <- data$education %>%
      mutate(
        education_score = normalize(graduation_rate) * 0.25 +
                          normalize(college_readiness_pct) * 0.25 +
                          normalize(pct_bachelors) * 0.20 +
                          normalize(per_pupil_spending) * 0.15 +
                          normalize(student_teacher_ratio, reverse = TRUE) * 0.15
      ) %>%
      select(city, education_score, graduation_rate)
    scores <- left_join(scores, education, by = "city")
  }
  
  # Add economic scores
  if (!is.null(data$economic)) {
    economic <- data$economic %>%
      mutate(
        economic_score = normalize(median_household_income) * 0.30 +
                         normalize(unemployment_rate, reverse = TRUE) * 0.25 +
                         normalize(job_growth_rate) * 0.20 +
                         normalize(poverty_rate, reverse = TRUE) * 0.15 +
                         normalize(businesses_per_1000) * 0.10
      ) %>%
      select(city, economic_score, unemployment_rate)
    scores <- left_join(scores, economic, by = "city")
  }
  
  # Add healthcare scores
  if (!is.null(data$healthcare)) {
    healthcare <- data$healthcare %>%
      mutate(
        healthcare_score = normalize(life_expectancy) * 0.25 +
                           normalize(health_insurance_coverage_pct) * 0.25 +
                           normalize(primary_care_physicians_per_1000) * 0.20 +
                           normalize(obesity_rate, reverse = TRUE) * 0.15 +
                           normalize(smoking_rate, reverse = TRUE) * 0.15
      ) %>%
      select(city, healthcare_score, life_expectancy)
    scores <- left_join(scores, healthcare, by = "city")
  }
  
  # Add livability scores from amenities
  if (!is.null(data$amenities)) {
    livability <- data$amenities %>%
      mutate(
        livability_score_calc = normalize(livability_score) * 0.3 +
                                normalize(restaurants_per_1000) * 0.2 +
                                normalize(fitness_centers_per_1000) * 0.2 +
                                normalize(cost_of_living_index, reverse = TRUE) * 0.3
      ) %>%
      select(city, livability_score_calc)
    scores <- left_join(scores, livability, by = "city")
  }
  
  # Add connectivity scores from infrastructure
  if (!is.null(data$infrastructure)) {
    connectivity <- data$infrastructure %>%
      mutate(
        connectivity_score = normalize(broadband_coverage_pct) * 0.35 +
                             normalize(bus_routes) * 0.25 +
                             normalize(ev_charging_stations) * 0.20 +
                             normalize(avg_commute_time_min, reverse = TRUE) * 0.20
      ) %>%
      select(city, connectivity_score)
    scores <- left_join(scores, connectivity, by = "city")
  }
  
  # Initialize optional category scores as NA (will be populated when data exists)
  scores <- scores %>%
    mutate(
      family_score = NA_real_,
      climate_score = NA_real_,
      senior_score = NA_real_,
      pet_score = NA_real_
    )
  
  # Add family scores if data exists
  if (!is.null(data$family)) {
    family <- data$family %>%
      mutate(
        family_score = normalize(daycare_centers_per_10000) * 0.25 +
                       normalize(pediatricians_per_10000) * 0.25 +
                       normalize(youth_sports_leagues) * 0.25 +
                       normalize(playgrounds_per_10000) * 0.25
      ) %>%
      select(city, family_score)
    scores <- scores %>%
      select(-family_score) %>%
      left_join(family, by = "city")
  }
  
  # Add climate scores if data exists
  if (!is.null(data$climate)) {
    climate <- data$climate %>%
      mutate(
        climate_score = normalize(sunny_days) * 0.30 +
                        normalize(severe_weather_events, reverse = TRUE) * 0.30 +
                        normalize(avg_summer_high, reverse = TRUE) * 0.20 +
                        normalize(avg_winter_low) * 0.20
      ) %>%
      select(city, climate_score)
    scores <- scores %>%
      select(-climate_score) %>%
      left_join(climate, by = "city")
  }
  
  # Add senior scores if data exists
  if (!is.null(data$senior)) {
    senior <- data$senior %>%
      mutate(
        senior_score = normalize(assisted_living_per_10000) * 0.30 +
                       normalize(senior_centers_per_10000) * 0.25 +
                       normalize(avg_nursing_home_rating) * 0.25 +
                       normalize(medicare_plan_options) * 0.20
      ) %>%
      select(city, senior_score)
    scores <- scores %>%
      select(-senior_score) %>%
      left_join(senior, by = "city")
  }
  
  # Add pet scores if data exists
  if (!is.null(data$pet)) {
    pet <- data$pet %>%
      mutate(
        pet_score = normalize(dog_parks_per_10000) * 0.30 +
                    normalize(vets_per_10000) * 0.30 +
                    normalize(pet_friendly_rentals_pct) * 0.25 +
                    normalize(pet_stores_per_10000) * 0.15
      ) %>%
      select(city, pet_score)
    scores <- scores %>%
      select(-pet_score) %>%
      left_join(pet, by = "city")
  }
  
  # Calculate overall score using default weights
  default_weights <- get_default_weights()
  scores <- scores %>%
    mutate(
      overall_score = calculate_custom_score(., default_weights),
      rank = row_number(desc(overall_score))
    ) %>%
    arrange(rank)
  
  scores
}

cat("✓ Scoring system loaded\n")
