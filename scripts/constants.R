# Iowa Cities Project Constants
# Centralized definitions for cities, regions, colors, and weights
# ================================================================

# =============================================================================
# IOWA CITIES LIST
# =============================================================================

#' List of 20 major Iowa cities included in analysis
IOWA_CITIES <- c(
  "Des Moines", "Cedar Rapids", "Davenport", "Sioux City", "Iowa City",
  "Waterloo", "Ames", "West Des Moines", "Ankeny", "Council Bluffs",
  "Dubuque", "Urbandale", "Cedar Falls", "Marion", "Bettendorf",
  "Mason City", "Marshalltown", "Clinton", "Burlington", "Fort Dodge"
)

#' City to region mapping
CITY_REGIONS <- list(
  Central = c("Des Moines", "West Des Moines", "Ankeny", "Urbandale", "Ames", 
              "Marshalltown", "Fort Dodge"),
  Eastern = c("Cedar Rapids", "Davenport", "Iowa City", "Dubuque", "Marion", 
              "Bettendorf", "Clinton", "Burlington"),
  Western = c("Sioux City", "Council Bluffs"),
  Northern = c("Waterloo", "Cedar Falls", "Mason City")
)

#' Get region for a city
get_region <- function(city) {
  for (region in names(CITY_REGIONS)) {
    if (city %in% CITY_REGIONS[[region]]) return(region)
  }
  return(NA_character_)
}

# =============================================================================
# COUNTY MAPPINGS
# =============================================================================

#' City to county mapping
CITY_COUNTIES <- c(
  "Des Moines" = "Polk",
  "Cedar Rapids" = "Linn",
  "Davenport" = "Scott",
  "Sioux City" = "Woodbury",
  "Iowa City" = "Johnson",
  "Waterloo" = "Black Hawk",
  "Ames" = "Story",
  "West Des Moines" = "Polk",
  "Ankeny" = "Polk",
  "Council Bluffs" = "Pottawattamie",
  "Dubuque" = "Dubuque",
  "Urbandale" = "Polk",
  "Cedar Falls" = "Black Hawk",
  "Marion" = "Linn",
  "Bettendorf" = "Scott",
  "Mason City" = "Cerro Gordo",
  "Marshalltown" = "Marshall",
  "Clinton" = "Clinton",
  "Burlington" = "Des Moines",
  "Fort Dodge" = "Webster"
)

# =============================================================================
# COLOR PALETTES
# =============================================================================

#' Primary project colors
COLORS <- list(
  primary = "#2c7fb8",
  secondary = "#7fcdbb",
  accent = "#253494",
  success = "#1a9850",
  warning = "#fc8d59",
  danger = "#d73027",
  neutral = "#7f7f7f",
  background = "#f7f7f7"
)

#' Region colors for maps and charts
REGION_COLORS <- c(
  Central = "#e41a1c",
  Eastern = "#377eb8",
  Western = "#4daf4a",
  Northern = "#984ea3"
)

#' Score category colors (good to bad)
SCORE_COLORS <- c(
  "Excellent" = "#1a9850",
  "Good" = "#91cf60",
  "Average" = "#fee08b",
  "Below Average" = "#fc8d59",
  "Poor" = "#d73027"
)

#' Market growth colors
GROWTH_COLORS <- c(
  "Hot Market" = "#d62728",
  "Growing Market" = "#ff7f0e",
  "Stable Market" = "#2ca02c",
  "Slow Market" = "#7f7f7f"
)

# =============================================================================
# SCORING WEIGHTS
# =============================================================================

#' Weights for overall city score calculation
OVERALL_WEIGHTS <- list(
  safety = 0.15,
  housing = 0.15,
  education = 0.15,
  economic = 0.20,
  healthcare = 0.15,
  connectivity = 0.10,
  amenities = 0.10
)

#' Weights for safety score components
SAFETY_WEIGHTS <- list(
  violent_crime = 0.40,
  property_crime = 0.30,
  officers = 0.15,
  response_time = 0.15
)

#' Weights for housing score components
HOUSING_WEIGHTS <- list(
  home_value = 0.30,
  rent = 0.30,
  ownership = 0.20,
  vacancy = 0.10,
  appreciation = 0.10
)

#' Weights for education score components
EDUCATION_WEIGHTS <- list(
  graduation = 0.25,
  college_ready = 0.25,
  bachelors = 0.20,
  spending = 0.15,
  student_teacher = 0.15
)

#' Weights for economic score components
ECONOMIC_WEIGHTS <- list(
  income = 0.30,
  unemployment = 0.25,
  job_growth = 0.20,
  poverty = 0.15,
  businesses = 0.10
)

# =============================================================================
# THRESHOLDS AND CATEGORIES
# =============================================================================

#' Population size categories
POPULATION_BREAKS <- c(0, 25000, 50000, 100000, Inf)
POPULATION_LABELS <- c("Small", "Medium", "Large", "Major")

#' Home value market categories
HOME_VALUE_BREAKS <- c(0, 90000, 125000, 175000, 250000, Inf)
HOME_VALUE_LABELS <- c("Very Affordable", "Affordable", "Mid Market", 
                        "Mid-Upper Market", "Premium Market")

#' Crime rate categories (violent crime per 100k)
CRIME_RATE_BREAKS <- c(0, 150, 300, 450, 600, Inf)
CRIME_RATE_LABELS <- c("Very Low", "Low", "Moderate", "High", "Very High")

#' Safety index categories
SAFETY_INDEX_BREAKS <- c(0, 40, 55, 70, 85, Inf)
SAFETY_INDEX_LABELS <- c("Poor", "Below Average", "Average", "Good", "Excellent")

# =============================================================================
# DATA FILE NAMES
# =============================================================================

#' Raw data files
RAW_DATA_FILES <- c(
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
  historical = "iowa_historical_data.csv"
)

#' Processed data files
PROCESSED_DATA_FILES <- c(
  comprehensive = "iowa_cities_comprehensive.csv",
  rankings = "iowa_cities_rankings.csv",
  scores = "iowa_cities_scores.csv",
  analyzed = "iowa_cities_analyzed.csv"
)

# =============================================================================
# BENCHMARK DATA - STATE AND NATIONAL AVERAGES
# =============================================================================

#' Iowa state benchmark data (2020 estimates)
IOWA_BENCHMARKS <- list(
  # Demographics
  median_household_income = 60523,
  median_age = 38.2,
  population_growth_rate = 3.6,  # 10-year %
  
  # Housing
  median_home_value = 154000,
  median_rent = 857,
  owner_occupied_pct = 70.1,
  
  # Safety (per 100,000)
  violent_crime_rate = 266.2,
  property_crime_rate = 1617.4,
  
  # Education
  high_school_graduation_rate = 91.3,
  pct_bachelors_degree = 28.6,
  pct_graduate_degree = 10.1,
  
  # Economy
  unemployment_rate = 4.1,
  poverty_rate = 11.2,
  
  # Healthcare
  physicians_per_10000 = 23.4,
  life_expectancy = 78.4
)

#' National (US) benchmark data (2020 estimates)
US_BENCHMARKS <- list(
  # Demographics
  median_household_income = 64994,
  median_age = 38.5,
  population_growth_rate = 6.7,  # 10-year %
  
  # Housing
  median_home_value = 229800,
  median_rent = 1096,
  owner_occupied_pct = 64.4,
  
  # Safety (per 100,000)
  violent_crime_rate = 398.5,
  property_crime_rate = 2109.9,
  
  # Education
  high_school_graduation_rate = 88.5,
  pct_bachelors_degree = 32.9,
  pct_graduate_degree = 13.1,
  
  # Economy
  unemployment_rate = 5.4,
  poverty_rate = 11.4,
  
  # Healthcare
  physicians_per_10000 = 26.4,
  life_expectancy = 77.0
)

#' Midwest regional benchmark data (2020 estimates)
MIDWEST_BENCHMARKS <- list(
  median_household_income = 59600,
  median_home_value = 180000,
  median_rent = 892,
  violent_crime_rate = 352.0,
  property_crime_rate = 1820.0,
  unemployment_rate = 4.6,
  pct_bachelors_degree = 29.4
)

#' Get benchmark value for a metric
#' 
#' @param metric Character. Name of metric
#' @param level Character. One of "iowa", "us", "midwest"
#' @return Numeric benchmark value or NA
get_benchmark <- function(metric, level = "iowa") {
  benchmarks <- switch(level,
    iowa = IOWA_BENCHMARKS,
    us = US_BENCHMARKS,
    midwest = MIDWEST_BENCHMARKS,
    IOWA_BENCHMARKS
  )
  
  if (metric %in% names(benchmarks)) {
    return(benchmarks[[metric]])
  }
  NA_real_
}

#' Compare value against benchmark
#' 
#' @param value Numeric. Value to compare
#' @param metric Character. Metric name
#' @param level Character. Benchmark level
#' @param higher_is_better Logical. Whether higher values are better
#' @return Character with comparison result
compare_to_benchmark <- function(value, metric, level = "iowa", higher_is_better = TRUE) {
  benchmark <- get_benchmark(metric, level)
  if (is.na(benchmark)) return("No benchmark available")
  
  pct_diff <- ((value - benchmark) / benchmark) * 100
  
  if (higher_is_better) {
    if (pct_diff > 10) return(paste0("Above ", level, " (+", round(pct_diff, 1), "%)"))
    if (pct_diff < -10) return(paste0("Below ", level, " (", round(pct_diff, 1), "%)"))
  } else {
    if (pct_diff < -10) return(paste0("Better than ", level, " (", round(abs(pct_diff), 1), "% lower)"))
    if (pct_diff > 10) return(paste0("Worse than ", level, " (", round(pct_diff, 1), "% higher)"))
  }
  paste0("Near ", level, " average")
}

# =============================================================================
# USER PROFILES FOR RECOMMENDATIONS
# =============================================================================

#' Pre-defined user profiles with weight preferences
USER_PROFILES <- list(
  family = list(
    description = "Families with children",
    weights = c(safety = 0.25, education = 0.25, housing = 0.20, 
                amenities = 0.15, healthcare = 0.15)
  ),
  young_professional = list(
    description = "Young professionals",
    weights = c(economic = 0.30, amenities = 0.25, connectivity = 0.20,
                housing = 0.15, safety = 0.10)
  ),
  retiree = list(
    description = "Retirees",
    weights = c(healthcare = 0.30, safety = 0.25, housing = 0.20,
                amenities = 0.15, connectivity = 0.10)
  ),
  entrepreneur = list(
    description = "Business owners / entrepreneurs",
    weights = c(economic = 0.35, connectivity = 0.25, housing = 0.15,
                amenities = 0.15, education = 0.10)
  ),
  budget_conscious = list(
    description = "Budget-focused individuals",
    weights = c(housing = 0.40, economic = 0.25, safety = 0.15,
                amenities = 0.10, education = 0.10)
  )
)

cat("✓ Constants loaded\n")
