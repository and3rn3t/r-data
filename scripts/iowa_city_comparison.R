# Iowa Cities Comparison Tool
# Side-by-side city comparison with radar charts
# ================================================

library(tidyverse)
library(here)
library(fmsb)

source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

cat("
╔═══════════════════════════════════════════════════════════════╗
║              IOWA CITY COMPARISON TOOL                        ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# LOAD DATA
# =============================================================================

crime <- read_csv(here("data/raw/iowa_crime_data.csv"), show_col_types = FALSE)
housing <- read_csv(here("data/raw/iowa_housing_data.csv"), show_col_types = FALSE)
education <- read_csv(here("data/raw/iowa_education_data.csv"), show_col_types = FALSE)
economic <- read_csv(here("data/raw/iowa_economic_data.csv"), show_col_types = FALSE)
healthcare <- read_csv(here("data/raw/iowa_healthcare_data.csv"), show_col_types = FALSE)
amenities <- read_csv(here("data/raw/iowa_amenities_data.csv"), show_col_types = FALSE)
major_cities <- read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)

# Helper function
normalize <- function(x, reverse = FALSE) {
  scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  if (reverse) scaled <- 100 - scaled
  return(scaled)
}

# =============================================================================
# CALCULATE SCORES
# =============================================================================

all_scores <- major_cities %>%
  select(city, region, population = population_2020) %>%
  left_join(crime %>% select(city, violent_crime_rate, property_crime_rate, officers_per_1000), by = "city") %>%
  left_join(housing %>% select(city, median_home_value, median_rent, owner_occupied_pct), by = "city") %>%
  left_join(education %>% select(city, graduation_rate, college_readiness_pct, pct_bachelors, per_pupil_spending), by = "city") %>%
  left_join(economic %>% select(city, median_household_income, unemployment_rate, poverty_rate, job_growth_rate), by = "city") %>%
  left_join(healthcare %>% select(city, life_expectancy, health_insurance_coverage_pct, obesity_rate), by = "city") %>%
  left_join(amenities %>% select(city, livability_score, cost_of_living_index, restaurants_per_1000), by = "city") %>%
  mutate(
    safety_score = (normalize(violent_crime_rate, TRUE) * 0.5 + 
                   normalize(property_crime_rate, TRUE) * 0.3 +
                   normalize(officers_per_1000) * 0.2),
    housing_score = (normalize(median_home_value, TRUE) * 0.4 + 
                    normalize(median_rent, TRUE) * 0.3 +
                    normalize(owner_occupied_pct) * 0.3),
    education_score = (normalize(graduation_rate) * 0.3 + 
                      normalize(college_readiness_pct) * 0.3 +
                      normalize(pct_bachelors) * 0.2 +
                      normalize(per_pupil_spending) * 0.2),
    economic_score = (normalize(median_household_income) * 0.35 + 
                     normalize(unemployment_rate, TRUE) * 0.25 +
                     normalize(poverty_rate, TRUE) * 0.25 +
                     normalize(job_growth_rate) * 0.15),
    healthcare_score = (normalize(life_expectancy) * 0.4 + 
                       normalize(health_insurance_coverage_pct) * 0.35 +
                       normalize(obesity_rate, TRUE) * 0.25),
    amenities_score = (normalize(livability_score) * 0.4 + 
                      normalize(cost_of_living_index, TRUE) * 0.3 +
                      normalize(restaurants_per_1000) * 0.3),
    overall_score = (safety_score + housing_score + education_score + 
                    economic_score + healthcare_score + amenities_score) / 6
  )

# =============================================================================
# COMPARISON FUNCTION
# =============================================================================

#' Compare cities side by side
#' 
#' @param city1 First city name
#' @param city2 Second city name
#' @param city3 Optional third city name
#' @return Prints comparison and returns data
compare_cities <- function(city1, city2, city3 = NULL) {
  
  cities_to_compare <- c(city1, city2)
  if (!is.null(city3)) cities_to_compare <- c(cities_to_compare, city3)
  
  # Validate cities exist
  missing <- setdiff(cities_to_compare, all_scores$city)
  if (length(missing) > 0) {
    stop("Cities not found: ", paste(missing, collapse = ", "))
  }
  
  # Get comparison data
  comp_data <- all_scores %>%
    filter(city %in% cities_to_compare) %>%
    select(city, region, population, 
           safety_score, housing_score, education_score,
           economic_score, healthcare_score, amenities_score, overall_score,
           median_household_income, median_home_value, graduation_rate,
           violent_crime_rate, life_expectancy, livability_score)
  
  # Print header
  cat("\n")
  cat(rep("═", 70), "\n", sep = "")
  cat("  CITY COMPARISON: ", paste(cities_to_compare, collapse = " vs "), "\n")
  cat(rep("═", 70), "\n", sep = "")
  
  # Print overall scores
  cat("\n📊 OVERALL SCORES:\n")
  cat(rep("─", 50), "\n", sep = "")
  for (c in cities_to_compare) {
    score <- round(comp_data$overall_score[comp_data$city == c], 1)
    bar <- paste(rep("█", round(score / 5)), collapse = "")
    cat(sprintf("  %-18s │ %5.1f │ %s\n", c, score, bar))
  }
  
  # Determine winner
  winner <- comp_data$city[which.max(comp_data$overall_score)]
  cat("\n  🏆 Overall Winner: ", winner, "\n")
  
  # Category-by-category comparison
  categories <- c("safety_score", "housing_score", "education_score",
                  "economic_score", "healthcare_score", "amenities_score")
  category_names <- c("Safety", "Housing", "Education", "Economy", "Healthcare", "Amenities")
  
  cat("\n📈 CATEGORY BREAKDOWN:\n")
  cat(rep("─", 70), "\n", sep = "")
  cat(sprintf("  %-15s", "Category"))
  for (c in cities_to_compare) {
    cat(sprintf(" │ %12s", c))
  }
  cat(" │ Winner\n")
  cat(rep("─", 70), "\n", sep = "")
  
  category_winners <- character(length(categories))
  
  for (i in seq_along(categories)) {
    cat(sprintf("  %-15s", category_names[i]))
    
    scores_vec <- numeric(length(cities_to_compare))
    for (j in seq_along(cities_to_compare)) {
      score <- round(comp_data[[categories[i]]][comp_data$city == cities_to_compare[j]], 1)
      scores_vec[j] <- score
      cat(sprintf(" │ %12.1f", score))
    }
    
    cat_winner <- cities_to_compare[which.max(scores_vec)]
    category_winners[i] <- cat_winner
    cat(sprintf(" │ %s\n", cat_winner))
  }
  
  # Key metrics comparison
  cat("\n💰 KEY METRICS:\n")
  cat(rep("─", 70), "\n", sep = "")
  
  metrics <- list(
    list(name = "Median Income", col = "median_household_income", prefix = "$", suffix = "", reverse = FALSE),
    list(name = "Home Value", col = "median_home_value", prefix = "$", suffix = "", reverse = FALSE),
    list(name = "Graduation Rate", col = "graduation_rate", prefix = "", suffix = "%", reverse = FALSE),
    list(name = "Crime Rate", col = "violent_crime_rate", prefix = "", suffix = "", reverse = TRUE),
    list(name = "Life Expectancy", col = "life_expectancy", prefix = "", suffix = " yrs", reverse = FALSE),
    list(name = "Livability", col = "livability_score", prefix = "", suffix = "", reverse = FALSE)
  )
  
  for (m in metrics) {
    cat(sprintf("  %-18s", m$name))
    
    values <- numeric(length(cities_to_compare))
    for (j in seq_along(cities_to_compare)) {
      val <- comp_data[[m$col]][comp_data$city == cities_to_compare[j]]
      values[j] <- val
      if (m$prefix == "$") {
        cat(sprintf(" │ %12s", paste0(m$prefix, format(val, big.mark = ","), m$suffix)))
      } else {
        cat(sprintf(" │ %12s", paste0(m$prefix, val, m$suffix)))
      }
    }
    
    best_idx <- if (m$reverse) which.min(values) else which.max(values)
    cat(sprintf(" │ ✓ %s\n", cities_to_compare[best_idx]))
  }
  
  # Summary
  cat("\n")
  cat(rep("═", 70), "\n", sep = "")
  cat("SUMMARY:\n")
  
  # Count category wins
  win_counts <- table(category_winners)
  for (c in cities_to_compare) {
    wins <- if (c %in% names(win_counts)) win_counts[c] else 0
    cat(sprintf("  %-18s won %d of 6 categories\n", c, wins))
  }
  
  cat(rep("═", 70), "\n", sep = "")
  
  # Return data for further use
  invisible(comp_data)
}

#' Create radar chart for city comparison
#' 
#' @param city1 First city name
#' @param city2 Second city name
#' @param city3 Optional third city name
#' @param save_file Optional file path to save PNG
create_radar_chart <- function(city1, city2, city3 = NULL, save_file = NULL) {
  
  cities_to_compare <- c(city1, city2)
  if (!is.null(city3)) cities_to_compare <- c(cities_to_compare, city3)
  
  # Prepare radar data
  radar_data <- all_scores %>%
    filter(city %in% cities_to_compare) %>%
    select(city, safety_score, housing_score, education_score,
           economic_score, healthcare_score, amenities_score)
  
  # Reshape for fmsb
  radar_matrix <- radar_data %>%
    column_to_rownames("city") %>%
    as.data.frame()
  
  # Add max and min rows (required by fmsb)
  radar_matrix <- rbind(
    rep(100, 6),  # Max
    rep(0, 6),    # Min
    radar_matrix
  )
  
  colnames(radar_matrix) <- c("Safety", "Housing", "Education", "Economy", "Healthcare", "Amenities")
  
  # Colors
  colors <- c("#1f77b4", "#ff7f0e", "#2ca02c")[1:length(cities_to_compare)]
  
  # Create chart
  if (!is.null(save_file)) {
    png(save_file, width = 800, height = 600)
  }
  
  radarchart(
    radar_matrix,
    axistype = 1,
    pcol = colors,
    pfcol = scales::alpha(colors, 0.3),
    plwd = 3,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey30",
    vlcex = 1.2,
    title = paste("City Comparison:", paste(cities_to_compare, collapse = " vs "))
  )
  
  legend(
    "topright",
    legend = cities_to_compare,
    col = colors,
    lty = 1,
    lwd = 3,
    bty = "n"
  )
  
  if (!is.null(save_file)) {
    dev.off()
    cat("Saved radar chart to:", save_file, "\n")
  }
}

# =============================================================================
# AVAILABLE CITIES
# =============================================================================

cat("\nAvailable cities for comparison:\n")
cat(paste(sort(all_scores$city), collapse = ", "), "\n")

cat("\n\nUsage:\n")
cat("  compare_cities('Bettendorf', 'Ankeny')\n")
cat("  compare_cities('Des Moines', 'Cedar Rapids', 'Iowa City')\n")
cat("  create_radar_chart('Bettendorf', 'Ankeny', save_file = 'outputs/comparison.png')\n")

# =============================================================================
# EXAMPLE COMPARISON
# =============================================================================

cat("\n\n")
compare_cities("Bettendorf", "Ankeny", "West Des Moines")

# Save example radar chart
create_radar_chart("Bettendorf", "Ankeny", "West Des Moines", 
                   save_file = here("outputs/top3_comparison_radar.png"))
