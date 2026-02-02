# Targets Pipeline Definition
# Run with: targets::tar_make()
# Visualize with: targets::tar_visnetwork()
# =============================================

library(targets)
library(tarchetypes)

# Set target options
tar_option_set(
  packages = c("tidyverse", "here", "janitor", "skimr", "pointblank"),
  format = "rds"
)

# Source utility functions
tar_source("scripts/utils.R")
tar_source("scripts/constants.R")

# Define the pipeline
list(
  # -------------------------------------------------------------------------
  # Data Import Targets
  # -------------------------------------------------------------------------
  tar_target(
    name = raw_iowa_cities,
    command = read_csv(here("data/raw/iowa_cities_raw.csv"), 
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_census,
    command = read_csv(here("data/raw/iowa_cities_census.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_crime,
    command = read_csv(here("data/raw/iowa_crime_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_housing,
    command = read_csv(here("data/raw/iowa_housing_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_education,
    command = read_csv(here("data/raw/iowa_education_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_major_cities,
    command = read_csv(here("data/raw/iowa_major_cities.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_economic,
    command = read_csv(here("data/raw/iowa_economic_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_healthcare,
    command = read_csv(here("data/raw/iowa_healthcare_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_family,
    command = read_csv(here("data/raw/iowa_family_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_climate,
    command = read_csv(here("data/raw/iowa_climate_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_senior,
    command = read_csv(here("data/raw/iowa_senior_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  tar_target(
    name = raw_iowa_pets,
    command = read_csv(here("data/raw/iowa_pets_data.csv"),
                       show_col_types = FALSE),
    format = "rds"
  ),
  
  # -------------------------------------------------------------------------
  # Data Cleaning Targets
  # -------------------------------------------------------------------------
  tar_target(
    name = clean_iowa_cities,
    command = {
      raw_iowa_cities %>%
        standardize_names() %>%
        distinct() %>%
        filter(!is.na(city))
    }
  ),
  
  tar_target(
    name = clean_iowa_census,
    command = {
      raw_iowa_census %>%
        standardize_names() %>%
        distinct()
    }
  ),
  
  # -------------------------------------------------------------------------
  # Analysis Targets
  # -------------------------------------------------------------------------
  tar_target(
    name = population_summary,
    command = {
      clean_iowa_cities %>%
        summarise_numeric()
    }
  ),
  
  tar_target(
    name = missing_report,
    command = {
      list(
        cities = count_missing(clean_iowa_cities),
        census = count_missing(clean_iowa_census)
      )
    }
  ),
  
  tar_target(
    name = city_scores,
    command = {
      raw_major_cities %>%
        select(city, county, population = population_2020, latitude, longitude, region) %>%
        left_join(raw_iowa_crime %>% select(city, violent_crime_rate, property_crime_rate), by = "city") %>%
        left_join(raw_iowa_housing %>% select(city, median_home_value, owner_occupied_pct), by = "city") %>%
        left_join(raw_iowa_education %>% select(city, graduation_rate, college_readiness_pct, pct_bachelors), by = "city") %>%
        left_join(raw_iowa_economic %>% select(city, median_household_income, unemployment_rate, poverty_rate), by = "city") %>%
        left_join(raw_iowa_healthcare %>% select(city, life_expectancy, health_insurance_coverage_pct), by = "city") %>%
        left_join(raw_iowa_family %>% select(city, daycare_centers_per_10000, pediatricians_per_10000, playgrounds_per_10000, daycare_availability), by = "city") %>%
        left_join(raw_iowa_climate %>% select(city, sunny_days, severe_weather_events, climate_comfort_index), by = "city") %>%
        left_join(raw_iowa_senior %>% select(city, senior_centers_per_10000, assisted_living_per_10000, medicare_plan_options), by = "city") %>%
        left_join(raw_iowa_pets %>% select(city, dog_parks_per_10000, vets_per_10000, pet_friendly_rentals_pct, pet_friendliness_index), by = "city") %>%
        mutate(
          # Core scores
          safety_score = (normalize(violent_crime_rate, TRUE) + normalize(property_crime_rate, TRUE)) / 2,
          housing_score = (normalize(median_home_value, TRUE) + normalize(owner_occupied_pct)) / 2,
          education_score = (normalize(graduation_rate) + normalize(college_readiness_pct) + normalize(pct_bachelors)) / 3,
          economic_score = (normalize(median_household_income) + normalize(unemployment_rate, TRUE) + normalize(poverty_rate, TRUE)) / 3,
          healthcare_score = (normalize(life_expectancy) + normalize(health_insurance_coverage_pct)) / 2,
          # New lifestyle scores
          family_score = (normalize(daycare_centers_per_10000) + normalize(pediatricians_per_10000) + normalize(playgrounds_per_10000) + normalize(daycare_availability)) / 4,
          climate_score = (normalize(sunny_days) + normalize(severe_weather_events, TRUE) + normalize(climate_comfort_index)) / 3,
          senior_score = (normalize(senior_centers_per_10000) + normalize(assisted_living_per_10000) + normalize(medicare_plan_options)) / 3,
          pet_score = (normalize(dog_parks_per_10000) + normalize(vets_per_10000) + normalize(pet_friendly_rentals_pct) + normalize(pet_friendliness_index)) / 4,
          # Overall score (core categories only for default)
          overall_score = (safety_score + housing_score + education_score + economic_score + healthcare_score) / 5
        ) %>%
        arrange(desc(overall_score)) %>%
        mutate(rank = row_number())
    }
  ),
  
  # -------------------------------------------------------------------------
  # Output Targets
  # -------------------------------------------------------------------------
  tar_target(
    name = save_clean_cities,
    command = {
      write_csv(clean_iowa_cities, 
                here("data/processed/iowa_cities_clean_targets.csv"))
      here("data/processed/iowa_cities_clean_targets.csv")
    },
    format = "file"
  ),
  
  # -------------------------------------------------------------------------
  # Report Generation
  # -------------------------------------------------------------------------
  tar_render(
    name = analysis_report,
    path = here("notebooks/analysis_template.Rmd"),
    output_dir = here("outputs/reports")
  )
)
