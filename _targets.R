# Targets Pipeline Definition
# Run with: targets::tar_make()
# Visualize with: targets::tar_visnetwork()
# =============================================

library(targets)
library(tarchetypes)

# Set target options
tar_option_set(
  packages = c("tidyverse", "here", "janitor", "skimr"),
  format = "rds"
)

# Source utility functions
tar_source("scripts/utils.R")

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
