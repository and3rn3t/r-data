# Claude AI Instructions

## Context

R data analysis project for Iowa cities with interactive Shiny dashboard

## Stack

- R 4.0+ with tidyverse
- Shiny/shinydashboard for web UI
- plotly/leaflet for interactive viz
- testthat for testing
- here package for paths

## Conventions

- Assignment: `<-` not `=`
- Paths: Always use `here("path/to/file")`
- Style: snake_case, tidyverse conventions
- Data: Raw data in `data/raw/` is immutable

## Entry Points

- `shiny::runApp("app.R")` - Dashboard
- `source("scripts/run_all_iowa_analyses.R")` - Full pipeline
- `source("example_workflow.R")` - Demo

## Data Files

Located in `data/raw/`:

- iowa_major_cities.csv (city, region, population, lat, lon)
- iowa_crime_data.csv, iowa_housing_data.csv, iowa_education_data.csv
- iowa_economic_data.csv, iowa_healthcare_data.csv
- iowa_demographics_data.csv, iowa_environment_data.csv
- iowa_amenities_data.csv, iowa_historical_data.csv

## Common Patterns

```r
# Load data
df <- read_csv(here("data/raw/file.csv"), show_col_types = FALSE)

# Transform
result <- df %>%
  filter(condition) %>%
  mutate(new_col = calculation) %>%
  arrange(desc(column))

# Save
write_csv(result, here("data/processed/output.csv"))
ggsave(here("outputs/figures/plot.png"))
```

## Testing

```r
testthat::test_dir("tests/testthat")
```
