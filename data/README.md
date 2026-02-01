# Data Directory - Iowa City Data Analysis

This directory contains data files for analyzing cities in the state of Iowa.

## Directory Structure

```
data/
├── raw/          # Source data (NEVER modify)
├── processed/    # Cleaned/transformed data
└── external/     # Third-party data
```

## Raw Data Files

All source data in `raw/` - these files are **immutable**.

| File | Records | Description |
|------|---------|-------------|
| `iowa_major_cities.csv` | 21 | City metadata (population, coordinates, region) |
| `iowa_crime_data.csv` | 21 | Crime rates and safety metrics |
| `iowa_housing_data.csv` | 21 | Housing market data |
| `iowa_education_data.csv` | 21 | School and education metrics |
| `iowa_economic_data.csv` | 20 | Income, employment, jobs |
| `iowa_healthcare_data.csv` | 20 | Health and healthcare access |
| `iowa_demographics_data.csv` | 20 | Age, diversity, households |
| `iowa_environment_data.csv` | 20 | Air, water, green space |
| `iowa_amenities_data.csv` | 20 | Quality of life metrics |
| `iowa_infrastructure_data.csv` | 20 | Transportation, connectivity |
| `iowa_historical_data.csv` | 20 | 1980-2020 trends |
| `iowa_cities_census.csv` | - | US Census population data |
| `iowa_cities_raw.csv` | - | Iowa Data Portal data |

## Processed Data Files

Generated outputs in `processed/`:

| File | Description |
|------|-------------|
| `iowa_cities_clean.csv` | Standardized city data |
| `iowa_cities_analyzed.csv` | Analysis with scores |
| `iowa_crime_analyzed.csv` | Crime analysis results |
| `iowa_housing_analyzed.csv` | Housing analysis results |
| `iowa_education_analyzed.csv` | Education analysis results |
| `iowa_cities_projections_2030.csv` | Forecasts to 2030 |
| `cleaning_log.txt` | Data cleaning log |

## Data Sources

| Source | URL | Data Types |
|--------|-----|------------|
| US Census Bureau | census.gov | Population, demographics, housing |
| FBI UCR | ucr.fbi.gov | Crime statistics |
| Bureau of Labor Statistics | bls.gov | Employment, wages |
| Iowa Data Portal | data.iowa.gov | State-specific data |

## Data Schema

For complete column definitions, see: [../docs/DATA_DICTIONARY.md](../docs/DATA_DICTIONARY.md)

### Key Join Column

All datasets use `city` as the primary key:

```r
library(tidyverse)
library(here)

full_data <- read_csv(here("data/raw/iowa_major_cities.csv")) %>%
  left_join(read_csv(here("data/raw/iowa_crime_data.csv")), by = "city") %>%
  left_join(read_csv(here("data/raw/iowa_housing_data.csv")), by = "city") %>%
  left_join(read_csv(here("data/raw/iowa_education_data.csv")), by = "city")
```

## Data Processing Workflow

1. **Import**: Run `scripts/iowa_cities_import.R`
2. **Clean**: Run `scripts/iowa_cities_cleaning.R`
3. **Analyze**: Run individual or `scripts/run_all_iowa_analyses.R`
4. **Report**: Render notebooks or launch Shiny app

## Guidelines

1. **Never modify raw data** - always save to `processed/`
2. **Use here() for paths** - `read_csv(here("data/raw/file.csv"))`
3. **Document transformations** - log cleaning steps
4. **Validate before analysis** - run `scripts/validate_data.R`
5. **Add show_col_types = FALSE** - suppress column type messages
