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

### iowa_major_cities.csv
| Column | Type | Description |
|--------|------|-------------|
| city | character | City name (primary key) |
| county | character | County name |
| population_2020 | integer | 2020 Census population |
| latitude | numeric | Latitude coordinate |
| longitude | numeric | Longitude coordinate |
| region | character | Iowa region (Central, Eastern, Western, Northern) |

### iowa_crime_data.csv
| Column | Type | Description |
|--------|------|-------------|
| city | character | City name |
| violent_crime_rate | numeric | Violent crimes per 100,000 |
| property_crime_rate | numeric | Property crimes per 100,000 |
| total_crime_rate | numeric | Total crimes per 100,000 |
| clearance_rate | numeric | Percentage of crimes solved |

### iowa_housing_data.csv
| Column | Type | Description |
|--------|------|-------------|
| city | character | City name |
| median_home_value | numeric | Median home value ($) |
| median_rent | numeric | Median monthly rent ($) |
| owner_occupied_pct | numeric | % owner-occupied units |
| vacancy_rate | numeric | Housing vacancy rate |
| housing_units | integer | Total housing units |

### iowa_education_data.csv
| Column | Type | Description |
|--------|------|-------------|
| city | character | City name |
| graduation_rate | numeric | High school graduation rate (%) |
| college_readiness_pct | numeric | College readiness score (%) |
| pct_bachelors | numeric | % with bachelor's degree |
| student_teacher_ratio | numeric | Students per teacher |
| school_spending_per_pupil | numeric | Spending per student ($) |

### iowa_economic_data.csv
| Column | Type | Description |
|--------|------|-------------|
| city | character | City name |
| median_household_income | numeric | Median household income ($) |
| unemployment_rate | numeric | Unemployment rate (%) |
| poverty_rate | numeric | Poverty rate (%) |
| labor_force_participation | numeric | Labor force participation (%) |
| job_growth_rate | numeric | Annual job growth (%) |

### iowa_healthcare_data.csv
| Column | Type | Description |
|--------|------|-------------|
| city | character | City name |
| life_expectancy | numeric | Life expectancy (years) |
| health_insurance_coverage_pct | numeric | % with health insurance |
| physicians_per_10000 | numeric | Physicians per 10,000 residents |
| hospital_beds_per_1000 | numeric | Hospital beds per 1,000 |

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
