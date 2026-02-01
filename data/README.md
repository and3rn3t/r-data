# Data Directory - Iowa City Data Analysis

This directory contains data files for analyzing cities in the state of Iowa.

## Subdirectories

- **raw/**: Original, immutable data files. Never modify files in this directory.
- **processed/**: Cleaned and processed data files ready for analysis.
- **external/**: External data from third-party sources.

## Iowa City Data Sources

### Iowa Data Portal
- **URL**: https://data.iowa.gov/
- **Description**: Official State of Iowa open data portal with various city datasets
- **Updates**: Varies by dataset

### US Census Bureau
- **URL**: https://www.census.gov/
- **Description**: Population estimates for incorporated places
- **Updates**: Annual estimates, decennial census

## Expected Data Files

### Raw Data (`raw/`)
| File | Description |
|------|-------------|
| `iowa_cities_census.csv` | US Census Bureau population data |
| `iowa_cities_raw.csv` | Iowa Data Portal city data |
| `iowa_major_cities.csv` | Reference data for top 20 Iowa cities |

### Processed Data (`processed/`)
| File | Description |
|------|-------------|
| `iowa_cities_clean.csv` | Cleaned and standardized city data |
| `iowa_cities_analyzed.csv` | Data with analysis results and classifications |
| `cleaning_log.txt` | Log of data cleaning operations |

## Available Variables

| Variable | Description |
|----------|-------------|
| `city` | City name |
| `county` | County name |
| `population_2020` | 2020 Census population |
| `latitude` | Geographic latitude |
| `longitude` | Geographic longitude |
| `region` | Iowa region (Central, Northeast, etc.) |
| `size_category` | City size classification |
| `population_rank` | Rank by population |

## Guidelines

1. Keep raw data unchanged - always work with copies
2. Document data sources and collection methods
3. Use consistent naming conventions (e.g., `YYYY-MM-DD_description.csv`)
4. Include metadata files when applicable

## Data Processing Workflow

1. **Import**: Run `scripts/iowa_cities_import.R` to download data
2. **Clean**: Run `scripts/iowa_cities_cleaning.R` to clean and standardize
3. **Analyze**: Run `scripts/iowa_cities_analysis.R` for analysis
4. **Report**: Render `notebooks/iowa_cities_report.Rmd` for full report
