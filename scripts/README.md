# Scripts Directory

This directory contains R scripts for data analysis of Iowa cities.

## Script Organization

### Core Scripts
- **00_setup.R**: Install and load required packages
- **01_data_import.R**: Import and initial data loading
- **02_data_cleaning.R**: Data cleaning and preprocessing
- **03_exploratory_analysis.R**: Exploratory data analysis (EDA)
- **04_analysis.R**: Main statistical analysis
- **05_visualization.R**: Create publication-ready visualizations
- **utils.R**: Helper functions used across scripts

### Iowa-Specific Analysis Scripts

| Script | Description |
|--------|-------------|
| `iowa_cities_import.R` | Download Iowa city data from Census & Iowa Data Portal |
| `iowa_cities_cleaning.R` | Clean and standardize Iowa city data |
| `iowa_cities_analysis.R` | Population and geographic analysis |
| `iowa_economic_analysis.R` | Income, employment, and industry analysis |
| `iowa_housing_analysis.R` | Home values, rental market, affordability |
| `iowa_education_analysis.R` | School performance, educational attainment |
| `iowa_crime_safety_analysis.R` | Crime rates, public safety metrics |

## Recommended Execution Order

```r
# 1. Setup environment
source("scripts/00_setup.R")

# 2. Import and clean data
source("scripts/iowa_cities_import.R")
source("scripts/iowa_cities_cleaning.R")

# 3. Run individual analyses
source("scripts/iowa_cities_analysis.R")
source("scripts/iowa_economic_analysis.R")
source("scripts/iowa_housing_analysis.R")
source("scripts/iowa_education_analysis.R")
source("scripts/iowa_crime_safety_analysis.R")

# 4. Generate comprehensive report
rmarkdown::render("notebooks/iowa_comprehensive_dashboard.Rmd")
```

## Output Files

Each analysis script saves:
- Processed data to `data/processed/`
- Visualizations to `outputs/`

## Naming Convention

Use numbered prefixes (00, 01, 02, etc.) to indicate the order of execution.
