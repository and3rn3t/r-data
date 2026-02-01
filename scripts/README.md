# Scripts Directory

This directory contains R scripts for data analysis of Iowa cities.

## Script Categories

### Core Workflow Scripts (Numbered)

Run in order for complete analysis:

| Script | Purpose |
|--------|---------|
| `00_setup.R` | Install and load packages |
| `01_data_import.R` | Import raw data |
| `02_data_cleaning.R` | Clean and preprocess |
| `03_exploratory_analysis.R` | EDA and summaries |
| `04_analysis.R` | Statistical analysis |
| `05_visualization.R` | Generate plots |

### Domain Analysis Scripts

| Script | Domain | Key Outputs |
|--------|--------|-------------|
| `iowa_cities_import.R` | Data import | Raw data files |
| `iowa_cities_cleaning.R` | Cleaning | Clean CSVs |
| `iowa_cities_analysis.R` | Population | Regional analysis |
| `iowa_economic_analysis.R` | Economy | Income, jobs |
| `iowa_housing_analysis.R` | Housing | Values, rent |
| `iowa_education_analysis.R` | Education | Schools, attainment |
| `iowa_crime_safety_analysis.R` | Safety | Crime rates |
| `iowa_demographics_analysis.R` | Demographics | Age, diversity |
| `iowa_environment_analysis.R` | Environment | Air, water, green space |
| `iowa_historical_analysis.R` | Trends | 40-year trajectories |
| `iowa_comprehensive_analysis.R` | Combined | Multi-factor scores |

### Interactive Tools

| Script | Purpose |
|--------|---------|
| `iowa_maps.R` | Generate Leaflet HTML maps |
| `iowa_city_comparison.R` | Compare cities with radar charts |
| `iowa_predictions.R` | Forecast to 2030 |
| `iowa_recommendations.R` | City recommendation engine |
| `generate_reports.R` | PDF report generation |

### Utility Scripts

| Script | Purpose |
|--------|---------|
| `utils.R` | Shared helper functions |
| `validate_data.R` | Data validation checks |
| `run_all_iowa_analyses.R` | Master pipeline script |

## Quick Start

```r
# Option 1: Run complete pipeline
source("scripts/run_all_iowa_analyses.R")

# Option 2: Run individual analyses
source("scripts/00_setup.R")
source("scripts/iowa_housing_analysis.R")

# Option 3: Use interactive tools
source("scripts/iowa_recommendations.R")
recommend_for_profile("family")
```

## Output Locations

| Output Type | Directory |
|-------------|-----------|
| Processed data | `data/processed/` |
| Visualizations | `outputs/figures/` |
| PDF reports | `outputs/reports/` |
| Interactive maps | `outputs/` |

## Adding New Scripts

1. Follow naming convention: `iowa_<domain>_analysis.R`
2. Add documentation header
3. Use `here()` for all paths
4. Save outputs to appropriate directories
5. Add to `run_all_iowa_analyses.R` if part of main pipeline

## See Also

- [../docs/API_REFERENCE.md](../docs/API_REFERENCE.md) - Function documentation
- [../docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md) - Project structure
- [../INTERACTIVE_TOOLS.md](../INTERACTIVE_TOOLS.md) - Tools guide
