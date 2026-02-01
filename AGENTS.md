# AI Agent Instructions for R Data Analysis Workspace

This file provides instructions for AI coding agents (GitHub Copilot, Claude, ChatGPT, Cursor, etc.) working with this R data analysis project.

## Project Overview

This is an R-based data analysis workspace focused on Iowa cities data. It includes:

- 13 datasets covering demographics, economics, housing, education, crime, healthcare, environment, and more
- Interactive Shiny dashboard
- Automated analysis pipelines
- Report generation capabilities

## Technology Stack

- **Language**: R 4.0+
- **Core Packages**: tidyverse (dplyr, ggplot2, tidyr, readr, purrr, stringr)
- **Web Framework**: Shiny + shinydashboard
- **Visualization**: ggplot2, plotly, leaflet
- **Tables**: DT, knitr, kableExtra
- **Reports**: rmarkdown, tinytex (for PDF)
- **Testing**: testthat
- **Pipeline**: targets (modern), Makefile (legacy)
- **Path Management**: here package
- **Package Management**: pacman

## Directory Structure

```
r-data/
├── .github/                 # GitHub configuration
│   └── copilot-instructions.md
├── app.R                    # Shiny dashboard entry point
├── config.yml               # Environment configuration
├── data/
│   ├── raw/                 # Original data (NEVER modify)
│   ├── processed/           # Cleaned/transformed data
│   └── external/            # Third-party data sources
├── docs/                    # Documentation
├── notebooks/               # R Markdown files
├── outputs/
│   ├── figures/             # Generated plots
│   ├── reports/             # PDF/HTML reports
│   └── tables/              # Generated tables
├── scripts/
│   ├── analysis/            # Domain-specific analyses
│   ├── core/                # Core workflow scripts (00-05)
│   └── interactive/         # Interactive tools
└── tests/testthat/          # Unit tests
```

## Code Conventions

### File Naming

- Use lowercase with underscores: `iowa_housing_analysis.R`
- Prefix core scripts with numbers: `00_setup.R`, `01_data_import.R`
- Use `.R` for scripts, `.Rmd` for notebooks

### Code Style

- Follow tidyverse style guide
- Use `<-` for assignment (not `=`)
- Use pipes `%>%` or `|>` for data transformation chains
- Use `here()` for all file paths (never hardcode paths)
- Prefer tidyverse functions over base R when available

### Function Documentation

```r
#' Brief description of function
#' 
#' @param param1 Description of parameter
#' @param param2 Description of parameter
#' @return Description of return value
#' @examples
#' function_name(param1 = "value")
function_name <- function(param1, param2 = default) {
  # Implementation
}
```

### Data Conventions

- Raw data is immutable - never modify files in `data/raw/`
- Save processed data to `data/processed/`
- Use CSV for data interchange
- Include data dictionaries for new datasets

## Key Files Reference

### Entry Points

- `app.R` - Launch Shiny dashboard: `shiny::runApp("app.R")`
- `scripts/run_all_iowa_analyses.R` - Run complete analysis pipeline
- `example_workflow.R` - Demo script for new users

### Configuration

- `.Rprofile` - Auto-runs on project load
- `config.yml` - Environment-specific settings
- `.lintr` - Linting rules
- `_targets.R` - Pipeline definition

### Core Data Files

| File | Description | Key Columns |
|------|-------------|-------------|
| `iowa_major_cities.csv` | City metadata | city, region, population, lat, lon |
| `iowa_crime_data.csv` | Safety metrics | violent_crime_rate, property_crime_rate |
| `iowa_housing_data.csv` | Housing market | median_home_value, median_rent |
| `iowa_education_data.csv` | Schools | graduation_rate, pct_bachelors |
| `iowa_economic_data.csv` | Economy | median_household_income, unemployment_rate |
| `iowa_healthcare_data.csv` | Health | life_expectancy, physicians_per_10000 |
| `iowa_demographics_data.csv` | Population | median_age, diversity_index |
| `iowa_environment_data.csv` | Environment | air_quality_index, green_space_pct |
| `iowa_historical_data.csv` | Trends 1980-2020 | population_YYYY, median_income_YYYY |

## Common Tasks

### Adding a New Analysis

1. Create script in `scripts/analysis/`
2. Load data with `read_csv(here("data/raw/filename.csv"))`
3. Save outputs to `data/processed/` or `outputs/`
4. Add to `run_all_iowa_analyses.R` if part of main pipeline

### Adding a New Dataset

1. Place raw file in `data/raw/`
2. Document schema in `data/README.md`
3. Create import script if needed
4. Update relevant analysis scripts

### Creating Visualizations

```r
# Standard ggplot with project theme
ggplot(data, aes(x, y)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Title", x = "X Label", y = "Y Label")

# Save to outputs
ggsave(here("outputs/figures/plot_name.png"), width = 10, height = 6, dpi = 300)
```

### Running Tests

```r
testthat::test_dir("tests/testthat")
# Or from terminal: Rscript -e "testthat::test_dir('tests/testthat')"
```

## Shiny Dashboard Structure

The `app.R` file contains:

- **UI**: dashboardPage with sidebar menu and tabItems
- **Server**: Reactive expressions and render functions
- **Tabs**: Overview, Rankings, Compare, Profiles, Map, Recommend, Trends

To add a new tab:

1. Add menuItem in dashboardSidebar
2. Add tabItem in dashboardBody
3. Add corresponding server logic

## Error Handling Patterns

```r
# Safe file reading
tryCatch({
  data <- read_csv(here("data/raw/file.csv"), show_col_types = FALSE)
}, error = function(e) {
  stop("Failed to load file: ", e$message)
})

# Check required packages
if (!require("package_name", quietly = TRUE)) {
  install.packages("package_name")
  library(package_name)
}
```

## Testing Guidelines

- Place tests in `tests/testthat/`
- Name test files `test-*.R`
- Test utility functions, data validation, and transformations
- Use `expect_*` functions from testthat

## Performance Considerations

- Use `show_col_types = FALSE` in read_csv to suppress messages
- For large datasets, consider data.table or arrow
- Cache expensive computations with targets or reactive values
- Limit plotly points for large scatter plots

## Common Gotchas

1. **Working Directory**: Always use `here()` for paths
2. **Package Conflicts**: Load tidyverse last, or use `dplyr::filter()`
3. **NA Handling**: Use `na.rm = TRUE` in summary functions
4. **Column Names**: Use backticks for names with spaces: `` `Column Name` ``

## Resources

- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [R for Data Science](https://r4ds.had.co.nz/)
- [Shiny Documentation](https://shiny.posit.co/)
- [targets Manual](https://books.ropensci.org/targets/)
