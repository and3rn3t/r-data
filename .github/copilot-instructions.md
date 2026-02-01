# GitHub Copilot Instructions

This file provides context for GitHub Copilot when working in this repository.

## Project Context

This is an R data analysis workspace for analyzing Iowa cities data across multiple dimensions including demographics, economics, housing, education, crime, healthcare, and environment.

## Language and Framework

- Primary language: R (version 4.0+)
- Key packages: tidyverse, shiny, plotly, leaflet, testthat
- Report generation: rmarkdown
- Path management: here package (always use `here()` for file paths)

## Code Style Preferences

- Use `<-` for assignment, not `=`
- Use tidyverse/dplyr for data manipulation
- Use ggplot2 for static visualizations
- Use plotly for interactive charts
- Prefer pipes (`%>%` or `|>`) for chaining operations
- Follow tidyverse style guide for naming (snake_case)

## File Patterns

- Scripts: `scripts/*.R`
- Notebooks: `notebooks/*.Rmd`
- Tests: `tests/testthat/test-*.R`
- Data: `data/raw/*.csv`, `data/processed/*.csv`
- Outputs: `outputs/figures/*.png`, `outputs/reports/*.pdf`

## Common Patterns

### Loading Data
```r
library(tidyverse)
library(here)
data <- read_csv(here("data/raw/filename.csv"), show_col_types = FALSE)
```

### Saving Outputs
```r
write_csv(data, here("data/processed/filename.csv"))
ggsave(here("outputs/figures/plot.png"), width = 10, height = 6, dpi = 300)
```

### Normalization Function (frequently used)
```r
normalize <- function(x, reverse = FALSE) {
  scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  if (reverse) scaled <- 100 - scaled
  return(round(scaled, 1))
}
```

## Key Datasets

All Iowa cities data is in `data/raw/`:
- `iowa_major_cities.csv` - Base city info (population, coordinates, region)
- `iowa_crime_data.csv` - Crime and safety metrics
- `iowa_housing_data.csv` - Housing market data
- `iowa_education_data.csv` - Education statistics
- `iowa_economic_data.csv` - Economic indicators
- `iowa_healthcare_data.csv` - Health metrics
- `iowa_demographics_data.csv` - Demographic data
- `iowa_environment_data.csv` - Environmental metrics
- `iowa_amenities_data.csv` - Quality of life metrics
- `iowa_historical_data.csv` - 40-year trend data

## Testing

Tests use testthat framework:
```r
test_that("description", {
  expect_equal(actual, expected)
  expect_true(condition)
})
```

## Shiny App

The main dashboard is in `app.R`. It uses shinydashboard with:
- dashboardHeader, dashboardSidebar, dashboardBody structure
- plotlyOutput for interactive charts
- DTOutput for data tables
- Reactive expressions for dynamic filtering

## Don't

- Don't modify files in `data/raw/` - these are immutable source files
- Don't hardcode file paths - always use `here()`
- Don't use `setwd()` - rely on here package instead
- Don't commit `.RData` or `.Rhistory` files
