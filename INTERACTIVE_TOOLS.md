# Iowa Cities Interactive Tools Guide

This guide explains how to use the interactive analysis tools available in this workspace.

## 🎯 Quick Launch

### 1. Shiny Dashboard (Recommended)

Launch the full interactive dashboard:

```r
# From R/RStudio
shiny::runApp("app.R")
```

The dashboard includes:

- **Overview**: Summary statistics and visualizations
- **City Rankings**: Sort cities by any metric
- **Compare Cities**: Side-by-side comparison with radar charts
- **City Profiles**: Detailed view of each city
- **Interactive Map**: Geographic visualization
- **Find Your City**: Personalized recommendations
- **Historical Trends**: 40-year data visualization

### 2. Geographic Maps (Leaflet)

Generate interactive HTML maps:

```r
source("scripts/iowa_maps.R")
```

This creates 4 maps in `outputs/`:

- `iowa_population_map.html` - Population bubbles
- `iowa_income_map.html` - Income choropleth
- `iowa_safety_map.html` - Crime rate visualization
- `iowa_comprehensive_map.html` - Multi-layer map with toggles

Open any map in your browser to explore.

### 3. City Comparison Tool

Compare 2-3 cities with detailed metrics:

```r
source("scripts/iowa_city_comparison.R")

# Compare two cities
compare_cities("Bettendorf", "Ankeny")

# Compare three cities
compare_cities("Des Moines", "Cedar Rapids", "Iowa City")

# Generate radar chart
create_radar_chart("Bettendorf", "Ankeny", "West Des Moines",
                   save_file = "outputs/my_comparison.png")
```

### 4. Recommendation Engine

Get personalized city recommendations:

```r
source("scripts/iowa_recommendations.R")

# Custom weights (0-10 scale)
recommend_city(
  safety_weight = 10,
  education_weight = 9,
  affordability_weight = 7,
  jobs_weight = 6,
  healthcare_weight = 5,
  outdoor_weight = 4,
  urban_weight = 3,
  family_weight = 8,
  commute_weight = 5,
  environment_weight = 5,
  max_budget = 300000
)

# Use predefined profiles
recommend_for_profile("family")
recommend_for_profile("young_professional")
recommend_for_profile("retiree")
recommend_for_profile("remote_worker")
recommend_for_profile("outdoor_enthusiast")
recommend_for_profile("budget_conscious")
```

### 5. Predictive Analysis

Get population and home value projections to 2030:

```r
source("scripts/iowa_predictions.R")
```

This generates:

- Population projections for all cities
- Home value appreciation forecasts
- Income growth predictions
- Affordability index projections
- Growth trajectory classifications

Results saved to: `data/processed/iowa_cities_projections_2030.csv`

### 6. PDF Report Generation

Generate professional PDF reports:

```r
source("scripts/generate_reports.R")

# Single city profile
generate_city_report("Bettendorf")
generate_city_report("Des Moines")

# Comparison report
generate_comparison_report(c("Bettendorf", "Ankeny", "West Des Moines"))

# Generate all city reports
generate_all_city_reports()
```

**Note**: Requires LaTeX for PDF generation. Install with:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## 📊 Available Datasets

| Dataset | File | Metrics |
|---------|------|---------|
| Major Cities | `iowa_major_cities.csv` | Population, coordinates, region |
| Crime | `iowa_crime_data.csv` | Violent/property crime rates, police |
| Housing | `iowa_housing_data.csv` | Home values, rent, ownership |
| Education | `iowa_education_data.csv` | Graduation rates, college readiness |
| Economic | `iowa_economic_data.csv` | Income, unemployment, job growth |
| Healthcare | `iowa_healthcare_data.csv` | Life expectancy, insurance, doctors |
| Amenities | `iowa_amenities_data.csv` | Livability, parks, walkability |
| Demographics | `iowa_demographics_data.csv` | Age, diversity, migration |
| Environment | `iowa_environment_data.csv` | Air quality, green space |
| Historical | `iowa_historical_data.csv` | 1980-2020 trends |

## 🏆 Top Ranked Cities (Overall Score)

1. **Ankeny** - Strong growth, excellent schools, safe
2. **Bettendorf** - Balanced excellence across all metrics
3. **West Des Moines** - Economic powerhouse, great amenities
4. **Waukee** - Fast-growing, family-friendly
5. **Clive** - High income, low crime, walkable

## 🔧 Troubleshooting

**Shiny app won't start?**

```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "plotly", "DT"))
```

**Maps not generating?**

```r
# Install leaflet
install.packages(c("leaflet", "htmlwidgets"))
```

**PDF reports fail?**

```r
# Install LaTeX
tinytex::install_tinytex()
```

## 📁 Output Files

Generated files are saved to:

- `outputs/` - Visualizations and maps
- `outputs/reports/` - PDF reports
- `data/processed/` - Analysis results

---

Happy exploring! 🌆
