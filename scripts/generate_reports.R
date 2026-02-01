# Iowa Cities PDF Report Generator
# Automated report generation using RMarkdown
# =============================================

library(tidyverse)
library(here)
library(rmarkdown)
library(knitr)

cat("
╔═══════════════════════════════════════════════════════════════╗
║              IOWA CITIES REPORT GENERATOR                     ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# CREATE REPORT TEMPLATES
# =============================================================================

# Create reports directory if needed
dir.create(here("outputs/reports"), showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# CITY PROFILE TEMPLATE
# =============================================================================

city_template <- '---
title: "City Profile: `r params$city`"
author: "Iowa Cities Analysis"
date: "`r format(Sys.Date(), \'%B %d, %Y\')`"
output:
  pdf_document:
    toc: true
    toc_depth: 2
params:
  city: "Bettendorf"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(tidyverse)
library(knitr)
library(here)

# Load data
crime <- read_csv(here("data/raw/iowa_crime_data.csv"), show_col_types = FALSE)
housing <- read_csv(here("data/raw/iowa_housing_data.csv"), show_col_types = FALSE)
education <- read_csv(here("data/raw/iowa_education_data.csv"), show_col_types = FALSE)
economic <- read_csv(here("data/raw/iowa_economic_data.csv"), show_col_types = FALSE)
healthcare <- read_csv(here("data/raw/iowa_healthcare_data.csv"), show_col_types = FALSE)
major_cities <- read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)

# Get city data
city_info <- major_cities %>% filter(city == params$city)
city_crime <- crime %>% filter(city == params$city)
city_housing <- housing %>% filter(city == params$city)
city_edu <- education %>% filter(city == params$city)
city_econ <- economic %>% filter(city == params$city)
city_health <- healthcare %>% filter(city == params$city)
```

# Overview

**`r params$city`** is located in `r city_info$county` County in the `r city_info$region` region of Iowa.

| Metric | Value |
|--------|-------|
| Population (2020) | `r format(city_info$population_2020, big.mark = ",")` |
| County | `r city_info$county` |
| Region | `r city_info$region` |
| Coordinates | `r city_info$latitude`, `r city_info$longitude` |

# Safety & Crime

| Metric | Value | State Comparison |
|--------|-------|-----------------|
| Violent Crime Rate | `r city_crime$violent_crime_rate` per 100k | `r ifelse(city_crime$violent_crime_rate < median(crime$violent_crime_rate), "Below Average ✓", "Above Average")` |
| Property Crime Rate | `r city_crime$property_crime_rate` per 100k | `r ifelse(city_crime$property_crime_rate < median(crime$property_crime_rate), "Below Average ✓", "Above Average")` |
| Police Officers | `r city_crime$officers_per_1000` per 1,000 | - |

# Housing Market

| Metric | Value |
|--------|-------|
| Median Home Value | $`r format(city_housing$median_home_value, big.mark = ",")` |
| Median Rent | $`r format(city_housing$median_rent, big.mark = ",")` |
| Owner Occupied | `r city_housing$owner_occupied_pct`% |
| Homes Built After 2000 | `r city_housing$pct_built_after_2000`% |

# Education

| Metric | Value |
|--------|-------|
| Graduation Rate | `r city_edu$graduation_rate`% |
| College Readiness | `r city_edu$college_readiness_pct`% |
| % with Bachelor\\'s+ | `r city_edu$pct_bachelors`% |
| Student-Teacher Ratio | `r city_edu$student_teacher_ratio`:1 |
| Per Pupil Spending | $`r format(city_edu$per_pupil_spending, big.mark = ",")` |

# Economy

| Metric | Value |
|--------|-------|
| Median Household Income | $`r format(city_econ$median_household_income, big.mark = ",")` |
| Unemployment Rate | `r city_econ$unemployment_rate`% |
| Poverty Rate | `r city_econ$poverty_rate`% |
| Job Growth Rate | `r city_econ$job_growth_rate`% |
| Average Commute | `r city_econ$commute_time_minutes` minutes |

# Healthcare

| Metric | Value |
|--------|-------|
| Life Expectancy | `r city_health$life_expectancy` years |
| Health Insurance Coverage | `r city_health$health_insurance_coverage_pct`% |
| Physicians per 10,000 | `r city_health$physicians_per_10000` |
| Nearby Hospitals | `r city_health$hospitals_nearby` |

# Summary

```{r summary-table}
# Create summary scores
normalize <- function(x, reverse = FALSE) {
  scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  if (reverse) scaled <- 100 - scaled
  return(round(scaled, 0))
}

summary_df <- data.frame(
  Category = c("Safety", "Housing Affordability", "Education", "Economy", "Healthcare"),
  Score = c(
    100 - normalize(city_crime$violent_crime_rate)[1],
    100 - normalize(city_housing$median_home_value)[1],
    normalize(city_edu$graduation_rate)[1],
    normalize(city_econ$median_household_income)[1],
    normalize(city_health$life_expectancy)[1]
  )
)

summary_df$Rating <- case_when(
  summary_df$Score >= 80 ~ "Excellent",
  summary_df$Score >= 60 ~ "Good",
  summary_df$Score >= 40 ~ "Average",
  TRUE ~ "Below Average"
)

kable(summary_df, caption = "Category Scores (0-100 scale)")
```

---
*Report generated automatically by Iowa Cities Analysis System*
'

writeLines(city_template, here("notebooks/city_profile_template.Rmd"))
cat("✓ Created city_profile_template.Rmd\n")

# =============================================================================
# COMPARISON REPORT TEMPLATE
# =============================================================================

comparison_template <- '---
title: "City Comparison Report"
subtitle: "`r paste(params$cities, collapse = \\" vs \\")`"
author: "Iowa Cities Analysis"
date: "`r format(Sys.Date(), \'%B %d, %Y\')`"
output:
  pdf_document:
    toc: true
params:
  cities: ["Bettendorf", "Ankeny", "West Des Moines"]
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 5)
library(tidyverse)
library(knitr)
library(here)

# Load all data
crime <- read_csv(here("data/raw/iowa_crime_data.csv"), show_col_types = FALSE)
housing <- read_csv(here("data/raw/iowa_housing_data.csv"), show_col_types = FALSE)
education <- read_csv(here("data/raw/iowa_education_data.csv"), show_col_types = FALSE)
economic <- read_csv(here("data/raw/iowa_economic_data.csv"), show_col_types = FALSE)
healthcare <- read_csv(here("data/raw/iowa_healthcare_data.csv"), show_col_types = FALSE)
major_cities <- read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)

# Filter to comparison cities
cities <- params$cities
```

# Overview Comparison

```{r overview}
major_cities %>%
  filter(city %in% cities) %>%
  select(City = city, Region = region, Population = population_2020, County = county) %>%
  kable(caption = "Basic Information")
```

# Safety Comparison

```{r safety-table}
crime %>%
  filter(city %in% cities) %>%
  select(City = city, 
         `Violent Crime` = violent_crime_rate,
         `Property Crime` = property_crime_rate,
         `Police per 1k` = officers_per_1000) %>%
  kable(caption = "Crime Statistics (per 100,000 residents)")
```

```{r safety-chart}
crime %>%
  filter(city %in% cities) %>%
  select(city, violent_crime_rate, property_crime_rate) %>%
  pivot_longer(-city, names_to = "type", values_to = "rate") %>%
  mutate(type = str_replace_all(type, "_", " ") %>% str_to_title()) %>%
  ggplot(aes(x = city, y = rate, fill = type)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Crime Rate Comparison", y = "Rate per 100k", x = "", fill = "Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

# Housing Comparison

```{r housing-table}
housing %>%
  filter(city %in% cities) %>%
  select(City = city,
         `Home Value` = median_home_value,
         `Rent` = median_rent,
         `Owner Occupied` = owner_occupied_pct) %>%
  mutate(`Home Value` = paste0("$", format(`Home Value`, big.mark = ",")),
         `Rent` = paste0("$", format(`Rent`, big.mark = ",")),
         `Owner Occupied` = paste0(`Owner Occupied`, "%")) %>%
  kable(caption = "Housing Market")
```

# Education Comparison

```{r education-table}
education %>%
  filter(city %in% cities) %>%
  select(City = city,
         `Grad Rate` = graduation_rate,
         `College Ready` = college_readiness_pct,
         `Bachelors+` = pct_bachelors,
         `Spending/Pupil` = per_pupil_spending) %>%
  mutate(`Spending/Pupil` = paste0("$", format(`Spending/Pupil`, big.mark = ","))) %>%
  kable(caption = "Education Metrics")
```

# Economic Comparison

```{r economic-table}
economic %>%
  filter(city %in% cities) %>%
  select(City = city,
         `Med Income` = median_household_income,
         `Unemployment` = unemployment_rate,
         `Poverty` = poverty_rate,
         `Job Growth` = job_growth_rate) %>%
  mutate(`Med Income` = paste0("$", format(`Med Income`, big.mark = ","))) %>%
  kable(caption = "Economic Indicators")
```

# Healthcare Comparison

```{r healthcare-table}
healthcare %>%
  filter(city %in% cities) %>%
  select(City = city,
         `Life Exp` = life_expectancy,
         `Insured` = health_insurance_coverage_pct,
         `Doctors/10k` = physicians_per_10000) %>%
  kable(caption = "Healthcare Access")
```

# Winner Summary

```{r winner-analysis}
# Calculate which city wins each category
normalize <- function(x, reverse = FALSE) {
  scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  if (reverse) scaled <- 100 - scaled
  return(scaled)
}

comparison_data <- major_cities %>%
  filter(city %in% cities) %>%
  left_join(crime %>% select(city, violent_crime_rate), by = "city") %>%
  left_join(housing %>% select(city, median_home_value), by = "city") %>%
  left_join(education %>% select(city, graduation_rate), by = "city") %>%
  left_join(economic %>% select(city, median_household_income), by = "city") %>%
  left_join(healthcare %>% select(city, life_expectancy), by = "city") %>%
  mutate(
    safety_rank = rank(-violent_crime_rate),
    housing_rank = rank(-median_home_value),
    edu_rank = rank(graduation_rate),
    econ_rank = rank(median_household_income),
    health_rank = rank(life_expectancy)
  )

winners <- data.frame(
  Category = c("Safety (Lowest Crime)", "Affordability", "Education", "Income", "Healthcare"),
  Winner = c(
    comparison_data$city[which.max(comparison_data$safety_rank)],
    comparison_data$city[which.max(comparison_data$housing_rank)],
    comparison_data$city[which.max(comparison_data$edu_rank)],
    comparison_data$city[which.max(comparison_data$econ_rank)],
    comparison_data$city[which.max(comparison_data$health_rank)]
  )
)

kable(winners, caption = "Category Winners")
```

---
*Report generated automatically by Iowa Cities Analysis System*
'

writeLines(comparison_template, here("notebooks/comparison_report_template.Rmd"))
cat("✓ Created comparison_report_template.Rmd\n")

# =============================================================================
# REPORT GENERATION FUNCTIONS
# =============================================================================

#' Generate a city profile PDF report
#' 
#' @param city_name Name of the city
#' @param output_dir Output directory (default: outputs/reports)
#' @return Path to generated PDF
generate_city_report <- function(city_name, output_dir = here("outputs/reports")) {
  
  output_file <- file.path(output_dir, paste0(gsub(" ", "_", city_name), "_profile.pdf"))
  
  cat(sprintf("Generating report for %s...\n", city_name))
  
  rmarkdown::render(
    input = here("notebooks/city_profile_template.Rmd"),
    output_file = output_file,
    params = list(city = city_name),
    quiet = TRUE
  )
  
  cat(sprintf("✓ Saved: %s\n", output_file))
  return(output_file)
}

#' Generate a comparison PDF report
#' 
#' @param cities Vector of city names to compare
#' @param output_dir Output directory
#' @return Path to generated PDF
generate_comparison_report <- function(cities, output_dir = here("outputs/reports")) {
  
  filename <- paste0("comparison_", paste(gsub(" ", "", cities), collapse = "_vs_"), ".pdf")
  output_file <- file.path(output_dir, filename)
  
  cat(sprintf("Generating comparison report for %s...\n", paste(cities, collapse = ", ")))
  
  rmarkdown::render(
    input = here("notebooks/comparison_report_template.Rmd"),
    output_file = output_file,
    params = list(cities = cities),
    quiet = TRUE
  )
  
  cat(sprintf("✓ Saved: %s\n", output_file))
  return(output_file)
}

#' Generate reports for all cities
generate_all_city_reports <- function() {
  major_cities <- read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)
  
  cat(sprintf("Generating reports for %d cities...\n\n", nrow(major_cities)))
  
  reports <- character(nrow(major_cities))
  
  for (i in 1:nrow(major_cities)) {
    city <- major_cities$city[i]
    reports[i] <- tryCatch(
      generate_city_report(city),
      error = function(e) {
        cat(sprintf("  ✗ Error with %s: %s\n", city, e$message))
        return(NA)
      }
    )
  }
  
  successful <- sum(!is.na(reports))
  cat(sprintf("\n✓ Generated %d of %d reports\n", successful, length(reports)))
  
  invisible(reports)
}

# =============================================================================
# USAGE INSTRUCTIONS
# =============================================================================

cat("\n")
cat(rep("═", 60), "\n", sep = "")
cat("REPORT GENERATOR READY\n")
cat(rep("═", 60), "\n", sep = "")

cat("\nTemplates created:\n")
cat("  • notebooks/city_profile_template.Rmd\n")
cat("  • notebooks/comparison_report_template.Rmd\n")

cat("\nUsage:\n")
cat("  # Generate single city profile:\n")
cat("  generate_city_report('Bettendorf')\n")
cat("  generate_city_report('Des Moines')\n")

cat("\n  # Generate comparison report:\n")
cat("  generate_comparison_report(c('Bettendorf', 'Ankeny', 'West Des Moines'))\n")

cat("\n  # Generate all city reports:\n")
cat("  generate_all_city_reports()\n")

cat("\nNote: Requires LaTeX for PDF generation.\n")
cat("Install with: tinytex::install_tinytex()\n")

cat(rep("═", 60), "\n", sep = "")
