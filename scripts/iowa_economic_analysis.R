# Iowa Cities Economic Analysis
# Analyzes income, employment, and economic indicators for Iowa cities

# Load required packages and utilities
library(tidyverse)
library(here)
library(scales)
library(corrplot)

source(here("scripts/00_setup.R"))
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

start_time <- Sys.time()

# =============================================================================
# Data Sources for Economic Data
# =============================================================================

cat("=== Iowa Economic Data Import ===\n\n")

# Census Bureau American Community Survey (ACS) - Economic Data
# Note: For production use, consider using the tidycensus package with an API key

# Create sample economic dataset for major Iowa cities
# In production, replace with actual API calls or downloaded data

iowa_economic <- tibble(

  city = c("Des Moines", "Cedar Rapids", "Davenport", "Sioux City", "Iowa City",
           "Waterloo", "Ames", "West Des Moines", "Ankeny", "Council Bluffs",
           "Dubuque", "Urbandale", "Cedar Falls", "Marion", "Bettendorf",
           "Mason City", "Marshalltown", "Clinton", "Burlington", "Fort Dodge"),
  county = c("Polk", "Linn", "Scott", "Woodbury", "Johnson",
             "Black Hawk", "Story", "Polk", "Polk", "Pottawattamie",
             "Dubuque", "Polk", "Black Hawk", "Linn", "Scott",
             "Cerro Gordo", "Marshall", "Clinton", "Des Moines", "Webster"),
  population = c(214237, 137710, 101724, 85797, 74828,
                 67314, 66427, 68723, 67355, 62230,
                 59667, 45580, 40713, 40359, 39102,
                 27088, 26486, 24462, 24505, 24264),
  median_household_income = c(54615, 58472, 52841, 48523, 52876,
                               46831, 42156, 85421, 92456, 51234,
                               56782, 89654, 55623, 71234, 78956,
                               45678, 43521, 41234, 44567, 42890),
  per_capita_income = c(31245, 33156, 29876, 26543, 32654,
                        25678, 24532, 48765, 45678, 28976,
                        32145, 47890, 29876, 38765, 42345,
                        26789, 24567, 23456, 25678, 24123),
  poverty_rate = c(16.2, 11.8, 15.4, 17.8, 22.1,
                   18.5, 28.3, 4.2, 3.8, 14.6,
                   12.3, 4.8, 15.2, 6.5, 5.1,
                   15.4, 18.7, 19.2, 16.8, 17.5),
  unemployment_rate = c(4.2, 3.8, 4.5, 4.8, 3.2,
                        5.1, 3.5, 2.8, 2.6, 4.3,
                        3.9, 2.9, 3.4, 3.2, 2.7,
                        4.6, 5.2, 5.8, 5.1, 5.4),
  labor_force_participation = c(68.5, 70.2, 66.8, 65.4, 67.8,
                                 64.2, 72.5, 74.8, 76.2, 67.5,
                                 69.8, 75.4, 68.9, 72.3, 73.6,
                                 62.4, 63.8, 61.2, 62.8, 61.5),
  major_industry = c("Government/Insurance", "Manufacturing", "Manufacturing", "Meatpacking",
                     "Education/Healthcare", "Manufacturing", "Education", "Finance/Insurance",
                     "Retail/Services", "Gaming/Retail", "Manufacturing", "Finance/Tech",
                     "Education", "Healthcare", "Finance", "Healthcare", "Manufacturing",
                     "Manufacturing", "Manufacturing", "Healthcare")
)

# Save raw economic data
write_csv(iowa_economic, here("data/raw/iowa_economic_data.csv"))
cat("Saved economic data to: data/raw/iowa_economic_data.csv\n\n")

# =============================================================================
# Income Analysis
# =============================================================================

cat("=== Income Analysis ===\n\n")

# Summary statistics for income
income_summary <- iowa_economic %>%
  summarise(
    mean_median_income = mean(median_household_income),
    median_median_income = median(median_household_income),
    min_income = min(median_household_income),
    max_income = max(median_household_income),
    income_range = max_income - min_income,
    std_dev = sd(median_household_income)
  )

cat("Median Household Income Statistics:\n")
print(income_summary)

# Income categories
iowa_economic <- iowa_economic %>%
  mutate(
    income_category = case_when(
      median_household_income >= 80000 ~ "High Income",
      median_household_income >= 60000 ~ "Upper Middle",
      median_household_income >= 50000 ~ "Middle Income",
      median_household_income >= 40000 ~ "Lower Middle",
      TRUE ~ "Low Income"
    ),
    income_category = factor(income_category, 
                              levels = c("Low Income", "Lower Middle", "Middle Income", 
                                        "Upper Middle", "High Income"))
  )

# Top and bottom cities by income
cat("\nTop 5 Cities by Median Household Income:\n")
iowa_economic %>%
  arrange(desc(median_household_income)) %>%
  select(city, county, median_household_income) %>%
  head(5) %>%
  print()

cat("\nBottom 5 Cities by Median Household Income:\n")
iowa_economic %>%
  arrange(median_household_income) %>%
  select(city, county, median_household_income) %>%
  head(5) %>%
  print()

# =============================================================================
# Employment Analysis
# =============================================================================

cat("\n=== Employment Analysis ===\n\n")

# Unemployment statistics
unemployment_summary <- iowa_economic %>%
  summarise(
    mean_unemployment = mean(unemployment_rate),
    min_unemployment = min(unemployment_rate),
    max_unemployment = max(unemployment_rate),
    cities_above_4pct = sum(unemployment_rate > 4.0)
  )

cat("Unemployment Rate Statistics:\n")
print(unemployment_summary)

# Employment health score (composite)
iowa_economic <- iowa_economic %>%
  mutate(
    employment_health_score = (
      (1 - unemployment_rate / max(unemployment_rate)) * 40 +
      (labor_force_participation / max(labor_force_participation)) * 60
    )
  )

cat("\nCities with Best Employment Health:\n")
iowa_economic %>%
  arrange(desc(employment_health_score)) %>%
  select(city, unemployment_rate, labor_force_participation, employment_health_score) %>%
  head(5) %>%
  print()

# =============================================================================
# Poverty Analysis
# =============================================================================

cat("\n=== Poverty Analysis ===\n\n")

poverty_summary <- iowa_economic %>%
  summarise(
    mean_poverty = mean(poverty_rate),
    median_poverty = median(poverty_rate),
    cities_above_15pct = sum(poverty_rate > 15),
    cities_below_10pct = sum(poverty_rate < 10)
  )

cat("Poverty Rate Statistics:\n")
print(poverty_summary)

# Note: Ames has high poverty due to student population
cat("\nNote: Ames shows high poverty rate (28.3%) due to large student population\n")

# =============================================================================
# Industry Analysis
# =============================================================================

cat("\n=== Industry Analysis ===\n\n")

industry_summary <- iowa_economic %>%
  count(major_industry, name = "num_cities") %>%
  arrange(desc(num_cities))

cat("Major Industries in Iowa Cities:\n")
print(industry_summary)

# =============================================================================
# Visualizations
# =============================================================================

cat("\n=== Creating Economic Visualizations ===\n\n")

# 1. Income comparison bar chart
p1 <- iowa_economic %>%
  arrange(desc(median_household_income)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(city, median_household_income), 
             y = median_household_income, 
             fill = income_category)) +
  geom_col() +
  geom_hline(yintercept = mean(iowa_economic$median_household_income), 
             linetype = "dashed", color = "red", linewidth = 1) +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(
    title = "Median Household Income by Iowa City",
    subtitle = "Red dashed line shows state average",
    x = NULL,
    y = "Median Household Income",
    fill = "Income Category"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
ggsave(here("outputs/iowa_income_comparison.png"), p1, width = 10, height = 8, dpi = 300)

# 2. Unemployment vs Labor Force Participation scatter
p2 <- iowa_economic %>%
  ggplot(aes(x = unemployment_rate, y = labor_force_participation, 
             size = population, color = income_category)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = city), hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  scale_size_continuous(labels = comma, range = c(3, 15)) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  labs(
    title = "Employment Metrics Across Iowa Cities",
    subtitle = "Point size indicates population",
    x = "Unemployment Rate (%)",
    y = "Labor Force Participation Rate (%)",
    size = "Population",
    color = "Income Category"
  ) +
  theme_minimal()

print(p2)
ggsave(here("outputs/iowa_employment_scatter.png"), p2, width = 12, height = 8, dpi = 300)

# 3. Income vs Poverty relationship
p3 <- iowa_economic %>%
  ggplot(aes(x = median_household_income, y = poverty_rate)) +
  geom_point(aes(size = population, color = major_industry), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray", linetype = "dashed") +
  geom_text(aes(label = city), hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  scale_x_continuous(labels = dollar) +
  scale_size_continuous(labels = comma, range = c(3, 12), guide = "none") +
  labs(
    title = "Income vs Poverty Rate in Iowa Cities",
    subtitle = "Negative correlation expected; Ames is an outlier (student population)",
    x = "Median Household Income",
    y = "Poverty Rate (%)",
    color = "Major Industry"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p3)
ggsave(here("outputs/iowa_income_poverty.png"), p3, width = 12, height = 8, dpi = 300)

# 4. Industry distribution
p4 <- iowa_economic %>%
  count(major_industry) %>%
  ggplot(aes(x = reorder(major_industry, n), y = n, fill = major_industry)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Primary Industries in Iowa Cities",
    x = NULL,
    y = "Number of Cities"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p4)
ggsave(here("outputs/iowa_industries.png"), p4, width = 10, height = 6, dpi = 300)

# =============================================================================
# Economic Health Index
# =============================================================================

cat("\n=== Economic Health Index ===\n\n")

# Create composite economic health score
iowa_economic <- iowa_economic %>%
  mutate(
    # Normalize each metric (0-100 scale)
    income_score = (median_household_income - min(median_household_income)) / 
                   (max(median_household_income) - min(median_household_income)) * 100,
    poverty_score = (1 - (poverty_rate - min(poverty_rate)) / 
                    (max(poverty_rate) - min(poverty_rate))) * 100,
    unemployment_score = (1 - (unemployment_rate - min(unemployment_rate)) / 
                         (max(unemployment_rate) - min(unemployment_rate))) * 100,
    participation_score = (labor_force_participation - min(labor_force_participation)) / 
                          (max(labor_force_participation) - min(labor_force_participation)) * 100,
    
    # Weighted composite score
    economic_health_index = (
      income_score * 0.30 +
      poverty_score * 0.25 +
      unemployment_score * 0.25 +
      participation_score * 0.20
    )
  )

cat("Top 10 Cities by Economic Health Index:\n")
iowa_economic %>%
  arrange(desc(economic_health_index)) %>%
  select(city, economic_health_index, median_household_income, 
         poverty_rate, unemployment_rate) %>%
  head(10) %>%
  print()

# =============================================================================
# Save Processed Data
# =============================================================================

write_csv(iowa_economic, here("data/processed/iowa_economic_analyzed.csv"))
cat("\n✓ Economic analysis data saved to: data/processed/iowa_economic_analyzed.csv\n")

cat("\n=== Economic Analysis Complete ===\n")
cat("Visualizations saved to outputs/ folder\n")
