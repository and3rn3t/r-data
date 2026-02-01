# Iowa Cities Analysis Script
# Comprehensive analysis of city data for the state of Iowa

# Load required packages and utilities
library(tidyverse)
library(here)
library(scales)
library(sf)

source(here("scripts/00_setup.R"))
source(here("scripts/utils.R"))

# =============================================================================
# Load Iowa City Data
# =============================================================================

# Try to load processed data first, then raw data
if (file.exists(here("data/processed/iowa_cities_clean.csv"))) {
  iowa_cities <- read_csv(here("data/processed/iowa_cities_clean.csv"))
  cat("Loaded cleaned Iowa cities data\n")
} else if (file.exists(here("data/raw/iowa_major_cities.csv"))) {
  iowa_cities <- read_csv(here("data/raw/iowa_major_cities.csv"))
  cat("Loaded major Iowa cities data (raw)\n")
} else {
  stop("No Iowa cities data found. Please run iowa_cities_import.R first.")
}

cat("Dataset contains", nrow(iowa_cities), "cities\n\n")

# =============================================================================
# Population Analysis
# =============================================================================

cat("=== Population Analysis ===\n\n")

# Summary statistics
pop_summary <- iowa_cities %>%
  summarise(
    total_cities = n(),
    total_population = sum(population_2020, na.rm = TRUE),
    mean_population = mean(population_2020, na.rm = TRUE),
    median_population = median(population_2020, na.rm = TRUE),
    min_population = min(population_2020, na.rm = TRUE),
    max_population = max(population_2020, na.rm = TRUE),
    std_dev = sd(population_2020, na.rm = TRUE)
  )

print(pop_summary)

# Top 10 cities by population
cat("\nTop 10 Iowa Cities by Population (2020):\n")
top_cities <- iowa_cities %>%
  arrange(desc(population_2020)) %>%
  head(10) %>%
  select(city, county, population_2020)

print(top_cities)

# =============================================================================
# Regional Analysis
# =============================================================================

cat("\n=== Regional Analysis ===\n\n")

if ("region" %in% names(iowa_cities)) {
  regional_summary <- iowa_cities %>%
    group_by(region) %>%
    summarise(
      num_cities = n(),
      total_population = sum(population_2020, na.rm = TRUE),
      avg_population = mean(population_2020, na.rm = TRUE),
      largest_city = city[which.max(population_2020)]
    ) %>%
    arrange(desc(total_population))
  
  cat("Population by Region:\n")
  print(regional_summary)
}

# County Analysis
cat("\n=== County Analysis ===\n\n")

if ("county" %in% names(iowa_cities)) {
  county_summary <- iowa_cities %>%
    group_by(county) %>%
    summarise(
      num_cities = n(),
      total_population = sum(population_2020, na.rm = TRUE),
      cities = paste(city, collapse = ", ")
    ) %>%
    arrange(desc(total_population)) %>%
    head(10)
  
  cat("Top 10 Counties by Urban Population:\n")
  print(county_summary)
}

# =============================================================================
# Visualizations
# =============================================================================

cat("\n=== Creating Visualizations ===\n\n")

# 1. Bar chart of top cities
p1 <- iowa_cities %>%
  arrange(desc(population_2020)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(city, population_2020), y = population_2020)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = comma(population_2020)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 15 Iowa Cities by Population",
    subtitle = "2020 Census Data",
    x = NULL,
    y = "Population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10)
  )

print(p1)
ggsave(here("outputs/iowa_cities_population_bar.png"), p1, 
       width = 10, height = 8, dpi = 300)

# 2. Population distribution histogram
p2 <- iowa_cities %>%
  ggplot(aes(x = population_2020)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  scale_x_log10(labels = comma) +
  labs(
    title = "Distribution of Iowa City Populations",
    subtitle = "Log scale - Most cities are small",
    x = "Population (log scale)",
    y = "Number of Cities"
  ) +
  theme_minimal()

print(p2)
ggsave(here("outputs/iowa_cities_distribution.png"), p2,
       width = 10, height = 6, dpi = 300)

# 3. Regional population comparison (if region data available)
if ("region" %in% names(iowa_cities)) {
  p3 <- iowa_cities %>%
    group_by(region) %>%
    summarise(total_pop = sum(population_2020, na.rm = TRUE)) %>%
    ggplot(aes(x = reorder(region, total_pop), y = total_pop, fill = region)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Iowa Urban Population by Region",
      x = NULL,
      y = "Total Urban Population"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p3)
  ggsave(here("outputs/iowa_regional_population.png"), p3,
         width = 10, height = 6, dpi = 300)
}

# 4. Geographic scatter plot (if lat/long available)
if (all(c("latitude", "longitude") %in% names(iowa_cities))) {
  p4 <- iowa_cities %>%
    ggplot(aes(x = longitude, y = latitude, size = population_2020, color = region)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(labels = comma, range = c(2, 15)) +
    labs(
      title = "Iowa Cities Geographic Distribution",
      subtitle = "Point size indicates population",
      x = "Longitude",
      y = "Latitude",
      size = "Population",
      color = "Region"
    ) +
    theme_minimal() +
    coord_fixed(ratio = 1.3)
  
  print(p4)
  ggsave(here("outputs/iowa_cities_map.png"), p4,
         width = 12, height = 8, dpi = 300)
}

# =============================================================================
# City Classification
# =============================================================================

cat("\n=== City Size Classification ===\n\n")

iowa_cities <- iowa_cities %>%
  mutate(
    size_category = case_when(
      population_2020 >= 100000 ~ "Large City (100k+)",
      population_2020 >= 50000  ~ "Medium City (50k-100k)",
      population_2020 >= 25000  ~ "Small City (25k-50k)",
      population_2020 >= 10000  ~ "Town (10k-25k)",
      population_2020 >= 5000   ~ "Small Town (5k-10k)",
      TRUE                      ~ "Village (<5k)"
    )
  )

size_summary <- iowa_cities %>%
  count(size_category) %>%
  arrange(desc(n))

cat("Cities by Size Category:\n")
print(size_summary)

# =============================================================================
# Save Processed Data
# =============================================================================

write_csv(iowa_cities, here("data/processed/iowa_cities_analyzed.csv"))
cat("\nAnalyzed data saved to: data/processed/iowa_cities_analyzed.csv\n")

cat("\n=== Analysis Complete ===\n")
cat("Visualizations saved to the outputs/ folder\n")
