# Iowa Cities Climate Analysis
# Analyzes weather patterns, climate comfort, and severe weather risk
# ==========================================================================

# Load required packages and utilities
library(tidyverse)
library(here)
library(scales)

# Load project utilities and constants
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

start_time <- Sys.time()

# =============================================================================
# Climate Data Import
# =============================================================================

cat("=== Iowa Climate Analysis ===\n\n")

iowa_climate <- read_csv(here("data/raw/iowa_climate_data.csv"), show_col_types = FALSE)

cat("Loaded climate data for", nrow(iowa_climate), "cities\n\n")

# =============================================================================
# Data Validation
# =============================================================================

cat("--- Data Validation ---\n")
cat("Columns:", paste(names(iowa_climate), collapse = ", "), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(iowa_climate)))
cat("\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("--- Summary Statistics ---\n\n")

climate_summary <- iowa_climate %>%
  summarise(
    avg_annual_temp = mean(avg_annual_temp_f, na.rm = TRUE),
    avg_sunny_days = mean(sunny_days, na.rm = TRUE),
    avg_precipitation = mean(annual_precipitation_in, na.rm = TRUE),
    avg_severe_events = mean(severe_weather_events, na.rm = TRUE),
    avg_comfort_index = mean(climate_comfort_index, na.rm = TRUE)
  )

cat("Average annual temperature:", round(climate_summary$avg_annual_temp, 1), "°F\n")
cat("Average sunny days per year:", round(climate_summary$avg_sunny_days, 0), "\n")
cat("Average annual precipitation:", round(climate_summary$avg_precipitation, 1), "inches\n")
cat("Average severe weather events:", round(climate_summary$avg_severe_events, 1), "per year\n")
cat("Average climate comfort index:", round(climate_summary$avg_comfort_index, 1), "\n\n")

# =============================================================================
# Climate Score Calculation
# =============================================================================

cat("--- Climate Score Calculation ---\n\n")

iowa_climate_scored <- iowa_climate %>%
  mutate(
    # Normalize each metric (0-100 scale)
    sunny_score = normalize(sunny_days),
    temp_score = normalize(climate_comfort_index),  # Pre-calculated comfort
    severe_weather_score = normalize(severe_weather_events, reverse = TRUE),  # Fewer is better
    tornado_score = normalize(tornado_risk_index, reverse = TRUE),  # Lower risk is better
    air_quality_score = normalize(avg_air_quality_good_days),
    
    # Composite climate score (weighted average)
    climate_score = (
      sunny_score * 0.25 +
      temp_score * 0.25 +
      severe_weather_score * 0.20 +
      tornado_score * 0.15 +
      air_quality_score * 0.15
    )
  ) %>%
  arrange(desc(climate_score)) %>%
  mutate(climate_rank = row_number())

# Display top 10 best climate cities
cat("Top 10 Cities by Climate Score:\n")
iowa_climate_scored %>%
  select(city, climate_score, climate_rank, sunny_days, 
         climate_comfort_index, severe_weather_events) %>%
  head(10) %>%
  print()

cat("\n")

# =============================================================================
# Visualizations
# =============================================================================

cat("--- Creating Visualizations ---\n\n")

# Climate Score Distribution
p_climate_scores <- ggplot(iowa_climate_scored, 
                           aes(x = reorder(city, climate_score), y = climate_score)) +
  geom_col(fill = "#F4A261") +
  coord_flip() +
  labs(
    title = "Iowa Cities Climate Score",
    subtitle = "Composite score based on weather patterns, comfort, and severe weather risk",
    x = NULL,
    y = "Climate Score (0-100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(here("outputs/figures/iowa_climate_scores.png"), p_climate_scores, 
       width = 10, height = 8, dpi = 300)
cat("Saved: iowa_climate_scores.png\n")

# Temperature Range by City
p_temp_range <- iowa_climate_scored %>%
  select(city, avg_summer_high, avg_winter_low) %>%
  pivot_longer(cols = c(avg_summer_high, avg_winter_low), 
               names_to = "season", values_to = "temp") %>%
  mutate(season = case_when(
    season == "avg_summer_high" ~ "Summer High",
    season == "avg_winter_low" ~ "Winter Low"
  )) %>%
  ggplot(aes(x = reorder(city, temp), y = temp, fill = season)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("Summer High" = "#E76F51", "Winter Low" = "#264653")) +
  labs(
    title = "Temperature Ranges Across Iowa Cities",
    x = NULL,
    y = "Temperature (°F)",
    fill = "Season"
  ) +
  theme_minimal()

ggsave(here("outputs/figures/iowa_temperature_ranges.png"), p_temp_range, 
       width = 10, height = 8, dpi = 300)
cat("Saved: iowa_temperature_ranges.png\n")

# Severe Weather vs Sunny Days
p_weather_risk <- ggplot(iowa_climate_scored, 
                          aes(x = severe_weather_events, 
                              y = sunny_days,
                              size = population,
                              color = climate_score)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = city), size = 2.5, vjust = -1, check_overlap = TRUE) +
  scale_color_viridis_c(option = "viridis") +
  scale_size_continuous(range = c(3, 12), labels = comma) +
  labs(
    title = "Severe Weather Events vs. Sunny Days by Iowa City",
    x = "Severe Weather Events (per year)",
    y = "Sunny Days (per year)",
    color = "Climate\nScore",
    size = "Population"
  ) +
  theme_minimal()

ggsave(here("outputs/figures/iowa_weather_risk.png"), p_weather_risk, 
       width = 10, height = 7, dpi = 300)
cat("Saved: iowa_weather_risk.png\n")

# =============================================================================
# Export Results
# =============================================================================

cat("\n--- Exporting Results ---\n\n")

write_csv(iowa_climate_scored, here("data/processed/iowa_climate_analyzed.csv"))
cat("Saved: data/processed/iowa_climate_analyzed.csv\n")

# =============================================================================
# Summary
# =============================================================================

end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("\n=== Analysis Complete ===\n")
cat("Duration:", duration, "seconds\n")
cat("Cities analyzed:", nrow(iowa_climate_scored), "\n")
cat("Best climate city:", iowa_climate_scored$city[1], 
    "(score:", round(iowa_climate_scored$climate_score[1], 1), ")\n")
