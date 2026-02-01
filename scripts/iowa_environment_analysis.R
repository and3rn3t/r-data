# Iowa Environment Analysis
# Analyzes environmental quality, sustainability, and green spaces
# =================================================================

library(tidyverse)
library(here)
library(scales)

cat("
╔═══════════════════════════════════════════════════════════════╗
║              IOWA ENVIRONMENT ANALYSIS                        ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

environment <- read_csv(here("data/raw/iowa_environment_data.csv"), 
                        show_col_types = FALSE)

cat("Loaded environment data for", nrow(environment), "cities\n\n")

# =============================================================================
# 2. AIR QUALITY ANALYSIS
# =============================================================================

cat("🌬️ AIR QUALITY ANALYSIS\n")
cat(rep("─", 50), "\n", sep = "")

# Best air quality
best_air <- environment %>%
  arrange(desc(air_quality_days_good)) %>%
  select(city, air_quality_days_good, air_quality_days_unhealthy, pm25_avg) %>%
  head(5)

cat("\nBest Air Quality Cities:\n")
best_air %>%
  mutate(display = sprintf("  %-18s │ Good Days: %3d │ Unhealthy: %2d │ PM2.5: %.1f",
                           city, air_quality_days_good, air_quality_days_unhealthy, pm25_avg)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# Worst air quality
worst_air <- environment %>%
  arrange(air_quality_days_good) %>%
  select(city, air_quality_days_good, air_quality_days_unhealthy, pm25_avg) %>%
  head(5)

cat("\nCities Needing Air Quality Improvement:\n")
worst_air %>%
  mutate(display = sprintf("  %-18s │ Good Days: %3d │ Unhealthy: %2d │ PM2.5: %.1f",
                           city, air_quality_days_good, air_quality_days_unhealthy, pm25_avg)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 3. WATER QUALITY
# =============================================================================

cat("\n\n💧 WATER QUALITY ANALYSIS\n")
cat(rep("─", 50), "\n", sep = "")

water <- environment %>%
  arrange(desc(water_quality_score)) %>%
  select(city, water_quality_score, drinking_water_violations) %>%
  head(5)

cat("\nBest Water Quality:\n")
water %>%
  mutate(display = sprintf("  %-18s │ Score: %2d │ Violations: %d",
                           city, water_quality_score, drinking_water_violations)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 4. SUSTAINABILITY METRICS
# =============================================================================

cat("\n\n♻️ SUSTAINABILITY ANALYSIS\n")
cat(rep("─", 50), "\n", sep = "")

# Most sustainable
sustainable <- environment %>%
  mutate(
    sustainability_score = (recycling_rate_pct * 0.3) + 
                          (renewable_energy_pct * 0.3) +
                          (solar_installations_per_1000 * 5) +
                          (ifelse(composting_available, 10, 0))
  ) %>%
  arrange(desc(sustainability_score)) %>%
  select(city, sustainability_score, recycling_rate_pct, renewable_energy_pct, solar_installations_per_1000) %>%
  head(5)

cat("\nMost Sustainable Cities:\n")
sustainable %>%
  mutate(display = sprintf("  %-18s │ Score: %5.1f │ Recycle: %4.1f%% │ Renewable: %4.1f%% │ Solar: %.1f/1k",
                           city, sustainability_score, recycling_rate_pct, 
                           renewable_energy_pct, solar_installations_per_1000)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 5. GREEN SPACE & NATURE
# =============================================================================

cat("\n\n🌳 GREEN SPACE ANALYSIS\n")
cat(rep("─", 50), "\n", sep = "")

# Greenest cities
green <- environment %>%
  mutate(
    green_score = tree_canopy_pct + (protected_land_pct * 2) + 
                  (wildlife_habitat_acres / 100)
  ) %>%
  arrange(desc(green_score)) %>%
  select(city, green_score, tree_canopy_pct, protected_land_pct, wildlife_habitat_acres) %>%
  head(5)

cat("\nGreenest Cities:\n")
green %>%
  mutate(display = sprintf("  %-18s │ Score: %5.1f │ Trees: %4.1f%% │ Protected: %4.1f%% │ Habitat: %,d ac",
                           city, green_score, tree_canopy_pct, 
                           protected_land_pct, wildlife_habitat_acres)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# Water features
water_features <- environment %>%
  mutate(water_total = river_miles + (lake_acres / 50)) %>%
  arrange(desc(water_total)) %>%
  select(city, river_miles, lake_acres) %>%
  head(5)

cat("\nBest Water Recreation Access:\n")
water_features %>%
  mutate(display = sprintf("  %-18s │ River Miles: %5.1f │ Lake Acres: %,d",
                           city, river_miles, lake_acres)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 6. ENVIRONMENTAL RISKS
# =============================================================================

cat("\n\n⚠️ ENVIRONMENTAL RISK ASSESSMENT\n")
cat(rep("─", 50), "\n", sep = "")

# Highest risk
risk <- environment %>%
  mutate(
    risk_score = (hazardous_waste_sites * 5) + 
                 (brownfield_sites * 2) + 
                 (flood_risk_properties_pct * 2) +
                 (drinking_water_violations * 10)
  ) %>%
  arrange(desc(risk_score)) %>%
  select(city, risk_score, hazardous_waste_sites, brownfield_sites, 
         flood_risk_properties_pct) %>%
  head(5)

cat("\nHighest Environmental Risk:\n")
risk %>%
  mutate(display = sprintf("  %-18s │ Risk: %5.1f │ Hazardous: %d │ Brownfield: %2d │ Flood: %4.1f%%",
                           city, risk_score, hazardous_waste_sites, 
                           brownfield_sites, flood_risk_properties_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 7. SAVE ANALYZED DATA
# =============================================================================

environment_analyzed <- environment %>%
  mutate(
    sustainability_score = (recycling_rate_pct * 0.3) + 
                          (renewable_energy_pct * 0.3) +
                          (solar_installations_per_1000 * 5) +
                          (ifelse(composting_available, 10, 0)),
    green_score = tree_canopy_pct + (protected_land_pct * 2) + 
                  (wildlife_habitat_acres / 100),
    risk_score = (hazardous_waste_sites * 5) + 
                 (brownfield_sites * 2) + 
                 (flood_risk_properties_pct * 2) +
                 (drinking_water_violations * 10),
    overall_environment_score = (air_quality_days_good / 3.65) + 
                                water_quality_score + 
                                sustainability_score - 
                                (risk_score / 2),
    environment_grade = case_when(
      overall_environment_score >= 150 ~ "A",
      overall_environment_score >= 130 ~ "B",
      overall_environment_score >= 110 ~ "C",
      TRUE ~ "D"
    )
  )

write_csv(environment_analyzed, 
          here("data/processed/iowa_environment_analyzed.csv"))

cat("\n\n✓ Saved iowa_environment_analyzed.csv\n")
