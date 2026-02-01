# Iowa Cities Predictive Analysis
# Forecasting population, home values, and trends
# =================================================

library(tidyverse)
library(here)

source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

cat("
╔═══════════════════════════════════════════════════════════════╗
║              IOWA CITIES PREDICTIVE ANALYSIS                  ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# LOAD DATA
# =============================================================================

historical <- read_csv(here("data/raw/iowa_historical_data.csv"), show_col_types = FALSE)
major_cities <- read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)

cat("Loaded historical data for", nrow(historical), "cities\n\n")

# =============================================================================
# POPULATION PROJECTIONS
# =============================================================================

cat("📈 POPULATION PROJECTIONS TO 2030\n")
cat(rep("─", 60), "\n", sep = "")

# Calculate compound annual growth rate (CAGR) for each city
population_projections <- historical %>%
  mutate(
    # Calculate 10-year CAGR (2010-2020)
    cagr_10yr = ((population_2020 / population_2010)^(1/10) - 1) * 100,
    
    # Calculate 20-year CAGR (2000-2020)
    cagr_20yr = ((population_2020 / population_2000)^(1/20) - 1) * 100,
    
    # Use weighted average of recent and long-term trends
    projected_cagr = (cagr_10yr * 0.7 + cagr_20yr * 0.3),
    
    # Project to 2025 and 2030
    population_2025 = round(population_2020 * (1 + projected_cagr/100)^5),
    population_2030 = round(population_2020 * (1 + projected_cagr/100)^10),
    
    # Calculate change
    change_2020_2030 = population_2030 - population_2020,
    pct_change_2030 = round((population_2030 / population_2020 - 1) * 100, 1)
  ) %>%
  arrange(desc(pct_change_2030))

cat("\nProjected Growth by 2030:\n\n")
population_projections %>%
  select(city, population_2020, population_2025, population_2030, pct_change_2030) %>%
  mutate(
    display = sprintf("  %-18s │ 2020: %,7d │ 2025: %,7d │ 2030: %,7d │ %+5.1f%%",
                      city, population_2020, population_2025, population_2030, pct_change_2030)
  ) %>%
  pull(display) %>%
  head(10) %>%
  walk(~cat(., "\n"))

cat("\n  ...\n\nDeclining populations:\n")
population_projections %>%
  filter(pct_change_2030 < 0) %>%
  select(city, population_2020, population_2030, pct_change_2030) %>%
  mutate(
    display = sprintf("  %-18s │ 2020: %,7d │ 2030: %,7d │ %+5.1f%%",
                      city, population_2020, population_2030, pct_change_2030)
  ) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# HOME VALUE PROJECTIONS
# =============================================================================

cat("\n\n🏠 HOME VALUE PROJECTIONS TO 2030\n")
cat(rep("─", 60), "\n", sep = "")

home_projections <- historical %>%
  mutate(
    # Calculate 10-year CAGR for home values (2010-2020)
    home_cagr = ((median_home_value_2020 / median_home_value_2010)^(1/10) - 1) * 100,
    
    # Apply some mean reversion (extreme growth rates tend to moderate)
    adjusted_cagr = ifelse(home_cagr > 5, home_cagr * 0.8, 
                          ifelse(home_cagr < 2, home_cagr * 1.2, home_cagr)),
    
    # Project values
    home_value_2025 = round(median_home_value_2020 * (1 + adjusted_cagr/100)^5, -2),
    home_value_2030 = round(median_home_value_2020 * (1 + adjusted_cagr/100)^10, -2),
    
    appreciation_2030 = round((home_value_2030 / median_home_value_2020 - 1) * 100, 1)
  ) %>%
  arrange(desc(appreciation_2030))

cat("\nProjected Highest Appreciation:\n\n")
home_projections %>%
  head(10) %>%
  mutate(
    display = sprintf("  %-18s │ 2020: $%,7d │ 2030: $%,7d │ +%.1f%%",
                      city, median_home_value_2020, home_value_2030, appreciation_2030)
  ) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

cat("\nLowest Appreciation (Best Affordability):\n\n")
home_projections %>%
  tail(5) %>%
  arrange(appreciation_2030) %>%
  mutate(
    display = sprintf("  %-18s │ 2020: $%,7d │ 2030: $%,7d │ +%.1f%%",
                      city, median_home_value_2020, home_value_2030, appreciation_2030)
  ) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# INCOME PROJECTIONS
# =============================================================================

cat("\n\n💰 INCOME PROJECTIONS TO 2030\n")
cat(rep("─", 60), "\n", sep = "")

income_projections <- historical %>%
  mutate(
    income_cagr = ((median_income_2020 / median_income_2010)^(1/10) - 1) * 100,
    income_2025 = round(median_income_2020 * (1 + income_cagr/100)^5, -2),
    income_2030 = round(median_income_2020 * (1 + income_cagr/100)^10, -2),
    income_growth_pct = round((income_2030 / median_income_2020 - 1) * 100, 1)
  ) %>%
  arrange(desc(income_growth_pct))

cat("\nProjected Income Growth:\n\n")
income_projections %>%
  head(10) %>%
  mutate(
    display = sprintf("  %-18s │ 2020: $%,6d │ 2030: $%,6d │ +%.1f%%",
                      city, median_income_2020, income_2030, income_growth_pct)
  ) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# AFFORDABILITY INDEX PROJECTIONS
# =============================================================================

cat("\n\n📊 AFFORDABILITY INDEX PROJECTIONS (Income/Home Value Ratio)\n")
cat(rep("─", 60), "\n", sep = "")

affordability <- historical %>%
  left_join(home_projections %>% select(city, home_value_2030), by = "city") %>%
  left_join(income_projections %>% select(city, income_2030), by = "city") %>%
  mutate(
    # Affordability = Years of income to buy home
    affordability_2020 = round(median_home_value_2020 / median_income_2020, 2),
    affordability_2030 = round(home_value_2030 / income_2030, 2),
    affordability_change = round(affordability_2030 - affordability_2020, 2)
  ) %>%
  arrange(affordability_2030)

cat("\nMost Affordable Cities in 2030 (years of income to buy home):\n\n")
affordability %>%
  head(10) %>%
  mutate(
    display = sprintf("  %-18s │ 2020: %.1fx income │ 2030: %.1fx income │ %+.1f change",
                      city, affordability_2020, affordability_2030, affordability_change)
  ) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

cat("\nLeast Affordable Cities in 2030:\n\n")
affordability %>%
  tail(5) %>%
  arrange(desc(affordability_2030)) %>%
  mutate(
    display = sprintf("  %-18s │ 2020: %.1fx income │ 2030: %.1fx income │ %+.1f change",
                      city, affordability_2020, affordability_2030, affordability_change)
  ) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# GROWTH TRAJECTORY PREDICTIONS
# =============================================================================

cat("\n\n🚀 GROWTH TRAJECTORY PREDICTIONS\n")
cat(rep("─", 60), "\n", sep = "")

trajectories <- population_projections %>%
  left_join(home_projections %>% select(city, appreciation_2030), by = "city") %>%
  left_join(income_projections %>% select(city, income_growth_pct), by = "city") %>%
  mutate(
    # Composite growth score
    growth_score = (pct_change_2030 * 0.4 + appreciation_2030 * 0.3 + income_growth_pct * 0.3),
    
    trajectory = case_when(
      growth_score > 40 ~ "🚀 Boom Town",
      growth_score > 25 ~ "📈 High Growth",
      growth_score > 15 ~ "➡️ Steady Growth",
      growth_score > 5 ~ "🔄 Stable",
      growth_score > -5 ~ "📉 Slow Decline",
      TRUE ~ "⚠️ Significant Decline"
    )
  ) %>%
  arrange(desc(growth_score))

cat("\nPredicted City Trajectories to 2030:\n\n")
trajectories %>%
  select(city, trajectory, growth_score, pct_change_2030, appreciation_2030) %>%
  mutate(
    display = sprintf("  %s %-15s │ Score: %+5.1f │ Pop: %+5.1f%% │ Homes: %+5.1f%%",
                      trajectory, city, growth_score, pct_change_2030, appreciation_2030)
  ) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# SAVE PROJECTIONS
# =============================================================================

cat("\n\nSaving projections...\n")

# Combine all projections
all_projections <- population_projections %>%
  select(city, population_2020, population_2025, population_2030, 
         population_cagr = projected_cagr, population_change_pct = pct_change_2030) %>%
  left_join(
    home_projections %>% select(city, median_home_value_2020, home_value_2025, 
                                 home_value_2030, home_cagr = adjusted_cagr, 
                                 home_appreciation_pct = appreciation_2030),
    by = "city"
  ) %>%
  left_join(
    income_projections %>% select(city, median_income_2020, income_2025, income_2030,
                                   income_cagr, income_growth_pct),
    by = "city"
  ) %>%
  left_join(
    affordability %>% select(city, affordability_2020, affordability_2030),
    by = "city"
  ) %>%
  left_join(
    trajectories %>% select(city, growth_score, trajectory),
    by = "city"
  )

write_csv(all_projections, here("data/processed/iowa_cities_projections_2030.csv"))

cat("\n✓ Saved iowa_cities_projections_2030.csv\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat(rep("═", 60), "\n", sep = "")
cat("PREDICTION SUMMARY TO 2030\n")
cat(rep("═", 60), "\n", sep = "")

# Count trajectories
traj_counts <- table(trajectories$trajectory)
for (t in names(sort(traj_counts, decreasing = TRUE))) {
  cat(sprintf("  %s: %d cities\n", t, traj_counts[t]))
}

cat("\n")
cat(sprintf("  Fastest growing: %-15s (+%.1f%% pop)\n", 
            trajectories$city[1], trajectories$pct_change_2030[1]))
cat(sprintf("  Highest appreciation: %-15s (+%.1f%% home value)\n",
            home_projections$city[1], home_projections$appreciation_2030[1]))
cat(sprintf("  Most affordable 2030: %-15s (%.1fx income)\n",
            affordability$city[1], affordability$affordability_2030[1]))

cat(rep("═", 60), "\n", sep = "")
