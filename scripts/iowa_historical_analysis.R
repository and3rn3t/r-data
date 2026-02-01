# Iowa Historical Trends Analysis
# Analyzes 20+ year trends in population, economy, and quality of life
# =====================================================================

library(tidyverse)
library(here)
library(scales)

source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

start_time <- Sys.time()

cat("
╔═══════════════════════════════════════════════════════════════╗
║              IOWA HISTORICAL TRENDS ANALYSIS                  ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

historical <- read_csv(here("data/raw/iowa_historical_data.csv"), 
                       show_col_types = FALSE)

cat("Loaded historical data for", nrow(historical), "cities\n")
cat("Spanning 1980-2020 (40 years of data)\n\n")

# =============================================================================
# 2. POPULATION TRENDS
# =============================================================================

cat("📈 POPULATION GROWTH TRENDS\n")
cat(rep("─", 50), "\n", sep = "")

# Fastest growing (10 year)
fast_growth <- historical %>%
  arrange(desc(population_growth_10yr_pct)) %>%
  select(city, population_2020, population_2010, population_growth_10yr_pct, 
         population_growth_20yr_pct) %>%
  head(5)

cat("\nFastest Growing Cities (10-Year):\n")
fast_growth %>%
  mutate(display = sprintf("  %-18s │ 2020: %,6d │ 10yr: %+5.1f%% │ 20yr: %+5.1f%%",
                           city, population_2020, population_growth_10yr_pct,
                           population_growth_20yr_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# Declining cities
declining <- historical %>%
  arrange(population_growth_10yr_pct) %>%
  select(city, population_2020, population_2010, population_growth_10yr_pct,
         population_growth_20yr_pct) %>%
  head(5)

cat("\nDeclining Cities (10-Year):\n")
declining %>%
  mutate(display = sprintf("  %-18s │ 2020: %,6d │ 10yr: %+5.1f%% │ 20yr: %+5.1f%%",
                           city, population_2020, population_growth_10yr_pct,
                           population_growth_20yr_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# Long-term population change (1980-2020)
cat("\n\n📊 40-YEAR POPULATION CHANGE (1980-2020)\n")
cat(rep("─", 50), "\n", sep = "")

long_term <- historical %>%
  mutate(
    growth_40yr = (population_2020 - population_1980) / population_1980 * 100,
    growth_absolute = population_2020 - population_1980
  ) %>%
  arrange(desc(growth_40yr)) %>%
  select(city, population_1980, population_2020, growth_40yr, growth_absolute)

cat("\nMost Growth (40 years):\n")
long_term %>%
  head(5) %>%
  mutate(display = sprintf("  %-18s │ 1980: %,6d → 2020: %,6d │ %+6.1f%% (%+,d)",
                           city, population_1980, population_2020, 
                           growth_40yr, growth_absolute)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

cat("\nLeast Growth/Decline (40 years):\n")
long_term %>%
  tail(5) %>%
  arrange(growth_40yr) %>%
  mutate(display = sprintf("  %-18s │ 1980: %,6d → 2020: %,6d │ %+6.1f%% (%+,d)",
                           city, population_1980, population_2020, 
                           growth_40yr, growth_absolute)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 3. HOME VALUE TRENDS
# =============================================================================

cat("\n\n🏠 HOME VALUE APPRECIATION (10-Year)\n")
cat(rep("─", 50), "\n", sep = "")

home_values <- historical %>%
  arrange(desc(home_value_growth_10yr_pct)) %>%
  select(city, median_home_value_2010, median_home_value_2020, 
         home_value_growth_10yr_pct)

cat("\nHighest Appreciation:\n")
home_values %>%
  head(5) %>%
  mutate(display = sprintf("  %-18s │ 2010: $%,6d → 2020: $%,6d │ %+5.1f%%",
                           city, median_home_value_2010, median_home_value_2020,
                           home_value_growth_10yr_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

cat("\nLowest Appreciation:\n")
home_values %>%
  tail(5) %>%
  arrange(home_value_growth_10yr_pct) %>%
  mutate(display = sprintf("  %-18s │ 2010: $%,6d → 2020: $%,6d │ %+5.1f%%",
                           city, median_home_value_2010, median_home_value_2020,
                           home_value_growth_10yr_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 4. INCOME TRENDS
# =============================================================================

cat("\n\n💰 INCOME GROWTH (10-Year)\n")
cat(rep("─", 50), "\n", sep = "")

income <- historical %>%
  arrange(desc(income_growth_10yr_pct)) %>%
  select(city, median_income_2010, median_income_2020, income_growth_10yr_pct)

cat("\nHighest Income Growth:\n")
income %>%
  head(5) %>%
  mutate(display = sprintf("  %-18s │ 2010: $%,5d → 2020: $%,5d │ %+5.1f%%",
                           city, median_income_2010, median_income_2020,
                           income_growth_10yr_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 5. CRIME TRENDS
# =============================================================================

cat("\n\n🔒 CRIME RATE CHANGES (10-Year)\n")
cat(rep("─", 50), "\n", sep = "")

crime <- historical %>%
  arrange(crime_change_10yr_pct) %>%
  select(city, crime_rate_2010, crime_rate_2020, crime_change_10yr_pct)

cat("\nMost Improved Safety:\n")
crime %>%
  head(5) %>%
  mutate(display = sprintf("  %-18s │ 2010: %,5d → 2020: %,5d │ %+5.1f%%",
                           city, crime_rate_2010, crime_rate_2020,
                           crime_change_10yr_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 6. EDUCATION TRENDS
# =============================================================================

cat("\n\n🎓 COLLEGE ATTAINMENT CHANGE (10-Year)\n")
cat(rep("─", 50), "\n", sep = "")

education <- historical %>%
  arrange(desc(college_change_10yr_pct)) %>%
  select(city, college_attainment_2010, college_attainment_2020, 
         college_change_10yr_pct)

cat("\nMost Improvement in Education:\n")
education %>%
  head(5) %>%
  mutate(display = sprintf("  %-18s │ 2010: %4.1f%% → 2020: %4.1f%% │ %+5.1f%%",
                           city, college_attainment_2010, college_attainment_2020,
                           college_change_10yr_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 7. UNEMPLOYMENT TRENDS
# =============================================================================

cat("\n\n📉 UNEMPLOYMENT TRENDS (2010-2020)\n")
cat(rep("─", 50), "\n", sep = "")

unemployment <- historical %>%
  mutate(unemployment_change = unemployment_2020 - unemployment_2010) %>%
  arrange(unemployment_change) %>%
  select(city, unemployment_2010, unemployment_2015, unemployment_2020, 
         unemployment_change)

cat("\nBest Recovery from 2010 Recession:\n")
unemployment %>%
  head(5) %>%
  mutate(display = sprintf("  %-18s │ 2010: %4.1f%% → 2015: %4.1f%% → 2020: %4.1f%% │ Change: %+4.1f%%",
                           city, unemployment_2010, unemployment_2015, 
                           unemployment_2020, unemployment_change)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 8. TRAJECTORY ANALYSIS
# =============================================================================

cat("\n\n🚀 CITY TRAJECTORY CLASSIFICATION\n")
cat(rep("─", 50), "\n", sep = "")

trajectory <- historical %>%
  mutate(
    # Composite improvement score
    improvement_score = (population_growth_10yr_pct / 5) +
                       (income_growth_10yr_pct / 10) +
                       (home_value_growth_10yr_pct / 10) +
                       (-crime_change_10yr_pct / 10) +
                       (college_change_10yr_pct / 5) +
                       (-(unemployment_2020 - unemployment_2010) * 2),
    trajectory = case_when(
      improvement_score > 15 ~ "🚀 Booming",
      improvement_score > 8 ~ "📈 Growing",
      improvement_score > 0 ~ "➡️ Stable",
      improvement_score > -8 ~ "📉 Declining",
      TRUE ~ "⚠️ Struggling"
    )
  ) %>%
  arrange(desc(improvement_score)) %>%
  select(city, improvement_score, trajectory, population_growth_10yr_pct,
         income_growth_10yr_pct)

cat("\nCity Trajectories:\n")
trajectory %>%
  mutate(display = sprintf("  %s %-15s │ Score: %+5.1f │ Pop: %+5.1f%% │ Income: %+5.1f%%",
                           trajectory, city, improvement_score,
                           population_growth_10yr_pct, income_growth_10yr_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 9. SAVE ANALYZED DATA
# =============================================================================

historical_analyzed <- historical %>%
  mutate(
    growth_40yr_pct = (population_2020 - population_1980) / population_1980 * 100,
    unemployment_change = unemployment_2020 - unemployment_2010,
    improvement_score = (population_growth_10yr_pct / 5) +
                       (income_growth_10yr_pct / 10) +
                       (home_value_growth_10yr_pct / 10) +
                       (-crime_change_10yr_pct / 10) +
                       (college_change_10yr_pct / 5) +
                       (-(unemployment_2020 - unemployment_2010) * 2),
    trajectory = case_when(
      improvement_score > 15 ~ "Booming",
      improvement_score > 8 ~ "Growing",
      improvement_score > 0 ~ "Stable",
      improvement_score > -8 ~ "Declining",
      TRUE ~ "Struggling"
    )
  )

write_csv(historical_analyzed, 
          here("data/processed/iowa_historical_analyzed.csv"))

cat("\n\n✓ Saved iowa_historical_analyzed.csv\n")

# Summary statistics
cat("\n")
cat(rep("═", 50), "\n", sep = "")
cat("SUMMARY: 10-Year Trends (2010-2020)\n")
cat(rep("═", 50), "\n", sep = "")
cat(sprintf("  Avg Population Growth:   %+.1f%%\n", mean(historical$population_growth_10yr_pct)))
cat(sprintf("  Avg Home Value Growth:   %+.1f%%\n", mean(historical$home_value_growth_10yr_pct)))
cat(sprintf("  Avg Income Growth:       %+.1f%%\n", mean(historical$income_growth_10yr_pct)))
cat(sprintf("  Avg Crime Change:        %.1f%%\n", mean(historical$crime_change_10yr_pct)))
cat(sprintf("  Avg Education Gain:      %+.1f%%\n", mean(historical$college_change_10yr_pct)))
cat(rep("═", 50), "\n", sep = "")
