# Iowa Demographics Analysis
# Analyzes population composition, age structure, and diversity
# ==============================================================

library(tidyverse)
library(here)
library(scales)

source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

start_time <- Sys.time()

cat("
╔═══════════════════════════════════════════════════════════════╗
║              IOWA DEMOGRAPHICS ANALYSIS                       ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

demographics <- read_csv(here("data/raw/iowa_demographics_data.csv"), 
                         show_col_types = FALSE)

cat("Loaded demographics for", nrow(demographics), "cities\n\n")

# =============================================================================
# 2. AGE STRUCTURE ANALYSIS
# =============================================================================

cat("📊 AGE STRUCTURE ANALYSIS\n")
cat(rep("─", 50), "\n", sep = "")

# Youngest cities (highest pct under 34)
youngest <- demographics %>%
  mutate(young_pct = pct_under_18 + pct_18_to_34) %>%
  arrange(desc(young_pct)) %>%
  select(city, median_age, young_pct, pct_under_18, pct_18_to_34) %>%
  head(5)

cat("\nYoungest Cities (% under 35):\n")
youngest %>%
  mutate(display = sprintf("  %-18s │ Median Age: %4.1f │ Under 35: %4.1f%%",
                           city, median_age, young_pct)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# Oldest cities (highest pct 65+)
oldest <- demographics %>%
  arrange(desc(pct_65_plus)) %>%
  select(city, median_age, pct_65_plus, pct_55_to_64) %>%
  head(5)

cat("\nOldest Cities (% 65+):\n")
oldest %>%
  mutate(display = sprintf("  %-18s │ Median Age: %4.1f │ 65+: %4.1f%%",
                           city, median_age, pct_65_plus)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 3. DIVERSITY ANALYSIS
# =============================================================================

cat("\n\n📊 DIVERSITY ANALYSIS\n")
cat(rep("─", 50), "\n", sep = "")

# Most diverse cities
diverse <- demographics %>%
  mutate(
    # Simple diversity index (1 - sum of squared proportions)
    diversity_calc = 1 - ((pct_white/100)^2 + (pct_black/100)^2 + 
                          (pct_hispanic/100)^2 + (pct_asian/100)^2 + 
                          (pct_two_or_more/100)^2)
  ) %>%
  arrange(desc(diversity_calc)) %>%
  select(city, diversity_calc, pct_white, pct_hispanic, pct_black, pct_asian) %>%
  head(5)

cat("\nMost Diverse Cities:\n")
diverse %>%
  mutate(display = sprintf("  %-18s │ Index: %.3f │ White: %4.1f%% Hispanic: %4.1f%% Black: %4.1f%%",
                           city, diversity_calc, pct_white, pct_hispanic, pct_black)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# Foreign born population
foreign_born <- demographics %>%
  arrange(desc(pct_foreign_born)) %>%
  select(city, pct_foreign_born, pct_speak_english_only) %>%
  head(5)

cat("\nHighest Foreign-Born Population:\n")
foreign_born %>%
  mutate(display = sprintf("  %-18s │ Foreign Born: %4.1f%% │ English Only: %4.1f%%",
                           city, pct_foreign_born, pct_speak_english_only)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 4. HOUSEHOLD STRUCTURE
# =============================================================================

cat("\n\n📊 HOUSEHOLD STRUCTURE\n")
cat(rep("─", 50), "\n", sep = "")

# Family-oriented cities
family <- demographics %>%
  arrange(desc(pct_family_households)) %>%
  select(city, pct_family_households, pct_married_couple, avg_household_size) %>%
  head(5)

cat("\nMost Family-Oriented Cities:\n")
family %>%
  mutate(display = sprintf("  %-18s │ Families: %4.1f%% │ Married: %4.1f%% │ HH Size: %.2f",
                           city, pct_family_households, pct_married_couple, avg_household_size)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 5. MIGRATION PATTERNS
# =============================================================================

cat("\n\n📊 MIGRATION PATTERNS\n")
cat(rep("─", 50), "\n", sep = "")

# Growing cities (positive migration)
growing <- demographics %>%
  arrange(desc(net_migration_rate)) %>%
  select(city, net_migration_rate, birth_rate_per_1000, death_rate_per_1000) %>%
  head(5)

cat("\nFastest Growing (Net Migration):\n")
growing %>%
  mutate(display = sprintf("  %-18s │ Migration: %+5.1f │ Births: %4.1f │ Deaths: %4.1f",
                           city, net_migration_rate, birth_rate_per_1000, death_rate_per_1000)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# Declining cities
declining <- demographics %>%
  arrange(net_migration_rate) %>%
  select(city, net_migration_rate, birth_rate_per_1000, death_rate_per_1000) %>%
  head(5)

cat("\nSlowest Growing/Declining:\n")
declining %>%
  mutate(display = sprintf("  %-18s │ Migration: %+5.1f │ Births: %4.1f │ Deaths: %4.1f",
                           city, net_migration_rate, birth_rate_per_1000, death_rate_per_1000)) %>%
  pull(display) %>%
  walk(~cat(., "\n"))

# =============================================================================
# 6. SAVE ANALYZED DATA
# =============================================================================

# Add calculated fields
demographics_analyzed <- demographics %>%
  mutate(
    young_population_pct = pct_under_18 + pct_18_to_34,
    senior_population_pct = pct_55_to_64 + pct_65_plus,
    diversity_index = 1 - ((pct_white/100)^2 + (pct_black/100)^2 + 
                           (pct_hispanic/100)^2 + (pct_asian/100)^2 + 
                           (pct_two_or_more/100)^2),
    natural_growth_rate = birth_rate_per_1000 - death_rate_per_1000,
    age_category = case_when(
      median_age < 30 ~ "Young",
      median_age < 38 ~ "Middle",
      TRUE ~ "Mature"
    )
  )

write_csv(demographics_analyzed, 
          here("data/processed/iowa_demographics_analyzed.csv"))

cat("\n\n✓ Saved iowa_demographics_analyzed.csv\n")
