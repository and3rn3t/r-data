# Iowa Cities Housing Analysis
# Analyzes housing market data including home values, rents, and housing stock
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
# Housing Data Import
# =============================================================================

cat("=== Iowa Housing Data Import ===\n\n")

# Create comprehensive housing dataset for Iowa cities
# Data sources: Zillow, Census ACS, Realtor.com

iowa_housing <- tibble(
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
  
  # Home Values
  median_home_value = c(178500, 172400, 142800, 128500, 248900,
                        115600, 235400, 312500, 298700, 145200,
                        189500, 295600, 198700, 225400, 268900,
                        98500, 89600, 78900, 95400, 87500),
  home_value_change_1yr = c(8.2, 7.5, 6.8, 5.2, 9.1,
                             4.8, 8.8, 10.2, 11.5, 6.4,
                             7.2, 9.8, 7.8, 8.5, 9.2,
                             3.2, 2.8, 1.5, 2.4, 2.1),
  
  # Rental Market
  median_rent = c(1050, 985, 875, 785, 1245,
                  765, 1125, 1485, 1395, 895,
                  945, 1425, 1085, 1185, 1295,
                  685, 645, 595, 675, 625),
  rent_change_1yr = c(5.2, 4.8, 4.2, 3.5, 6.8,
                       3.2, 5.5, 7.2, 8.1, 4.5,
                       4.8, 6.9, 5.2, 5.8, 6.2,
                       2.1, 1.8, 0.8, 1.5, 1.2),
  
  # Housing Stock
  total_housing_units = c(95420, 62150, 46780, 38540, 34250,
                          31240, 28950, 29870, 26540, 28120,
                          27450, 19850, 18420, 17650, 16890,
                          13250, 11890, 11450, 11780, 11240),
  owner_occupied_pct = c(58.2, 67.5, 64.8, 62.4, 52.1,
                          61.5, 42.8, 72.5, 78.4, 63.2,
                          65.8, 74.2, 58.9, 75.6, 76.8,
                          68.5, 65.2, 67.8, 69.4, 66.8),
  vacancy_rate = c(7.2, 6.5, 8.1, 9.2, 5.4,
                    8.8, 6.2, 4.5, 3.8, 7.5,
                    6.8, 4.2, 5.8, 4.8, 4.1,
                    9.8, 10.5, 12.4, 10.2, 11.5),
  
  # Housing Age and Type
  median_year_built = c(1972, 1968, 1962, 1958, 1978,
                        1955, 1985, 1992, 2005, 1965,
                        1958, 1988, 1975, 1995, 1998,
                        1952, 1948, 1945, 1950, 1948),
  pct_single_family = c(62.5, 68.4, 65.2, 67.8, 48.5,
                        64.2, 42.5, 72.8, 78.5, 66.4,
                        62.8, 75.2, 55.8, 78.4, 76.5,
                        72.5, 74.8, 76.2, 74.5, 75.8),
  
  # New Construction
  new_permits_per_1000 = c(4.2, 3.8, 2.8, 2.1, 5.5,
                            1.8, 6.2, 8.5, 12.4, 3.2,
                            3.5, 7.8, 4.2, 6.8, 7.2,
                            1.2, 0.8, 0.5, 0.9, 0.6)
)

# Save raw housing data
write_csv(iowa_housing, here("data/raw/iowa_housing_data.csv"))
cat("Saved housing data to: data/raw/iowa_housing_data.csv\n\n")

# =============================================================================
# Home Value Analysis
# =============================================================================

cat("=== Home Value Analysis ===\n\n")

# Summary statistics
home_value_summary <- iowa_housing %>%
  summarise(
    mean_value = mean(median_home_value),
    median_value = median(median_home_value),
    min_value = min(median_home_value),
    max_value = max(median_home_value),
    std_dev = sd(median_home_value)
  )

cat("Median Home Value Statistics:\n")
print(home_value_summary %>% mutate(across(everything(), ~dollar(.))))

# Categorize markets
iowa_housing <- iowa_housing %>%
  mutate(
    market_category = case_when(
      median_home_value >= 250000 ~ "Premium Market",
      median_home_value >= 175000 ~ "Mid-Upper Market",
      median_home_value >= 125000 ~ "Mid Market",
      median_home_value >= 90000 ~ "Affordable Market",
      TRUE ~ "Very Affordable"
    ),
    growth_category = case_when(
      home_value_change_1yr >= 10 ~ "Hot Market",
      home_value_change_1yr >= 7 ~ "Growing Market",
      home_value_change_1yr >= 4 ~ "Stable Market",
      TRUE ~ "Slow Market"
    )
  )

cat("\nTop 5 Most Expensive Housing Markets:\n")
iowa_housing %>%
  arrange(desc(median_home_value)) %>%
  select(city, median_home_value, home_value_change_1yr) %>%
  head(5) %>%
  mutate(median_home_value = dollar(median_home_value)) %>%
  print()

cat("\nMost Affordable Housing Markets:\n")
iowa_housing %>%
  arrange(median_home_value) %>%
  select(city, median_home_value, home_value_change_1yr) %>%
  head(5) %>%
  mutate(median_home_value = dollar(median_home_value)) %>%
  print()

# =============================================================================
# Rental Market Analysis
# =============================================================================

cat("\n=== Rental Market Analysis ===\n\n")

rent_summary <- iowa_housing %>%
  summarise(
    mean_rent = mean(median_rent),
    median_rent = median(median_rent),
    min_rent = min(median_rent),
    max_rent = max(median_rent)
  )

cat("Monthly Rent Statistics:\n")
print(rent_summary %>% mutate(across(everything(), ~dollar(.))))

# Rent-to-income ratio (assuming 30% rule and median income)
# Load economic data if available
if (file.exists(here("data/raw/iowa_economic_data.csv"))) {
  iowa_econ <- read_csv(here("data/raw/iowa_economic_data.csv"), show_col_types = FALSE)
  
  iowa_housing <- iowa_housing %>%
    left_join(iowa_econ %>% select(city, median_household_income), by = "city") %>%
    mutate(
      monthly_income = median_household_income / 12,
      rent_burden_pct = (median_rent / monthly_income) * 100,
      affordable = rent_burden_pct <= 30
    )
  
  cat("\nRent Affordability Analysis (30% Rule):\n")
  iowa_housing %>%
    arrange(desc(rent_burden_pct)) %>%
    select(city, median_rent, rent_burden_pct, affordable) %>%
    head(10) %>%
    mutate(
      median_rent = dollar(median_rent),
      rent_burden_pct = paste0(round(rent_burden_pct, 1), "%")
    ) %>%
    print()
}

# =============================================================================
# Housing Stock Analysis
# =============================================================================

cat("\n=== Housing Stock Analysis ===\n\n")

# Ownership vs rental markets
ownership_summary <- iowa_housing %>%
  summarise(
    mean_owner_pct = mean(owner_occupied_pct),
    renter_heavy = sum(owner_occupied_pct < 55),
    owner_heavy = sum(owner_occupied_pct > 70)
  )

cat("Homeownership Statistics:\n")
cat("Average owner-occupied rate:", round(ownership_summary$mean_owner_pct, 1), "%\n")
cat("Cities with high renter population (<55% owners):", ownership_summary$renter_heavy, "\n")
cat("Cities with high ownership (>70% owners):", ownership_summary$owner_heavy, "\n")

# Housing age analysis
cat("\nHousing Age by City:\n")
iowa_housing %>%
  mutate(
    housing_age_category = case_when(
      median_year_built >= 2000 ~ "New (2000+)",
      median_year_built >= 1980 ~ "Modern (1980-1999)",
      median_year_built >= 1960 ~ "Mid-Century (1960-1979)",
      TRUE ~ "Historic (<1960)"
    )
  ) %>%
  count(housing_age_category) %>%
  print()

# Vacancy concerns
cat("\nCities with High Vacancy Rates (>10%):\n")
iowa_housing %>%
  filter(vacancy_rate > 10) %>%
  select(city, vacancy_rate, population) %>%
  arrange(desc(vacancy_rate)) %>%
  print()

# =============================================================================
# Visualizations
# =============================================================================

cat("\n=== Creating Housing Visualizations ===\n\n")

# 1. Home Values Comparison
p1 <- iowa_housing %>%
  arrange(desc(median_home_value)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(city, median_home_value), 
             y = median_home_value, 
             fill = market_category)) +
  geom_col() +
  geom_text(aes(label = dollar(median_home_value)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_viridis_d(option = "viridis", end = 0.85) +
  labs(
    title = "Median Home Values in Iowa Cities",
    subtitle = "2025 Estimates",
    x = NULL,
    y = "Median Home Value",
    fill = "Market Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
ggsave(here("outputs/iowa_home_values.png"), p1, width = 10, height = 8, dpi = 300)

# 2. Home Value Growth
p2 <- iowa_housing %>%
  ggplot(aes(x = reorder(city, home_value_change_1yr), 
             y = home_value_change_1yr,
             fill = growth_category)) +
  geom_col() +
  geom_hline(yintercept = mean(iowa_housing$home_value_change_1yr), 
             linetype = "dashed", color = "red") +
  coord_flip() +
  scale_fill_manual(values = c("Hot Market" = "#d62728", 
                                "Growing Market" = "#ff7f0e",
                                "Stable Market" = "#2ca02c",
                                "Slow Market" = "#7f7f7f")) +
  labs(
    title = "Home Value Appreciation by City",
    subtitle = "Year-over-Year Change (Red line = average)",
    x = NULL,
    y = "Price Change (%)",
    fill = "Market Status"
  ) +
  theme_minimal()

print(p2)
ggsave(here("outputs/iowa_home_appreciation.png"), p2, width = 10, height = 8, dpi = 300)

# 3. Rent vs Home Value Scatter
p3 <- iowa_housing %>%
  ggplot(aes(x = median_home_value, y = median_rent, 
             size = population, color = growth_category)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = city), hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray", linetype = "dashed") +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  scale_size_continuous(range = c(3, 15), guide = "none") +
  labs(
    title = "Home Values vs Monthly Rent",
    subtitle = "Point size indicates population",
    x = "Median Home Value",
    y = "Median Monthly Rent",
    color = "Market Growth"
  ) +
  theme_minimal()

print(p3)
ggsave(here("outputs/iowa_value_vs_rent.png"), p3, width = 12, height = 8, dpi = 300)

# 4. Ownership Rate Map
p4 <- iowa_housing %>%
  ggplot(aes(x = reorder(city, owner_occupied_pct), 
             y = owner_occupied_pct,
             fill = owner_occupied_pct)) +
  geom_col() +
  geom_hline(yintercept = 65, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850", 
                        midpoint = 65) +
  labs(
    title = "Homeownership Rates in Iowa Cities",
    subtitle = "Red line = 65% (state average)",
    x = NULL,
    y = "Owner-Occupied Rate (%)",
    fill = "Ownership %"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p4)
ggsave(here("outputs/iowa_ownership_rates.png"), p4, width = 10, height = 8, dpi = 300)

# 5. Housing Construction Activity
p5 <- iowa_housing %>%
  ggplot(aes(x = reorder(city, new_permits_per_1000), 
             y = new_permits_per_1000,
             fill = new_permits_per_1000)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +
  labs(
    title = "New Housing Construction Activity",
    subtitle = "Building Permits per 1,000 Residents",
    x = NULL,
    y = "Permits per 1,000 Population",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p5)
safe_ggsave(p5, here("outputs/figures/iowa_construction_activity.png"))

# =============================================================================
# Housing Affordability Index
# =============================================================================

cat("\n=== Housing Affordability Index ===\n\n")

iowa_housing <- iowa_housing %>%
  mutate(
    # Normalize metrics using utility function (lower is more affordable)
    value_score = normalize(median_home_value, reverse = TRUE),
    rent_score = normalize(median_rent, reverse = TRUE),
    vacancy_score = normalize(vacancy_rate) * 0.5,  # Some vacancy is good
    
    # Composite affordability index
    affordability_index = (value_score * 0.5 + rent_score * 0.4 + vacancy_score * 0.1)
  )

cat("Most Affordable Housing Markets:\n")
iowa_housing %>%
  arrange(desc(affordability_index)) %>%
  select(city, affordability_index, median_home_value, median_rent) %>%
  head(10) %>%
  mutate(
    median_home_value = dollar(median_home_value),
    median_rent = dollar(median_rent),
    affordability_index = round(affordability_index, 1)
  ) %>%
  print()

# =============================================================================
# Save Processed Data
# =============================================================================

safe_write_csv(iowa_housing, here("data/processed/iowa_housing_analyzed.csv"))

cat("\n=== Housing Analysis Complete ===\n")
cat("Time elapsed:", format_elapsed(start_time), "\n")
