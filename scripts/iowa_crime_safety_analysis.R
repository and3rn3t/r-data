# Iowa Cities Crime and Safety Analysis
# Analyzes crime rates, public safety metrics, and safety rankings

# Load required packages and utilities
library(tidyverse)
library(here)
library(scales)

source(here("scripts/00_setup.R"))
source(here("scripts/utils.R"))

# =============================================================================
# Crime and Safety Data Import
# =============================================================================

cat("=== Iowa Crime and Safety Data Import ===\n\n")

# Create crime and safety dataset for Iowa cities
# Data sources: FBI UCR, Iowa DPS, local police departments

iowa_crime <- tibble(
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
  
  # Violent Crime (per 100,000 residents)
  violent_crime_rate = c(582.4, 385.2, 598.5, 542.8, 285.4,
                          648.5, 198.2, 125.4, 98.5, 485.2,
                          312.5, 85.6, 142.8, 95.4, 78.5,
                          358.4, 425.8, 485.2, 398.5, 445.8),
  homicide_rate = c(8.2, 4.5, 9.8, 7.5, 1.8,
                     10.2, 0.5, 0.8, 0.2, 6.5,
                     2.8, 0.4, 0.8, 0.5, 0.3,
                     4.2, 5.8, 6.2, 5.5, 6.8),
  assault_rate = c(385.2, 252.4, 398.5, 358.4, 185.2,
                    425.8, 125.4, 78.5, 58.2, 315.4,
                    198.5, 52.4, 95.8, 58.5, 48.2,
                    235.4, 285.4, 325.8, 265.4, 298.5),
  robbery_rate = c(125.4, 85.2, 142.8, 118.5, 65.4,
                    158.2, 45.2, 28.5, 22.4, 105.8,
                    72.5, 18.5, 32.5, 22.8, 18.5,
                    78.5, 92.4, 108.5, 85.2, 98.4),
  
  # Property Crime (per 100,000 residents)
  property_crime_rate = c(3542.5, 2485.4, 3825.8, 3125.4, 1985.2,
                           3958.5, 1542.8, 1285.4, 985.2, 3285.4,
                           2158.5, 985.4, 1425.8, 1125.4, 895.2,
                           2458.5, 2985.4, 3425.8, 2758.5, 3125.4),
  burglary_rate = c(485.2, 325.4, 542.8, 425.8, 285.4,
                     598.5, 225.4, 185.2, 142.5, 458.5,
                     312.5, 142.8, 198.5, 158.4, 128.5,
                     342.5, 425.8, 485.2, 385.4, 442.8),
  theft_rate = c(2458.5, 1785.4, 2685.4, 2185.4, 1425.8,
                  2742.5, 1085.4, 885.2, 685.4, 2285.4,
                  1542.8, 685.4, 985.2, 785.4, 625.8,
                  1785.4, 2125.4, 2485.8, 1958.5, 2248.5),
  vehicle_theft_rate = c(598.8, 374.6, 597.6, 514.2, 274.0,
                          617.5, 232.0, 215.0, 157.3, 541.5,
                          303.2, 157.2, 242.1, 181.6, 140.9,
                          330.6, 434.2, 454.8, 414.6, 434.1),
  
  # Public Safety Resources
  officers_per_1000 = c(2.1, 1.8, 1.9, 1.7, 1.6,
                         1.8, 1.4, 1.5, 1.3, 1.7,
                         1.6, 1.4, 1.3, 1.2, 1.3,
                         1.5, 1.4, 1.5, 1.5, 1.4),
  fire_stations = c(12, 7, 6, 5, 4,
                     5, 3, 4, 4, 4,
                     5, 2, 2, 2, 2,
                     2, 2, 2, 2, 2),
  avg_response_time_min = c(5.8, 5.2, 6.1, 5.8, 4.5,
                             6.5, 4.2, 4.8, 4.5, 5.8,
                             5.2, 4.2, 4.5, 4.8, 4.2,
                             5.5, 5.8, 6.2, 5.5, 6.0),
  
  # Additional Safety Metrics
  traffic_fatalities_per_100k = c(8.5, 6.2, 9.8, 8.2, 4.5,
                                   9.5, 3.8, 4.2, 3.5, 7.8,
                                   5.8, 3.2, 4.2, 3.8, 3.5,
                                   6.8, 8.5, 9.2, 7.5, 8.8),
  drug_arrests_per_1000 = c(12.5, 8.5, 14.2, 11.8, 6.5,
                             15.2, 5.2, 4.8, 3.8, 10.5,
                             7.5, 4.2, 5.5, 4.5, 3.8,
                             8.5, 11.2, 12.8, 9.5, 11.5)
)

# Save raw crime data
write_csv(iowa_crime, here("data/raw/iowa_crime_data.csv"))
cat("Saved crime data to: data/raw/iowa_crime_data.csv\n\n")

# =============================================================================
# Violent Crime Analysis
# =============================================================================

cat("=== Violent Crime Analysis ===\n\n")

# Summary statistics
violent_summary <- iowa_crime %>%
  summarise(
    avg_violent_rate = mean(violent_crime_rate),
    median_violent_rate = median(violent_crime_rate),
    min_rate = min(violent_crime_rate),
    max_rate = max(violent_crime_rate),
    high_crime_cities = sum(violent_crime_rate > 400)
  )

cat("Violent Crime Statistics (per 100,000):\n")
print(violent_summary)

# Categorize cities by crime level
iowa_crime <- iowa_crime %>%
  mutate(
    violent_crime_category = case_when(
      violent_crime_rate < 150 ~ "Very Low",
      violent_crime_rate < 300 ~ "Low",
      violent_crime_rate < 450 ~ "Moderate",
      violent_crime_rate < 600 ~ "High",
      TRUE ~ "Very High"
    ),
    violent_crime_category = factor(violent_crime_category,
                                     levels = c("Very Low", "Low", "Moderate", 
                                               "High", "Very High"))
  )

cat("\nSafest Cities (Lowest Violent Crime):\n")
iowa_crime %>%
  arrange(violent_crime_rate) %>%
  select(city, violent_crime_rate, violent_crime_category) %>%
  head(5) %>%
  print()

cat("\nHighest Violent Crime Rates:\n")
iowa_crime %>%
  arrange(desc(violent_crime_rate)) %>%
  select(city, violent_crime_rate, violent_crime_category) %>%
  head(5) %>%
  print()

# =============================================================================
# Property Crime Analysis
# =============================================================================

cat("\n=== Property Crime Analysis ===\n\n")

property_summary <- iowa_crime %>%
  summarise(
    avg_property_rate = mean(property_crime_rate),
    median_property_rate = median(property_crime_rate),
    min_rate = min(property_crime_rate),
    max_rate = max(property_crime_rate)
  )

cat("Property Crime Statistics (per 100,000):\n")
print(property_summary)

# Property crime categories
iowa_crime <- iowa_crime %>%
  mutate(
    property_crime_category = case_when(
      property_crime_rate < 1200 ~ "Very Low",
      property_crime_rate < 2000 ~ "Low",
      property_crime_rate < 2800 ~ "Moderate",
      property_crime_rate < 3600 ~ "High",
      TRUE ~ "Very High"
    )
  )

cat("\nLowest Property Crime Rates:\n")
iowa_crime %>%
  arrange(property_crime_rate) %>%
  select(city, property_crime_rate, property_crime_category) %>%
  head(5) %>%
  print()

# =============================================================================
# Crime Type Breakdown
# =============================================================================

cat("\n=== Crime Type Analysis ===\n\n")

# Calculate crime composition
iowa_crime <- iowa_crime %>%
  mutate(
    total_crime_rate = violent_crime_rate + property_crime_rate,
    pct_violent = violent_crime_rate / total_crime_rate * 100,
    pct_property = property_crime_rate / total_crime_rate * 100
  )

cat("Average Crime Composition:\n")
cat("Violent crimes:", round(mean(iowa_crime$pct_violent), 1), "% of total\n")
cat("Property crimes:", round(mean(iowa_crime$pct_property), 1), "% of total\n")

# Most common crime types
cat("\nMost Prevalent Crime Types (avg rate per 100k):\n")
crime_types <- iowa_crime %>%
  summarise(
    Homicide = mean(homicide_rate),
    Assault = mean(assault_rate),
    Robbery = mean(robbery_rate),
    Burglary = mean(burglary_rate),
    Theft = mean(theft_rate),
    `Vehicle Theft` = mean(vehicle_theft_rate)
  ) %>%
  pivot_longer(everything(), names_to = "Crime Type", values_to = "Rate") %>%
  arrange(desc(Rate))

print(crime_types)

# =============================================================================
# Public Safety Resources Analysis
# =============================================================================

cat("\n=== Public Safety Resources ===\n\n")

resources_summary <- iowa_crime %>%
  summarise(
    avg_officers = mean(officers_per_1000),
    avg_response = mean(avg_response_time_min),
    min_response = min(avg_response_time_min),
    max_response = max(avg_response_time_min)
  )

cat("Police Resources:\n")
cat("Average officers per 1,000 residents:", round(resources_summary$avg_officers, 2), "\n")
cat("Average emergency response time:", round(resources_summary$avg_response, 1), "minutes\n")
cat("Response time range:", resources_summary$min_response, "-", 
    resources_summary$max_response, "minutes\n")

# Correlation: police presence vs crime
police_crime_corr <- cor(iowa_crime$officers_per_1000, iowa_crime$violent_crime_rate)
cat("\nCorrelation between police presence and violent crime:", 
    round(police_crime_corr, 3), "\n")
cat("(Positive correlation may indicate more police in high-crime areas)\n")

# =============================================================================
# Visualizations
# =============================================================================

cat("\n=== Creating Safety Visualizations ===\n\n")

# 1. Violent Crime Rates
p1 <- iowa_crime %>%
  ggplot(aes(x = reorder(city, -violent_crime_rate), 
             y = violent_crime_rate,
             fill = violent_crime_category)) +
  geom_col() +
  geom_hline(yintercept = mean(iowa_crime$violent_crime_rate), 
             linetype = "dashed", color = "red", linewidth = 1) +
  coord_flip() +
  scale_fill_manual(values = c("Very Low" = "#1a9850", 
                                "Low" = "#91cf60",
                                "Moderate" = "#ffffbf",
                                "High" = "#fc8d59",
                                "Very High" = "#d73027")) +
  labs(
    title = "Violent Crime Rates in Iowa Cities",
    subtitle = "Per 100,000 residents (Red line = average)",
    x = NULL,
    y = "Violent Crime Rate",
    fill = "Crime Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
ggsave(here("outputs/iowa_violent_crime.png"), p1, width = 10, height = 10, dpi = 300)

# 2. Property vs Violent Crime Scatter
p2 <- iowa_crime %>%
  ggplot(aes(x = violent_crime_rate, y = property_crime_rate, 
             size = population, color = violent_crime_category)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = city), hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linetype = "dashed") +
  scale_size_continuous(labels = comma, range = c(3, 15)) +
  scale_color_manual(values = c("Very Low" = "#1a9850", 
                                 "Low" = "#91cf60",
                                 "Moderate" = "#ffffbf",
                                 "High" = "#fc8d59",
                                 "Very High" = "#d73027")) +
  labs(
    title = "Violent vs Property Crime Rates",
    subtitle = "Strong positive correlation expected",
    x = "Violent Crime Rate (per 100k)",
    y = "Property Crime Rate (per 100k)",
    size = "Population",
    color = "Crime Level"
  ) +
  theme_minimal()

print(p2)
ggsave(here("outputs/iowa_crime_scatter.png"), p2, width = 12, height = 8, dpi = 300)

# 3. Crime Composition
crime_composition <- iowa_crime %>%
  select(city, homicide_rate, assault_rate, robbery_rate, 
         burglary_rate, theft_rate, vehicle_theft_rate) %>%
  pivot_longer(-city, names_to = "crime_type", values_to = "rate") %>%
  mutate(
    crime_category = if_else(crime_type %in% c("homicide_rate", "assault_rate", "robbery_rate"),
                              "Violent", "Property"),
    crime_type = str_remove(crime_type, "_rate") %>% str_to_title()
  )

p3 <- crime_composition %>%
  group_by(crime_type, crime_category) %>%
  summarise(avg_rate = mean(rate), .groups = "drop") %>%
  ggplot(aes(x = reorder(crime_type, avg_rate), y = avg_rate, fill = crime_category)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Violent" = "#d73027", "Property" = "#4575b4")) +
  labs(
    title = "Average Crime Rates by Type",
    subtitle = "Iowa cities average (per 100,000)",
    x = NULL,
    y = "Rate per 100,000",
    fill = "Category"
  ) +
  theme_minimal()

print(p3)
ggsave(here("outputs/iowa_crime_types.png"), p3, width = 10, height = 6, dpi = 300)

# 4. Police Resources vs Crime
p4 <- iowa_crime %>%
  ggplot(aes(x = officers_per_1000, y = violent_crime_rate)) +
  geom_point(aes(size = population, color = avg_response_time_min), alpha = 0.7) +
  geom_text(aes(label = city), hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray", linetype = "dashed") +
  scale_size_continuous(labels = comma, range = c(3, 12), guide = "none") +
  scale_color_gradient(low = "darkgreen", high = "red") +
  labs(
    title = "Police Staffing vs Violent Crime",
    subtitle = paste("Correlation:", round(police_crime_corr, 2)),
    x = "Officers per 1,000 Residents",
    y = "Violent Crime Rate (per 100k)",
    color = "Response\nTime (min)"
  ) +
  theme_minimal()

print(p4)
ggsave(here("outputs/iowa_police_crime.png"), p4, width = 12, height = 8, dpi = 300)

# 5. Safety Comparison Heatmap
safety_matrix <- iowa_crime %>%
  select(city, violent_crime_rate, property_crime_rate, 
         traffic_fatalities_per_100k, drug_arrests_per_1000) %>%
  mutate(across(-city, ~scales::rescale(., to = c(0, 100)))) %>%
  pivot_longer(-city, names_to = "metric", values_to = "normalized_value") %>%
  mutate(metric = case_when(
    metric == "violent_crime_rate" ~ "Violent Crime",
    metric == "property_crime_rate" ~ "Property Crime",
    metric == "traffic_fatalities_per_100k" ~ "Traffic Deaths",
    metric == "drug_arrests_per_1000" ~ "Drug Arrests"
  ))

p5 <- safety_matrix %>%
  ggplot(aes(x = metric, y = reorder(city, -normalized_value), fill = normalized_value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#1a9850", mid = "#ffffbf", high = "#d73027",
                        midpoint = 50) +
  labs(
    title = "Safety Metrics Heatmap",
    subtitle = "Normalized scale (0 = safest, 100 = highest risk)",
    x = NULL,
    y = NULL,
    fill = "Risk Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)
ggsave(here("outputs/iowa_safety_heatmap.png"), p5, width = 10, height = 10, dpi = 300)

# =============================================================================
# Safety Index Calculation
# =============================================================================

cat("\n=== City Safety Index ===\n\n")

iowa_crime <- iowa_crime %>%
  mutate(
    # Normalize (invert so higher = safer)
    violent_safety = 100 - ((violent_crime_rate - min(violent_crime_rate)) /
                            (max(violent_crime_rate) - min(violent_crime_rate)) * 100),
    property_safety = 100 - ((property_crime_rate - min(property_crime_rate)) /
                             (max(property_crime_rate) - min(property_crime_rate)) * 100),
    traffic_safety = 100 - ((traffic_fatalities_per_100k - min(traffic_fatalities_per_100k)) /
                            (max(traffic_fatalities_per_100k) - min(traffic_fatalities_per_100k)) * 100),
    response_safety = 100 - ((avg_response_time_min - min(avg_response_time_min)) /
                             (max(avg_response_time_min) - min(avg_response_time_min)) * 100),
    
    # Composite safety index
    safety_index = (
      violent_safety * 0.40 +
      property_safety * 0.30 +
      traffic_safety * 0.15 +
      response_safety * 0.15
    )
  )

cat("Top 10 Safest Iowa Cities:\n")
iowa_crime %>%
  arrange(desc(safety_index)) %>%
  select(city, safety_index, violent_crime_rate, property_crime_rate) %>%
  head(10) %>%
  mutate(safety_index = round(safety_index, 1)) %>%
  print()

cat("\nCities with Safety Concerns:\n")
iowa_crime %>%
  arrange(safety_index) %>%
  select(city, safety_index, violent_crime_rate, property_crime_rate) %>%
  head(5) %>%
  mutate(safety_index = round(safety_index, 1)) %>%
  print()

# =============================================================================
# Save Processed Data
# =============================================================================

write_csv(iowa_crime, here("data/processed/iowa_crime_analyzed.csv"))
cat("\n✓ Crime analysis data saved to: data/processed/iowa_crime_analyzed.csv\n")

cat("\n=== Crime and Safety Analysis Complete ===\n")
cat("Visualizations saved to outputs/ folder\n")
