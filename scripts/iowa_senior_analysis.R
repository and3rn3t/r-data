# Iowa Cities Senior Living Analysis
# Analyzes senior-friendly metrics including assisted living, healthcare, and services
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
# Senior Data Import
# =============================================================================

cat("=== Iowa Senior Living Analysis ===\n\n")

iowa_senior <- read_csv(here("data/raw/iowa_senior_data.csv"), show_col_types = FALSE)

cat("Loaded senior living data for", nrow(iowa_senior), "cities\n\n")

# =============================================================================
# Data Validation
# =============================================================================

cat("--- Data Validation ---\n")
cat("Columns:", paste(names(iowa_senior), collapse = ", "), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(iowa_senior)))
cat("\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("--- Summary Statistics ---\n\n")

senior_summary <- iowa_senior %>%
  summarise(
    avg_assisted_living = mean(assisted_living_per_10000, na.rm = TRUE),
    avg_nursing_rating = mean(avg_nursing_home_rating, na.rm = TRUE),
    avg_senior_centers = mean(senior_centers_per_10000, na.rm = TRUE),
    avg_medicare_plans = mean(medicare_plan_options, na.rm = TRUE),
    avg_geriatricians = mean(geriatricians_per_10000, na.rm = TRUE)
  )

cat("Average assisted living facilities per 10,000:", round(senior_summary$avg_assisted_living, 2), "\n")
cat("Average nursing home rating:", round(senior_summary$avg_nursing_rating, 2), "/5\n")
cat("Average senior centers per 10,000:", round(senior_summary$avg_senior_centers, 2), "\n")
cat("Average Medicare plan options:", round(senior_summary$avg_medicare_plans, 0), "\n\n")

# =============================================================================
# Senior Score Calculation
# =============================================================================

cat("--- Senior Score Calculation ---\n\n")

iowa_senior_scored <- iowa_senior %>%
  mutate(
    # Normalize each metric (0-100 scale)
    assisted_living_score = normalize(assisted_living_per_10000),
    nursing_quality_score = normalize(avg_nursing_home_rating),
    senior_center_score = normalize(senior_centers_per_10000),
    medicare_score = normalize(medicare_plan_options),
    geriatrician_score = normalize(geriatricians_per_10000),
    meals_score = normalize(meals_on_wheels_coverage),
    transport_score = normalize(senior_transportation_routes),
    housing_score = normalize(senior_housing_units_per_1000),
    
    # Composite senior score (weighted average)
    senior_score = (
      assisted_living_score * 0.15 +
      nursing_quality_score * 0.15 +
      senior_center_score * 0.15 +
      medicare_score * 0.15 +
      geriatrician_score * 0.15 +
      meals_score * 0.10 +
      transport_score * 0.10 +
      housing_score * 0.05
    )
  ) %>%
  arrange(desc(senior_score)) %>%
  mutate(senior_rank = row_number())

# Display top 10 senior-friendly cities
cat("Top 10 Most Senior-Friendly Cities:\n")
iowa_senior_scored %>%
  select(city, senior_score, senior_rank, assisted_living_per_10000, 
         avg_nursing_home_rating, medicare_plan_options) %>%
  head(10) %>%
  print()

cat("\n")

# =============================================================================
# Visualizations
# =============================================================================

cat("--- Creating Visualizations ---\n\n")

# Senior Score Distribution
p_senior_scores <- ggplot(iowa_senior_scored, 
                          aes(x = reorder(city, senior_score), y = senior_score)) +
  geom_col(fill = "#6B4E71") +
  coord_flip() +
  labs(
    title = "Iowa Cities Senior-Friendliness Score",
    subtitle = "Composite score based on senior care, services, and healthcare access",
    x = NULL,
    y = "Senior Score (0-100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(here("outputs/figures/iowa_senior_scores.png"), p_senior_scores, 
       width = 10, height = 8, dpi = 300)
cat("Saved: iowa_senior_scores.png\n")

# Medicare Options vs Nursing Home Quality
p_medicare_quality <- ggplot(iowa_senior_scored, 
                              aes(x = medicare_plan_options, 
                                  y = avg_nursing_home_rating,
                                  size = population,
                                  color = senior_score)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = city), size = 2.5, vjust = -1, check_overlap = TRUE) +
  scale_color_viridis_c(option = "magma") +
  scale_size_continuous(range = c(3, 12), labels = comma) +
  labs(
    title = "Medicare Options vs. Nursing Home Quality by Iowa City",
    x = "Medicare Advantage Plan Options",
    y = "Average Nursing Home Rating (1-5)",
    color = "Senior\nScore",
    size = "Population"
  ) +
  theme_minimal()

ggsave(here("outputs/figures/iowa_senior_care_quality.png"), p_medicare_quality, 
       width = 10, height = 7, dpi = 300)
cat("Saved: iowa_senior_care_quality.png\n")

# Age-Friendly Certification Status
p_age_friendly <- iowa_senior_scored %>%
  mutate(age_friendly = factor(age_friendly_certification, 
                               levels = c(0, 1), 
                               labels = c("Not Certified", "Certified"))) %>%
  ggplot(aes(x = age_friendly, fill = age_friendly)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("Not Certified" = "#E0E0E0", "Certified" = "#4CAF50")) +
  labs(
    title = "AARP Age-Friendly Community Certification",
    subtitle = "Iowa cities with official age-friendly designation",
    x = NULL,
    y = "Number of Cities"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("outputs/figures/iowa_age_friendly_certification.png"), p_age_friendly, 
       width = 8, height = 6, dpi = 300)
cat("Saved: iowa_age_friendly_certification.png\n")

# =============================================================================
# Export Results
# =============================================================================

cat("\n--- Exporting Results ---\n\n")

write_csv(iowa_senior_scored, here("data/processed/iowa_senior_analyzed.csv"))
cat("Saved: data/processed/iowa_senior_analyzed.csv\n")

# =============================================================================
# Summary
# =============================================================================

end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("\n=== Analysis Complete ===\n")
cat("Duration:", duration, "seconds\n")
cat("Cities analyzed:", nrow(iowa_senior_scored), "\n")
cat("Top senior-friendly city:", iowa_senior_scored$city[1], 
    "(score:", round(iowa_senior_scored$senior_score[1], 1), ")\n")
cat("Age-friendly certified cities:", sum(iowa_senior_scored$age_friendly_certification), "\n")
