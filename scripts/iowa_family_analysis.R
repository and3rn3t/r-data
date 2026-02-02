# Iowa Cities Family-Friendly Analysis
# Analyzes family-oriented metrics including childcare, pediatric care, and youth programs
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
# Family Data Import
# =============================================================================

cat("=== Iowa Family-Friendly Analysis ===\n\n")

iowa_family <- read_csv(here("data/raw/iowa_family_data.csv"), show_col_types = FALSE)

cat("Loaded family data for", nrow(iowa_family), "cities\n\n")

# =============================================================================
# Data Validation
# =============================================================================

cat("--- Data Validation ---\n")
cat("Columns:", paste(names(iowa_family), collapse = ", "), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(iowa_family)))
cat("\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("--- Summary Statistics ---\n\n")

family_summary <- iowa_family %>%
  summarise(
    avg_daycare_per_10k = mean(daycare_centers_per_10000, na.rm = TRUE),
    median_daycare_cost = median(avg_daycare_cost_monthly, na.rm = TRUE),
    avg_pediatricians = mean(pediatricians_per_10000, na.rm = TRUE),
    avg_playgrounds = mean(playgrounds_per_10000, na.rm = TRUE),
    avg_youth_programs = mean(youth_sports_leagues, na.rm = TRUE)
  )

cat("Average daycare centers per 10,000:", round(family_summary$avg_daycare_per_10k, 2), "\n")
cat("Median monthly daycare cost: $", format(family_summary$median_daycare_cost, big.mark = ","), "\n")
cat("Average pediatricians per 10,000:", round(family_summary$avg_pediatricians, 2), "\n")
cat("Average playgrounds per 10,000:", round(family_summary$avg_playgrounds, 2), "\n\n")

# =============================================================================
# Family Score Calculation
# =============================================================================

cat("--- Family Score Calculation ---\n\n")

iowa_family_scored <- iowa_family %>%
  mutate(
    # Normalize each metric (0-100 scale)
    daycare_score = normalize(daycare_centers_per_10000),
    daycare_cost_score = normalize(avg_daycare_cost_monthly, reverse = TRUE),  # Lower cost is better
    pediatric_score = normalize(pediatricians_per_10000),
    playground_score = normalize(playgrounds_per_10000),
    youth_program_score = normalize(youth_sports_leagues),
    after_school_score = normalize(after_school_programs),
    availability_score = normalize(daycare_availability),
    
    # Composite family score (weighted average)
    family_score = (
      daycare_score * 0.15 +
      daycare_cost_score * 0.15 +
      pediatric_score * 0.20 +
      playground_score * 0.15 +
      youth_program_score * 0.10 +
      after_school_score * 0.10 +
      availability_score * 0.15
    )
  ) %>%
  arrange(desc(family_score)) %>%
  mutate(family_rank = row_number())

# Display top 10 family-friendly cities
cat("Top 10 Most Family-Friendly Cities:\n")
iowa_family_scored %>%
  select(city, family_score, family_rank, daycare_centers_per_10000, 
         avg_daycare_cost_monthly, pediatricians_per_10000) %>%
  head(10) %>%
  print()

cat("\n")

# =============================================================================
# Visualizations
# =============================================================================

cat("--- Creating Visualizations ---\n\n")

# Family Score Distribution
p_family_scores <- ggplot(iowa_family_scored, 
                          aes(x = reorder(city, family_score), y = family_score)) +
  geom_col(fill = "#2E86AB") +
  coord_flip() +
  labs(
    title = "Iowa Cities Family-Friendliness Score",
    subtitle = "Composite score based on childcare, pediatric care, and youth programs",
    x = NULL,
    y = "Family Score (0-100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(here("outputs/figures/iowa_family_scores.png"), p_family_scores, 
       width = 10, height = 8, dpi = 300)
cat("Saved: iowa_family_scores.png\n")

# Daycare Cost vs Availability
p_cost_availability <- ggplot(iowa_family_scored, 
                               aes(x = avg_daycare_cost_monthly, 
                                   y = daycare_availability,
                                   size = population,
                                   color = family_score)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = city), size = 2.5, vjust = -1, check_overlap = TRUE) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(3, 12), labels = comma) +
  labs(
    title = "Daycare Cost vs. Availability by Iowa City",
    x = "Average Monthly Daycare Cost ($)",
    y = "Daycare Availability Index",
    color = "Family\nScore",
    size = "Population"
  ) +
  theme_minimal()

ggsave(here("outputs/figures/iowa_daycare_cost_availability.png"), p_cost_availability, 
       width = 10, height = 7, dpi = 300)
cat("Saved: iowa_daycare_cost_availability.png\n")

# =============================================================================
# Export Results
# =============================================================================

cat("\n--- Exporting Results ---\n\n")

write_csv(iowa_family_scored, here("data/processed/iowa_family_analyzed.csv"))
cat("Saved: data/processed/iowa_family_analyzed.csv\n")

# =============================================================================
# Summary
# =============================================================================

end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("\n=== Analysis Complete ===\n")
cat("Duration:", duration, "seconds\n")
cat("Cities analyzed:", nrow(iowa_family_scored), "\n")
cat("Top family-friendly city:", iowa_family_scored$city[1], 
    "(score:", round(iowa_family_scored$family_score[1], 1), ")\n")
