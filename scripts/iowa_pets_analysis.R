# Iowa Cities Pet-Friendly Analysis
# Analyzes pet-friendly metrics including dog parks, vets, and pet-friendly housing
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
# Pet Data Import
# =============================================================================

cat("=== Iowa Pet-Friendly Analysis ===\n\n")

iowa_pets <- read_csv(here("data/raw/iowa_pets_data.csv"), show_col_types = FALSE)

cat("Loaded pet data for", nrow(iowa_pets), "cities\n\n")

# =============================================================================
# Data Validation
# =============================================================================

cat("--- Data Validation ---\n")
cat("Columns:", paste(names(iowa_pets), collapse = ", "), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(iowa_pets)))
cat("\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("--- Summary Statistics ---\n\n")

pet_summary <- iowa_pets %>%
  summarise(
    avg_dog_parks = mean(dog_parks_per_10000, na.rm = TRUE),
    avg_vets = mean(vets_per_10000, na.rm = TRUE),
    avg_pet_friendly_rentals = mean(pet_friendly_rentals_pct, na.rm = TRUE),
    avg_pet_stores = mean(pet_stores_per_10000, na.rm = TRUE),
    avg_pet_trails = mean(pet_friendly_trails_miles, na.rm = TRUE)
  )

cat("Average dog parks per 10,000:", round(pet_summary$avg_dog_parks, 2), "\n")
cat("Average veterinarians per 10,000:", round(pet_summary$avg_vets, 2), "\n")
cat("Average pet-friendly rental %:", round(pet_summary$avg_pet_friendly_rentals, 1), "%\n")
cat("Average pet stores per 10,000:", round(pet_summary$avg_pet_stores, 2), "\n")
cat("Average pet-friendly trail miles:", round(pet_summary$avg_pet_trails, 1), "\n\n")

# =============================================================================
# Pet Score Calculation
# =============================================================================

cat("--- Pet Score Calculation ---\n\n")

iowa_pets_scored <- iowa_pets %>%
  mutate(
    # Normalize each metric (0-100 scale)
    dog_park_score = normalize(dog_parks_per_10000),
    off_leash_score = normalize(off_leash_areas),
    vet_score = normalize(vets_per_10000),
    emergency_vet_score = normalize(emergency_vet_clinics),
    pet_store_score = normalize(pet_stores_per_10000),
    rental_score = normalize(pet_friendly_rentals_pct),
    breed_score = normalize(breed_restrictions_score),  # Higher = fewer restrictions
    trails_score = normalize(pet_friendly_trails_miles),
    grooming_score = normalize(grooming_salons_per_10000),
    
    # Use pre-calculated pet_friendliness_index as weighted component
    # Composite pet score (weighted average)
    pet_score = (
      dog_park_score * 0.15 +
      off_leash_score * 0.10 +
      vet_score * 0.20 +
      emergency_vet_score * 0.10 +
      rental_score * 0.15 +
      breed_score * 0.10 +
      trails_score * 0.10 +
      grooming_score * 0.10
    )
  ) %>%
  arrange(desc(pet_score)) %>%
  mutate(pet_rank = row_number())

# Display top 10 pet-friendly cities
cat("Top 10 Most Pet-Friendly Cities:\n")
iowa_pets_scored %>%
  select(city, pet_score, pet_rank, dog_parks_per_10000, 
         vets_per_10000, pet_friendly_rentals_pct) %>%
  head(10) %>%
  print()

cat("\n")

# =============================================================================
# Visualizations
# =============================================================================

cat("--- Creating Visualizations ---\n\n")

# Pet Score Distribution
p_pet_scores <- ggplot(iowa_pets_scored, 
                       aes(x = reorder(city, pet_score), y = pet_score)) +
  geom_col(fill = "#8B4513") +
  coord_flip() +
  labs(
    title = "Iowa Cities Pet-Friendliness Score",
    subtitle = "Composite score based on dog parks, vet access, and pet-friendly amenities",
    x = NULL,
    y = "Pet Score (0-100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(here("outputs/figures/iowa_pet_scores.png"), p_pet_scores, 
       width = 10, height = 8, dpi = 300)
cat("Saved: iowa_pet_scores.png\n")

# Dog Parks vs Vet Access
p_parks_vets <- ggplot(iowa_pets_scored, 
                        aes(x = dog_parks_per_10000, 
                            y = vets_per_10000,
                            size = population,
                            color = pet_score)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = city), size = 2.5, vjust = -1, check_overlap = TRUE) +
  scale_color_viridis_c(option = "cividis") +
  scale_size_continuous(range = c(3, 12), labels = comma) +
  labs(
    title = "Dog Parks vs. Veterinarian Access by Iowa City",
    x = "Dog Parks per 10,000 Residents",
    y = "Veterinarians per 10,000 Residents",
    color = "Pet\nScore",
    size = "Population"
  ) +
  theme_minimal()

ggsave(here("outputs/figures/iowa_pet_amenities.png"), p_parks_vets, 
       width = 10, height = 7, dpi = 300)
cat("Saved: iowa_pet_amenities.png\n")

# Pet-Friendly Rentals by City
p_rentals <- ggplot(iowa_pets_scored, 
                     aes(x = reorder(city, pet_friendly_rentals_pct), 
                         y = pet_friendly_rentals_pct,
                         fill = pet_score)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Pet-Friendly Rental Availability in Iowa Cities",
    subtitle = "Percentage of rentals that allow pets",
    x = NULL,
    y = "Pet-Friendly Rentals (%)",
    fill = "Pet\nScore"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(here("outputs/figures/iowa_pet_friendly_rentals.png"), p_rentals, 
       width = 10, height = 8, dpi = 300)
cat("Saved: iowa_pet_friendly_rentals.png\n")

# =============================================================================
# Export Results
# =============================================================================

cat("\n--- Exporting Results ---\n\n")

write_csv(iowa_pets_scored, here("data/processed/iowa_pets_analyzed.csv"))
cat("Saved: data/processed/iowa_pets_analyzed.csv\n")

# =============================================================================
# Summary
# =============================================================================

end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("\n=== Analysis Complete ===\n")
cat("Duration:", duration, "seconds\n")
cat("Cities analyzed:", nrow(iowa_pets_scored), "\n")
cat("Top pet-friendly city:", iowa_pets_scored$city[1], 
    "(score:", round(iowa_pets_scored$pet_score[1], 1), ")\n")
