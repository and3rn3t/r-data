# Iowa Cities Master Analysis Script
# This script runs all Iowa city analyses in sequence
# Run this script to perform complete analysis of Iowa city data
# ================================================================

cat("
╔═══════════════════════════════════════════════════════════════╗
║           IOWA CITIES COMPREHENSIVE ANALYSIS                  ║
║                                                               ║
║   This script will run all analyses for Iowa city data        ║
╚═══════════════════════════════════════════════════════════════╝
\n")

library(here)

# Load utilities first for helper functions
source(here("scripts/utils.R"))

# Track timing
start_time <- Sys.time()

# Configuration
STEPS <- list(
  list(name = "Setup", script = "scripts/00_setup.R", required = TRUE),
  list(name = "Data Import", script = "scripts/iowa_cities_import.R", required = TRUE),
  list(name = "Data Cleaning", script = "scripts/iowa_cities_cleaning.R", required = TRUE),
  list(name = "Population Analysis", script = "scripts/iowa_cities_analysis.R", required = FALSE),
  list(name = "Economic Analysis", script = "scripts/iowa_economic_analysis.R", required = FALSE),
  list(name = "Housing Analysis", script = "scripts/iowa_housing_analysis.R", required = FALSE),
  list(name = "Education Analysis", script = "scripts/iowa_education_analysis.R", required = FALSE),
  list(name = "Crime Analysis", script = "scripts/iowa_crime_safety_analysis.R", required = FALSE),
  list(name = "Demographics Analysis", script = "scripts/iowa_demographics_analysis.R", required = FALSE),
  list(name = "Environment Analysis", script = "scripts/iowa_environment_analysis.R", required = FALSE),
  list(name = "Historical Analysis", script = "scripts/iowa_historical_analysis.R", required = FALSE),
  list(name = "Comprehensive Analysis", script = "scripts/iowa_comprehensive_analysis.R", required = FALSE)
)

# Track results
results <- list()
total_steps <- length(STEPS)

#' Run a single analysis step with error handling
#' 
#' @param step List with name, script, required fields
#' @param step_num Current step number
#' @return List with success status and any error message
run_step <- function(step, step_num) {
  cat(sprintf("\n[%d/%d] %s...\n", step_num, total_steps, step$name))
  cat(rep("-", 50), "\n", sep = "")
  
  script_path <- here(step$script)
  
  # Check if script exists
  if (!file.exists(script_path)) {
    msg <- paste("Script not found:", step$script)
    if (step$required) {
      stop(msg)
    } else {
      cat("⚠ ", msg, " (skipped)\n", sep = "")
      return(list(success = FALSE, skipped = TRUE, error = msg))
    }
  }
  
  # Run with error handling
  result <- tryCatch({
    source(script_path, local = new.env())
    cat("✓", step$name, "complete\n")
    list(success = TRUE, skipped = FALSE, error = NULL)
  }, error = function(e) {
    msg <- paste("Error:", e$message)
    if (step$required) {
      stop(paste(step$name, "-", msg))
    } else {
      cat("✗", step$name, "failed:", e$message, "\n")
      list(success = FALSE, skipped = FALSE, error = msg)
    }
  })
  
  return(result)
}

# =============================================================================
# RUN ALL STEPS
# =============================================================================

for (i in seq_along(STEPS)) {
  results[[STEPS[[i]]$name]] <- run_step(STEPS[[i]], i)
}

# =============================================================================
# SUMMARY
# =============================================================================

end_time <- Sys.time()
elapsed <- format_elapsed(start_time)

# Count successes and failures
successes <- sum(sapply(results, function(r) r$success))
failures <- sum(sapply(results, function(r) !r$success && !r$skipped))
skipped <- sum(sapply(results, function(r) r$skipped))

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS COMPLETE                          ║\n")
cat("╠═══════════════════════════════════════════════════════════════╣\n")
cat("║                                                               ║\n")
cat(sprintf("║  ✓ Successful:  %-3d                                          ║\n", successes))
if (failures > 0) {
  cat(sprintf("║  ✗ Failed:      %-3d                                          ║\n", failures))
}
if (skipped > 0) {
  cat(sprintf("║  ⚠ Skipped:     %-3d                                          ║\n", skipped))
}
cat("║                                                               ║\n")

# List output files
cat("║  Processed Data Files:                                        ║\n")
processed_files <- list.files(here("data/processed"), pattern = "\\.csv$")
for (f in head(processed_files, 8)) {
  display_name <- if (nchar(f) > 48) paste0(substr(f, 1, 45), "...") else f
  cat("║    • ", display_name, rep(" ", max(0, 50 - nchar(display_name))), "║\n", sep = "")
}
if (length(processed_files) > 8) {
  cat(sprintf("║    ... and %d more files                                    ║\n", 
              length(processed_files) - 8))
}

cat("║                                                               ║\n")
cat("║  Visualization Files:                                         ║\n")
output_files <- list.files(here("outputs"), pattern = "\\.(png|pdf)$", recursive = TRUE)
for (f in head(output_files, 6)) {
  display_name <- if (nchar(f) > 48) paste0(substr(f, 1, 45), "...") else f
  cat("║    • ", display_name, rep(" ", max(0, 50 - nchar(display_name))), "║\n", sep = "")
}
if (length(output_files) > 6) {
  cat(sprintf("║    ... and %d more files                                    ║\n", 
              length(output_files) - 6))
}

cat("║                                                               ║\n")
cat("╠═══════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  Time elapsed: %-44s║\n", elapsed))
cat("╚═══════════════════════════════════════════════════════════════╝\n")

# Print any errors that occurred
if (failures > 0) {
  cat("\n⚠ Failed Steps:\n")
  for (name in names(results)) {
    if (!results[[name]]$success && !results[[name]]$skipped) {
      cat("  -", name, ":", results[[name]]$error, "\n")
    }
  }
}

cat("\n📊 Next Steps:\n")
cat("  1. Review processed data in data/processed/ folder\n")
cat("  2. Review visualizations in outputs/ folder\n")
cat("  3. Launch interactive dashboard:\n")
cat("     shiny::runApp('app.R')\n")
cat("  4. Generate city reports:\n")
cat("     source('scripts/generate_reports.R')\n")
cat("     generate_city_report('Bettendorf')\n")
cat("  5. Explore interactive maps:\n")
cat("     source('scripts/iowa_maps.R')\n")
cat("  6. Get personalized recommendations:\n")
cat("     source('scripts/iowa_recommendations.R')\n")
cat("     recommend_for_profile('family')\n")
