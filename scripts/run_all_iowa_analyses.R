# Iowa Cities Master Analysis Script
# This script runs all Iowa city analyses in sequence
# Run this script to perform complete analysis of Iowa city data

cat("
╔═══════════════════════════════════════════════════════════════╗
║           IOWA CITIES COMPREHENSIVE ANALYSIS                  ║
║                                                               ║
║   This script will run all analyses for Iowa city data        ║
╚═══════════════════════════════════════════════════════════════╝
\n")

library(here)

# Track timing
start_time <- Sys.time()

# =============================================================================
# Step 1: Setup and Package Installation
# =============================================================================

cat("\n[1/7] Setting up environment...\n")
cat(rep("-", 50), "\n", sep = "")

source(here("scripts/00_setup.R"))
cat("✓ Packages loaded\n")

# =============================================================================
# Step 2: Import Iowa City Data
# =============================================================================

cat("\n[2/7] Importing Iowa city data...\n")
cat(rep("-", 50), "\n", sep = "")

source(here("scripts/iowa_cities_import.R"))
cat("✓ Data import complete\n")

# =============================================================================
# Step 3: Clean Data
# =============================================================================

cat("\n[3/7] Cleaning and standardizing data...\n")
cat(rep("-", 50), "\n", sep = "")

source(here("scripts/iowa_cities_cleaning.R"))
cat("✓ Data cleaning complete\n")

# =============================================================================
# Step 4: Population Analysis
# =============================================================================

cat("\n[4/7] Running population and geographic analysis...\n")
cat(rep("-", 50), "\n", sep = "")

source(here("scripts/iowa_cities_analysis.R"))
cat("✓ Population analysis complete\n")

# =============================================================================
# Step 5: Economic Analysis
# =============================================================================

cat("\n[5/7] Running economic analysis...\n")
cat(rep("-", 50), "\n", sep = "")

source(here("scripts/iowa_economic_analysis.R"))
cat("✓ Economic analysis complete\n")

# =============================================================================
# Step 6: Housing Analysis
# =============================================================================

cat("\n[6/7] Running housing market analysis...\n")
cat(rep("-", 50), "\n", sep = "")

source(here("scripts/iowa_housing_analysis.R"))
cat("✓ Housing analysis complete\n")

# =============================================================================
# Step 7: Education Analysis
# =============================================================================

cat("\n[7/7] Running education analysis...\n")
cat(rep("-", 50), "\n", sep = "")

source(here("scripts/iowa_education_analysis.R"))
cat("✓ Education analysis complete\n")

# =============================================================================
# Step 8: Crime and Safety Analysis
# =============================================================================

cat("\n[8/8] Running crime and safety analysis...\n")
cat(rep("-", 50), "\n", sep = "")

source(here("scripts/iowa_crime_safety_analysis.R"))
cat("✓ Crime and safety analysis complete\n")

# =============================================================================
# Summary
# =============================================================================

end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "mins"), 2)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS COMPLETE                          ║\n")
cat("╠═══════════════════════════════════════════════════════════════╣\n")
cat("║                                                               ║\n")

# List output files
cat("║  Processed Data Files:                                        ║\n")
processed_files <- list.files(here("data/processed"), pattern = "\\.csv$")
for (f in processed_files) {
  cat("║    • ", f, rep(" ", 50 - nchar(f)), "║\n", sep = "")
}

cat("║                                                               ║\n")
cat("║  Visualization Files:                                         ║\n")
output_files <- list.files(here("outputs"), pattern = "\\.(png|pdf)$")
for (f in head(output_files, 10)) {
  cat("║    • ", f, rep(" ", 50 - nchar(f)), "║\n", sep = "")
}
if (length(output_files) > 10) {
  cat("║    ... and", length(output_files) - 10, "more                                    ║\n")
}

cat("║                                                               ║\n")
cat("╠═══════════════════════════════════════════════════════════════╣\n")
cat("║  Time elapsed:", duration, "minutes", rep(" ", 40), "║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")

cat("\n📊 Next Steps:\n")
cat("  1. Review visualizations in outputs/ folder\n")
cat("  2. Render the comprehensive dashboard:\n")
cat("     rmarkdown::render('notebooks/iowa_comprehensive_dashboard.Rmd')\n")
cat("  3. Explore individual reports in notebooks/ folder\n")
