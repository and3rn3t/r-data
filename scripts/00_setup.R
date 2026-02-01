# Setup Script
# This script installs and loads all required packages for the analysis

# Install pacman if not already installed (for easy package management)
if (!require("pacman")) install.packages("pacman")

# Load (and install if needed) required packages
pacman::p_load(
  # Data manipulation
  tidyverse,     # Collection of packages (dplyr, ggplot2, tidyr, etc.)
  data.table,    # Fast data manipulation
  
  # Data import/export
  readxl,        # Read Excel files
  writexl,       # Write Excel files
  haven,         # Read SPSS, Stata, and SAS files
  
  # Data visualization
  ggplot2,       # Grammar of graphics plotting
  plotly,        # Interactive plots
  patchwork,     # Combine multiple plots
  
  # Statistical analysis
  broom,         # Tidy model outputs
  
  # Reporting
  rmarkdown,     # Dynamic documents
  knitr,         # Report generation
  kableExtra,    # Enhanced tables
  
  # Utilities
  here,          # Easy file paths
  janitor,       # Data cleaning
  skimr          # Quick data summaries
)

# Set global options
options(
  scipen = 999,              # Avoid scientific notation
  stringsAsFactors = FALSE   # Don't auto-convert strings to factors
)

# Create a custom ggplot2 theme (optional)
theme_custom <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Set as default theme
theme_set(theme_custom())

cat("Setup complete! All packages loaded successfully.\n")
