# Install Required Packages
# Alternative installation script for required R packages
# Run this once to set up your environment

# List of required packages
packages <- c(
  # Data manipulation
  "tidyverse",
  "data.table",
  
  # Data import/export
  "readxl",
  "writexl",
  "haven",
  
  # Data visualization
  "ggplot2",
  "plotly",
  "patchwork",
  
  # Statistical analysis
  "broom",
  
  # Reporting
  "rmarkdown",
  "knitr",
  "kableExtra",
  
  # Utilities
  "here",
  "janitor",
  "skimr"
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Installing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE)
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Install missing packages
install_if_missing(packages)

cat("\nPackage installation complete!\n")
cat("You can now load packages with: library(tidyverse)\n")
cat("Or source the setup script: source('scripts/00_setup.R')\n")
