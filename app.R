# Iowa Cities Interactive Dashboard
# Shiny app for exploring Iowa city data
# ========================================
#
# This is the main entry point for the dashboard.
# The app is modularized into separate files in the R/ directory:
#   - R/themes.R    : Theme definitions and switching
#   - R/scoring.R   : Scoring system and lifestyle presets
#   - R/helpers.R   : Utility functions
#   - R/ui.R        : UI components and layout
#   - R/server.R    : Server logic and reactive functions
#
# ========================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(plotly)
library(DT)
library(here)
library(shinycssloaders)

# =============================================================================
# LOAD MODULES
# =============================================================================

# Load utility functions first (includes normalize function)
source(here("scripts/utils.R"))

# Load modular components
source(here("R/themes.R"))
source(here("R/scoring.R"))
source(here("R/helpers.R"))
source(here("R/ui.R"))
source(here("R/server.R"))

# Load project utilities (for backward compatibility)
source(here("scripts/constants.R"))
source(here("scripts/security.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

message("Starting Iowa Cities Dashboard...")
message("Loading data...")

# Load cached or compute scores
cached_result <- load_cached_or_compute()
data <- cached_result$data
scores <- cached_result$scores
cache_time <- cached_result$cache_time

# If cache had scores, use them; otherwise calculate
if (is.null(scores)) {
  message("Calculating city scores...")
  scores <- calculate_city_scores(data)
}

# Get list of cities
cities <- sort(unique(scores$city))

message(paste0("Loaded ", nrow(scores), " cities"))

# =============================================================================
# BUILD APP
# =============================================================================

# Build UI
ui <- build_ui(cities)

# Build Server
server <- build_server(data, scores, cities, cache_time)

# =============================================================================
# RUN APP
# =============================================================================

message("Starting Shiny app...")
shinyApp(ui, server)
