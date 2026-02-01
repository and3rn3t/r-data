# App Tour Configuration for Iowa Cities Dashboard
# Uses cicerone package for interactive guided tours
# ========================================

# Check if cicerone is available
if (!requireNamespace("cicerone", quietly = TRUE)) {
  message("cicerone package not installed. Tour feature will be disabled.")
  message("Install with: install.packages('cicerone')")
}

#' Create the dashboard tour
#' @return A cicerone Cicerone object
create_dashboard_tour <- function() {
  if (!requireNamespace("cicerone", quietly = TRUE)) {
    return(NULL)
  }
  
  cicerone::Cicerone$
    new(
      id = "iowa_dashboard_tour",
      opacity = 0.7,
      padding = 10
    )$
    step(
      el = ".sidebar-menu",
      title = "Navigation Menu",
      description = "Use these tabs to explore different aspects of Iowa cities. Each tab focuses on a specific analysis area.",
      position = "right"
    )$
    step(
      el = "#dark_mode",
      title = "Dark Mode Toggle",
      description = "Toggle dark mode for a more comfortable viewing experience in low light. You can also press 'D' on your keyboard.",
      position = "right"
    )$
    step(
      el = "#data_timestamp",
      title = "Data Freshness",
      description = "This shows when the data was last updated, so you know how current the information is.",
      position = "right"
    )$
    step(
      el = "#export_all_data",
      title = "Export All Data",
      description = "Download all city data and scores as a CSV file for your own analysis.",
      position = "right"
    )$
    step(
      "total_cities",
      title = "Quick Stats",
      description = "These value boxes show key statistics at a glance - total cities analyzed, population, average income, and the top-ranked city.",
      position = "bottom"
    )$
    step(
      "overview_chart",
      title = "City Scores Chart",
      description = "Interactive chart showing overall scores for all cities. Hover over bars for details, click and drag to zoom.",
      position = "top"
    )$
    step(
      el = ".keyboard-help-toggle",
      title = "Keyboard Shortcuts",
      description = "Press '?' anytime to see available keyboard shortcuts for faster navigation.",
      position = "left"
    )
}

#' Initialize tour on first visit
#' @param session Shiny session object
#' @param tour Cicerone tour object
start_tour_if_first_visit <- function(session, tour) {
  if (is.null(tour)) return()
  
  # Check if user has seen the tour before using a cookie
  # For now, we'll trigger based on a button click
  tour$init()
}

#' Attach tour to Shiny app
#' @param tour Cicerone tour object
#' @param input Shiny input object
#' @param session Shiny session object
observe_tour_button <- function(tour, input, session) {
  if (is.null(tour)) return()
  
  observeEvent(input$start_tour, {
    tour$init()$start()
  })
}
