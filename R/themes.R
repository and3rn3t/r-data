# Theme System for Iowa Cities Dashboard
# Full theming with Light, Dark, High Contrast, and Iowa Gold themes
# ===================================================================

library(bslib)

# =============================================================================
# THEME DEFINITIONS
# =============================================================================

#' Theme configuration list
#' Each theme includes colors for UI, charts, maps, and tables
THEMES <- list(
  light = list(
    name = "Light",
    description = "Default light theme",
    ui = list(
      bg_primary = "#ffffff",
      bg_secondary = "#f7f7f7",
      bg_box = "#ffffff",
      text_primary = "#333333",
      text_secondary = "#666666",
      text_muted = "#888888",
      border = "#dddddd",
      accent = "#3498db",
      success = "#27ae60",
      warning = "#f39c12",
      danger = "#e74c3c"
    ),
    chart = list(
      bg = "#ffffff",
      grid = "#eeeeee",
      text = "#333333",
      palette = c("#2c7fb8", "#7fcdbb", "#edf8b1", "#c7e9b4", "#253494",
                  "#41b6c4", "#a1dab4", "#225ea8", "#1d91c0", "#0c2c84")
    ),
    map = list(
      tiles = "CartoDB.Positron",
      marker_fill = "#3498db",
      marker_stroke = "#ffffff"
    ),
    table = list(
      bg_header = "#3498db",
      text_header = "#ffffff",
      bg_row_even = "#f9f9f9",
      bg_row_odd = "#ffffff",
      bg_row_hover = "#e8f4fc"
    )
  ),
  
  dark = list(
    name = "Dark",
    description = "Dark theme for low-light environments",
    ui = list(
      bg_primary = "#1a1a2e",
      bg_secondary = "#16213e",
      bg_box = "#1a1a2e",
      text_primary = "#eeeeee",
      text_secondary = "#cccccc",
      text_muted = "#888888",
      border = "#2d3748",
      accent = "#4299e1",
      success = "#48bb78",
      warning = "#ed8936",
      danger = "#fc8181"
    ),
    chart = list(
      bg = "#1a1a2e",
      grid = "#2d3748",
      text = "#eeeeee",
      palette = c("#4299e1", "#48bb78", "#ed8936", "#9f7aea", "#38b2ac",
                  "#f687b3", "#fc8181", "#68d391", "#63b3ed", "#fbb6ce")
    ),
    map = list(
      tiles = "CartoDB.DarkMatter",
      marker_fill = "#4299e1",
      marker_stroke = "#1a1a2e"
    ),
    table = list(
      bg_header = "#2d3748",
      text_header = "#eeeeee",
      bg_row_even = "#16213e",
      bg_row_odd = "#1a1a2e",
      bg_row_hover = "#2d3748"
    )
  ),
  
  high_contrast = list(
    name = "High Contrast",
    description = "High contrast theme for accessibility",
    ui = list(
      bg_primary = "#000000",
      bg_secondary = "#1a1a1a",
      bg_box = "#000000",
      text_primary = "#ffffff",
      text_secondary = "#ffff00",
      text_muted = "#00ffff",
      border = "#ffffff",
      accent = "#00ffff",
      success = "#00ff00",
      warning = "#ffff00",
      danger = "#ff0000"
    ),
    chart = list(
      bg = "#000000",
      grid = "#333333",
      text = "#ffffff",
      palette = c("#00ffff", "#ff00ff", "#ffff00", "#00ff00", "#ff6600",
                  "#0066ff", "#ff0066", "#66ff00", "#6600ff", "#00ff66")
    ),
    map = list(
      tiles = "CartoDB.DarkMatter",
      marker_fill = "#00ffff",
      marker_stroke = "#ffffff"
    ),
    table = list(
      bg_header = "#000000",
      text_header = "#ffff00",
      bg_row_even = "#1a1a1a",
      bg_row_odd = "#000000",
      bg_row_hover = "#333333"
    )
  ),
  
  iowa_gold = list(
    name = "Iowa Gold",
    description = "Inspired by Iowa's golden corn fields",
    ui = list(
      bg_primary = "#fffbf0",
      bg_secondary = "#fff5e0",
      bg_box = "#ffffff",
      text_primary = "#3d2e0a",
      text_secondary = "#5c4a1f",
      text_muted = "#8b7355",
      border = "#e8d5b0",
      accent = "#c9a227",
      success = "#6b8e23",
      warning = "#daa520",
      danger = "#cd5c5c"
    ),
    chart = list(
      bg = "#fffbf0",
      grid = "#e8d5b0",
      text = "#3d2e0a",
      palette = c("#c9a227", "#6b8e23", "#8b4513", "#daa520", "#228b22",
                  "#b8860b", "#556b2f", "#d2691e", "#9acd32", "#f4a460")
    ),
    map = list(
      tiles = "Stamen.Terrain",
      marker_fill = "#c9a227",
      marker_stroke = "#3d2e0a"
    ),
    table = list(
      bg_header = "#c9a227",
      text_header = "#3d2e0a",
      bg_row_even = "#fff5e0",
      bg_row_odd = "#fffbf0",
      bg_row_hover = "#f5e6c8"
    )
  )
)

# =============================================================================
# THEME FUNCTIONS
# =============================================================================

#' Get theme configuration by name
#' 
#' @param theme_name Character. One of "light", "dark", "high_contrast", "iowa_gold"
#' @return List with theme configuration
#' @export
get_theme <- function(theme_name = "light") {
  if (!theme_name %in% names(THEMES)) {
    warning("Unknown theme '", theme_name, "'. Using 'light'.")
    theme_name <- "light"
  }
  THEMES[[theme_name]]
}

#' Get available theme names
#' 
#' @return Character vector of theme names
#' @export
get_theme_names <- function() {
  names(THEMES)
}

#' Get theme choices for selectInput
#' 
#' @return Named vector for use in selectInput choices
#' @export
get_theme_choices <- function() {
  sapply(THEMES, function(t) t$name, USE.NAMES = TRUE) |>
    setNames(names(THEMES)) |>
    (\(x) setNames(names(x), x))()
}

#' Generate CSS variables for a theme
#' 
#' @param theme_name Character. Theme name
#' @return Character string of CSS custom properties
#' @export
generate_theme_css_vars <- function(theme_name) {
  theme <- get_theme(theme_name)
  ui <- theme$ui
  chart <- theme$chart
  table <- theme$table
  
  paste0(
    ":root {\n",
    "  /* UI Colors */\n",
    "  --bg-primary: ", ui$bg_primary, ";\n",
    "  --bg-secondary: ", ui$bg_secondary, ";\n",
    "
  --bg-box: ", ui$bg_box, ";\n",
    "  --text-primary: ", ui$text_primary, ";\n",
    "  --text-secondary: ", ui$text_secondary, ";\n",
    "  --text-muted: ", ui$text_muted, ";\n",
    "  --border-color: ", ui$border, ";\n",
    "  --accent-color: ", ui$accent, ";\n",
    "  --success-color: ", ui$success, ";\n",
    "  --warning-color: ", ui$warning, ";\n",
    "  --danger-color: ", ui$danger, ";\n",
    "\n",
    "  /* Chart Colors */\n",
    "  --chart-bg: ", chart$bg, ";\n",
    "  --chart-grid: ", chart$grid, ";\n",
    "  --chart-text: ", chart$text, ";\n",
    "\n",
    "  /* Table Colors */\n",
    "  --table-header-bg: ", table$bg_header, ";\n",
    "  --table-header-text: ", table$text_header, ";\n",
    "  --table-row-even: ", table$bg_row_even, ";\n",
    "  --table-row-odd: ", table$bg_row_odd, ";\n",
    "  --table-row-hover: ", table$bg_row_hover, ";\n",
    "}\n"
  )
}

#' Get plotly layout for current theme
#' 
#' @param theme_name Character. Theme name
#' @return List suitable for plotly::layout()
#' @export
get_plotly_theme <- function(theme_name = "light") {
  theme <- get_theme(theme_name)
  
  list(
    paper_bgcolor = theme$chart$bg,
    plot_bgcolor = theme$chart$bg,
    font = list(color = theme$chart$text),
    xaxis = list(
      gridcolor = theme$chart$grid,
      linecolor = theme$chart$grid,
      tickfont = list(color = theme$chart$text)
    ),
    yaxis = list(
      gridcolor = theme$chart$grid,
      linecolor = theme$chart$grid,
      tickfont = list(color = theme$chart$text)
    ),
    legend = list(
      font = list(color = theme$chart$text)
    )
  )
}

#' Get chart color palette for current theme
#' 
#' @param theme_name Character. Theme name
#' @param n Integer. Number of colors needed
#' @return Character vector of hex colors
#' @export
get_chart_palette <- function(theme_name = "light", n = 10) {
  theme <- get_theme(theme_name)
  palette <- theme$chart$palette
  
  if (n <= length(palette)) {
    palette[1:n]
  } else {
    colorRampPalette(palette)(n)
  }
}

#' Get leaflet tile provider for current theme
#' 
#' @param theme_name Character. Theme name
#' @return Character string of tile provider name
#' @export
get_map_tiles <- function(theme_name = "light") {
  theme <- get_theme(theme_name)
  theme$map$tiles
}

#' Create DT styling options for current theme
#' 
#' @param theme_name Character. Theme name
#' @return List suitable for DT::datatable() options
#' @export
get_dt_theme <- function(theme_name = "light") {
  theme <- get_theme(theme_name)
  
  list(
    class = paste0("theme-", theme_name),
    style = "bootstrap4",
    options = list(
      initComplete = DT::JS(sprintf(
        "function(settings, json) {
          $(this.api().table().header()).css({
            'background-color': '%s',
            'color': '%s'
          });
        }",
        theme$table$bg_header,
        theme$table$text_header
      ))
    )
  )
}

# =============================================================================
# BSLIB THEME GENERATION
# =============================================================================

#' Generate bslib theme object
#' 
#' @param theme_name Character. Theme name
#' @return bs_theme object
#' @export
get_bslib_theme <- function(theme_name = "light") {
  theme <- get_theme(theme_name)
  ui <- theme$ui
  
  bs_theme(
    bg = ui$bg_primary,
    fg = ui$text_primary,
    primary = ui$accent,
    secondary = ui$text_secondary,
    success = ui$success,
    warning = ui$warning,
    danger = ui$danger,
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto")
  )
}

cat("✓ Theme system loaded\n")
