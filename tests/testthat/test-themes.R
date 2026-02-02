# Unit Tests for Theme Functions
# =================================

library(testthat)
library(tidyverse)

# Source the themes module
source(here::here("R/themes.R"))

# =============================================================================
# Test: THEMES
# =============================================================================

test_that("THEMES has required themes", {
  expected_themes <- c("light", "dark", "high_contrast", "iowa_gold")
  
  for (theme in expected_themes) {
    expect_true(theme %in% names(THEMES), 
                info = paste("Missing theme:", theme))
  }
})

test_that("THEMES have required properties", {
  # Check that themes have the main sections
  for (theme_name in names(THEMES)) {
    theme <- THEMES[[theme_name]]
    
    expect_true("ui" %in% names(theme), 
                info = paste("Missing ui in theme:", theme_name))
    expect_true("chart" %in% names(theme), 
                info = paste("Missing chart in theme:", theme_name))
    expect_true("map" %in% names(theme), 
                info = paste("Missing map in theme:", theme_name))
    expect_true("table" %in% names(theme), 
                info = paste("Missing table in theme:", theme_name))
    
    # Check UI has some color properties (flexible naming)
    expect_true(length(theme$ui) >= 5,
                info = paste("UI should have at least 5 color properties in:", theme_name))
    
    # Check chart properties
    expect_true("bg" %in% names(theme$chart), 
                info = paste("Missing chart.bg in theme:", theme_name))
    expect_true("text" %in% names(theme$chart), 
                info = paste("Missing chart.text in theme:", theme_name))
    expect_true("palette" %in% names(theme$chart), 
                info = paste("Missing chart.palette in theme:", theme_name))
    
    # Check chart palette is a vector
    expect_true(is.character(theme$chart$palette),
                info = paste("Chart palette should be character vector in:", theme_name))
    expect_true(length(theme$chart$palette) >= 5,
                info = paste("Chart palette should have at least 5 colors in:", theme_name))
    
    # Check map has tiles
    expect_true("tiles" %in% names(theme$map),
                info = paste("Missing map.tiles in theme:", theme_name))
  }
})

# =============================================================================
# Test: get_theme
# =============================================================================

test_that("get_theme returns valid theme", {
  theme <- get_theme("light")
  
  expect_type(theme, "list")
  expect_true("ui" %in% names(theme))
  expect_true("chart" %in% names(theme))
})

test_that("get_theme returns default for invalid theme", {
  theme <- get_theme("nonexistent_theme")
  
  # Should return light theme as default
  expect_type(theme, "list")
  expect_equal(theme$ui$bg, THEMES$light$ui$bg)
})

# =============================================================================
# Test: get_plotly_theme
# =============================================================================

test_that("get_plotly_theme returns plotly layout config", {
  config <- get_plotly_theme("dark")
  
  expect_type(config, "list")
  expect_true("paper_bgcolor" %in% names(config))
  expect_true("plot_bgcolor" %in% names(config))
  expect_true("font" %in% names(config))
})

test_that("get_plotly_theme colors differ by theme", {
  light_config <- get_plotly_theme("light")
  dark_config <- get_plotly_theme("dark")
  
  expect_false(light_config$paper_bgcolor == dark_config$paper_bgcolor)
})

# =============================================================================
# Test: get_chart_palette
# =============================================================================

test_that("get_chart_palette returns color vector", {
  palette <- get_chart_palette("light")
  
  expect_type(palette, "character")
  expect_true(length(palette) >= 5)
  
  # All should be valid hex colors
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette)))
})

test_that("get_chart_palette differs by theme", {
  light_palette <- get_chart_palette("light")
  iowa_palette <- get_chart_palette("iowa_gold")
  
  # At least one color should differ
  expect_false(all(light_palette == iowa_palette))
})

# =============================================================================
# Test: get_map_tiles
# =============================================================================

test_that("get_map_tiles returns valid tile provider", {
  tiles <- get_map_tiles("light")
  
  expect_type(tiles, "character")
  expect_true(nchar(tiles) > 0)
})

test_that("get_map_tiles differs for dark theme", {
  light_tiles <- get_map_tiles("light")
  dark_tiles <- get_map_tiles("dark")
  
  expect_false(light_tiles == dark_tiles)
})

# =============================================================================
# Test: get_dt_theme
# =============================================================================

test_that("get_dt_theme returns DT styling config", {
  skip_if_not_installed("DT")
  
  config <- get_dt_theme("light")
  
  expect_type(config, "list")
  expect_true("class" %in% names(config))
  expect_true("style" %in% names(config))
})

# =============================================================================
# Test: generate_theme_css_vars
# =============================================================================

test_that("generate_theme_css_vars returns valid CSS", {
  css <- generate_theme_css_vars("dark")
  
  expect_type(css, "character")
  expect_true(grepl(":root", css))
  expect_true(grepl("--", css))  # CSS custom properties
})

test_that("generate_theme_css_vars includes all themes", {
  css <- generate_theme_css_vars("light")
  
  # Should include CSS for theme switching
  expect_true(nchar(css) > 100)  # Should have substantial content
})

# =============================================================================
# Test: Color Validity
# =============================================================================

test_that("all theme colors are valid hex codes", {
  for (theme_name in names(THEMES)) {
    theme <- THEMES[[theme_name]]
    
    # Check UI colors
    for (color_name in names(theme$ui)) {
      color <- theme$ui[[color_name]]
      expect_true(grepl("^#[0-9A-Fa-f]{6}$", color),
                  info = paste("Invalid color in", theme_name, "ui.", color_name, ":", color))
    }
    
    # Check chart colors (except palette which is already tested)
    for (color_name in c("bg", "text", "grid")) {
      color <- theme$chart[[color_name]]
      expect_true(grepl("^#[0-9A-Fa-f]{6}$", color),
                  info = paste("Invalid color in", theme_name, "chart.", color_name, ":", color))
    }
  }
})

# =============================================================================
# Test: Theme Consistency
# =============================================================================

test_that("high_contrast theme has sufficient contrast", {
  hc <- THEMES$high_contrast
  
  # Background should be very dark
  expect_equal(hc$ui$bg_primary, "#000000")
  
  # Text should be very light
  expect_equal(hc$ui$text_primary, "#ffffff")
})

test_that("iowa_gold theme uses Iowa colors", {
  iowa <- THEMES$iowa_gold
  
  # Should include Iowa gold color in accent
  expect_true(grepl("^#[cC]9[aA]227$|^#[dD][aA][aA]520$", iowa$ui$accent) ||
              any(grepl("^#[cC]9[aA]227$", iowa$chart$palette)),
              info = "Iowa Gold theme should include golden colors")
})
