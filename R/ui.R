# UI Module for Iowa Cities Dashboard
# Dashboard layout, sidebar, and tab definitions
# ===============================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)

# Source theme system
source(here::here("R/themes.R"))
source(here::here("R/scoring.R"))

# =============================================================================
# UI COMPONENTS
# =============================================================================

#' Create dashboard header with theme selector
create_header <- function() {
  dashboardHeader(
    title = "Iowa Cities Dashboard",
    tags$li(
      class = "dropdown",
      style = "padding: 8px 15px;",
      selectInput(
        "app_theme",
        label = NULL,
        choices = get_theme_choices(),
        selected = "light",
        width = "140px"
      )
    )
  )
}

#' Create dashboard sidebar
create_sidebar <- function(cities) {
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("City Rankings", tabName = "rankings", icon = icon("trophy")),
      menuItem("City Comparison", tabName = "compare", icon = icon("balance-scale")),
      menuItem("City Profiles", tabName = "profiles", icon = icon("city")),
      menuItem("Maps", tabName = "maps", icon = icon("map")),
      menuItem("Recommendations", tabName = "recommend", icon = icon("star")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      hr(),
      div(style = "padding: 10px; color: #888; font-size: 11px;",
          icon("database"), " Data: 2020 Census",
          br(),
          icon("clock"), " Updated: Jan 2026",
          br(),
          uiOutput("data_freshness")
      ),
      div(style = "padding: 10px;",
          downloadButton("download_all_data", "Export All Data", 
                         style = "width: 100%; font-size: 12px;")
      ),
      hr(),
      div(style = "padding: 10px; font-size: 11px; color: #888;",
          icon("keyboard"), " Press ", tags$kbd("?"), " for shortcuts"
      )
    )
  )
}

#' Create custom CSS with theme support
create_theme_css <- function() {
  tags$head(
    # Theme CSS file
    tags$link(rel = "stylesheet", type = "text/css", href = "themes.css"),
    
    # Inline styles for components not covered by CSS variables
    tags$style(HTML("
      /* =============================================
         MOBILE RESPONSIVE STYLES
         ============================================= */
      
      @media screen and (max-width: 768px) {
        .content-wrapper {
          margin-left: 0 !important;
        }
        .main-sidebar {
          transform: translateX(-230px);
          transition: transform 0.3s ease;
        }
        .sidebar-open .main-sidebar {
          transform: translateX(0);
        }
        .box {
          margin-bottom: 10px;
        }
        .value-box {
          min-height: 80px;
        }
        .value-box .icon {
          display: none;
        }
        .value-box-value {
          font-size: 20px !important;
        }
        .value-box-subtitle {
          font-size: 12px !important;
        }
        .dataTables_wrapper {
          overflow-x: auto;
        }
        .keyboard-help {
          display: none !important;
        }
        .plotly {
          max-width: 100%;
          overflow-x: auto;
        }
        .theme-selector {
          display: none;
        }
      }
      
      @media screen and (max-width: 480px) {
        .box-header {
          padding: 8px;
        }
        .box-title {
          font-size: 14px;
        }
        .form-group {
          margin-bottom: 10px;
        }
        .btn {
          width: 100%;
          margin-bottom: 5px;
        }
      }
      
      /* =============================================
         ACCESSIBILITY IMPROVEMENTS
         ============================================= */
      
      a:focus, button:focus, input:focus, select:focus, .btn:focus {
        outline: 3px solid var(--accent-color, #3498db) !important;
        outline-offset: 2px !important;
      }
      
      .skip-link {
        position: absolute;
        top: -40px;
        left: 0;
        background: var(--accent-color, #3498db);
        color: white;
        padding: 8px 16px;
        z-index: 10000;
        transition: top 0.3s;
      }
      .skip-link:focus {
        top: 0;
      }
      
      @media (prefers-contrast: high) {
        .box {
          border: 2px solid #000 !important;
        }
        .btn {
          border: 2px solid #000 !important;
        }
      }
      
      @media (prefers-reduced-motion: reduce) {
        *, *::before, *::after {
          animation-duration: 0.01ms !important;
          animation-iteration-count: 1 !important;
          transition-duration: 0.01ms !important;
        }
      }
      
      @media print {
        .main-sidebar, .main-header, .keyboard-help {
          display: none !important;
        }
        .content-wrapper {
          margin-left: 0 !important;
        }
        .box {
          page-break-inside: avoid;
        }
      }
      
      /* =============================================
         KEYBOARD HELP OVERLAY
         ============================================= */
      
      .keyboard-help {
        position: fixed;
        bottom: 20px;
        right: 20px;
        background: rgba(0,0,0,0.9);
        color: white;
        padding: 15px;
        border-radius: 8px;
        font-size: 12px;
        z-index: 9999;
        display: none;
      }
      .keyboard-help.show {
        display: block;
      }
      kbd {
        background: #333;
        padding: 2px 6px;
        border-radius: 3px;
        border: 1px solid #555;
      }
      
      /* =============================================
         LIFESTYLE MODE SELECTOR
         ============================================= */
      
      .lifestyle-card {
        padding: 10px;
        margin-bottom: 10px;
        border-radius: 8px;
        border: 2px solid var(--border-color, #ddd);
        cursor: pointer;
        transition: all 0.2s ease;
      }
      .lifestyle-card:hover {
        border-color: var(--accent-color, #3498db);
        background: var(--bg-secondary, #f7f7f7);
      }
      .lifestyle-card.selected {
        border-color: var(--accent-color, #3498db);
        background: var(--accent-color, #3498db);
        color: white;
      }
      .lifestyle-card .icon {
        font-size: 24px;
        margin-bottom: 5px;
      }
      .lifestyle-card .name {
        font-weight: bold;
      }
      .lifestyle-card .description {
        font-size: 11px;
        opacity: 0.8;
      }
      
      /* =============================================
         SHARE BUTTON
         ============================================= */
      
      .share-url {
        font-family: monospace;
        font-size: 11px;
        padding: 8px;
        background: var(--bg-secondary, #f7f7f7);
        border-radius: 4px;
        word-break: break-all;
      }
    "))
  )
}

#' Create keyboard navigation JavaScript
create_keyboard_js <- function() {
  tags$script(HTML("
    $(document).ready(function() {
      // ARIA labels for accessibility
      $('.sidebar-menu').attr('role', 'navigation');
      $('.sidebar-menu').attr('aria-label', 'Main navigation');
      $('.small-box, .info-box').each(function() {
        $(this).attr('role', 'status');
        $(this).attr('aria-live', 'polite');
      });
      $('.plotly').attr('role', 'img');
      $('.plotly').attr('aria-label', 'Interactive chart');
      $('.dataTables_wrapper').attr('role', 'region');
      $('.dataTables_wrapper').attr('aria-label', 'Data table');
      $('.sidebar-toggle').attr('aria-label', 'Toggle navigation');
      $('.sidebar-toggle').attr('aria-expanded', 'true');
      
      $(document).on('click', '.sidebar-toggle', function() {
        var expanded = !$('body').hasClass('sidebar-collapse');
        $(this).attr('aria-expanded', expanded);
      });
      
      // Keyboard shortcuts
      $(document).on('keydown', function(e) {
        if ($(e.target).is('input, textarea, select')) return;
        
        var tabs = ['overview', 'rankings', 'compare', 'profiles', 'maps', 'recommend', 'trends'];
        
        switch(e.key) {
          case '1': case '2': case '3': case '4': case '5': case '6': case '7':
            var idx = parseInt(e.key) - 1;
            if (idx < tabs.length) {
              $('a[data-value=\"' + tabs[idx] + '\"]').click();
            }
            break;
          case '?':
            $('.keyboard-help').toggleClass('show');
            break;
          case 'Escape':
            $('.keyboard-help').removeClass('show');
            break;
        }
      });
      
      // Theme persistence in localStorage
      var savedTheme = localStorage.getItem('iowa_dashboard_theme');
      if (savedTheme) {
        Shiny.setInputValue('saved_theme', savedTheme);
      }
    });
    
    // Theme change handler
    Shiny.addCustomMessageHandler('applyTheme', function(theme) {
      document.body.className = document.body.className.replace(/theme-\\w+/g, '');
      document.body.classList.add('theme-' + theme);
      localStorage.setItem('iowa_dashboard_theme', theme);
    });
  "))
}

# =============================================================================
# TAB CONTENT
# =============================================================================

#' Overview tab content
tab_overview <- function() {
  tabItem(tabName = "overview",
    fluidRow(
      valueBoxOutput("total_cities", width = 3),
      valueBoxOutput("total_population", width = 3),
      valueBoxOutput("avg_income", width = 3),
      valueBoxOutput("top_city", width = 3)
    ),
    fluidRow(
      box(title = "Overall City Scores", status = "primary", solidHeader = TRUE, width = 8,
          withSpinner(plotlyOutput("overview_chart", height = 400))),
      box(title = "Score Distribution", status = "info", solidHeader = TRUE, width = 4,
          withSpinner(plotlyOutput("score_distribution", height = 400)))
    )
  )
}

#' Rankings tab content
tab_rankings <- function() {
  tabItem(tabName = "rankings",
    fluidRow(
      box(title = "City Rankings by Category", status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(
            column(8, selectInput("ranking_category", "Select Category:",
                        choices = c("Overall" = "overall_score",
                                    "Safety" = "safety_score",
                                    "Housing" = "housing_score",
                                    "Education" = "education_score",
                                    "Economy" = "economic_score",
                                    "Healthcare" = "healthcare_score",
                                    "Livability" = "livability_score_calc",
                                    "Connectivity" = "connectivity_score",
                                    "Family" = "family_score",
                                    "Climate" = "climate_score",
                                    "Senior Living" = "senior_score",
                                    "Pet-Friendly" = "pet_score"))),
            column(4, br(), downloadButton("download_rankings", "Download Rankings", class = "btn-info"))
          ),
          withSpinner(DTOutput("rankings_table")))
    )
  )
}

#' Comparison tab content
tab_compare <- function(cities) {
  tabItem(tabName = "compare",
    fluidRow(
      box(title = "Select Cities to Compare", status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(
            column(4, selectInput("city1", "City 1:", choices = cities, selected = cities[1])),
            column(4, selectInput("city2", "City 2:", choices = cities, selected = cities[2])),
            column(4, selectInput("city3", "City 3:", choices = c("None", cities), selected = "None"))
          ),
          checkboxInput("compare_to_state_avg", "Include Iowa State Average", value = FALSE))
    ),
    fluidRow(
      box(title = "Radar Comparison", status = "info", solidHeader = TRUE, width = 6,
          withSpinner(plotlyOutput("radar_chart", height = 400))),
      box(title = "Side-by-Side Metrics", status = "success", solidHeader = TRUE, width = 6,
          withSpinner(DTOutput("comparison_table")))
    ),
    fluidRow(
      box(title = "Category Winners", status = "warning", solidHeader = TRUE, width = 12,
          uiOutput("category_winners"))
    )
  )
}

#' Profiles tab content
tab_profiles <- function(cities) {
  tabItem(tabName = "profiles",
    fluidRow(
      box(title = "City Profile", status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(
            column(6, selectInput("profile_city", "Select City:", choices = cities, selected = cities[1])),
            column(6, 
                   br(),
                   downloadButton("download_profile_pdf", "Download PDF", class = "btn-success"),
                   downloadButton("download_report", "Download HTML", class = "btn-info"),
                   downloadButton("download_data", "Download CSV", class = "btn-default"))
          ))
    ),
    fluidRow(
      valueBoxOutput("profile_rank", width = 3),
      valueBoxOutput("profile_pop", width = 3),
      valueBoxOutput("profile_income", width = 3),
      valueBoxOutput("profile_safety", width = 3)
    ),
    fluidRow(
      box(title = "Score Breakdown", status = "info", solidHeader = TRUE, width = 6,
          withSpinner(plotlyOutput("profile_scores", height = 300))),
      box(title = "Key Statistics", status = "success", solidHeader = TRUE, width = 6,
          withSpinner(DTOutput("profile_stats")))
    )
  )
}

#' Maps tab content
tab_maps <- function() {
  tabItem(tabName = "maps",
    fluidRow(
      box(title = "Iowa Cities Map", status = "primary", solidHeader = TRUE, width = 12,
          selectInput("map_metric", "Color by:",
                      choices = c("Overall Score" = "overall_score",
                                  "Population" = "population",
                                  "Median Income" = "median_household_income",
                                  "Safety Score" = "safety_score",
                                  "Education Score" = "education_score",
                                  "Healthcare Score" = "healthcare_score",
                                  "Family Score" = "family_score",
                                  "Climate Score" = "climate_score")),
          withSpinner(plotlyOutput("city_map", height = 500)))
    )
  )
}

#' Recommendations tab content with lifestyle mode selector
tab_recommend <- function() {
  tabItem(tabName = "recommend",
    fluidRow(
      box(title = "Choose Your Lifestyle", status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(
            column(12,
              selectInput("lifestyle_mode", "Lifestyle Mode:",
                          choices = get_lifestyle_choices(),
                          selected = "balanced",
                          width = "100%"),
              uiOutput("lifestyle_description")
            )
          )
      )
    ),
    fluidRow(
      box(title = "Fine-Tune Your Priorities", status = "info", solidHeader = TRUE, width = 4,
          collapsible = TRUE, collapsed = FALSE,
          h5("Core Factors"),
          sliderInput("weight_safety", "Safety:", 0, 10, 5),
          sliderInput("weight_housing", "Housing:", 0, 10, 5),
          sliderInput("weight_education", "Education:", 0, 10, 5),
          sliderInput("weight_economy", "Economy/Jobs:", 0, 10, 5),
          sliderInput("weight_healthcare", "Healthcare:", 0, 10, 5),
          sliderInput("weight_livability", "Livability:", 0, 10, 5),
          sliderInput("weight_connectivity", "Connectivity:", 0, 10, 3),
          hr(),
          h5("Lifestyle Factors"),
          sliderInput("weight_family", "Family-Friendly:", 0, 10, 0),
          sliderInput("weight_climate", "Climate:", 0, 10, 0),
          sliderInput("weight_senior", "Senior Living:", 0, 10, 0),
          sliderInput("weight_pet", "Pet-Friendly:", 0, 10, 0),
          hr(),
          actionButton("get_recommendations", "Get Recommendations", class = "btn-primary btn-block"),
          br(), br(),
          actionButton("share_results", "Share Results", class = "btn-default btn-block", icon = icon("share-alt"))
      ),
      box(title = "Your Top 5 Cities", status = "success", solidHeader = TRUE, width = 8,
          withSpinner(DTOutput("recommendations_table")),
          br(),
          uiOutput("top_recommendation"),
          uiOutput("share_url_display")
      )
    )
  )
}

#' Trends tab content
tab_trends <- function(cities) {
  tabItem(tabName = "trends",
    fluidRow(
      box(title = "Historical Trends", status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(
            column(6, selectInput("trend_city", "Select City:", choices = cities, selected = cities[1])),
            column(6, selectInput("trend_metric", "Select Metric:",
                      choices = c("Population" = "population",
                                  "Home Value" = "home_value",
                                  "Income" = "income",
                                  "Unemployment" = "unemployment")))
          ))
    ),
    fluidRow(
      box(title = "Trend Chart", status = "info", solidHeader = TRUE, width = 8,
          withSpinner(plotlyOutput("trend_chart", height = 400))),
      box(title = "Growth Summary", status = "success", solidHeader = TRUE, width = 4,
          uiOutput("trend_summary"))
    )
  )
}

# =============================================================================
# MAIN UI BUILDER
# =============================================================================

#' Build complete dashboard UI
#' 
#' @param cities Character vector of city names
#' @return dashboardPage object
#' @export
build_ui <- function(cities) {
  dashboardPage(
    header = create_header(),
    sidebar = create_sidebar(cities),
    body = dashboardBody(
      useShinyjs(),
      create_theme_css(),
      create_keyboard_js(),
      
      # Skip to content link for accessibility
      tags$a(href = "#main-content", class = "skip-link", "Skip to main content"),
      
      # Main content wrapper
      tags$div(id = "main-content",
        # Keyboard help overlay
        div(class = "keyboard-help",
            tags$h4("Keyboard Shortcuts"),
            tags$p(tags$kbd("1-7"), " Switch tabs"),
            tags$p(tags$kbd("?"), " Show/hide this help"),
            tags$p(tags$kbd("Esc"), " Close help")
        ),
        
        tabItems(
          tab_overview(),
          tab_rankings(),
          tab_compare(cities),
          tab_profiles(cities),
          tab_maps(),
          tab_recommend(),
          tab_trends(cities)
        )
      )
    )
  )
}

cat("✓ UI module loaded\n")
