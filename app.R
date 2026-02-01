# Iowa Cities Interactive Dashboard
# Shiny app for exploring Iowa city data
# ========================================

library(shiny)
library(shinydashboard)
library(shinyjs)  # For JavaScript interactions
library(tidyverse)
library(plotly)
library(DT)
library(here)
library(shinycssloaders)  # For loading spinners

# Load project utilities
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))
source(here("scripts/security.R"))  # Security module

# =============================================================================
# LOAD DATA
# =============================================================================

# Use centralized data loading from utils.R
# Define file mapping for all Iowa datasets
IOWA_DATA_FILES <- c(
  crime = "iowa_crime_data.csv",
  housing = "iowa_housing_data.csv",
  education = "iowa_education_data.csv",
  economic = "iowa_economic_data.csv",
  healthcare = "iowa_healthcare_data.csv",
  demographics = "iowa_demographics_data.csv",
  environment = "iowa_environment_data.csv",
  amenities = "iowa_amenities_data.csv",
  historical = "iowa_historical_data.csv",
  major_cities = "iowa_major_cities.csv"
)

load_all_data <- function() {
  load_datasets(IOWA_DATA_FILES)
}

# Try to load cached data first, fall back to computing
load_cached_or_compute <- function() {
  cache_file <- here("data/cache/city_scores.rds")
  
  if (file.exists(cache_file)) {
    cache_info <- tryCatch(
      readRDS(here("data/cache/cache_info.rds")),
      error = function(e) NULL
    )
    
    # Check if cache is less than 24 hours old
    if (!is.null(cache_info) && 
        difftime(Sys.time(), cache_info$created, units = "hours") < 24) {
      message("Loading cached city scores...")
      return(list(
        scores = readRDS(cache_file),
        data = readRDS(here("data/cache/all_datasets.rds"))
      ))
    }
  }
  
  # Fall back to computing
  message("Computing city scores (run cache_city_data.R to speed up)...")
  data <- load_all_data()
  scores <- calculate_scores(data)
  return(list(scores = scores, data = data))
}

data <- load_all_data()
cities <- sort(unique(data$major_cities$city))

# normalize() function is now loaded from utils.R

# Calculate scores for all cities
calculate_scores <- function(data) {
  scores <- data$major_cities %>%
    select(city, county, population = population_2020, latitude, longitude, region) %>%
    left_join(data$crime %>% select(city, violent_crime_rate, property_crime_rate), by = "city") %>%
    left_join(data$housing %>% select(city, median_home_value, owner_occupied_pct), by = "city") %>%
    left_join(data$education %>% select(city, graduation_rate, college_readiness_pct, pct_bachelors), by = "city") %>%
    left_join(data$economic %>% select(city, median_household_income, unemployment_rate, poverty_rate), by = "city") %>%
    left_join(data$healthcare %>% select(city, life_expectancy, health_insurance_coverage_pct), by = "city") %>%
    left_join(data$amenities %>% select(city, livability_score, cost_of_living_index), by = "city") %>%
    mutate(
      safety_score = (normalize(violent_crime_rate, TRUE) + normalize(property_crime_rate, TRUE)) / 2,
      housing_score = (normalize(median_home_value, TRUE) + normalize(owner_occupied_pct)) / 2,
      education_score = (normalize(graduation_rate) + normalize(college_readiness_pct) + normalize(pct_bachelors)) / 3,
      economic_score = (normalize(median_household_income) + normalize(unemployment_rate, TRUE) + normalize(poverty_rate, TRUE)) / 3,
      healthcare_score = (normalize(life_expectancy) + normalize(health_insurance_coverage_pct)) / 2,
      livability_score_calc = normalize(livability_score),
      overall_score = (safety_score + housing_score + education_score + economic_score + healthcare_score + livability_score_calc) / 6
    ) %>%
    arrange(desc(overall_score)) %>%
    mutate(rank = row_number())
  
  return(scores)
}

scores <- calculate_scores(data)

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Iowa Cities Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
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
          icon("clock"), " Updated: Jan 2026"
      ),
      div(style = "padding: 10px;",
          downloadButton("download_all_data", "Export All Data", 
                         style = "width: 100%; font-size: 12px;")
      ),
      hr(),
      div(style = "padding: 10px;",
          checkboxInput("dark_mode", "Dark Mode", value = FALSE)
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),  # Enable shinyjs
    # Skip to content link for accessibility
    tags$a(href = "#main-content", class = "skip-link", "Skip to main content"),
    # Main content wrapper with ID for skip link
    tags$div(id = "main-content",
    # Custom CSS for dark mode
    tags$head(
      tags$style(HTML("
        .dark-mode .content-wrapper,
        .dark-mode .main-footer,
        .dark-mode .box {
          background-color: #1a1a2e !important;
          color: #eee !important;
        }
        .dark-mode .box-header {
          background-color: #16213e !important;
        }
        .dark-mode .sidebar-menu > li > a {
          color: #ccc !important;
        }
        .dark-mode .dataTables_wrapper {
          color: #eee !important;
        }
        .dark-mode table.dataTable tbody tr {
          background-color: #1a1a2e !important;
          color: #eee !important;
        }
        .dark-mode table.dataTable tbody tr:hover {
          background-color: #16213e !important;
        }
        .dark-mode .form-control {
          background-color: #0f3460 !important;
          color: #eee !important;
          border-color: #16213e !important;
        }
        .dark-mode .selectize-input {
          background-color: #0f3460 !important;
          color: #eee !important;
        }
        .keyboard-help {
          position: fixed;
          bottom: 20px;
          right: 20px;
          background: rgba(0,0,0,0.8);
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
           MOBILE RESPONSIVE STYLES
           ============================================= */
        
        /* Mobile-first responsive design */
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
        
        /* Focus indicators for keyboard navigation */
        a:focus, button:focus, input:focus, select:focus, .btn:focus {
          outline: 3px solid #3498db !important;
          outline-offset: 2px !important;
        }
        
        /* Skip to content link */
        .skip-link {
          position: absolute;
          top: -40px;
          left: 0;
          background: #3498db;
          color: white;
          padding: 8px 16px;
          z-index: 10000;
          transition: top 0.3s;
        }
        .skip-link:focus {
          top: 0;
        }
        
        /* High contrast mode support */
        @media (prefers-contrast: high) {
          .box {
            border: 2px solid #000 !important;
          }
          .btn {
            border: 2px solid #000 !important;
          }
        }
        
        /* Reduced motion support */
        @media (prefers-reduced-motion: reduce) {
          *, *::before, *::after {
            animation-duration: 0.01ms !important;
            animation-iteration-count: 1 !important;
            transition-duration: 0.01ms !important;
          }
        }
        
        /* Print styles */
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
      ")),
      # Keyboard navigation and accessibility JavaScript
      tags$script(HTML("
        $(document).ready(function() {
          // ===== ACCESSIBILITY ENHANCEMENTS =====
          
          // Add ARIA labels to navigation
          $('.sidebar-menu').attr('role', 'navigation');
          $('.sidebar-menu').attr('aria-label', 'Main navigation');
          
          // Add ARIA labels to value boxes
          $('.small-box, .info-box').each(function() {
            $(this).attr('role', 'status');
            $(this).attr('aria-live', 'polite');
          });
          
          // Add ARIA labels to interactive charts
          $('.plotly').attr('role', 'img');
          $('.plotly').attr('aria-label', 'Interactive chart');
          
          // Add ARIA labels to data tables
          $('.dataTables_wrapper').attr('role', 'region');
          $('.dataTables_wrapper').attr('aria-label', 'Data table');
          
          // Make sidebar toggle accessible
          $('.sidebar-toggle').attr('aria-label', 'Toggle navigation');
          $('.sidebar-toggle').attr('aria-expanded', 'true');
          
          // Update ARIA on sidebar toggle
          $(document).on('click', '.sidebar-toggle', function() {
            var expanded = !$('body').hasClass('sidebar-collapse');
            $(this).attr('aria-expanded', expanded);
          });
          
          // ===== KEYBOARD SHORTCUTS =====
          $(document).on('keydown', function(e) {
            // Ignore if in input field
            if ($(e.target).is('input, textarea, select')) return;
            
            var tabs = ['overview', 'rankings', 'compare', 'profiles', 'maps', 'recommend', 'trends'];
            
            switch(e.key) {
              case '1': case '2': case '3': case '4': case '5': case '6': case '7':
                var idx = parseInt(e.key) - 1;
                if (idx < tabs.length) {
                  $('a[data-value=\"' + tabs[idx] + '\"]').click();
                }
                break;
              case 'd':
                if (!e.ctrlKey && !e.metaKey) {
                  $('#dark_mode').click();
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
        });
      "))
    ),
    # Keyboard help overlay
    div(class = "keyboard-help",
        tags$h4("Keyboard Shortcuts"),
        tags$p(tags$kbd("1-7"), " Switch tabs"),
        tags$p(tags$kbd("d"), " Toggle dark mode"),
        tags$p(tags$kbd("?"), " Show/hide this help"),
        tags$p(tags$kbd("Esc"), " Close help")
    ),
    tabItems(
      # Overview Tab
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
      ),
      
      # Rankings Tab
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
                                        "Livability" = "livability_score_calc"))),
                column(4, br(), downloadButton("download_rankings", "Download Rankings", class = "btn-info"))
              ),
              withSpinner(DTOutput("rankings_table")))
        )
      ),
      
      # Comparison Tab
      tabItem(tabName = "compare",
        fluidRow(
          box(title = "Select Cities to Compare", status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4, selectInput("city1", "City 1:", choices = cities, selected = "Bettendorf")),
                column(4, selectInput("city2", "City 2:", choices = cities, selected = "Ankeny")),
                column(4, selectInput("city3", "City 3:", choices = c("None", cities), selected = "None"))
              ))
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
      ),
      
      # Profiles Tab
      tabItem(tabName = "profiles",
        fluidRow(
          box(title = "City Profile", status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(8, selectInput("profile_city", "Select City:", choices = cities, selected = "Des Moines")),
                column(4, 
                       br(),
                       downloadButton("download_report", "Download Report", class = "btn-success"),
                       downloadButton("download_data", "Download Data (CSV)", class = "btn-info"))
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
      ),
      
      # Maps Tab
      tabItem(tabName = "maps",
        fluidRow(
          box(title = "Iowa Cities Map", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("map_metric", "Color by:",
                          choices = c("Overall Score" = "overall_score",
                                      "Population" = "population",
                                      "Median Income" = "median_household_income",
                                      "Safety Score" = "safety_score",
                                      "Education Score" = "education_score")),
              withSpinner(plotlyOutput("city_map", height = 500)))
        )
      ),
      
      # Recommendations Tab
      tabItem(tabName = "recommend",
        fluidRow(
          box(title = "What's Important to You?", status = "primary", solidHeader = TRUE, width = 4,
              sliderInput("weight_safety", "Safety:", 1, 10, 5),
              sliderInput("weight_education", "Education:", 1, 10, 5),
              sliderInput("weight_economy", "Economy/Jobs:", 1, 10, 5),
              sliderInput("weight_housing", "Housing Affordability:", 1, 10, 5),
              sliderInput("weight_healthcare", "Healthcare:", 1, 10, 5),
              sliderInput("weight_livability", "Livability/Amenities:", 1, 10, 5),
              actionButton("get_recommendations", "Get Recommendations", class = "btn-primary")
          ),
          box(title = "Your Top 5 Cities", status = "success", solidHeader = TRUE, width = 8,
              withSpinner(DTOutput("recommendations_table")),
              br(),
              uiOutput("top_recommendation"))
        )
      ),
      
      # Trends Tab
      tabItem(tabName = "trends",
        fluidRow(
          box(title = "Historical Trends", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("trend_city", "Select City:", choices = cities, selected = "Des Moines"),
              selectInput("trend_metric", "Select Metric:",
                          choices = c("Population" = "population",
                                      "Home Value" = "home_value",
                                      "Income" = "income",
                                      "Unemployment" = "unemployment")))
        ),
        fluidRow(
          box(title = "Trend Chart", status = "info", solidHeader = TRUE, width = 8,
              withSpinner(plotlyOutput("trend_chart", height = 400))),
          box(title = "Growth Summary", status = "success", solidHeader = TRUE, width = 4,
              uiOutput("trend_summary"))
        )
      )
    )
    )  # Close main-content div
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # -------------------------------------------------------------------------
  # Security Context Initialization
  # -------------------------------------------------------------------------
  
  security_ctx <- create_security_context(session)
  
  # Secure input validation wrapper
  secure_city_input <- function(city) {
    if (!validate_city_input(city, cities)) {
      record_failed_validation(security_ctx)
      log_security_event("INVALID_CITY_INPUT", city %||% "NULL", session_id)
      return(NULL)
    }
    city
  }
  
  # Rate-limited action wrapper
  rate_limited_action <- function(action_name, action_fn) {
    check <- check_rate_limit(security_ctx$rate_limiter, action_name)
    if (!check$allowed) {
      log_security_event("RATE_LIMIT_EXCEEDED", action_name, session_id)
      showNotification(
        paste("Too many requests. Please wait", check$wait_seconds, "seconds."),
        type = "warning"
      )
      return(NULL)
    }
    action_fn()
  }
  
  # -------------------------------------------------------------------------
  # Session Logging
  # -------------------------------------------------------------------------
  
  # Log session start
  session_id <- paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1))
  log_dir <- here("outputs/logs")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  log_file <- file.path(log_dir, paste0("usage_", format(Sys.Date(), "%Y%m%d"), ".log"))
  
  log_event <- function(event, details = "") {
    tryCatch({
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- paste(timestamp, session_id, event, details, sep = " | ")
      cat(log_entry, "\n", file = log_file, append = TRUE)
    }, error = function(e) NULL)
  }
  
  log_event("SESSION_START", paste0("User agent: ", session$request$HTTP_USER_AGENT %||% "Unknown"))
  
  # Log session end

  session$onSessionEnded(function() {
    log_event("SESSION_END", "")
  })
  
  # -------------------------------------------------------------------------
  # Theme Toggle (Dark Mode)
  # -------------------------------------------------------------------------
  
  observeEvent(input$dark_mode, {
    if (input$dark_mode) {
      shinyjs::runjs("document.body.classList.add('dark-mode');")
      log_event("THEME_CHANGE", "dark")
    } else {
      shinyjs::runjs("document.body.classList.remove('dark-mode');")
      log_event("THEME_CHANGE", "light")
    }
  })
  
  # -------------------------------------------------------------------------
  # Input Validation Helpers (Enhanced with Security)
  # -------------------------------------------------------------------------
  
  validate_city <- function(city) {
    # Use security module for validation
    validate_city_input(city, cities)
  }
  
  safe_filter_city <- function(data, city_name) {
    validated <- secure_city_input(city_name)
    if (is.null(validated)) return(data[0, ])
    filter(data, city == validated)
  }
  
  # Allowed categories for dropdown validation
  allowed_categories <- c("overall_score", "safety_score", "housing_score",
                          "education_score", "economic_score", "healthcare_score",
                          "livability_score_calc")
  
  validate_ranking_category <- function(category) {
    validate_category(category, allowed_categories)
  }
  
  # -------------------------------------------------------------------------
  # Error Handling Wrapper
  # -------------------------------------------------------------------------
  
  safe_render <- function(expr, default = NULL, error_msg = "Error loading data") {
    tryCatch(
      expr,
      error = function(e) {
        message("Shiny error: ", e$message)
        default
      }
    )
  }
  
  # -------------------------------------------------------------------------
  # Overview Value Boxes
  # -------------------------------------------------------------------------
  output$total_cities <- renderValueBox({
    valueBox(nrow(scores), "Cities Analyzed", icon = icon("city"), color = "blue")
  })
  
  output$total_population <- renderValueBox({
    pop <- sum(scores$population, na.rm = TRUE)
    valueBox(format(pop, big.mark = ","), "Total Population", icon = icon("users"), color = "green")
  })
  
  output$avg_income <- renderValueBox({
    inc <- mean(scores$median_household_income, na.rm = TRUE)
    valueBox(paste0("$", format(round(inc), big.mark = ",")), "Avg Income", icon = icon("dollar-sign"), color = "yellow")
  })
  
  output$top_city <- renderValueBox({
    top <- scores$city[1]
    valueBox(top, "Top Ranked City", icon = icon("trophy"), color = "purple")
  })
  
  # Overview Chart
  output$overview_chart <- renderPlotly({
    p <- scores %>%
      plot_ly(x = ~reorder(city, overall_score), y = ~overall_score, 
              type = "bar", marker = list(color = ~overall_score, colorscale = "Viridis")) %>%
      layout(xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Overall Score"),
             showlegend = FALSE)
    p
  })
  
  # Score Distribution
  output$score_distribution <- renderPlotly({
    scores %>%
      plot_ly(x = ~overall_score, type = "histogram", nbinsx = 10,
              marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Overall Score"),
             yaxis = list(title = "Count"))
  })
  
  # Rankings Table
  output$rankings_table <- renderDT({
    cat_col <- input$ranking_category
    
    scores %>%
      arrange(desc(.data[[cat_col]])) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, City = city, Region = region, Population = population,
             Score = all_of(cat_col)) %>%
      mutate(Score = round(Score, 1),
             Population = format(Population, big.mark = ",")) %>%
      datatable(options = list(pageLength = 20))
  })
  
  # Comparison Radar Chart
  output$radar_chart <- renderPlotly({
    selected_cities <- c(input$city1, input$city2)
    if (input$city3 != "None") selected_cities <- c(selected_cities, input$city3)
    
    radar_data <- scores %>%
      filter(city %in% selected_cities) %>%
      select(city, safety_score, housing_score, education_score, 
             economic_score, healthcare_score, livability_score_calc)
    
    categories <- c("Safety", "Housing", "Education", "Economy", "Healthcare", "Livability")
    
    plot_ly(type = 'scatterpolar', mode = 'lines+markers', fill = 'toself') %>%
      add_trace(
        r = as.numeric(radar_data[1, 2:7]),
        theta = categories,
        name = radar_data$city[1]
      ) %>%
      add_trace(
        r = as.numeric(radar_data[2, 2:7]),
        theta = categories,
        name = radar_data$city[2]
      ) %>%
      {if (nrow(radar_data) > 2) add_trace(., r = as.numeric(radar_data[3, 2:7]), theta = categories, name = radar_data$city[3]) else .} %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))
  })
  
  # Comparison Table
  output$comparison_table <- renderDT({
    selected_cities <- c(input$city1, input$city2)
    if (input$city3 != "None") selected_cities <- c(selected_cities, input$city3)
    
    comp_data <- scores %>%
      filter(city %in% selected_cities) %>%
      select(city, overall_score, safety_score, housing_score, education_score,
             economic_score, healthcare_score) %>%
      pivot_longer(-city, names_to = "Metric", values_to = "Score") %>%
      pivot_wider(names_from = city, values_from = Score) %>%
      mutate(Metric = str_replace_all(Metric, "_score", "") %>% str_to_title(),
             across(where(is.numeric), ~round(., 1)))
    
    datatable(comp_data, options = list(dom = 't'))
  })
  
  # Category Winners
  output$category_winners <- renderUI({
    selected_cities <- c(input$city1, input$city2)
    if (input$city3 != "None") selected_cities <- c(selected_cities, input$city3)
    
    comp <- scores %>% filter(city %in% selected_cities)
    
    winners <- tibble(
      Category = c("Safety", "Housing", "Education", "Economy", "Healthcare", "Livability", "OVERALL"),
      Winner = c(
        comp$city[which.max(comp$safety_score)],
        comp$city[which.max(comp$housing_score)],
        comp$city[which.max(comp$education_score)],
        comp$city[which.max(comp$economic_score)],
        comp$city[which.max(comp$healthcare_score)],
        comp$city[which.max(comp$livability_score_calc)],
        comp$city[which.max(comp$overall_score)]
      )
    )
    
    tags$table(class = "table table-striped",
      tags$thead(tags$tr(tags$th("Category"), tags$th("Winner"))),
      tags$tbody(
        lapply(1:nrow(winners), function(i) {
          tags$tr(
            tags$td(winners$Category[i]),
            tags$td(tags$strong(winners$Winner[i]))
          )
        })
      )
    )
  })
  
  # Profile Value Boxes
  output$profile_rank <- renderValueBox({
    city_data <- scores %>% filter(city == input$profile_city)
    valueBox(paste0("#", city_data$rank), "Overall Rank", icon = icon("trophy"), color = "purple")
  })
  
  output$profile_pop <- renderValueBox({
    city_data <- scores %>% filter(city == input$profile_city)
    valueBox(format(city_data$population, big.mark = ","), "Population", icon = icon("users"), color = "blue")
  })
  
  output$profile_income <- renderValueBox({
    city_data <- scores %>% filter(city == input$profile_city)
    valueBox(paste0("$", format(city_data$median_household_income, big.mark = ",")), 
             "Median Income", icon = icon("dollar-sign"), color = "green")
  })
  
  output$profile_safety <- renderValueBox({
    city_data <- scores %>% filter(city == input$profile_city)
    valueBox(round(city_data$safety_score, 1), "Safety Score", icon = icon("shield-alt"), color = "yellow")
  })
  
  # Profile Scores Chart
  output$profile_scores <- renderPlotly({
    city_data <- scores %>% filter(city == input$profile_city)
    
    score_df <- tibble(
      Category = c("Safety", "Housing", "Education", "Economy", "Healthcare", "Livability"),
      Score = c(city_data$safety_score, city_data$housing_score, city_data$education_score,
                city_data$economic_score, city_data$healthcare_score, city_data$livability_score_calc)
    )
    
    plot_ly(score_df, x = ~Category, y = ~Score, type = "bar",
            marker = list(color = ~Score, colorscale = "Viridis")) %>%
      layout(yaxis = list(range = c(0, 100)))
  })
  
  # Profile Stats Table with Benchmark Comparisons
  output$profile_stats <- renderDT({
    city <- input$profile_city
    city_scores <- scores %>% filter(city == !!city)
    
    # Get city values
    pop <- city_scores$population
    home_val <- city_scores$median_home_value
    grad_rate <- city_scores$graduation_rate
    unemp <- city_scores$unemployment_rate
    life_exp <- city_scores$life_expectancy
    crime <- city_scores$violent_crime_rate
    
    stats <- tibble(
      Metric = c("Population", "Median Home Value", "Graduation Rate", 
                 "Unemployment Rate", "Life Expectancy", "Violent Crime Rate"),
      Value = c(
        format(pop, big.mark = ","),
        paste0("$", format(home_val, big.mark = ",")),
        paste0(grad_rate, "%"),
        paste0(unemp, "%"),
        paste0(life_exp, " years"),
        round(crime, 1)
      ),
      `Iowa Avg` = c(
        "—",
        paste0("$", format(IOWA_BENCHMARKS$median_home_value, big.mark = ",")),
        paste0(IOWA_BENCHMARKS$high_school_graduation_rate, "%"),
        paste0(IOWA_BENCHMARKS$unemployment_rate, "%"),
        paste0(IOWA_BENCHMARKS$life_expectancy, " yrs"),
        round(IOWA_BENCHMARKS$violent_crime_rate / 100, 1)  # Convert per 100k to per 1k
      ),
      `vs Iowa` = c(
        "—",
        ifelse(home_val < IOWA_BENCHMARKS$median_home_value, "↓ Below", 
               ifelse(home_val > IOWA_BENCHMARKS$median_home_value * 1.1, "↑ Above", "≈ Similar")),
        ifelse(grad_rate > IOWA_BENCHMARKS$high_school_graduation_rate, "↑ Better", 
               ifelse(grad_rate < IOWA_BENCHMARKS$high_school_graduation_rate * 0.95, "↓ Lower", "≈ Similar")),
        ifelse(unemp < IOWA_BENCHMARKS$unemployment_rate, "↑ Better", 
               ifelse(unemp > IOWA_BENCHMARKS$unemployment_rate * 1.1, "↓ Higher", "≈ Similar")),
        ifelse(life_exp > IOWA_BENCHMARKS$life_expectancy, "↑ Better", 
               ifelse(life_exp < IOWA_BENCHMARKS$life_expectancy - 1, "↓ Lower", "≈ Similar")),
        ifelse(crime < IOWA_BENCHMARKS$violent_crime_rate / 100, "↑ Safer", 
               ifelse(crime > IOWA_BENCHMARKS$violent_crime_rate / 100 * 1.2, "↓ Higher", "≈ Similar"))
      )
    )
    
    datatable(stats, options = list(dom = 't', pageLength = 10),
              rownames = FALSE) %>%
      formatStyle('vs Iowa',
                  color = styleEqual(c('↑ Better', '↑ Safer', '↑ Above', '↓ Below', '↓ Lower', '↓ Higher', '≈ Similar'),
                                     c('green', 'green', 'orange', 'green', 'red', 'red', 'gray')))
  })
  
  # City Map
  output$city_map <- renderPlotly({
    metric <- input$map_metric
    
    plot_ly(scores, lat = ~latitude, lon = ~longitude, 
            type = "scattergeo", mode = "markers",
            marker = list(size = ~sqrt(population) / 10, 
                          color = ~.data[[metric]], 
                          colorscale = "Viridis",
                          showscale = TRUE),
            text = ~paste(city, "<br>", metric, ":", round(.data[[metric]], 1)),
            hoverinfo = "text") %>%
      layout(geo = list(
        scope = "usa",
        projection = list(type = "albers usa"),
        center = list(lon = -93.5, lat = 42),
        lonaxis = list(range = c(-97, -90)),
        lataxis = list(range = c(40, 44))
      ))
  })
  
  # Recommendations
  recommendations <- eventReactive(input$get_recommendations, {
    # Log recommendation request
    log_event("RECOMMENDATION_REQUEST", paste0(
      "Weights: safety=", input$weight_safety, 
      " edu=", input$weight_education,
      " econ=", input$weight_economy,
      " housing=", input$weight_housing
    ))
    
    weights <- c(
      safety = input$weight_safety,
      education = input$weight_education,
      economy = input$weight_economy,
      housing = input$weight_housing,
      healthcare = input$weight_healthcare,
      livability = input$weight_livability
    )
    weights <- weights / sum(weights)
    
    scores %>%
      mutate(
        custom_score = safety_score * weights["safety"] +
                       education_score * weights["education"] +
                       economic_score * weights["economy"] +
                       housing_score * weights["housing"] +
                       healthcare_score * weights["healthcare"] +
                       livability_score_calc * weights["livability"]
      ) %>%
      arrange(desc(custom_score)) %>%
      head(5)
  }) %>%
    bindCache(input$weight_safety, input$weight_education, input$weight_economy,
              input$weight_housing, input$weight_healthcare, input$weight_livability)
  
  output$recommendations_table <- renderDT({
    req(input$get_recommendations)
    
    recommendations() %>%
      mutate(Rank = row_number()) %>%
      select(Rank, City = city, Region = region, 
             `Your Score` = custom_score, Population = population) %>%
      mutate(`Your Score` = round(`Your Score`, 1),
             Population = format(Population, big.mark = ",")) %>%
      datatable(options = list(dom = 't'))
  })
  
  output$top_recommendation <- renderUI({
    req(input$get_recommendations)
    top <- recommendations()$city[1]
    score <- round(recommendations()$custom_score[1], 1)
    
    div(class = "alert alert-success",
        tags$h4(icon("star"), " Your Best Match: ", tags$strong(top)),
        tags$p(paste0("Based on your priorities, ", top, " scores ", score, " points."))
    )
  })
  
  # Trends
  output$trend_chart <- renderPlotly({
    city <- input$trend_city
    metric <- input$trend_metric
    
    hist <- data$historical %>% filter(city == !!city)
    
    if (metric == "population") {
      years <- c(1980, 1990, 2000, 2005, 2010, 2015, 2020)
      values <- c(hist$population_1980, hist$population_1990, hist$population_2000,
                  hist$population_2005, hist$population_2010, hist$population_2015, hist$population_2020)
      title <- "Population Trend"
    } else if (metric == "home_value") {
      years <- c(2010, 2015, 2020)
      values <- c(hist$median_home_value_2010, hist$median_home_value_2015, hist$median_home_value_2020)
      title <- "Median Home Value Trend"
    } else if (metric == "income") {
      years <- c(2010, 2015, 2020)
      values <- c(hist$median_income_2010, hist$median_income_2015, hist$median_income_2020)
      title <- "Median Income Trend"
    } else {
      years <- c(2010, 2015, 2020)
      values <- c(hist$unemployment_2010, hist$unemployment_2015, hist$unemployment_2020)
      title <- "Unemployment Rate Trend"
    }
    
    plot_ly(x = years, y = values, type = "scatter", mode = "lines+markers",
            line = list(color = "steelblue", width = 3),
            marker = list(size = 10)) %>%
      layout(title = title, xaxis = list(title = "Year"), yaxis = list(title = ""))
  })
  
  output$trend_summary <- renderUI({
    city <- input$trend_city
    hist <- data$historical %>% filter(city == !!city)
    
    div(
      tags$h4("Growth Summary"),
      tags$ul(
        tags$li(paste0("10-Year Pop Growth: ", hist$population_growth_10yr_pct, "%")),
        tags$li(paste0("20-Year Pop Growth: ", hist$population_growth_20yr_pct, "%")),
        tags$li(paste0("Home Value Growth: ", hist$home_value_growth_10yr_pct, "%")),
        tags$li(paste0("Income Growth: ", hist$income_growth_10yr_pct, "%")),
        tags$li(paste0("Crime Change: ", hist$crime_change_10yr_pct, "%"))
      )
    )
  })
  
  # ===========================================================================
  # DOWNLOAD HANDLERS
  # ===========================================================================
  
  # Get city profile data as reactive
  city_profile_data <- reactive({
    city <- validate_city(input$profile_city, names(cities))
    
    # Log city profile view
    log_event("PROFILE_VIEW", paste0("City: ", city))
    
    # Compile all city data
    crime <- safe_filter_city(data$crime, city)
    housing <- safe_filter_city(data$housing, city)
    education <- safe_filter_city(data$education, city)
    cities_data <- safe_filter_city(data$cities, city)
    
    if (is.null(crime) || is.null(housing) || is.null(education) || is.null(cities_data)) {
      return(NULL)
    }
    
    list(
      city = city,
      crime = crime,
      housing = housing,
      education = education,
      cities = cities_data
    )
  })
  
  # Download CSV data
  output$download_data <- downloadHandler(
    filename = function() {
      city <- input$profile_city
      log_event("DOWNLOAD_CSV", paste0("City: ", city))
      paste0(gsub(" ", "_", tolower(city)), "_profile_", Sys.Date(), ".csv")
    },
    content = function(file) {
      profile <- city_profile_data()
      if (is.null(profile)) {
        write_csv(tibble(error = "No data available"), file)
        return()
      }
      
      # Create summary data frame
      summary_df <- tibble(
        City = profile$city,
        Population = profile$cities$population,
        Region = profile$cities$region,
        Median_Income = profile$cities$median_household_income,
        Median_Home_Value = profile$housing$median_home_value,
        Median_Rent = profile$housing$median_rent,
        Violent_Crime_Rate = profile$crime$violent_crime_rate,
        Property_Crime_Rate = profile$crime$property_crime_rate,
        HS_Graduation_Rate = profile$education$high_school_graduation_rate,
        Bachelors_Degree_Pct = profile$education$pct_bachelors_degree,
        Student_Teacher_Ratio = profile$education$student_teacher_ratio
      )
      
      write_csv(summary_df, file)
    }
  )
  
  # Download HTML report
  output$download_report <- downloadHandler(
    filename = function() {
      city <- input$profile_city
      log_event("DOWNLOAD_REPORT", paste0("City: ", city))
      paste0(gsub(" ", "_", tolower(city)), "_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      profile <- city_profile_data()
      if (is.null(profile)) {
        writeLines("<h1>Error: No data available</h1>", file)
        return()
      }
      
      # Generate simple HTML report
      html_content <- paste0(
        "<!DOCTYPE html>",
        "<html><head><style>",
        "body { font-family: Arial, sans-serif; margin: 40px; }",
        "h1 { color: #2c3e50; }",
        "h2 { color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 5px; }",
        "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
        "th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }",
        "th { background-color: #3498db; color: white; }",
        "tr:nth-child(even) { background-color: #f2f2f2; }",
        ".metric { font-size: 1.2em; color: #27ae60; font-weight: bold; }",
        "</style></head><body>",
        "<h1>", profile$city, " City Profile Report</h1>",
        "<p>Generated: ", format(Sys.Date(), "%B %d, %Y"), "</p>",
        
        "<h2>Demographics</h2>",
        "<table>",
        "<tr><th>Metric</th><th>Value</th></tr>",
        "<tr><td>Population</td><td class='metric'>", format(profile$cities$population, big.mark = ","), "</td></tr>",
        "<tr><td>Region</td><td>", profile$cities$region, "</td></tr>",
        "<tr><td>Median Household Income</td><td class='metric'>$", format(profile$cities$median_household_income, big.mark = ","), "</td></tr>",
        "</table>",
        
        "<h2>Housing</h2>",
        "<table>",
        "<tr><th>Metric</th><th>Value</th></tr>",
        "<tr><td>Median Home Value</td><td class='metric'>$", format(profile$housing$median_home_value, big.mark = ","), "</td></tr>",
        "<tr><td>Median Rent</td><td class='metric'>$", format(profile$housing$median_rent, big.mark = ","), "</td></tr>",
        "</table>",
        
        "<h2>Safety</h2>",
        "<table>",
        "<tr><th>Metric</th><th>Value</th></tr>",
        "<tr><td>Violent Crime Rate (per 1,000)</td><td>", round(profile$crime$violent_crime_rate, 2), "</td></tr>",
        "<tr><td>Property Crime Rate (per 1,000)</td><td>", round(profile$crime$property_crime_rate, 2), "</td></tr>",
        "</table>",
        
        "<h2>Education</h2>",
        "<table>",
        "<tr><th>Metric</th><th>Value</th></tr>",
        "<tr><td>High School Graduation Rate</td><td>", round(profile$education$high_school_graduation_rate, 1), "%</td></tr>",
        "<tr><td>Bachelor's Degree or Higher</td><td>", round(profile$education$pct_bachelors_degree, 1), "%</td></tr>",
        "<tr><td>Student-Teacher Ratio</td><td>", round(profile$education$student_teacher_ratio, 1), "</td></tr>",
        "</table>",
        
        "<hr>",
        "<p><em>Report generated by Iowa Cities Dashboard</em></p>",
        "</body></html>"
      )
      
      writeLines(html_content, file)
    }
  )
  
  # Download all data (bulk export)
  output$download_all_data <- downloadHandler(
    filename = function() {
      log_event("DOWNLOAD_BULK", "All cities data")
      paste0("iowa_cities_complete_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_data <- scores %>%
        select(
          City = city,
          Region = region,
          Population = population,
          Rank = rank,
          Overall_Score = overall_score,
          Safety_Score = safety_score,
          Housing_Score = housing_score,
          Education_Score = education_score,
          Economic_Score = economic_score,
          Healthcare_Score = healthcare_score,
          Livability_Score = livability_score_calc,
          Median_Income = median_household_income,
          Median_Home_Value = median_home_value,
          Graduation_Rate = graduation_rate,
          Unemployment_Rate = unemployment_rate,
          Violent_Crime_Rate = violent_crime_rate
        ) %>%
        mutate(across(where(is.numeric), ~round(., 2)))
      
      write_csv(export_data, file)
    }
  )
  
  # Download rankings
  output$download_rankings <- downloadHandler(
    filename = function() {
      cat_name <- gsub("_score", "", input$ranking_category)
      log_event("DOWNLOAD_RANKINGS", paste0("Category: ", cat_name))
      paste0("iowa_rankings_", cat_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      cat_col <- input$ranking_category
      
      rankings <- scores %>%
        arrange(desc(.data[[cat_col]])) %>%
        mutate(Rank = row_number()) %>%
        select(Rank, City = city, Region = region, Population = population,
               Score = all_of(cat_col)) %>%
        mutate(Score = round(Score, 1))
      
      write_csv(rankings, file)
    }
  )
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)
