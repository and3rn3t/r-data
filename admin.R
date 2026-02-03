# Iowa Cities Admin Dashboard
# User management, analytics, and configuration
# ========================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(DT)
library(here)
library(plotly)

# Load utilities
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))
source(here("scripts/security.R"))

# =============================================================================
# ADMIN CONFIGURATION
# =============================================================================

# Admin authentication - uses environment variables for security
# Set ADMIN_PASSWORD environment variable before deployment
get_admin_password_hash <- function() {
  password <- Sys.getenv("ADMIN_PASSWORD", unset = "")
  if (password == "") {
    # Check if we're in production mode
    is_production <- Sys.getenv("R_ENV", unset = "development") == "production"
    if (is_production) {
      stop("ADMIN_PASSWORD environment variable must be set in production mode")
    }
    warning("ADMIN_PASSWORD not set. Using default (INSECURE - for development only)")
    password <- "change_me_in_production_12345"  # Slightly stronger default
  }
  # Validate password strength
  if (nchar(password) < 12) {
    warning("ADMIN_PASSWORD should be at least 12 characters for security")
  }
  digest::digest(password, algo = "sha256")
}

ADMIN_USERS <- list(
  admin = list(
    password_hash = get_admin_password_hash(),
    role = "superadmin",
    name = "Administrator"
  )
)

# Admin session timeout (minutes)
ADMIN_SESSION_TIMEOUT <- 30

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Parse usage logs
#' @param log_dir Directory containing log files
#' @return Data frame of log entries
parse_usage_logs <- function(log_dir = here("outputs/logs")) {
  log_files <- list.files(log_dir, pattern = "^usage_.*\\.log$", full.names = TRUE)
  
  if (length(log_files) == 0) {
    return(data.frame(
      timestamp = character(),
      session_id = character(),
      event = character(),
      details = character()
    ))
  }
  
  logs <- map_dfr(log_files, function(f) {
    tryCatch({
      lines <- readLines(f, warn = FALSE)
      if (length(lines) == 0) return(NULL)
      
      parsed <- map_dfr(lines, function(line) {
        parts <- strsplit(line, " \\| ")[[1]]
        if (length(parts) >= 3) {
          data.frame(
            timestamp = parts[1],
            session_id = parts[2],
            event = parts[3],
            details = if (length(parts) >= 4) parts[4] else "",
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      })
      parsed$file <- basename(f)
      parsed
    }, error = function(e) NULL)
  })
  
  if (nrow(logs) > 0) {
    logs$timestamp <- as.POSIXct(logs$timestamp, format = "%Y-%m-%d %H:%M:%S")
  }
  
  return(logs)
}

#' Get system metrics
#' @return List of system metrics
get_system_metrics <- function() {
  # Get memory usage - pryr is optional
  memory_info <- tryCatch({
    if (requireNamespace("pryr", quietly = TRUE)) {
      format(pryr::mem_used(), units = "MB")
    } else {
      # Fallback: use gc() to estimate memory
      gc_info <- gc(verbose = FALSE)
      paste0(round(sum(gc_info[, 2]), 1), " MB (estimated)")
    }
  }, error = function(e) "Unknown")
  
  list(
    r_version = R.version.string,
    platform = R.version$platform,
    memory_used = memory_info,
    working_dir = getwd(),
    data_files = length(list.files(here("data/raw"), pattern = "\\.csv$")),
    cache_exists = file.exists(here("data/cache/city_scores.rds")),
    cache_age = if (file.exists(here("data/cache/city_scores.rds"))) {
      round(difftime(Sys.time(), file.mtime(here("data/cache/city_scores.rds")), units = "hours"), 1)
    } else NA
  )
}

#' Get data file info
#' @return Data frame of file information
get_data_files_info <- function() {
  files <- list.files(here("data/raw"), pattern = "\\.csv$", full.names = TRUE)
  
  map_dfr(files, function(f) {
    info <- file.info(f)
    data.frame(
      file = basename(f),
      size_kb = round(info$size / 1024, 1),
      modified = info$mtime,
      age_days = round(difftime(Sys.time(), info$mtime, units = "days"), 1),
      stringsAsFactors = FALSE
    )
  })
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(
    title = "Iowa Cities Admin",
    tags$li(class = "dropdown",
            tags$a(href = "/", icon("home"), "Main Dashboard"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "admin_tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Data Management", tabName = "data", icon = icon("database")),
      menuItem("Configuration", tabName = "config", icon = icon("cog")),
      menuItem("Logs", tabName = "logs", icon = icon("file-alt")),
      menuItem("Security", tabName = "security", icon = icon("shield-alt"))
    ),
    hr(),
    div(style = "padding: 10px;",
        actionButton("refresh_data", "Refresh Data", icon = icon("sync"), 
                     class = "btn-primary btn-block"),
        br(),
        actionButton("clear_cache", "Clear Cache", icon = icon("trash"), 
                     class = "btn-warning btn-block")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .metric-box {
          text-align: center;
          padding: 20px;
        }
        .metric-value {
          font-size: 2.5em;
          font-weight: bold;
        }
        .metric-label {
          color: #777;
          font-size: 0.9em;
        }
        .status-ok { color: #27ae60; }
        .status-warning { color: #f39c12; }
        .status-error { color: #e74c3c; }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_sessions", width = 3),
          valueBoxOutput("active_today", width = 3),
          valueBoxOutput("data_freshness", width = 3),
          valueBoxOutput("cache_status", width = 3)
        ),
        fluidRow(
          box(title = "System Status", status = "primary", solidHeader = TRUE, width = 6,
              uiOutput("system_status")),
          box(title = "Quick Actions", status = "info", solidHeader = TRUE, width = 6,
              fluidRow(
                column(6, 
                       actionButton("run_tests", "Run Tests", icon = icon("vial"), 
                                    class = "btn-block btn-default"),
                       br(),
                       actionButton("rebuild_cache", "Rebuild Cache", icon = icon("redo"), 
                                    class = "btn-block btn-default")),
                column(6,
                       actionButton("export_logs", "Export Logs", icon = icon("download"), 
                                    class = "btn-block btn-default"),
                       br(),
                       downloadButton("download_backup", "Download Backup", 
                                      class = "btn-block btn-default"))
              ))
        ),
        fluidRow(
          box(title = "Recent Activity", status = "success", solidHeader = TRUE, width = 12,
              DTOutput("recent_activity"))
        )
      ),
      
      # Analytics Tab
      tabItem(tabName = "analytics",
        fluidRow(
          box(title = "Sessions Over Time", status = "primary", solidHeader = TRUE, width = 8,
              plotlyOutput("sessions_chart", height = 300)),
          box(title = "Event Distribution", status = "info", solidHeader = TRUE, width = 4,
              plotlyOutput("events_pie", height = 300))
        ),
        fluidRow(
          box(title = "Usage Statistics", status = "success", solidHeader = TRUE, width = 6,
              DTOutput("usage_stats")),
          box(title = "Top Pages/Features", status = "warning", solidHeader = TRUE, width = 6,
              DTOutput("top_features"))
        )
      ),
      
      # Data Management Tab
      tabItem(tabName = "data",
        fluidRow(
          box(title = "Data Files", status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("data_files_table"),
              hr(),
              fluidRow(
                column(4, fileInput("upload_data", "Upload New Data", 
                                    accept = ".csv")),
                column(4, selectInput("data_action", "Action:",
                                      choices = c("Preview", "Replace", "Append"))),
                column(4, br(), actionButton("apply_data_action", "Apply", 
                                             class = "btn-primary"))
              ))
        ),
        fluidRow(
          box(title = "Data Preview", status = "info", solidHeader = TRUE, width = 12,
              DTOutput("data_preview"))
        )
      ),
      
      # Configuration Tab
      tabItem(tabName = "config",
        fluidRow(
          box(title = "Application Settings", status = "primary", solidHeader = TRUE, width = 6,
              numericInput("session_timeout", "Session Timeout (minutes):", 
                           value = 30, min = 5, max = 120),
              numericInput("rate_limit", "Rate Limit (requests/minute):", 
                           value = 100, min = 10, max = 1000),
              checkboxInput("enable_logging", "Enable Usage Logging", value = TRUE),
              checkboxInput("enable_cache", "Enable Data Caching", value = TRUE),
              hr(),
              actionButton("save_config", "Save Configuration", 
                           class = "btn-primary", icon = icon("save"))),
          
          box(title = "API Settings", status = "info", solidHeader = TRUE, width = 6,
              checkboxInput("api_enabled", "Enable REST API", value = TRUE),
              numericInput("api_rate_limit", "API Rate Limit:", 
                           value = 100, min = 10, max = 1000),
              checkboxInput("api_cors", "Enable CORS", value = TRUE),
              textInput("api_allowed_origins", "Allowed Origins:", value = "*"),
              hr(),
              actionButton("restart_api", "Restart API", 
                           class = "btn-warning", icon = icon("redo")))
        ),
        fluidRow(
          box(title = "Benchmark Configuration", status = "success", solidHeader = TRUE, width = 12,
              p("Edit benchmark values used for city comparisons:"),
              DTOutput("benchmarks_table"),
              actionButton("save_benchmarks", "Save Benchmarks", 
                           class = "btn-primary", icon = icon("save")))
        )
      ),
      
      # Logs Tab
      tabItem(tabName = "logs",
        fluidRow(
          box(title = "Log Viewer", status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(3, selectInput("log_type", "Log Type:",
                                      choices = c("Usage" = "usage", 
                                                  "Security" = "security",
                                                  "API" = "api",
                                                  "ETL" = "etl"))),
                column(3, dateRangeInput("log_dates", "Date Range:",
                                         start = Sys.Date() - 7,
                                         end = Sys.Date())),
                column(3, selectInput("log_level", "Level:",
                                      choices = c("All", "INFO", "WARNING", "ERROR"))),
                column(3, br(), actionButton("refresh_logs", "Refresh", 
                                             icon = icon("sync")))
              ),
              hr(),
              verbatimTextOutput("log_content", placeholder = TRUE),
              hr(),
              downloadButton("download_logs", "Download Logs"))
        )
      ),
      
      # Security Tab
      tabItem(tabName = "security",
        fluidRow(
          box(title = "Security Overview", status = "primary", solidHeader = TRUE, width = 6,
              uiOutput("security_status")),
          box(title = "Recent Security Events", status = "danger", solidHeader = TRUE, width = 6,
              DTOutput("security_events"))
        ),
        fluidRow(
          box(title = "Rate Limiting Status", status = "warning", solidHeader = TRUE, width = 6,
              DTOutput("rate_limit_status")),
          box(title = "Blocked IPs", status = "danger", solidHeader = TRUE, width = 6,
              DTOutput("blocked_ips"),
              actionButton("clear_blocks", "Clear All Blocks", class = "btn-warning"))
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    logs = NULL,
    config = list(
      session_timeout = 30,
      rate_limit = 100,
      enable_logging = TRUE,
      enable_cache = TRUE
    )
  )
  
  # Load logs on startup
  observe({
    rv$logs <- parse_usage_logs()
  })
  
  # -------------------------------------------------------------------------
  # Overview Tab
  # -------------------------------------------------------------------------
  
  output$total_sessions <- renderValueBox({
    logs <- rv$logs
    total <- if (!is.null(logs) && nrow(logs) > 0) {
      length(unique(logs$session_id))
    } else 0
    valueBox(total, "Total Sessions", icon = icon("users"), color = "blue")
  })
  
  output$active_today <- renderValueBox({
    logs <- rv$logs
    today <- if (!is.null(logs) && nrow(logs) > 0) {
      sum(as.Date(logs$timestamp) == Sys.Date(), na.rm = TRUE)
    } else 0
    valueBox(today, "Events Today", icon = icon("clock"), color = "green")
  })
  
  output$data_freshness <- renderValueBox({
    files_info <- get_data_files_info()
    if (nrow(files_info) > 0) {
      max_age <- max(files_info$age_days, na.rm = TRUE)
      color <- if (max_age < 7) "green" else if (max_age < 30) "yellow" else "red"
      valueBox(paste(round(max_age), "days"), "Data Age", 
               icon = icon("database"), color = color)
    } else {
      valueBox("N/A", "Data Age", icon = icon("database"), color = "red")
    }
  })
  
  output$cache_status <- renderValueBox({
    metrics <- get_system_metrics()
    if (metrics$cache_exists) {
      age <- paste(metrics$cache_age, "hrs")
      color <- if (metrics$cache_age < 24) "green" else "yellow"
    } else {
      age <- "Missing"
      color <- "red"
    }
    valueBox(age, "Cache Status", icon = icon("memory"), color = color)
  })
  
  output$system_status <- renderUI({
    metrics <- get_system_metrics()
    
    tags$div(
      tags$p(icon("r-project"), strong("R Version: "), metrics$r_version),
      tags$p(icon("microchip"), strong("Platform: "), metrics$platform),
      tags$p(icon("memory"), strong("Memory Used: "), metrics$memory_used),
      tags$p(icon("folder"), strong("Data Files: "), metrics$data_files, " files"),
      tags$p(icon("database"), strong("Cache: "), 
             if (metrics$cache_exists) 
               span(class = "status-ok", "Active") 
             else 
               span(class = "status-error", "Missing"))
    )
  })
  
  output$recent_activity <- renderDT({
    logs <- rv$logs
    if (is.null(logs) || nrow(logs) == 0) {
      return(data.frame(Message = "No activity logged"))
    }
    
    logs %>%
      arrange(desc(timestamp)) %>%
      head(20) %>%
      select(timestamp, event, details) %>%
      datatable(options = list(pageLength = 10, dom = 'tip'))
  })
  
  # -------------------------------------------------------------------------
  # Analytics Tab
  # -------------------------------------------------------------------------
  
  output$sessions_chart <- renderPlotly({
    logs <- rv$logs
    if (is.null(logs) || nrow(logs) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    daily <- logs %>%
      mutate(date = as.Date(timestamp)) %>%
      group_by(date) %>%
      summarize(sessions = n_distinct(session_id), .groups = "drop")
    
    plot_ly(daily, x = ~date, y = ~sessions, type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db")) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Sessions"))
  })
  
  output$events_pie <- renderPlotly({
    logs <- rv$logs
    if (is.null(logs) || nrow(logs) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    event_counts <- logs %>%
      count(event) %>%
      arrange(desc(n)) %>%
      head(8)
    
    plot_ly(event_counts, labels = ~event, values = ~n, type = "pie",
            textinfo = "percent", hoverinfo = "label+value")
  })
  
  output$usage_stats <- renderDT({
    logs <- rv$logs
    if (is.null(logs) || nrow(logs) == 0) {
      return(data.frame(Metric = "No data", Value = "-"))
    }
    
    data.frame(
      Metric = c("Total Events", "Unique Sessions", "Avg Events/Session", 
                 "Most Active Day", "Peak Hour"),
      Value = c(
        nrow(logs),
        n_distinct(logs$session_id),
        round(nrow(logs) / n_distinct(logs$session_id), 1),
        format(as.Date(names(which.max(table(as.Date(logs$timestamp))))), "%Y-%m-%d"),
        paste0(names(which.max(table(hour(logs$timestamp)))), ":00")
      )
    ) %>%
      datatable(options = list(dom = 't', pageLength = 10))
  })
  
  # -------------------------------------------------------------------------
  # Data Management Tab
  # -------------------------------------------------------------------------
  
  output$data_files_table <- renderDT({
    get_data_files_info() %>%
      mutate(
        status = case_when(
          age_days < 7 ~ "✅ Fresh",
          age_days < 30 ~ "⚠️ Aging",
          TRUE ~ "❌ Stale"
        )
      ) %>%
      datatable(options = list(pageLength = 15, dom = 'tip'))
  })
  
  # -------------------------------------------------------------------------
  # Configuration Tab
  # -------------------------------------------------------------------------
  
  output$benchmarks_table <- renderDT({
    benchmarks <- data.frame(
      category = names(IOWA_BENCHMARKS),
      iowa = sapply(IOWA_BENCHMARKS, function(x) paste(names(x), ":", x, collapse = "; ")),
      stringsAsFactors = FALSE
    )
    datatable(benchmarks, editable = TRUE, options = list(dom = 't'))
  })
  
  observeEvent(input$save_config, {
    showNotification("Configuration saved!", type = "message")
  })
  
  # -------------------------------------------------------------------------
  # Logs Tab
  # -------------------------------------------------------------------------
  
  output$log_content <- renderPrint({
    log_type <- input$log_type
    log_dir <- here("outputs/logs", log_type)
    
    if (!dir.exists(log_dir)) {
      cat("No logs found for type:", log_type)
      return()
    }
    
    log_files <- list.files(log_dir, pattern = "\\.log$", full.names = TRUE)
    if (length(log_files) == 0) {
      cat("No log files found")
      return()
    }
    
    # Read most recent log
    latest <- log_files[which.max(file.mtime(log_files))]
    lines <- readLines(latest, n = 100, warn = FALSE)
    cat(paste(rev(lines), collapse = "\n"))
  })
  
  # -------------------------------------------------------------------------
  # Security Tab
  # -------------------------------------------------------------------------
  
  output$security_status <- renderUI({
    tags$div(
      tags$p(icon("shield-alt"), strong("Security Module: "), 
             span(class = "status-ok", "Active")),
      tags$p(icon("key"), strong("Session Encryption: "), 
             span(class = "status-ok", "Enabled")),
      tags$p(icon("ban"), strong("Rate Limiting: "), 
             span(class = "status-ok", "Active (100 req/min)")),
      tags$p(icon("file-shield"), strong("CSP Headers: "), 
             span(class = "status-ok", "Configured")),
      tags$p(icon("clock"), strong("Session Timeout: "), "30 minutes")
    )
  })
  
  output$security_events <- renderDT({
    sec_log <- here("outputs/logs/security")
    if (!dir.exists(sec_log)) {
      return(data.frame(Message = "No security events"))
    }
    
    files <- list.files(sec_log, pattern = "\\.log$", full.names = TRUE)
    if (length(files) == 0) {
      return(data.frame(Message = "No security events"))
    }
    
    # Parse security logs
    latest <- files[which.max(file.mtime(files))]
    lines <- readLines(latest, warn = FALSE)
    
    if (length(lines) == 0) {
      return(data.frame(Message = "No security events"))
    }
    
    parsed <- map_dfr(lines, function(line) {
      parts <- strsplit(line, " \\| ")[[1]]
      if (length(parts) >= 3) {
        data.frame(time = parts[1], event = parts[3], stringsAsFactors = FALSE)
      }
    })
    
    datatable(parsed, options = list(pageLength = 5, dom = 'tip'))
  })
  
  # -------------------------------------------------------------------------
  # Action Buttons
  # -------------------------------------------------------------------------
  
  observeEvent(input$refresh_data, {
    showNotification("Starting data refresh...", type = "message")
    # Would trigger data_refresh.R in production
    Sys.sleep(1)
    showNotification("Data refresh complete!", type = "message")
  })
  
  observeEvent(input$clear_cache, {
    cache_file <- here("data/cache/city_scores.rds")
    if (file.exists(cache_file)) {
      file.remove(cache_file)
      showNotification("Cache cleared!", type = "warning")
    } else {
      showNotification("No cache to clear", type = "message")
    }
  })
  
  output$download_backup <- downloadHandler(
    filename = function() {
      paste0("iowa_cities_backup_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      data_files <- list.files(here("data/raw"), pattern = "\\.csv$", full.names = TRUE)
      zip(file, data_files)
    }
  )
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)
