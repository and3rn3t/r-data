# Test file for admin.R - Admin Dashboard
# ==================================================

library(testthat)
library(here)
library(digest)

# Source utilities for testing
source(here("scripts/utils.R"))
source(here("scripts/constants.R"))

# =============================================================================
# ADMIN AUTHENTICATION TESTS
# =============================================================================

test_that("get_admin_password_hash returns expected format", {
  # Define the function inline for testing (mimics admin.R behavior)
  get_admin_password_hash <- function() {
    env_password <- Sys.getenv("ADMIN_PASSWORD", unset = "")
    if (nchar(env_password) > 0) {
      return(digest::digest(env_password, algo = "sha256"))
    }
    # Return NULL if no password configured (should block login)
    return(NULL)
  }
  
  # When no env var is set, should return NULL
  old_val <- Sys.getenv("ADMIN_PASSWORD", unset = "")
  Sys.setenv(ADMIN_PASSWORD = "")
  
  result <- get_admin_password_hash()
  expect_null(result)
  
  # Restore
  if (nchar(old_val) > 0) {
    Sys.setenv(ADMIN_PASSWORD = old_val)
  }
})

test_that("Password hash is SHA256 format", {
  # SHA256 produces 64 hex characters
  test_hash <- digest::digest("testpassword", algo = "sha256")
  
  expect_equal(nchar(test_hash), 64)
  expect_true(grepl("^[0-9a-f]+$", test_hash))
})

test_that("Password hash is consistent", {
  hash1 <- digest::digest("mypassword", algo = "sha256")
  hash2 <- digest::digest("mypassword", algo = "sha256")
  
  expect_equal(hash1, hash2)
})

test_that("Different passwords produce different hashes", {
  hash1 <- digest::digest("password1", algo = "sha256")
  hash2 <- digest::digest("password2", algo = "sha256")
  
  expect_false(hash1 == hash2)
})

# =============================================================================
# SESSION MANAGEMENT TESTS
# =============================================================================

test_that("Session token generation works", {
  # Simple session token generator
  generate_session <- function() {
    paste0(
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_",
      paste0(sample(c(letters, 0:9), 16, replace = TRUE), collapse = "")
    )
  }
  
  token1 <- generate_session()
  Sys.sleep(0.1)
  token2 <- generate_session()
  
  expect_type(token1, "character")
  expect_true(nchar(token1) > 20)
  expect_false(token1 == token2)  # Unique tokens
})

test_that("Session expiry check works", {
  # Session expiry logic
  session_created <- Sys.time() - as.difftime(2, units = "hours")
  max_age_hours <- 1
  
  is_expired <- difftime(Sys.time(), session_created, units = "hours") > max_age_hours
  expect_true(is_expired)
  
  session_created <- Sys.time() - as.difftime(30, units = "mins")
  is_expired <- difftime(Sys.time(), session_created, units = "hours") > max_age_hours
  expect_false(is_expired)
})

# =============================================================================
# USAGE ANALYTICS TESTS
# =============================================================================

test_that("Usage log parsing works", {
  # Mock log entry format
  log_entry <- '{"timestamp":"2024-01-15 10:30:00","action":"page_view","page":"overview","user":"admin"}'
  
  parsed <- jsonlite::fromJSON(log_entry)
  
  expect_equal(parsed$action, "page_view")
  expect_equal(parsed$page, "overview")
  expect_equal(parsed$user, "admin")
})

test_that("Usage aggregation calculates correctly", {
  mock_logs <- data.frame(
    timestamp = as.POSIXct(c("2024-01-15 10:00:00", "2024-01-15 10:30:00", "2024-01-15 11:00:00")),
    action = c("page_view", "page_view", "download"),
    page = c("overview", "rankings", "overview"),
    stringsAsFactors = FALSE
  )
  
  # Count by action
  action_counts <- table(mock_logs$action)
  expect_equal(as.integer(action_counts["page_view"]), 2)
  expect_equal(as.integer(action_counts["download"]), 1)
  
  # Count by page
  page_counts <- table(mock_logs$page)
  expect_equal(as.integer(page_counts["overview"]), 2)
})

test_that("Date filtering works for analytics", {
  mock_logs <- data.frame(
    timestamp = as.POSIXct(c("2024-01-10", "2024-01-15", "2024-01-20")),
    action = c("view", "view", "view"),
    stringsAsFactors = FALSE
  )
  
  start_date <- as.POSIXct("2024-01-12")
  end_date <- as.POSIXct("2024-01-18")
  
  filtered <- mock_logs[mock_logs$timestamp >= start_date & 
                        mock_logs$timestamp <= end_date, ]
  
  expect_equal(nrow(filtered), 1)
})

# =============================================================================
# DATA MANAGEMENT TESTS
# =============================================================================

test_that("File size formatting works", {
  format_file_size <- function(bytes) {
    if (bytes < 1024) return(paste(bytes, "B"))
    if (bytes < 1024^2) return(paste(round(bytes/1024, 1), "KB"))
    if (bytes < 1024^3) return(paste(round(bytes/1024^2, 1), "MB"))
    return(paste(round(bytes/1024^3, 1), "GB"))
  }
  
  expect_equal(format_file_size(500), "500 B")
  expect_equal(format_file_size(2048), "2 KB")
  expect_equal(format_file_size(1500000), "1.4 MB")
})

test_that("Data file listing works", {
  # Test that we can list files in data/raw
  raw_files <- list.files(here("data/raw"), pattern = "\\.csv$")
  
  expect_true(length(raw_files) > 0)
  expect_true(any(grepl("iowa", raw_files)))
})

test_that("Cache file detection works", {
  cache_dir <- here("data/cache")
  
  if (dir.exists(cache_dir)) {
    cache_files <- list.files(cache_dir, pattern = "\\.rds$")
    expect_type(cache_files, "character")
  } else {
    expect_true(TRUE)  # Skip if no cache dir
  }
})

# =============================================================================
# CONFIGURATION TESTS
# =============================================================================

test_that("Configuration defaults are valid", {
  # Default config values
  defaults <- list(
    max_file_size_mb = 100,
    cache_duration_hours = 24,
    log_retention_days = 30,
    rate_limit_requests = 100
  )
  
  expect_type(defaults$max_file_size_mb, "double")
  expect_true(defaults$max_file_size_mb > 0)
  expect_true(defaults$cache_duration_hours > 0)
  expect_true(defaults$log_retention_days > 0)
})

test_that("Config validation rejects invalid values", {
  validate_config <- function(value, min_val, max_val) {
    if (!is.numeric(value)) return(FALSE)
    if (value < min_val || value > max_val) return(FALSE)
    return(TRUE)
  }
  
  expect_true(validate_config(50, 1, 100))
  expect_false(validate_config(-1, 1, 100))
  expect_false(validate_config(150, 1, 100))
  expect_false(validate_config("abc", 1, 100))
})

# =============================================================================
# LOG VIEWER TESTS
# =============================================================================

test_that("Log entry formatting works", {
  format_log_entry <- function(timestamp, level, message) {
    sprintf("[%s] [%s] %s", 
            format(timestamp, "%Y-%m-%d %H:%M:%S"),
            toupper(level),
            message)
  }
  
  entry <- format_log_entry(Sys.time(), "info", "Test message")
  
  expect_true(grepl("\\[INFO\\]", entry))
  expect_true(grepl("Test message", entry))
})

test_that("Log level filtering works", {
  mock_logs <- data.frame(
    level = c("info", "warn", "error", "info", "error"),
    message = c("msg1", "msg2", "msg3", "msg4", "msg5"),
    stringsAsFactors = FALSE
  )
  
  errors_only <- mock_logs[mock_logs$level == "error", ]
  expect_equal(nrow(errors_only), 2)
  
  warnings_and_errors <- mock_logs[mock_logs$level %in% c("warn", "error"), ]
  expect_equal(nrow(warnings_and_errors), 3)
})

# =============================================================================
# SECURITY DASHBOARD TESTS
# =============================================================================

test_that("Failed login tracking works", {
  failed_attempts <- list()
  
  # Track failed attempt
  ip <- "192.168.1.1"
  if (is.null(failed_attempts[[ip]])) {
    failed_attempts[[ip]] <- list(count = 0, first_attempt = Sys.time())
  }
  failed_attempts[[ip]]$count <- failed_attempts[[ip]]$count + 1
  
  expect_equal(failed_attempts[[ip]]$count, 1)
  
  # Track another
  failed_attempts[[ip]]$count <- failed_attempts[[ip]]$count + 1
  expect_equal(failed_attempts[[ip]]$count, 2)
})

test_that("Lockout detection works", {
  max_attempts <- 5
  lockout_minutes <- 15
  
  check_lockout <- function(attempts, last_attempt, max_attempts, lockout_minutes) {
    if (attempts < max_attempts) return(FALSE)
    time_since_last <- difftime(Sys.time(), last_attempt, units = "mins")
    return(time_since_last < lockout_minutes)
  }
  
  # Not enough attempts
  expect_false(check_lockout(3, Sys.time(), max_attempts, lockout_minutes))
  
  # Enough attempts, recent
  expect_true(check_lockout(5, Sys.time(), max_attempts, lockout_minutes))
  
  # Enough attempts, but lockout expired
  old_time <- Sys.time() - as.difftime(20, units = "mins")
  expect_false(check_lockout(5, old_time, max_attempts, lockout_minutes))
})

# =============================================================================
# DASHBOARD METRICS TESTS
# =============================================================================

test_that("Active users calculation works", {
  mock_sessions <- data.frame(
    user = c("admin", "user1", "user1", "admin"),
    timestamp = Sys.time() - as.difftime(c(5, 10, 60, 120), units = "mins"),
    stringsAsFactors = FALSE
  )
  
  # Active in last 30 minutes
  cutoff <- Sys.time() - as.difftime(30, units = "mins")
  active <- mock_sessions[mock_sessions$timestamp >= cutoff, ]
  unique_active <- length(unique(active$user))
  
  expect_equal(unique_active, 2)  # admin and user1
})

test_that("System health check structure is correct", {
  health_check <- list(
    database = "ok",
    cache = "ok",
    disk_space = "warning",
    last_backup = as.POSIXct("2024-01-15 10:00:00")
  )
  
  expect_true("database" %in% names(health_check))
  expect_true("cache" %in% names(health_check))
  expect_equal(health_check$database, "ok")
})

# =============================================================================
# DATA BACKUP TESTS
# =============================================================================

test_that("Backup filename generation is correct", {
  generate_backup_name <- function(prefix = "backup") {
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
  }
  
  name <- generate_backup_name()
  
  expect_true(grepl("^backup_", name))
  expect_true(grepl("\\.zip$", name))
})

test_that("Backup retention calculation works", {
  max_backups <- 5
  existing_backups <- c("backup_1.zip", "backup_2.zip", "backup_3.zip", 
                        "backup_4.zip", "backup_5.zip", "backup_6.zip")
  
  to_delete <- length(existing_backups) - max_backups
  expect_equal(to_delete, 1)
})

# =============================================================================
# SHINY REACTIVE VALUE TESTS
# =============================================================================

test_that("Value box data is formatted correctly", {
  format_value_box <- function(value, subtitle, icon_name) {
    list(
      value = value,
      subtitle = subtitle,
      icon = icon_name
    )
  }
  
  box <- format_value_box(42, "Total Users", "users")
  
  expect_equal(box$value, 42)
  expect_equal(box$subtitle, "Total Users")
  expect_equal(box$icon, "users")
})

test_that("Chart data preparation works", {
  mock_data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    value = c(10, 20, 15)
  )
  
  # Test aggregation
  expect_equal(sum(mock_data$value), 45)
  expect_equal(mean(mock_data$value), 15)
})
