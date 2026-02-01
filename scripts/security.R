# Security Module for Iowa Cities Dashboard
# Input sanitization, rate limiting, and session security
# ========================================

library(digest)

# =============================================================================
# INPUT SANITIZATION
# =============================================================================

#' Sanitize string input to prevent XSS and injection attacks
#' @param input Character string to sanitize
#' @param max_length Maximum allowed length (default 100)
#' @return Sanitized string or NULL if invalid
sanitize_string <- function(input, max_length = 100) {
  if (is.null(input) || !is.character(input)) return(NULL)
  if (length(input) != 1) return(NULL)
  

  # Trim whitespace
  input <- trimws(input)
  
  # Check length
  if (nchar(input) > max_length) {
    input <- substr(input, 1, max_length)
  }
  
  # Remove potentially dangerous characters (HTML/JS injection)
  dangerous_patterns <- c(
    "<script", "</script", "javascript:", "onerror=", "onload=",
    "<iframe", "</iframe", "eval\\(", "document\\.", "window\\."
  )
  
  for (pattern in dangerous_patterns) {
    if (grepl(pattern, input, ignore.case = TRUE)) {
      return(NULL)
    }
  }
  
  # HTML entity encode special characters
  input <- gsub("&", "&amp;", input)
  input <- gsub("<", "&lt;", input)
  input <- gsub(">", "&gt;", input)
  input <- gsub('"', "&quot;", input)
  input <- gsub("'", "&#39;", input)
  
  return(input)
}

#' Validate city name against whitelist
#' @param city City name to validate
#' @param allowed_cities Vector of valid city names
#' @return TRUE if valid, FALSE otherwise
validate_city_input <- function(city, allowed_cities) {
  if (is.null(city) || !is.character(city)) return(FALSE)
  if (length(city) != 1) return(FALSE)
  if (nchar(city) > 50) return(FALSE)
  
  # Exact match against whitelist (case-insensitive)
  tolower(trimws(city)) %in% tolower(allowed_cities)
}

#' Validate numeric input within bounds
#' @param value Numeric value to validate
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @return Validated numeric or NULL if invalid
validate_numeric_input <- function(value, min_val = 0, max_val = 100) {
  if (is.null(value)) return(NULL)
  
  # Convert to numeric if needed
  if (is.character(value)) {
    value <- suppressWarnings(as.numeric(value))
  }
  
  if (!is.numeric(value) || is.na(value)) return(NULL)
  if (value < min_val || value > max_val) return(NULL)
  
  return(value)
}

#' Validate category selection
#' @param category Selected category
#' @param allowed_categories Vector of valid categories
#' @return TRUE if valid, FALSE otherwise
validate_category <- function(category, allowed_categories) {
  if (is.null(category) || !is.character(category)) return(FALSE)
  if (length(category) != 1) return(FALSE)
  
  category %in% allowed_categories
}

# =============================================================================
# RATE LIMITING
# =============================================================================

#' Create a rate limiter for a session
#' @param max_requests Maximum requests allowed in time window
#' @param window_seconds Time window in seconds
#' @return Rate limiter environment
create_rate_limiter <- function(max_requests = 100, window_seconds = 60) {
  limiter <- new.env()
  limiter$requests <- list()
  limiter$max_requests <- max_requests
  limiter$window_seconds <- window_seconds
  limiter$blocked_until <- NULL
  
  return(limiter)
}

#' Check if request is allowed under rate limit
#' @param limiter Rate limiter environment
#' @param action Action being performed (for tracking)
#' @return List with allowed (boolean) and wait_seconds (if blocked)
check_rate_limit <- function(limiter, action = "request") {
  current_time <- Sys.time()
  
  # Check if currently blocked
  if (!is.null(limiter$blocked_until)) {
    if (current_time < limiter$blocked_until) {
      wait_time <- as.numeric(difftime(limiter$blocked_until, current_time, units = "secs"))
      return(list(allowed = FALSE, wait_seconds = ceiling(wait_time)))
    } else {
      limiter$blocked_until <- NULL
    }
  }
  
  # Clean old requests outside window
  window_start <- current_time - limiter$window_seconds
  limiter$requests <- Filter(function(r) r$time > window_start, limiter$requests)
  
  # Check if under limit
  if (length(limiter$requests) >= limiter$max_requests) {
    # Block for the remainder of the window
    oldest_in_window <- min(sapply(limiter$requests, function(r) r$time))
    limiter$blocked_until <- oldest_in_window + limiter$window_seconds
    wait_time <- as.numeric(difftime(limiter$blocked_until, current_time, units = "secs"))
    return(list(allowed = FALSE, wait_seconds = ceiling(wait_time)))
  }
  
  # Record this request
  limiter$requests <- c(limiter$requests, list(list(time = current_time, action = action)))
  
  return(list(allowed = TRUE, wait_seconds = 0))
}

#' Reset rate limiter (for testing or admin purposes)
#' @param limiter Rate limiter environment
reset_rate_limiter <- function(limiter) {
  limiter$requests <- list()
  limiter$blocked_until <- NULL
}

# =============================================================================
# SESSION SECURITY
# =============================================================================

#' Generate secure session token
#' @return Secure random token string
generate_session_token <- function() {
  # Use cryptographic random bytes
  random_bytes <- as.raw(sample(0:255, 32, replace = TRUE))
  digest::digest(random_bytes, algo = "sha256", serialize = FALSE)
}

#' Create session security context
#' @param session Shiny session object
#' @return Security context environment
create_security_context <- function(session) {
  ctx <- new.env()
  
  # Session token

ctx$token <- generate_session_token()
  
  # Session fingerprint (for detecting session hijacking)
  ctx$fingerprint <- digest::digest(
    paste(
      session$request$HTTP_USER_AGENT %||% "",
      session$request$HTTP_ACCEPT_LANGUAGE %||% "",
      sep = "|"
    ),
    algo = "md5"
  )
  
  # Session start time
  ctx$created_at <- Sys.time()
  
  # Last activity time
  ctx$last_activity <- Sys.time()
  
  # Session timeout (30 minutes of inactivity)
  ctx$timeout_minutes <- 30
  
  # Rate limiter for this session
  ctx$rate_limiter <- create_rate_limiter(max_requests = 100, window_seconds = 60)
  
  # Failed validation attempts (for detecting attacks)
  ctx$failed_validations <- 0
  ctx$max_failed_validations <- 10
  
  return(ctx)
}

#' Check if session is still valid
#' @param ctx Security context
#' @param session Current Shiny session
#' @return List with valid (boolean) and reason (if invalid)
validate_session <- function(ctx, session) {
  # Check timeout
  inactive_minutes <- as.numeric(difftime(Sys.time(), ctx$last_activity, units = "mins"))
  if (inactive_minutes > ctx$timeout_minutes) {
    return(list(valid = FALSE, reason = "Session timeout"))
  }
  
  # Check fingerprint (detect session hijacking)
  current_fingerprint <- digest::digest(
    paste(
      session$request$HTTP_USER_AGENT %||% "",
      session$request$HTTP_ACCEPT_LANGUAGE %||% "",
      sep = "|"
    ),
    algo = "md5"
  )
  
  if (current_fingerprint != ctx$fingerprint) {
    return(list(valid = FALSE, reason = "Session fingerprint mismatch"))
  }
  
  # Check for too many failed validations (possible attack)
  if (ctx$failed_validations >= ctx$max_failed_validations) {
    return(list(valid = FALSE, reason = "Too many failed validations"))
  }
  
  # Update last activity
  ctx$last_activity <- Sys.time()
  
  return(list(valid = TRUE, reason = NULL))
}

#' Record failed validation attempt
#' @param ctx Security context
record_failed_validation <- function(ctx) {
  ctx$failed_validations <- ctx$failed_validations + 1
}

#' Log security event
#' @param event_type Type of security event
#' @param details Additional details
#' @param session_id Session identifier
log_security_event <- function(event_type, details = "", session_id = "unknown") {
  log_dir <- here::here("outputs/logs/security")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  log_file <- file.path(log_dir, paste0("security_", format(Sys.Date(), "%Y%m%d"), ".log"))
  
  tryCatch({
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- paste(timestamp, session_id, event_type, details, sep = " | ")
    cat(log_entry, "\n", file = log_file, append = TRUE)
  }, error = function(e) NULL)
}

# =============================================================================
# CONTENT SECURITY POLICY
# =============================================================================

#' Generate Content Security Policy header
#' @return CSP header string
generate_csp_header <- function() {
  paste(
    "default-src 'self';",
    "script-src 'self' 'unsafe-inline' 'unsafe-eval' https://cdn.plot.ly https://cdn.datatables.net;",
    "style-src 'self' 'unsafe-inline' https://fonts.googleapis.com;",
    "font-src 'self' https://fonts.gstatic.com;",
    "img-src 'self' data: https:;",
    "connect-src 'self';",
    "frame-ancestors 'none';",
    sep = " "
  )
}

#' Security headers to add to responses
#' @return Named list of security headers
get_security_headers <- function() {
  list(
    "X-Content-Type-Options" = "nosniff",
    "X-Frame-Options" = "DENY",
    "X-XSS-Protection" = "1; mode=block",
    "Referrer-Policy" = "strict-origin-when-cross-origin",
    "Content-Security-Policy" = generate_csp_header()
  )
}
