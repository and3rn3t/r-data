# Tests for Security Module
# ========================================

library(testthat)
library(here)

# Load security module
source(here("scripts/security.R"))

# =============================================================================
# SANITIZE STRING TESTS
# =============================================================================

test_that("sanitize_string handles valid input", {
  expect_equal(sanitize_string("Hello"), "Hello")
  expect_equal(sanitize_string("Des Moines"), "Des Moines")
  expect_equal(sanitize_string("  trimmed  "), "trimmed")
})

test_that("sanitize_string rejects NULL and non-character", {
 expect_null(sanitize_string(NULL))
  expect_null(sanitize_string(123))
  expect_null(sanitize_string(c("a", "b")))
})

test_that("sanitize_string truncates long strings", {
  long_string <- paste(rep("a", 150), collapse = "")
  result <- sanitize_string(long_string, max_length = 100)
  expect_equal(nchar(result), 100)
})

test_that("sanitize_string blocks XSS patterns", {
  expect_null(sanitize_string("<script>alert('xss')</script>"))
  expect_null(sanitize_string("javascript:void(0)"))
  expect_null(sanitize_string("<img onerror=alert(1)>"))
  expect_null(sanitize_string("<iframe src='evil'>"))
  expect_null(sanitize_string("document.cookie"))
  expect_null(sanitize_string("window.location"))
})

test_that("sanitize_string encodes special characters", {
  expect_equal(sanitize_string("a < b"), "a &lt; b")
  expect_equal(sanitize_string("a > b"), "a &gt; b")
  expect_equal(sanitize_string("a & b"), "a &amp; b")
  expect_equal(sanitize_string('a "quoted"'), "a &quot;quoted&quot;")
})

# =============================================================================
# VALIDATE CITY INPUT TESTS
# =============================================================================

test_that("validate_city_input accepts valid cities", {
  cities <- c("Des Moines", "Ames", "Iowa City")
  expect_true(validate_city_input("Des Moines", cities))
  expect_true(validate_city_input("Ames", cities))
  expect_true(validate_city_input("  Ames  ", cities))  # Trimmed
})

test_that("validate_city_input is case insensitive", {
  cities <- c("Des Moines", "Ames")
  expect_true(validate_city_input("des moines", cities))
  expect_true(validate_city_input("DES MOINES", cities))
})

test_that("validate_city_input rejects invalid input", {
  cities <- c("Des Moines", "Ames")
  expect_false(validate_city_input("Chicago", cities))
  expect_false(validate_city_input(NULL, cities))
  expect_false(validate_city_input("", cities))
  expect_false(validate_city_input(123, cities))
  expect_false(validate_city_input(c("Ames", "Des Moines"), cities))
})

test_that("validate_city_input rejects overly long input", {
  cities <- c("Des Moines")
  long_city <- paste(rep("a", 100), collapse = "")
  expect_false(validate_city_input(long_city, cities))
})

# =============================================================================
# VALIDATE NUMERIC INPUT TESTS
# =============================================================================

test_that("validate_numeric_input accepts valid numbers", {
  expect_equal(validate_numeric_input(50), 50)
  expect_equal(validate_numeric_input(0), 0)
  expect_equal(validate_numeric_input(100), 100)
})

test_that("validate_numeric_input handles string numbers", {
  expect_equal(validate_numeric_input("50"), 50)
  expect_equal(validate_numeric_input("99.5"), 99.5)
})

test_that("validate_numeric_input rejects out of range", {
  expect_null(validate_numeric_input(-1))
  expect_null(validate_numeric_input(101))
  expect_null(validate_numeric_input(50, min_val = 60, max_val = 100))
})

test_that("validate_numeric_input rejects invalid input", {
  expect_null(validate_numeric_input(NULL))
  expect_null(validate_numeric_input("abc"))
  expect_null(validate_numeric_input(NA))
})

# =============================================================================
# VALIDATE CATEGORY TESTS
# =============================================================================

test_that("validate_category accepts valid categories", {
  categories <- c("overall", "safety", "housing")
  expect_true(validate_category("overall", categories))
  expect_true(validate_category("safety", categories))
})

test_that("validate_category rejects invalid input", {
  categories <- c("overall", "safety")
  expect_false(validate_category("invalid", categories))
  expect_false(validate_category(NULL, categories))
  expect_false(validate_category("", categories))
  expect_false(validate_category(c("a", "b"), categories))
})

# =============================================================================
# RATE LIMITER TESTS
# =============================================================================

test_that("create_rate_limiter creates valid limiter", {
  limiter <- create_rate_limiter()
  expect_true(is.environment(limiter))
  expect_equal(limiter$max_requests, 100)
  expect_equal(limiter$window_seconds, 60)
})

test_that("check_rate_limit allows requests under limit", {
  limiter <- create_rate_limiter(max_requests = 10, window_seconds = 60)
  
  for (i in 1:5) {
    result <- check_rate_limit(limiter, "test")
    expect_true(result$allowed)
  }
})

test_that("check_rate_limit blocks requests over limit", {
  limiter <- create_rate_limiter(max_requests = 3, window_seconds = 60)
  
  # Use up the limit
  for (i in 1:3) {
    check_rate_limit(limiter, "test")
  }
  
  # Next request should be blocked
  result <- check_rate_limit(limiter, "test")
  expect_false(result$allowed)
  expect_true(result$wait_seconds > 0)
})

test_that("reset_rate_limiter clears the limiter", {
  limiter <- create_rate_limiter(max_requests = 2, window_seconds = 60)
  
  check_rate_limit(limiter, "test")
  check_rate_limit(limiter, "test")
  
  reset_rate_limiter(limiter)
  
  result <- check_rate_limit(limiter, "test")
  expect_true(result$allowed)
})

# =============================================================================
# SESSION SECURITY TESTS
# =============================================================================

test_that("generate_session_token creates unique tokens", {
  token1 <- generate_session_token()
  token2 <- generate_session_token()
  
  expect_type(token1, "character")
  expect_equal(nchar(token1), 64)  # SHA256 hex = 64 chars
  expect_false(token1 == token2)
})

# =============================================================================
# SECURITY HEADERS TESTS
# =============================================================================

test_that("get_security_headers returns required headers", {
  headers <- get_security_headers()
  
  expect_type(headers, "list")
  expect_true("X-Content-Type-Options" %in% names(headers))
  expect_true("X-Frame-Options" %in% names(headers))
  expect_true("X-XSS-Protection" %in% names(headers))
  expect_true("Content-Security-Policy" %in% names(headers))
})

test_that("generate_csp_header creates valid CSP", {
  csp <- generate_csp_header()
  
  expect_type(csp, "character")
  expect_true(grepl("default-src", csp))
  expect_true(grepl("script-src", csp))
  expect_true(grepl("frame-ancestors 'none'", csp))
})
