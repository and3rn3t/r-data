# Iowa Cities Master Analysis Script
# This script runs all Iowa city analyses in sequence
# Run this script to perform complete analysis of Iowa city data
# 
# Usage:
#   Rscript scripts/run_all_iowa_analyses.R [options]
#
# Options:
#   --steps=step1,step2    Run only specified steps (by name or number)
#   --exclude=step1,step2  Exclude specified steps
#   --dry-run              Show what would run without executing
#   --resume               Resume from last failed step
#   --parallel             Run independent steps in parallel
#   --verbose              Show detailed output
#   --quiet                Minimal output
#   --log                  Save log to outputs/logs/
#   --validate             Validate data checksums before running
#   --env=development      Use specific config environment
#
# Examples:
#   Rscript scripts/run_all_iowa_analyses.R --steps=housing,crime
#   Rscript scripts/run_all_iowa_analyses.R --exclude=historical --parallel
#   Rscript scripts/run_all_iowa_analyses.R --dry-run --verbose
# ================================================================

library(here)

# =============================================================================
# COMMAND LINE ARGUMENT PARSING
# =============================================================================

#' Parse command line arguments
#' @return List of parsed options
parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  opts <- list(
    steps = NULL,
    exclude = NULL,
    dry_run = FALSE,
    resume = FALSE,
    parallel = FALSE,
    verbose = FALSE,
    quiet = FALSE,
    log = FALSE,
    validate = FALSE,
    env = "default"
  )
  
  for (arg in args) {
    if (grepl("^--steps=", arg)) {
      opts$steps <- strsplit(sub("^--steps=", "", arg), ",")[[1]]
    } else if (grepl("^--exclude=", arg)) {
      opts$exclude <- strsplit(sub("^--exclude=", "", arg), ",")[[1]]
    } else if (arg == "--dry-run") {
      opts$dry_run <- TRUE
    } else if (arg == "--resume") {
      opts$resume <- TRUE
    } else if (arg == "--parallel") {
      opts$parallel <- TRUE
    } else if (arg == "--verbose") {
      opts$verbose <- TRUE
    } else if (arg == "--quiet") {
      opts$quiet <- TRUE
    } else if (arg == "--log") {
      opts$log <- TRUE
    } else if (arg == "--validate") {
      opts$validate <- TRUE
    } else if (grepl("^--env=", arg)) {
      opts$env <- sub("^--env=", "", arg)
    } else if (arg == "--help" || arg == "-h") {
      cat("Usage: Rscript run_all_iowa_analyses.R [options]\n")
      cat("\nOptions:\n")
      cat("  --steps=step1,step2    Run only specified steps\n")
      cat("  --exclude=step1,step2  Exclude specified steps\n")
      cat("  --dry-run              Preview without executing\n")
      cat("  --resume               Resume from last failure\n")
      cat("  --parallel             Run independent steps in parallel\n")
      cat("  --verbose              Detailed output\n")
      cat("  --quiet                Minimal output\n")
      cat("  --log                  Save log to file\n")
      cat("  --validate             Check data checksums\n")
      cat("  --env=environment      Config environment (default/development/production)\n")
      quit(save = "no", status = 0)
    }
  }
  
  return(opts)
}

# Parse arguments early
CLI_OPTS <- parse_args()

# =============================================================================
# LOGGING SETUP
# =============================================================================

#' Initialize logging
#' @param enable Whether to enable file logging
#' @return Path to log file or NULL
init_logging <- function(enable = FALSE) {
  if (!enable) return(NULL)
  
  log_dir <- here("outputs/logs")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0("analysis_", timestamp, ".log"))
  
  # Create log file with header
  cat(paste0("Iowa Cities Analysis Log - ", Sys.time(), "\n"),
      paste0("R Version: ", R.version.string, "\n"),
      paste0("Platform: ", Sys.info()["sysname"], "\n"),
      "=" %>% rep(60) %>% paste(collapse = ""), "\n\n",
      file = log_file)
  
  return(log_file)
}

#' Log message to console and optionally to file
#' @param msg Message to log
#' @param level Log level (INFO, WARN, ERROR, DEBUG)
#' @param log_file Path to log file
log_msg <- function(msg, level = "INFO", log_file = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  formatted <- sprintf("[%s] [%s] %s", timestamp, level, msg)
  
  # Console output based on verbosity
  if (!CLI_OPTS$quiet || level %in% c("ERROR", "WARN")) {
    if (level == "DEBUG" && !CLI_OPTS$verbose) {
      # Skip debug messages unless verbose
    } else {
      cat(formatted, "\n")
    }
  }
  
  # File logging
  if (!is.null(log_file)) {
    cat(formatted, "\n", file = log_file, append = TRUE)
  }
}

LOG_FILE <- init_logging(CLI_OPTS$log)

# =============================================================================
# CONFIGURATION LOADING
# =============================================================================

#' Load project configuration
#' @param env Environment name
#' @return Configuration list
load_config <- function(env = "default") {
  config_path <- here("config.yml")
  
  if (file.exists(config_path)) {
    if (requireNamespace("config", quietly = TRUE)) {
      return(config::get(config = env, file = config_path))
    }
  }
  
  # Default configuration if config package not available
  list(
    project_name = "Iowa Cities Data Analysis",
    data = list(
      raw_dir = "data/raw",
      processed_dir = "data/processed"
    ),
    outputs = list(
      figures_dir = "outputs/figures",
      figure_dpi = 300
    ),
    analysis = list(
      verbose = FALSE
    )
  )
}

CONFIG <- load_config(CLI_OPTS$env)

# =============================================================================
# DISPLAY BANNER
# =============================================================================

if (!CLI_OPTS$quiet) {
  cat("
╔═══════════════════════════════════════════════════════════════╗
║           IOWA CITIES COMPREHENSIVE ANALYSIS                  ║
║                                                               ║
║   This script will run all analyses for Iowa city data        ║
╚═══════════════════════════════════════════════════════════════╝
\n")
  
  if (CLI_OPTS$dry_run) {
    cat("⚠ DRY RUN MODE - No changes will be made\n\n")
  }
  if (CLI_OPTS$parallel) {
    cat("⚡ Parallel execution enabled\n\n")
  }
}

# Load utilities first for helper functions
source(here("scripts/utils.R"))

# Track timing
start_time <- Sys.time()

# =============================================================================
# STEP DEFINITIONS
# =============================================================================

# Step configuration with dependencies and grouping for parallel execution
STEPS <- list(
  list(
    name = "Setup", 
    script = "scripts/00_setup.R", 
    required = TRUE,
    group = 1,  # Must run first
    description = "Install and load required packages"
  ),
  list(
    name = "Data Import", 
    script = "scripts/iowa_cities_import.R", 
    required = TRUE,
    group = 2,  # Depends on Setup
    description = "Import raw data files"
  ),
  list(
    name = "Data Cleaning", 
    script = "scripts/iowa_cities_cleaning.R", 
    required = TRUE,
    group = 3,  # Depends on Import
    description = "Clean and validate data"
  ),
  list(
    name = "Population Analysis", 
    script = "scripts/iowa_cities_analysis.R", 
    required = FALSE,
    group = 4,  # Can run in parallel with other group 4
    description = "Analyze population trends"
  ),
  list(
    name = "Economic Analysis", 
    script = "scripts/iowa_economic_analysis.R", 
    required = FALSE,
    group = 4,
    description = "Economic indicators analysis"
  ),
  list(
    name = "Housing Analysis", 
    script = "scripts/iowa_housing_analysis.R", 
    required = FALSE,
    group = 4,
    description = "Housing market analysis"
  ),
  list(
    name = "Education Analysis", 
    script = "scripts/iowa_education_analysis.R", 
    required = FALSE,
    group = 4,
    description = "Education metrics analysis"
  ),
  list(
    name = "Crime Analysis", 
    script = "scripts/iowa_crime_safety_analysis.R", 
    required = FALSE,
    group = 4,
    description = "Crime and safety analysis"
  ),
  list(
    name = "Demographics Analysis", 
    script = "scripts/iowa_demographics_analysis.R", 
    required = FALSE,
    group = 4,
    description = "Demographics analysis"
  ),
  list(
    name = "Environment Analysis", 
    script = "scripts/iowa_environment_analysis.R", 
    required = FALSE,
    group = 4,
    description = "Environmental metrics analysis"
  ),
  list(
    name = "Historical Analysis", 
    script = "scripts/iowa_historical_analysis.R", 
    required = FALSE,
    group = 4,
    description = "Historical trends analysis"
  ),
  list(
    name = "Comprehensive Analysis", 
    script = "scripts/iowa_comprehensive_analysis.R", 
    required = FALSE,
    group = 5,  # Must run after all domain analyses
    description = "Combined analysis and scoring"
  )
)

# Track results
results <- list()
total_steps <- length(STEPS)

# State file for resume functionality
STATE_FILE <- here("outputs/.analysis_state.rds")

# =============================================================================
# DATA VALIDATION (CHECKSUM)
# =============================================================================

#' Calculate checksum for raw data files
#' @return Named vector of checksums
calculate_checksums <- function() {
  raw_dir <- here(CONFIG$data$raw_dir %||% "data/raw")
  csv_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
  
  checksums <- sapply(csv_files, function(f) {
    digest::digest(file = f, algo = "md5")
  }, USE.NAMES = TRUE)
  
  names(checksums) <- basename(names(checksums))
  checksums
}

#' Validate data checksums against last run
#' @return TRUE if valid or user confirms, FALSE to abort
validate_checksums <- function() {
  checksum_file <- here("outputs/.data_checksums.rds")
  
  if (!requireNamespace("digest", quietly = TRUE)) {
    log_msg("Package 'digest' not installed, skipping checksum validation", "WARN", LOG_FILE)
    return(TRUE)
  }
  
  current <- calculate_checksums()
  
  if (!file.exists(checksum_file)) {
    log_msg("No previous checksums found. Saving current state.", "INFO", LOG_FILE)
    saveRDS(current, checksum_file)
    return(TRUE)
  }
  
  previous <- readRDS(checksum_file)
  
  # Find changes
  all_files <- union(names(current), names(previous))
  changes <- list()
  
  for (f in all_files) {
    if (!(f %in% names(previous))) {
      changes[[f]] <- "NEW"
    } else if (!(f %in% names(current))) {
      changes[[f]] <- "DELETED"
    } else if (current[f] != previous[f]) {
      changes[[f]] <- "MODIFIED"
    }
  }
  
  if (length(changes) > 0) {
    log_msg("Data files have changed since last run:", "WARN", LOG_FILE)
    for (f in names(changes)) {
      log_msg(sprintf("  %s: %s", changes[[f]], f), "WARN", LOG_FILE)
    }
    
    # Update checksums
    saveRDS(current, checksum_file)
  } else {
    log_msg("All data files unchanged since last run.", "DEBUG", LOG_FILE)
  }
  
  return(TRUE)
}

# =============================================================================
# STEP FILTERING
# =============================================================================

#' Filter steps based on CLI options
#' @param steps List of step definitions
#' @return Filtered list of steps
filter_steps <- function(steps) {
  filtered <- steps
  
  # Filter by --steps
  if (!is.null(CLI_OPTS$steps)) {
    filtered <- Filter(function(s) {
      name_lower <- tolower(s$name)
      any(sapply(CLI_OPTS$steps, function(sel) {
        grepl(tolower(sel), name_lower, fixed = TRUE)
      }))
    }, filtered)
  }
  
  # Filter by --exclude
  if (!is.null(CLI_OPTS$exclude)) {
    filtered <- Filter(function(s) {
      name_lower <- tolower(s$name)
      !any(sapply(CLI_OPTS$exclude, function(excl) {
        grepl(tolower(excl), name_lower, fixed = TRUE)
      }))
    }, filtered)
  }
  
  # Resume from last failure
  if (CLI_OPTS$resume && file.exists(STATE_FILE)) {
    state <- readRDS(STATE_FILE)
    if (!is.null(state$last_failed)) {
      log_msg(sprintf("Resuming from step: %s", state$last_failed), "INFO", LOG_FILE)
      found <- FALSE
      filtered <- Filter(function(s) {
        if (found) return(TRUE)
        if (s$name == state$last_failed) {
          found <<- TRUE
          return(TRUE)
        }
        return(FALSE)
      }, filtered)
    }
  }
  
  return(filtered)
}

# =============================================================================
# STEP EXECUTION
# =============================================================================

#' Get memory usage in MB
get_memory_mb <- function() {
  mem <- gc(reset = FALSE, full = FALSE)
  sum(mem[, 2])  # Used memory in MB
}

#' Run a single analysis step with error handling
#' 
#' @param step List with name, script, required fields
#' @param step_num Current step number
#' @param total Total number of steps
#' @return List with success status and any error message
run_step <- function(step, step_num, total = total_steps) {
  if (!CLI_OPTS$quiet) {
    cat(sprintf("\n[%d/%d] %s...\n", step_num, total, step$name))
    cat(rep("-", 50), "\n", sep = "")
    if (CLI_OPTS$verbose && !is.null(step$description)) {
      cat("    ", step$description, "\n")
    }
  }
  
  log_msg(sprintf("Starting: %s", step$name), "INFO", LOG_FILE)
  
  script_path <- here(step$script)
  step_start <- Sys.time()
  mem_before <- get_memory_mb()
  
  # Check if script exists
  if (!file.exists(script_path)) {
    msg <- paste("Script not found:", step$script)
    if (step$required) {
      log_msg(msg, "ERROR", LOG_FILE)
      stop(msg)
    } else {
      if (!CLI_OPTS$quiet) cat("⚠ ", msg, " (skipped)\n", sep = "")
      log_msg(paste(msg, "(skipped)"), "WARN", LOG_FILE)
      return(list(success = FALSE, skipped = TRUE, error = msg, time = 0, memory = 0))
    }
  }
  
  # Dry run mode
  if (CLI_OPTS$dry_run) {
    if (!CLI_OPTS$quiet) cat("  [DRY RUN] Would execute:", step$script, "\n")
    log_msg(sprintf("[DRY RUN] Would execute: %s", step$script), "INFO", LOG_FILE)
    return(list(success = TRUE, skipped = FALSE, error = NULL, time = 0, memory = 0, dry_run = TRUE))
  }
  
  # Run with error handling
  result <- tryCatch({
    source(script_path, local = new.env())
    
    elapsed <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    mem_used <- get_memory_mb() - mem_before
    
    if (!CLI_OPTS$quiet) {
      cat("✓", step$name, "complete")
      if (CLI_OPTS$verbose) {
        cat(sprintf(" (%.1fs, %.1f MB)", elapsed, max(0, mem_used)))
      }
      cat("\n")
    }
    
    log_msg(sprintf("Completed: %s (%.1fs)", step$name, elapsed), "INFO", LOG_FILE)
    
    list(success = TRUE, skipped = FALSE, error = NULL, time = elapsed, memory = mem_used)
  }, error = function(e) {
    elapsed <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
    msg <- paste("Error:", e$message)
    
    log_msg(sprintf("Failed: %s - %s", step$name, e$message), "ERROR", LOG_FILE)
    
    # Save state for resume
    saveRDS(list(last_failed = step$name, timestamp = Sys.time()), STATE_FILE)
    
    if (step$required) {
      stop(paste(step$name, "-", msg))
    } else {
      if (!CLI_OPTS$quiet) cat("✗", step$name, "failed:", e$message, "\n")
      list(success = FALSE, skipped = FALSE, error = msg, time = elapsed, memory = 0)
    }
  })
  
  return(result)
}

#' Run steps in parallel within a group
#' @param steps List of steps to run in parallel
#' @param start_num Starting step number for display
#' @return List of results
run_parallel_group <- function(steps, start_num) {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    log_msg("parallel package not available, running sequentially", "WARN", LOG_FILE)
    results <- list()
    for (i in seq_along(steps)) {
      results[[steps[[i]]$name]] <- run_step(steps[[i]], start_num + i - 1)
    }
    return(results)
  }
  
  if (!CLI_OPTS$quiet) {
    cat("\n⚡ Running", length(steps), "steps in parallel:\n")
    for (s in steps) cat("    •", s$name, "\n")
    cat("\n")
  }
  
  log_msg(sprintf("Running %d steps in parallel", length(steps)), "INFO", LOG_FILE)
  
  # Determine number of cores (use at most half of available, minimum 2)
  n_cores <- min(length(steps), max(2, parallel::detectCores() / 2))
  
  # Create cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl))
  
  # Export required objects to workers
  parallel::clusterExport(cl, c("here", "CLI_OPTS", "LOG_FILE", "get_memory_mb"), 
                          envir = environment())
  
  # Run in parallel
  results_list <- parallel::parLapply(cl, seq_along(steps), function(i) {
    step <- steps[[i]]
    tryCatch({
      step_start <- Sys.time()
      source(here::here(step$script), local = new.env())
      elapsed <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))
      list(name = step$name, success = TRUE, skipped = FALSE, error = NULL, time = elapsed, memory = 0)
    }, error = function(e) {
      list(name = step$name, success = FALSE, skipped = FALSE, error = e$message, time = 0, memory = 0)
    })
  })
  
  # Convert to named list
  results <- list()
  for (r in results_list) {
    results[[r$name]] <- r
    status <- if (r$success) "✓" else "✗"
    if (!CLI_OPTS$quiet) cat(status, r$name, "\n")
  }
  
  return(results)
}

# =============================================================================
# RUN ALL STEPS
# =============================================================================

# Validate checksums if requested
if (CLI_OPTS$validate) {
  validate_checksums()
}

# Filter steps based on CLI options
filtered_steps <- filter_steps(STEPS)
total_steps <- length(filtered_steps)

if (total_steps == 0) {
  log_msg("No steps to run after filtering.", "WARN", LOG_FILE)
  quit(save = "no", status = 0)
}

log_msg(sprintf("Running %d of %d steps", total_steps, length(STEPS)), "INFO", LOG_FILE)

if (CLI_OPTS$parallel) {
  # Group steps by their group number for parallel execution
  groups <- split(filtered_steps, sapply(filtered_steps, function(s) s$group))
  step_num <- 0
  
  for (group_id in sort(as.numeric(names(groups)))) {
    group_steps <- groups[[as.character(group_id)]]
    
    if (length(group_steps) == 1) {
      # Single step, run normally
      step_num <- step_num + 1
      results[[group_steps[[1]]$name]] <- run_step(group_steps[[1]], step_num, total_steps)
    } else {
      # Multiple steps in group, run in parallel
      group_results <- run_parallel_group(group_steps, step_num + 1)
      results <- c(results, group_results)
      step_num <- step_num + length(group_steps)
    }
  }
} else {
  # Sequential execution
  for (i in seq_along(filtered_steps)) {
    results[[filtered_steps[[i]]$name]] <- run_step(filtered_steps[[i]], i, total_steps)
  }
}

# Clear resume state on successful completion
if (file.exists(STATE_FILE) && all(sapply(results, function(r) r$success || r$skipped))) {
  file.remove(STATE_FILE)
}

# =============================================================================
# SUMMARY
# =============================================================================

end_time <- Sys.time()
elapsed <- format_elapsed(start_time)

# Count successes and failures
successes <- sum(sapply(results, function(r) isTRUE(r$success)))
failures <- sum(sapply(results, function(r) !isTRUE(r$success) && !isTRUE(r$skipped)))
skipped <- sum(sapply(results, function(r) isTRUE(r$skipped)))
dry_runs <- sum(sapply(results, function(r) isTRUE(r$dry_run)))

# Calculate total time across steps
total_step_time <- sum(sapply(results, function(r) r$time %||% 0))

if (!CLI_OPTS$quiet) {
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  if (CLI_OPTS$dry_run) {
    cat("║                    DRY RUN COMPLETE                           ║\n")
  } else {
    cat("║                    ANALYSIS COMPLETE                          ║\n")
  }
  cat("╠═══════════════════════════════════════════════════════════════╣\n")
  cat("║                                                               ║\n")
  cat(sprintf("║  ✓ Successful:  %-3d                                          ║\n", successes))
  if (failures > 0) {
    cat(sprintf("║  ✗ Failed:      %-3d                                          ║\n", failures))
  }
  if (skipped > 0) {
    cat(sprintf("║  ⚠ Skipped:     %-3d                                          ║\n", skipped))
  }
  if (dry_runs > 0) {
    cat(sprintf("║  ○ Dry run:     %-3d                                          ║\n", dry_runs))
  }
  cat("║                                                               ║\n")
  
  # Performance metrics (verbose mode)
  if (CLI_OPTS$verbose && !CLI_OPTS$dry_run) {
    cat("║  Step Timing:                                                 ║\n")
    for (name in names(results)) {
      if (!is.null(results[[name]]$time) && results[[name]]$time > 0) {
        display_name <- if (nchar(name) > 30) paste0(substr(name, 1, 27), "...") else name
        cat(sprintf("║    %-32s %6.1fs                  ║\n", display_name, results[[name]]$time))
      }
    }
    cat("║                                                               ║\n")
  }
  
  # List output files
  if (!CLI_OPTS$dry_run) {
    cat("║  Processed Data Files:                                        ║\n")
    processed_files <- list.files(here("data/processed"), pattern = "\\.csv$")
    for (f in head(processed_files, 8)) {
      display_name <- if (nchar(f) > 48) paste0(substr(f, 1, 45), "...") else f
      cat("║    • ", display_name, rep(" ", max(0, 50 - nchar(display_name))), "║\n", sep = "")
    }
    if (length(processed_files) > 8) {
      cat(sprintf("║    ... and %d more files                                    ║\n", 
                  length(processed_files) - 8))
    }
    
    cat("║                                                               ║\n")
    cat("║  Visualization Files:                                         ║\n")
    output_files <- list.files(here("outputs"), pattern = "\\.(png|pdf)$", recursive = TRUE)
    for (f in head(output_files, 6)) {
      display_name <- if (nchar(f) > 48) paste0(substr(f, 1, 45), "...") else f
      cat("║    • ", display_name, rep(" ", max(0, 50 - nchar(display_name))), "║\n", sep = "")
    }
    if (length(output_files) > 6) {
      cat(sprintf("║    ... and %d more files                                    ║\n", 
                  length(output_files) - 6))
    }
  }
  
  cat("║                                                               ║\n")
  cat("╠═══════════════════════════════════════════════════════════════╣\n")
  cat(sprintf("║  Time elapsed: %-44s║\n", elapsed))
  if (CLI_OPTS$log && !is.null(LOG_FILE)) {
    log_rel_path <- sub(here(), "", LOG_FILE, fixed = TRUE)
    cat(sprintf("║  Log file: %-48s║\n", log_rel_path))
  }
  cat("╚═══════════════════════════════════════════════════════════════╝\n")
}

# Print any errors that occurred
if (failures > 0 && !CLI_OPTS$quiet) {
  cat("\n⚠ Failed Steps:\n")
  for (name in names(results)) {
    if (!isTRUE(results[[name]]$success) && !isTRUE(results[[name]]$skipped)) {
      cat("  -", name, ":", results[[name]]$error, "\n")
    }
  }
  cat("\n💡 Tip: Use --resume to continue from the failed step after fixing the issue.\n")
}

# Log final summary
log_msg(sprintf("Analysis complete: %d success, %d failed, %d skipped", 
                successes, failures, skipped), "INFO", LOG_FILE)

if (!CLI_OPTS$quiet && !CLI_OPTS$dry_run) {
  cat("\n📊 Next Steps:\n")
  cat("  1. Review processed data in data/processed/ folder\n")
  cat("  2. Review visualizations in outputs/ folder\n")
  cat("  3. Launch interactive dashboard:\n")
  cat("     shiny::runApp('app.R')\n")
  cat("  4. Generate city reports:\n")
  cat("     source('scripts/generate_reports.R')\n")
  cat("     generate_city_report('Bettendorf')\n")
  cat("  5. Explore interactive maps:\n")
  cat("     source('scripts/iowa_maps.R')\n")
  cat("  6. Get personalized recommendations:\n")
  cat("     source('scripts/iowa_recommendations.R')\n")
  cat("     recommend_for_profile('family')\n")
}

# Return exit code based on failures
if (failures > 0) {
  quit(save = "no", status = 1)
}
