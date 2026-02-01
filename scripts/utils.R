# Utility Functions
# This script contains helper functions used across multiple scripts
# ================================================================

# =============================================================================
# NORMALIZATION & SCORING FUNCTIONS
# =============================================================================

#' Normalize values to 0-100 scale
#' 
#' @param x Numeric vector to normalize
#' @param reverse If TRUE, higher original values get lower scores
#' @param na.rm Remove NA values (default TRUE)
#' @return Numeric vector scaled 0-100
#' @examples
#' normalize(c(10, 20, 30))
#' normalize(c(100, 200, 300), reverse = TRUE)
normalize <- function(x, reverse = FALSE, na.rm = TRUE) {
  if (all(is.na(x))) return(x)
  min_x <- min(x, na.rm = na.rm)
  max_x <- max(x, na.rm = na.rm)
  
  # Handle case where all values are the same

  if (min_x == max_x) return(rep(50, length(x)))
  
  scaled <- (x - min_x) / (max_x - min_x) * 100
  if (reverse) scaled <- 100 - scaled
  round(scaled, 1)
}

#' Calculate weighted composite score
#' 
#' @param ... Named arguments where names are column values and values are weights
#' @param data Data frame containing the columns
#' @return Numeric vector of composite scores
#' @examples
#' weighted_score(safety = 0.3, housing = 0.3, education = 0.4, data = df)
weighted_score <- function(..., data) {
  args <- list(...)
  total_weight <- sum(unlist(args))
  
  if (abs(total_weight - 1) > 0.001) {
    warning("Weights do not sum to 1. Normalizing weights.")
    args <- lapply(args, function(w) w / total_weight)
  }
  
  result <- rep(0, nrow(data))
  for (col in names(args)) {
    if (col %in% names(data)) {
      result <- result + (data[[col]] * args[[col]])
    }
  }
  round(result, 1)
}

#' Calculate confidence intervals
#'
#' @param x A numeric vector
#' @param conf.level Confidence level (default 0.95)
#' @return A named vector with lower and upper bounds
calculate_ci <- function(x, conf.level = 0.95) {
  x <- na.omit(x)
  if (length(x) < 2) return(c(mean = NA, lower = NA, upper = NA))
  
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf.level
  margin <- qt(1 - alpha/2, df = length(x) - 1) * se
  mean_x <- mean(x)
  
  c(
    mean = mean_x,
    lower = mean_x - margin,
    upper = mean_x + margin
  )
}

#' Standardize variable names
#'
#' @param df A data frame
#' @return A data frame with standardized column names
standardize_names <- function(df) {
  df %>%
    janitor::clean_names() %>%
    rename_with(~str_replace_all(., "\\.", "_"))
}

#' Count missing values by column
#'
#' @param df A data frame
#' @return A data frame with column names and missing counts
count_missing <- function(df) {
  df %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), 
                 names_to = "variable", 
                 values_to = "missing_count") %>%
    mutate(missing_pct = round(missing_count / nrow(df) * 100, 2)) %>%
    arrange(desc(missing_count))
}

# =============================================================================
# FILE I/O FUNCTIONS  
# =============================================================================

#' Safe CSV reader with fallback paths
#' 
#' @param primary_path Primary file path to try
#' @param fallback_path Optional fallback path if primary doesn't exist
#' @param required If TRUE, stop with error if file not found
#' @return Data frame or NULL
#' @examples
#' safe_read_csv(here("data/processed/cities.csv"), here("data/raw/cities.csv"))
safe_read_csv <- function(primary_path, fallback_path = NULL, required = FALSE) {
  if (file.exists(primary_path)) {
    tryCatch({
      read_csv(primary_path, show_col_types = FALSE)
    }, error = function(e) {
      warning("Error reading ", primary_path, ": ", e$message)
      if (!is.null(fallback_path)) safe_read_csv(fallback_path, required = required)
      else if (required) stop("Required file not found: ", primary_path)
      else NULL
    })
  } else if (!is.null(fallback_path) && file.exists(fallback_path)) {
    safe_read_csv(fallback_path, required = required)
  } else if (required) {
    stop("Required file not found: ", primary_path)
  } else {
    NULL
  }
}

#' Load multiple datasets at once
#' 
#' @param file_list Named list of file paths (names become dataset names)
#' @param data_dir Base directory for files
#' @return Named list of data frames
#' @examples
#' load_datasets(c(crime = "iowa_crime_data.csv", housing = "iowa_housing_data.csv"))
load_datasets <- function(file_list, data_dir = here::here("data/raw")) {
  datasets <- list()
  for (name in names(file_list)) {
    path <- file.path(data_dir, file_list[[name]])
    datasets[[name]] <- safe_read_csv(path)
    if (!is.null(datasets[[name]])) {
      cat("  ✓ Loaded", name, ":", nrow(datasets[[name]]), "rows\n")
    } else {
      cat("  ✗ Failed to load", name, "\n")
    }
  }
  datasets
}

#' Safe file saver with directory creation
#' 
#' @param data Data to save
#' @param path File path
#' @param verbose Print confirmation message
safe_write_csv <- function(data, path, verbose = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch({
    write_csv(data, path)
    if (verbose) cat("✓ Saved:", path, "\n")
    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to save ", path, ": ", e$message)
    invisible(FALSE)
  })
}

#' Safe plot saver with directory creation
#' 
#' @param plot ggplot object
#' @param path File path
#' @param width Width in inches (default 10)
#' @param height Height in inches (default 6)
#' @param dpi Resolution (default 300)
#' @param verbose Print confirmation message
safe_ggsave <- function(plot, path, width = 10, height = 6, dpi = 300, verbose = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch({
    ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
    if (verbose) cat("✓ Saved plot:", basename(path), "\n")
    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to save plot ", path, ": ", e$message)
    invisible(FALSE)
  })
}

#' Create a summary table for numeric variables
#'
#' @param df A data frame
#' @param group_var Optional grouping variable (as string)
#' @return A summary data frame
summarise_numeric <- function(df, group_var = NULL) {
  if (!is.null(group_var)) {
    df %>%
      group_by(across(all_of(group_var))) %>%
      summarise(
        across(where(is.numeric), 
               list(
                 n = ~sum(!is.na(.)),
                 mean = ~mean(., na.rm = TRUE),
                 sd = ~sd(., na.rm = TRUE),
                 median = ~median(., na.rm = TRUE),
                 min = ~min(., na.rm = TRUE),
                 max = ~max(., na.rm = TRUE)
               ),
               .names = "{.col}_{.fn}")
      )
  } else {
    df %>%
      summarise(
        across(where(is.numeric), 
               list(
                 n = ~sum(!is.na(.)),
                 mean = ~mean(., na.rm = TRUE),
                 sd = ~sd(., na.rm = TRUE),
                 median = ~median(., na.rm = TRUE),
                 min = ~min(., na.rm = TRUE),
                 max = ~max(., na.rm = TRUE)
               ),
               .names = "{.col}_{.fn}")
      )
  }
}

#' Export multiple data frames to Excel with separate sheets
#'
#' @param data_list A named list of data frames
#' @param file_path Path to save the Excel file
export_to_excel <- function(data_list, file_path) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(data_list, path = file_path)
  cat("Data exported to:", file_path, "\n")
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

#' Create consistent project theme for ggplot
#' 
#' @param base_size Base font size
#' @param legend_position Legend position
#' @return ggplot theme object
theme_iowa <- function(base_size = 12, legend_position = "right") {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "gray40"),
      legend.position = legend_position,
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

#' Project color palettes
#' 
#' @param n Number of colors needed
#' @param palette Palette name: "main", "diverging", "sequential", "status"
#' @return Character vector of hex colors
iowa_colors <- function(n = 5, palette = "main") {
  palettes <- list(
    main = c("#2c7fb8", "#7fcdbb", "#edf8b1", "#c7e9b4", "#253494"),
    diverging = c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850"),
    sequential = c("#f7fcf5", "#c7e9c0", "#74c476", "#31a354", "#006d2c"),
    status = c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027"),  # Good to bad
    regions = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
  )
  
  if (!palette %in% names(palettes)) {
    warning("Unknown palette, using 'main'")
    palette <- "main"
  }
  
  colorRampPalette(palettes[[palette]])(n)
}

#' Create a standard bar chart for city comparisons
#' 
#' @param data Data frame
#' @param x_var Variable for x-axis (unquoted)
#' @param y_var Variable for y-axis (unquoted)
#' @param fill_var Optional fill variable (unquoted)
#' @param title Chart title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @param top_n Show only top N cities
#' @param flip Flip coordinates
#' @return ggplot object
plot_city_bars <- function(data, x_var, y_var, fill_var = NULL, 
                            title = "", x_label = NULL, y_label = NULL,
                            top_n = NULL, flip = TRUE) {
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  fill_var <- enquo(fill_var)
  
  if (!is.null(top_n)) {
    data <- data %>% 
      arrange(desc(!!y_var)) %>% 
      head(top_n)
  }
  
  p <- ggplot(data, aes(x = reorder(!!x_var, !!y_var), y = !!y_var))
  
  if (!quo_is_null(fill_var)) {
    p <- p + geom_col(aes(fill = !!fill_var))
  } else {
    p <- p + geom_col(fill = "#2c7fb8")
  }
  
  if (flip) p <- p + coord_flip()
  
  p + labs(title = title, x = x_label, y = y_label) + theme_iowa()
}

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate Iowa city dataset
#' 
#' @param df Data frame to validate
#' @param required_cols Vector of required column names
#' @param check_cities Validate city names against known list
#' @return List with is_valid (logical) and issues (character vector)
validate_iowa_data <- function(df, required_cols = c("city"), check_cities = TRUE) {
  issues <- character(0)
  
  # Check required columns
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for empty data
  if (nrow(df) == 0) {
    issues <- c(issues, "Dataset has no rows")
  }
  
  # Validate city names if requested
  if (check_cities && "city" %in% names(df)) {
    known_cities <- c("Des Moines", "Cedar Rapids", "Davenport", "Sioux City", 
                      "Iowa City", "Waterloo", "Ames", "West Des Moines", 
                      "Ankeny", "Council Bluffs", "Dubuque", "Urbandale", 
                      "Cedar Falls", "Marion", "Bettendorf", "Mason City", 
                      "Marshalltown", "Clinton", "Burlington", "Fort Dodge")
    
    unknown <- setdiff(df$city, known_cities)
    if (length(unknown) > 0) {
      issues <- c(issues, paste("Unknown cities:", paste(unknown, collapse = ", ")))
    }
  }
  
  # Check for excessive missing values
  missing_pct <- sapply(df, function(x) sum(is.na(x)) / length(x) * 100)
  high_missing <- names(missing_pct[missing_pct > 20])
  if (length(high_missing) > 0) {
    issues <- c(issues, paste("High missing data (>20%):", paste(high_missing, collapse = ", ")))
  }
  
  list(
    is_valid = length(issues) == 0,
    issues = issues
  )
}

#' Check if required datasets exist
#' 
#' @param datasets Character vector of dataset names (without path)
#' @param data_dir Data directory
#' @return Named logical vector
check_datasets_exist <- function(datasets, data_dir = here::here("data/raw")) {
  paths <- file.path(data_dir, datasets)
  exists <- file.exists(paths)
  names(exists) <- datasets
  exists
}

# =============================================================================
# PROGRESS & TIMING FUNCTIONS
# =============================================================================

#' Create a progress tracker for multi-step analyses
#' 
#' @param total_steps Total number of steps
#' @param title Analysis title
#' @return Function to call for each step
create_progress_tracker <- function(total_steps, title = "Analysis") {
  start_time <- Sys.time()
  current_step <- 0
  
  function(step_name) {
    current_step <<- current_step + 1
    cat(sprintf("\n[%d/%d] %s...\n", current_step, total_steps, step_name))
    cat(rep("-", 50), "\n", sep = "")
  }
}

#' Format elapsed time nicely
#' 
#' @param start_time POSIXt object from Sys.time()
#' @return Formatted string
format_elapsed <- function(start_time) {
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  secs <- as.numeric(elapsed)
  
  if (secs < 60) {
    sprintf("%.1f seconds", secs)
  } else if (secs < 3600) {
    sprintf("%.1f minutes", secs / 60)
  } else {
    sprintf("%.1f hours", secs / 3600)
  }
}

cat("✓ Utility functions loaded successfully.\n")
