# Utility Functions
# This script contains helper functions used across multiple scripts

#' Calculate confidence intervals
#'
#' @param x A numeric vector
#' @param conf.level Confidence level (default 0.95)
#' @return A named vector with lower and upper bounds
calculate_ci <- function(x, conf.level = 0.95) {
  x <- na.omit(x)
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
  writexl::write_xlsx(data_list, path = file_path)
  cat("Data exported to:", file_path, "\n")
}

cat("Utility functions loaded successfully.\n")
