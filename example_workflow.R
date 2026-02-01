# Example Workflow: Complete Data Analysis
# This script demonstrates a complete analysis workflow using example data

# ============================================================
# 1. SETUP
# ============================================================
cat("Step 1: Setting up environment...\n")

# Load required packages (install if needed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here)

set.seed(42)  # For reproducibility

# ============================================================
# 2. CREATE EXAMPLE DATA
# ============================================================
cat("\nStep 2: Creating example dataset...\n")

# Create a sample dataset
example_data <- tibble(
  id = 1:100,
  age = sample(18:65, 100, replace = TRUE),
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  department = sample(c("Sales", "Marketing", "IT", "HR"), 100, replace = TRUE),
  salary = round(rnorm(100, mean = 50000, sd = 15000)),
  years_experience = sample(0:20, 100, replace = TRUE),
  performance_score = round(rnorm(100, mean = 75, sd = 10))
)

# Save example data
dir.create(here("data/raw"), showWarnings = FALSE, recursive = TRUE)
write_csv(example_data, here("data/raw/employee_data.csv"))
cat("Example data saved to: data/raw/employee_data.csv\n")

# ============================================================
# 3. DATA IMPORT
# ============================================================
cat("\nStep 3: Importing data...\n")

data <- read_csv(here("data/raw/employee_data.csv"), show_col_types = FALSE)
cat("Rows:", nrow(data), "| Columns:", ncol(data), "\n")

# ============================================================
# 4. DATA EXPLORATION
# ============================================================
cat("\nStep 4: Exploring data...\n")

# Basic summary
cat("\nData structure:\n")
glimpse(data)

cat("\nSummary statistics:\n")
summary(data)

# Check for missing values
cat("\nMissing values:\n")
print(colSums(is.na(data)))

# ============================================================
# 5. DATA CLEANING
# ============================================================
cat("\nStep 5: Cleaning data...\n")

data_clean <- data %>%
  # Ensure positive salary
  filter(salary > 0) %>%
  # Ensure valid performance scores
  filter(performance_score >= 0 & performance_score <= 100) %>%
  # Remove any duplicates
  distinct()

cat("Cleaned data rows:", nrow(data_clean), "\n")

# Save cleaned data
dir.create(here("data/processed"), showWarnings = FALSE, recursive = TRUE)
write_csv(data_clean, here("data/processed/employee_data_clean.csv"))
cat("Cleaned data saved to: data/processed/employee_data_clean.csv\n")

# ============================================================
# 6. DESCRIPTIVE ANALYSIS
# ============================================================
cat("\nStep 6: Descriptive analysis...\n")

# Overall statistics
overall_stats <- data_clean %>%
  summarise(
    avg_salary = mean(salary),
    median_salary = median(salary),
    avg_performance = mean(performance_score),
    total_employees = n()
  )

cat("\nOverall Statistics:\n")
print(overall_stats)

# By department
dept_stats <- data_clean %>%
  group_by(department) %>%
  summarise(
    count = n(),
    avg_salary = round(mean(salary)),
    avg_performance = round(mean(performance_score), 1),
    avg_experience = round(mean(years_experience), 1)
  ) %>%
  arrange(desc(avg_salary))

cat("\nStatistics by Department:\n")
print(dept_stats)

# ============================================================
# 7. VISUALIZATIONS
# ============================================================
cat("\nStep 7: Creating visualizations...\n")

# Create output directory
dir.create(here("outputs/figures"), showWarnings = FALSE, recursive = TRUE)

# Plot 1: Salary distribution
p1 <- ggplot(data_clean, aes(x = salary)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Employee Salaries",
    x = "Salary ($)",
    y = "Count"
  )

ggsave(here("outputs/figures/salary_distribution.png"), p1, 
       width = 8, height = 6, dpi = 300)

# Plot 2: Salary by department
p2 <- ggplot(data_clean, aes(x = department, y = salary, fill = department)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Salary by Department",
    x = "Department",
    y = "Salary ($)"
  )

ggsave(here("outputs/figures/salary_by_department.png"), p2, 
       width = 8, height = 6, dpi = 300)

# Plot 3: Salary vs Experience
p3 <- ggplot(data_clean, aes(x = years_experience, y = salary)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(
    title = "Salary vs Years of Experience",
    x = "Years of Experience",
    y = "Salary ($)"
  )

ggsave(here("outputs/figures/salary_vs_experience.png"), p3, 
       width = 8, height = 6, dpi = 300)

cat("Plots saved to: outputs/figures/\n")

# ============================================================
# 8. STATISTICAL ANALYSIS
# ============================================================
cat("\nStep 8: Statistical analysis...\n")

# Correlation between salary and experience
cor_test <- cor.test(data_clean$years_experience, data_clean$salary)
cat("\nCorrelation (Salary vs Experience):\n")
cat("  r =", round(cor_test$estimate, 3), "\n")
cat("  p-value =", format.pval(cor_test$p.value, digits = 3), "\n")

# Linear regression model
model <- lm(salary ~ years_experience + performance_score + department, 
            data = data_clean)
cat("\nRegression Model Summary:\n")
print(summary(model))

# ============================================================
# 9. SAVE RESULTS
# ============================================================
cat("\nStep 9: Saving results...\n")

# Create tables directory
dir.create(here("outputs/tables"), showWarnings = FALSE, recursive = TRUE)

# Save summary tables
write_csv(dept_stats, here("outputs/tables/department_statistics.csv"))

cat("Tables saved to: outputs/tables/\n")

# ============================================================
# COMPLETE
# ============================================================
cat("\n" , rep("=", 60), "\n", sep = "")
cat("WORKFLOW COMPLETE!\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("Generated files:\n")
cat("  - data/raw/employee_data.csv (example raw data)\n")
cat("  - data/processed/employee_data_clean.csv (cleaned data)\n")
cat("  - outputs/figures/*.png (3 plots)\n")
cat("  - outputs/tables/department_statistics.csv (summary table)\n\n")

cat("Next steps:\n")
cat("  1. Check the outputs/figures/ folder for visualizations\n")
cat("  2. Review the outputs/tables/ folder for summary statistics\n")
cat("  3. Try modifying this script with your own data\n")
cat("  4. Explore the R Markdown templates in notebooks/\n\n")
