# Quick Start Guide

Welcome to your R Data Analysis Workspace! This guide will help you get started in minutes.

## 🚀 5-Minute Setup

### Step 1: Open the Project
- Double-click `r-data.Rproj` to open in RStudio
- OR open RStudio → File → Open Project → select `r-data.Rproj`

### Step 2: Install Packages
Run ONE of these options in R:

**Option A - Using the install script:**
```r
source("install_packages.R")
```

**Option B - Using the setup script (recommended):**
```r
source("scripts/00_setup.R")
```

Option B also loads packages and sets up your environment!

### Step 3: Start Analyzing!
You're ready to go. Choose your path:

#### Path A: Use Example Scripts
1. Add your data file to `data/raw/`
2. Open `scripts/01_data_import.R`
3. Uncomment and modify the examples
4. Run the scripts in order (01, 02, 03, etc.)

#### Path B: Use R Markdown Template
1. Open `notebooks/analysis_template.Rmd`
2. Click "Knit" to see how it works
3. Modify with your own analysis

#### Path C: Start Fresh
```r
# Load packages
library(tidyverse)
library(here)

# Import your data
my_data <- read_csv(here("data/raw/your_file.csv"))

# Quick exploration
summary(my_data)
glimpse(my_data)

# Simple plot
ggplot(my_data, aes(x = variable)) +
  geom_histogram() +
  theme_minimal()
```

## 📊 Example: Complete Analysis in 10 Lines

```r
# 1. Setup
source("scripts/00_setup.R")

# 2. Load data (replace with your file)
data <- read_csv(here("data/raw/my_data.csv"))

# 3. Quick summary
library(skimr)
skim(data)

# 4. Clean data
data_clean <- data %>%
  drop_na() %>%
  filter(value > 0)

# 5. Visualize
ggplot(data_clean, aes(x = category, y = value)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "My Analysis")

# 6. Save plot
ggsave(here("outputs/figures/my_plot.png"), width = 8, height = 6)
```

## 🎯 Common Tasks

### Import Different File Types

```r
# CSV
data <- read_csv("data/raw/file.csv")

# Excel
data <- read_excel("data/raw/file.xlsx", sheet = 1)

# SPSS
data <- read_spss("data/raw/file.sav")

# Stata
data <- read_stata("data/raw/file.dta")
```

### Clean Data

```r
library(janitor)

clean_data <- raw_data %>%
  clean_names() %>%           # Standardize column names
  drop_na(important_col) %>%  # Remove missing values
  distinct() %>%              # Remove duplicates
  filter(value > 0)           # Filter rows
```

### Create Visualizations

```r
# Bar plot
ggplot(data, aes(x = category)) +
  geom_bar(fill = "steelblue")

# Scatter plot
ggplot(data, aes(x = var1, y = var2)) +
  geom_point() +
  geom_smooth(method = "lm")

# Time series
ggplot(data, aes(x = date, y = value)) +
  geom_line()
```

### Generate Reports

1. Open any `.Rmd` file in `notebooks/`
2. Edit the content
3. Click "Knit" button (or press Ctrl+Shift+K)
4. Choose output format (HTML, PDF, or Word)

## 🆘 Troubleshooting

### Package Installation Issues

If package installation fails:
```r
# Update R packages
update.packages(ask = FALSE)

# Install individual package
install.packages("package_name")

# Try a different CRAN mirror
options(repos = "https://cloud.r-project.org/")
install.packages("package_name")
```

### Path Issues

Always use the `here` package for file paths:
```r
library(here)

# ✅ Good - works anywhere
data <- read_csv(here("data/raw/file.csv"))

# ❌ Bad - breaks if working directory changes
data <- read_csv("data/raw/file.csv")
```

### Memory Issues with Large Data

```r
# Use data.table for large files
library(data.table)
data <- fread("data/raw/large_file.csv")

# Or read in chunks
data <- read_csv("data/raw/file.csv", n_max = 10000)
```

## 📚 Next Steps

1. Check out the [main README](README.md) for detailed information
2. Explore example scripts in `scripts/` directory
3. Read subdirectory README files for specific guidance
4. Try the R Markdown templates in `notebooks/`

## 🎓 Learning Resources

- [R for Data Science](https://r4ds.had.co.nz/) - Free online book
- [RStudio Cheatsheets](https://posit.co/resources/cheatsheets/) - Quick references
- [Tidyverse Documentation](https://www.tidyverse.org/) - Package guides
- [R Markdown Guide](https://rmarkdown.rstudio.com/) - Report creation

---

**Need help?** Open an issue on GitHub or check the documentation!

Happy analyzing! 📊✨
