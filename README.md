# R Data Analysis Workspace

A complete, ready-to-use workspace for data analysis with R. This repository provides an organized structure, example scripts, and templates to streamline your data analysis workflow.

## 📋 Table of Contents

- [Features](#features)
- [Quick Start](#quick-start)
- [Directory Structure](#directory-structure)
- [Getting Started](#getting-started)
- [Workflow](#workflow)
- [Usage Examples](#usage-examples)
- [Best Practices](#best-practices)
- [Additional Resources](#additional-resources)
- [Contributing](#contributing)

## 🚀 Quick Start

**New to this workspace?** Start here:

1. **[Read the Quick Start Guide](QUICKSTART.md)** - Get up and running in 5 minutes
2. **Run the example workflow**: `source("example_workflow.R")` - See a complete analysis in action
3. **Explore the templates** in `notebooks/` - Ready-to-use R Markdown files

**Already familiar?** Jump to [Getting Started](#getting-started) or [Workflow](#workflow)

## ✨ Features

- **Organized Directory Structure**: Separate folders for raw data, processed data, scripts, outputs, and notebooks
- **Example Scripts**: Pre-built R scripts demonstrating common data analysis tasks
- **R Markdown Templates**: Ready-to-use templates for analysis reports and exploratory data analysis
- **Utility Functions**: Helper functions for common data manipulation tasks
- **Best Practices**: Built-in guidance for reproducible research and clean code
- **RStudio Integration**: Includes `.Rproj` file for seamless RStudio integration

## 📁 Directory Structure

```
r-data/
├── data/                    # All data files
│   ├── raw/                # Original, immutable data
│   ├── processed/          # Cleaned and processed data
│   └── external/           # External data sources
├── scripts/                # R scripts for analysis
│   ├── 00_setup.R         # Package installation and setup
│   ├── 01_data_import.R   # Data import examples
│   ├── 02_data_cleaning.R # Data cleaning workflow
│   ├── 03_exploratory_analysis.R  # EDA examples
│   ├── 04_analysis.R      # Statistical analysis
│   ├── 05_visualization.R # Data visualization
│   └── utils.R            # Utility functions
├── notebooks/              # R Markdown notebooks
│   ├── analysis_template.Rmd
│   └── exploratory_analysis.Rmd
├── outputs/                # Generated outputs
│   ├── figures/           # Plots and visualizations
│   ├── tables/            # Generated tables
│   └── reports/           # Rendered reports
├── .gitignore             # Git ignore file
├── r-data.Rproj          # RStudio project file
└── README.md              # This file
```

## 🚀 Getting Started

### Prerequisites

- **R** (version 4.0 or higher recommended) - [Download R](https://www.r-project.org/)
- **RStudio** (optional but recommended) - [Download RStudio](https://posit.co/download/rstudio-desktop/)

### Installation

1. **Clone this repository**:
   ```bash
   git clone https://github.com/and3rn3t/r-data.git
   cd r-data
   ```

2. **Open in RStudio** (recommended):
   - Double-click `r-data.Rproj` to open the project in RStudio
   - OR open RStudio and navigate to File → Open Project

3. **Install required packages**:
   ```r
   source("scripts/00_setup.R")
   ```
   
   This will install and load all necessary packages including:
   - `tidyverse` - Data manipulation and visualization
   - `readxl`, `writexl` - Excel file handling
   - `haven` - Import SPSS, Stata, SAS files
   - `rmarkdown`, `knitr` - Report generation
   - `here` - Easy file path management
   - And more!

## 🔄 Workflow

### Typical Analysis Workflow

1. **Setup Environment** (`scripts/00_setup.R`)
   - Install and load required packages
   - Set global options
   - Configure custom themes

2. **Import Data** (`scripts/01_data_import.R`)
   - Load raw data from various formats (CSV, Excel, SPSS, etc.)
   - Place raw data files in `data/raw/`

3. **Clean Data** (`scripts/02_data_cleaning.R`)
   - Handle missing values
   - Remove duplicates
   - Create new variables
   - Save cleaned data to `data/processed/`

4. **Explore Data** (`scripts/03_exploratory_analysis.R`)
   - Generate summary statistics
   - Create exploratory visualizations
   - Identify patterns and outliers

5. **Analyze Data** (`scripts/04_analysis.R`)
   - Perform statistical tests
   - Build models
   - Generate results

6. **Visualize Results** (`scripts/05_visualization.R`)
   - Create publication-ready plots
   - Save figures to `outputs/figures/`

7. **Generate Reports** (`notebooks/`)
   - Use R Markdown templates
   - Render to HTML, PDF, or Word
   - Save to `outputs/reports/`

## 📊 Usage Examples

### Quick Start Example

```r
# 1. Load packages
source("scripts/00_setup.R")

# 2. Import your data
library(tidyverse)
library(here)
my_data <- read_csv(here("data/raw/my_file.csv"))

# 3. Quick exploration
library(skimr)
skim(my_data)

# 4. Create a simple plot
ggplot(my_data, aes(x = variable1, y = variable2)) +
  geom_point() +
  theme_minimal()
```

### Using R Markdown Templates

1. Open `notebooks/analysis_template.Rmd` or `notebooks/exploratory_analysis.Rmd`
2. Modify the YAML header (title, author, etc.)
3. Update code chunks with your analysis
4. Click "Knit" in RStudio to generate a report

### Using Utility Functions

```r
# Load utility functions
source("scripts/utils.R")

# Count missing values
missing_data <- count_missing(my_data)

# Calculate confidence intervals
ci <- calculate_ci(my_data$numeric_var)

# Standardize column names
clean_data <- standardize_names(my_data)
```

## 📚 Best Practices

### Data Management

- **Never modify raw data**: Keep original files in `data/raw/` unchanged
- **Version control**: Track changes with Git (data files can be gitignored if large)
- **Document everything**: Add comments and README files
- **Use consistent naming**: Follow the naming conventions in example scripts

### Code Organization

- **Use projects**: Always work within the RStudio project (`r-data.Rproj`)
- **Use here()**: Use the `here` package for file paths (works regardless of working directory)
- **Source scripts**: Break code into logical scripts and source them as needed
- **Comment your code**: Explain what and why, not just how

### Reproducibility

- **Set seed**: Use `set.seed()` for reproducible random operations
- **Version packages**: Consider using `renv` for package version management
- **Document sessions**: Include `sessionInfo()` in reports
- **Test your code**: Ensure scripts can run from start to finish

## 📖 Additional Resources

### Included Documentation
- **[QUICKSTART.md](QUICKSTART.md)** - 5-minute setup guide with common tasks
- **[example_workflow.R](example_workflow.R)** - Complete working example from data to results
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Guidelines for contributing
- **Directory READMEs** - Specific guidance for each folder

### External Learning Resources
- [R for Data Science](https://r4ds.had.co.nz/) - Free online book
- [RStudio Cheatsheets](https://posit.co/resources/cheatsheets/) - Quick references
- [Tidyverse Documentation](https://www.tidyverse.org/) - Package guides
- [R Markdown Guide](https://rmarkdown.rstudio.com/) - Report creation

## 🤝 Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

Feel free to:
- Report bugs or suggest features via GitHub Issues
- Submit pull requests with improvements
- Share your own analysis templates or utility functions

## 📝 License

This project is available under the MIT License. See [LICENSE](LICENSE) for details.

## 📧 Contact

For questions or suggestions, please open an issue on GitHub.

---

**Happy Analyzing! 📊✨**