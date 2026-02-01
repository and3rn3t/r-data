# Makefile for R Data Analysis Project
# =====================================
# Usage:
#   make all       - Run complete analysis pipeline
#   make clean     - Remove generated outputs
#   make test      - Run unit tests
#   make lint      - Check code style
#   make validate  - Validate data quality
#   make report    - Generate analysis reports

.PHONY: all clean test lint validate report setup pipeline help

# Default R command
R := Rscript

# Help target
help:
	@echo "Available targets:"
	@echo "  make all       - Run complete analysis pipeline"
	@echo "  make setup     - Install required packages"
	@echo "  make pipeline  - Run targets pipeline"
	@echo "  make test      - Run unit tests"
	@echo "  make lint      - Check code style with lintr"
	@echo "  make validate  - Validate data quality"
	@echo "  make report    - Generate analysis reports"
	@echo "  make clean     - Remove generated outputs"

# Setup environment
setup:
	$(R) -e "source('scripts/00_setup.R')"
	$(R) -e "if (!require('testthat')) install.packages('testthat')"
	$(R) -e "if (!require('lintr')) install.packages('lintr')"
	$(R) -e "if (!require('targets')) install.packages('targets')"

# Run full analysis (traditional method)
all: setup
	$(R) -e "source('scripts/run_all_iowa_analyses.R')"

# Run targets pipeline (modern method)
pipeline:
	$(R) -e "targets::tar_make()"

# Visualize pipeline
pipeline-viz:
	$(R) -e "targets::tar_visnetwork()"

# Run unit tests
test:
	$(R) -e "testthat::test_dir('tests/testthat')"

# Run linter
lint:
	$(R) -e "lintr::lint_dir('scripts')"

# Validate data quality
validate:
	$(R) -e "source('scripts/validate_data.R')"

# Generate reports
report:
	$(R) -e "rmarkdown::render('notebooks/analysis_template.Rmd', output_dir = 'outputs/reports')"

# Clean generated files
clean:
	rm -rf outputs/figures/*
	rm -rf outputs/tables/*
	rm -rf outputs/reports/*
	rm -rf _targets/
	$(R) -e "unlink('data/processed/*_targets.csv')"

# Create output directories if they don't exist
dirs:
	mkdir -p outputs/figures
	mkdir -p outputs/tables
	mkdir -p outputs/reports
