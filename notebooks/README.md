# Notebooks Directory

R Markdown notebooks for analysis reports and interactive documents.

## Available Notebooks

| File | Purpose |
|------|---------|
| `analysis_template.Rmd` | Generic analysis template |
| `exploratory_analysis.Rmd` | EDA workflow template |
| `iowa_cities_report.Rmd` | Iowa cities analysis report |
| `iowa_comprehensive_dashboard.Rmd` | Full dashboard with all metrics |
| `city_profile_template.Rmd` | Single city PDF template |
| `comparison_report_template.Rmd` | Multi-city comparison template |

## Rendering Reports

### Interactive (HTML)

``r
rmarkdown::render("notebooks/iowa_comprehensive_dashboard.Rmd")
``

### PDF Report

``r
# Requires LaTeX (install with tinytex::install_tinytex())
rmarkdown::render("notebooks/iowa_cities_report.Rmd", 
                  output_format = "pdf_document")
``

### Parameterized Reports

``r
# Generate report for specific city
rmarkdown::render(
  "notebooks/city_profile_template.Rmd",
  params = list(city = "Bettendorf"),
  output_file = "outputs/reports/Bettendorf_profile.pdf"
)
``

## Creating New Notebooks

1. Copy `analysis_template.Rmd` as starting point
2. Update YAML header (title, author, date)
3. Modify setup chunk with required packages
4. Add analysis sections
5. Render to test output

## Best Practices

- Use `here()` for all file paths
- Set `echo = FALSE` for clean reports
- Include `sessionInfo()` for reproducibility
- Cache expensive computations with `cache = TRUE`
