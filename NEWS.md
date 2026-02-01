# Iowa Cities Data Analysis Changelog

## Version 1.0.0 (2026-02-01)

### New Features

#### Interactive Dashboard
- Full-featured Shiny dashboard with 7 analysis tabs
- Dark mode theme toggle with keyboard shortcut (D)
- Keyboard navigation (1-7 for tabs, ? for help)
- Mobile-responsive design for tablets and phones
- Accessibility improvements (ARIA labels, focus indicators, skip links)

#### REST API
- Plumber-based REST API for programmatic access
- Endpoints for cities, scores, comparisons, benchmarks
- CSV and JSON export functionality
- Swagger UI documentation
- Rate limiting and security headers

#### Data & Analysis
- 13 comprehensive datasets covering Iowa's 30 major cities
- Automated scoring across safety, housing, education, economy, healthcare
- Benchmark comparisons against Iowa and US averages
- Historical trend analysis (1980-2020)
- City recommendation engine based on user preferences

#### Security
- Input sanitization for XSS prevention
- Session security with timeout and fingerprinting
- Rate limiting to prevent abuse
- Content Security Policy headers

#### Infrastructure
- Docker support for easy deployment
- Docker Compose for multi-service orchestration
- GitHub Actions CI/CD pipeline
- Automated Docker image builds on release
- Vulnerability scanning with Trivy

#### Testing & Quality
- 350+ unit tests with testthat
- Code coverage with covr/codecov
- renv for reproducible dependencies
- targets pipeline for workflow automation

### Documentation
- Comprehensive README with quick start guide
- pkgdown documentation site
- API endpoint documentation
- Data dictionary with schema details
- Contributing guidelines

---

## Version 0.9.0 (2026-01-15)

### Initial Release
- Basic Shiny dashboard
- Core data loading and transformation
- Initial test suite
- Project structure and utilities
