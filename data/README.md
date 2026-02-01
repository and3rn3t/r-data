# Data Directory

This directory contains all data files used in the analysis.

## Subdirectories

- **raw/**: Original, immutable data files. Never modify files in this directory.
- **processed/**: Cleaned and processed data files ready for analysis.
- **external/**: External data from third-party sources.

## Guidelines

1. Keep raw data unchanged - always work with copies
2. Document data sources and collection methods
3. Use consistent naming conventions (e.g., `YYYY-MM-DD_description.csv`)
4. Include metadata files when applicable
