# MCDA-VISTA

## Overview
MCDA-VISTA (Value-based Interactive Shiny Tool for Appraisal) is an open-source R Shiny application for structured execution and transparent communication of Multi-Criteria Decision Analysis (MCDA) in healthcare.

## Key Features
- **Guided MCDA workflow** following ISPOR good practices
- **Multiple scoring methods**: Point allocation, Analytic Hierarchy Process (AHP), and value functions
- **Multiple weighting methods**: Point allocation, AHP, and swing weighting
- **Interactive visualisations**: Stacked bar charts, spider plots, and heatmaps
- **Uncertainty documentation** and simple sensitivity analyses
- **Deliberation tracking** with notes at each step
- **Structured reporting** for stakeholder communication

## Getting Started

### Installation
```r
# Install required packages
install.packages("shiny")
install.packages("shinydashboard")

# Clone the repository or download the code
```

### Two Versions Available
1. **Empty tool**: https://mcda-vista.shinyapps.io/tool/
2. **Customisable version** (case-specific applications): Adapt the code to your decision problem, see example at https://mcda-vista.shinyapps.io/custom_cgp/


## Customization
Edit `config.R` to:
- Pre-define decision parameters
- Lock specific fields
- Set default scoring/weighting methods
- Add case-specific evidence and sensitivity analyses

## Documentation
See the Supplementary Material for detailed step-by-step user guide for each tab.

## Citation
Fernández Coves A, Ramaekers B, van Schaik L, Retèl V, van Til J, Grimm S, Joore M. MCDA-VISTA (Value-based Interactive Shiny Tool for Appraisal): An Open-Source Platform to Support Multi-Criteria Decision Analysis and Communication in Healthcare. Pending for Publication.

## Support
For any issues please contact andrea.fernandez.coves@mumc.nl
