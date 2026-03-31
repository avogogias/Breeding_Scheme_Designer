# Breeding Scheme Designer

**Version 2.0.0**

The Breeding Scheme Designer is a tool to explore trade offs between different evaluation strategies in breeding programs using deterministic simulation. The purpose for developing this tool was to help simulate the full cycle of a breeding program to help estimate parameters such as heritability and the expected genetic gain, to help understand how breeder's equation terms interact and to enable comparisons between different strategies and understand trade-offs between multiple resource allocation scenarios and their effect to the expected genetic gain. In addition, comparisons of different scenarios with respect to genetic gain per year and genetic gain per dollar invested are enabled.

The tool helps define scenarios with multiple stages of selection.

<img width="800" alt="Breeding_scheme_designer" src="https://user-images.githubusercontent.com/8427251/134358626-ee38189a-fe40-4cac-bf6d-4cb5b630703f.png">

Figure 1: a comparison between boxplots of three breeding scenarios with three stages each.

<img width="800" alt="ranges" src="https://user-images.githubusercontent.com/8427251/134371478-7acde81b-543e-4b13-a2f2-f09b94a57412.png">

Figure 2: heatmaps help to explore the solution space for combinations of input parameters set for different ranges.

## What's New in v2.0.0

- **Unlimited Scenarios**: Dynamic multi-scenario support replaces the previous hard-coded limit of 10 scenarios. Create and compare as many breeding scenarios as needed.
- **Interactive Charts**: All charts are now powered by Plotly, providing hover tooltips, zoom, pan, and export capabilities.
- **Improved Architecture**: Scenario management uses dynamic observer registration, reducing code duplication from 1300+ lines to ~120 lines.
- **Test Suite**: Comprehensive automated tests covering the C++ engine, calculation functions, multi-scenario storage, and chart generation.
- **Dependency Management**: Added DESCRIPTION file and install script for streamlined setup.

## Testing & Evaluation

This tool has been tested with a small group of breeders from different teams and it is now available as part of the EiB toolbox. Some documents related to the user study can be found in /doc/eval directory of the project.

### Running Tests

```r
# Install test dependencies
install.packages("testthat")

# Run the test suite
testthat::test_file("tests/test_core_functions.r")
```

## Set Up

### Quick Install

```r
# Install all dependencies at once
source("install_dependencies.r")
```

### Manual Install

The software can be executed through RStudio. Make sure that the following library dependencies (R packages) are first installed:

1. shiny
2. DT
3. Rcpp
4. RcppArmadillo
5. ggplot2
6. shinyjs
7. data.table
8. dplyr
9. plotly
10. openxlsx
11. shinyBS
12. shinyalert

### Running the App

```r
# From RStudio or R console
shiny::runApp()
```

An instance of the tool is deployed online in the shinyapps.io server: https://alphagenes.shinyapps.io/cycle_scenarios/

## Authorship

* Repo owner/admin : Thanasis Vogogias
* Other community or team contact: Chris Gaynor
