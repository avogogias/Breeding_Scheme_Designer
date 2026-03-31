# Install all dependencies for Breeding Scheme Designer
# Run this script once before first use: Rscript install_dependencies.r

required_packages <- c(
  "shiny",
  "DT",
  "shinyBS",
  "Rcpp",
  "RcppArmadillo",
  "ggplot2",
  "shinyjs",
  "data.table",
  "dplyr",
  "plotly",
  "openxlsx",
  "shinyalert",
  "testthat"  # for running tests
)

cat("Checking and installing required packages...\n\n")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("Installing: ", pkg, "\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    cat(paste0("OK: ", pkg, " (", packageVersion(pkg), ")\n"))
  }
}

cat("\nAll dependencies installed. Run the app with: shiny::runApp()\n")
