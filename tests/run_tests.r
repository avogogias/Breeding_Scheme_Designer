# Run all tests for Breeding Scheme Designer
# Usage: Rscript tests/run_tests.r
# Or from RStudio: source("tests/run_tests.r")

# Set working directory to project root if running from tests/
if (grepl("tests$", getwd())) setwd("..")

library(testthat)

cat("=== Running Breeding Scheme Designer Tests ===\n\n")

# Run test suite
test_results <- test_file("tests/test_core_functions.r", reporter = "summary")

cat("\n=== Test Summary ===\n")
print(test_results)
