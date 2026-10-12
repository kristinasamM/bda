# tests/testthat.R - Testthat Runner for ClimateWatch
library(testthat)

cat("=== Running ClimateWatch Analytics Test Suite ===\n")
test_results <- test_dir("tests/testthat", reporter = "summary")
print(test_results)
