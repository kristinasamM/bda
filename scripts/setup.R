# ClimateWatch - Environment Setup and Dependency Installer
# Installs all required packages for ClimateWatch

required_packages <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "plotly",
  "leaflet",
  "DT",
  "dplyr",
  "lubridate",
  "httr2",
  "jsonlite",
  "yaml",
  "DBI",
  "RSQLite",
  "duckdb",
  "arrow",
  "forecast",
  "testthat"
)

cat("=== ClimateWatch Dependency Setup ===\n")
cat("Checking R version: ", R.version.string, "\n")

# Use binary repository for fast and reliable Windows installation
options(repos = c(CRAN = "https://cloud.r-project.org"))

installed <- rownames(installed.packages())
missing_pkgs <- setdiff(required_packages, installed)

if (length(missing_pkgs) == 0) {
  cat("All required packages are already installed!\n")
} else {
  cat("Installing missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs, dependencies = TRUE, type = "binary")
}

# Verify installation
cat("\n--- Verification Summary ---\n")
still_missing <- character(0)
for (pkg in required_packages) {
  available <- requireNamespace(pkg, quietly = TRUE)
  status <- if (available) "OK" else "FAILED"
  cat(sprintf("  %-15s : %s\n", pkg, status))
  if (!available) still_missing <- c(still_missing, pkg)
}

if (length(still_missing) == 0) {
  cat("\nSUCCESS: All dependencies are verified and ready to use.\n")
} else {
  warning("Some packages could not be installed: ", paste(still_missing, collapse = ", "))
}
