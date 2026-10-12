# scripts/run_app.R - Launch Script for ClimateWatch Shiny Application

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) > 0) as.integer(args[1]) else 3838

cat("========================================================\n")
cat("  ClimateWatch: Spatio-Temporal Climate Anomaly Detection\n")
cat("========================================================\n")
cat(sprintf("Starting Shiny application on http://127.0.0.1:%d\n", port))
cat("Press Ctrl+C to stop the application server.\n\n")

shiny::runApp(
  appDir = ".",
  port = port,
  host = "127.0.0.1",
  launch.browser = FALSE
)
