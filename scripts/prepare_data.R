# scripts/prepare_data.R - Data Cleaning and Aggregation Orchestrator
# Prepares cleaned Parquet partitions and precomputed DuckDB monthly aggregates

args <- commandArgs(trailingOnly = TRUE)
target_mode <- if (length(args) > 0) args[1] else "demo"

source("R/config.R")
source("R/data_cleaning.R")
source("R/storage.R")
source("R/aggregation.R")

cat(sprintf("=== ClimateWatch Data Preparation Pipeline [%s Mode] ===\n", toupper(target_mode)))

cfg <- load_config()
target_dir <- get_data_dir(cfg, mode = target_mode)

cat("Target directory: ", target_dir, "\n")

# Load existing daily dataset
daily_df <- load_daily_data_parquet(target_dir)

if (nrow(daily_df) == 0) {
  if (target_mode == "demo") {
    cat("Demo data not found. Running generate_demo_data.R...\n")
    source("scripts/generate_demo_data.R")
  } else {
    stop("Real data not found. Run 'scripts/download_data.R' first to fetch NASA POWER data.")
  }
} else {
  cat(sprintf("Loaded %d daily records across %d locations.\n", nrow(daily_df), length(unique(daily_df$location_id))))
  
  cat("Cleaning daily observations...")
  cleaned_df <- clean_daily_weather_data(daily_df, cfg)
  cat(" DONE.\n")

  cat("Saving partitioned Parquet...")
  save_daily_data_parquet(cleaned_df, output_dir = target_dir, mode = target_mode)
  cat(" DONE.\n")

  cat("Computing monthly aggregates via DuckDB...")
  monthly_df <- compute_monthly_aggregates_duckdb(cleaned_df, min_coverage = cfg$quality$min_monthly_coverage)
  saveRDS(monthly_df, file.path(target_dir, "monthly_aggregates.rds"))
  cat(sprintf(" DONE. (%d monthly records cached)\n", nrow(monthly_df)))
}

cat("Data preparation pipeline complete.\n")
