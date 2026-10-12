# scripts/download_data.R - NASA POWER Real Meteorological Data Pipeline
# Downloads daily meteorological data for configured locations from NASA POWER API

source("R/config.R")
source("R/data_download.R")
source("R/data_cleaning.R")
source("R/storage.R")
source("R/aggregation.R")

cat("=== NASA POWER Real Historical Data Downloader ===\n")

cfg <- load_config()
ensure_directories(cfg)
locations <- load_locations()

start_yr <- cfg$historical_period$start_year
end_yr <- cfg$historical_period$end_year

cat(sprintf("Locations to process: %d\nPeriod: %d to %d\n", nrow(locations), start_yr, end_yr))
cat("Note: NASA POWER provides gridded satellite/model estimates at the given coordinates.\n\n")

all_real <- list()
failed_locations <- character(0)

for (i in seq_len(nrow(locations))) {
  loc <- locations[i, ]
  cat(sprintf("[%d/%d] Fetching %s (%s)...\n", i, nrow(locations), loc$location_name, loc$location_id))

  df_loc <- tryCatch({
    download_nasa_power_point(
      location_row = c(
        location_id = loc$location_id,
        location_name = loc$location_name,
        latitude = loc$latitude,
        longitude = loc$longitude
      ),
      start_year = start_yr,
      end_year = end_yr,
      config = cfg,
      raw_dir = cfg$data$raw_dir
    )
  }, error = function(e) {
    warning("Download failed for ", loc$location_name, ": ", e$message)
    NULL
  })

  if (!is.null(df_loc) && nrow(df_loc) > 0) {
    all_real[[loc$location_id]] <- df_loc
    cat(sprintf("  Successfully retrieved %d daily records for %s.\n", nrow(df_loc), loc$location_name))
  } else {
    failed_locations <- c(failed_locations, loc$location_name)
    cat(sprintf("  WARNING: Could not retrieve data for %s.\n", loc$location_name))
  }
}

if (length(all_real) > 0) {
  combined_real <- do.call(rbind, all_real)
  cat(sprintf("\nTotal raw records retrieved: %d\n", nrow(combined_real)))

  # Clean real data
  cat("Cleaning and validating real data...\n")
  cleaned_real <- clean_daily_weather_data(combined_real, cfg)
  
  # Save to partitioned Parquet
  cat("Saving real data to partitioned Parquet (data/processed/real/)...")
  save_daily_data_parquet(cleaned_real, output_dir = cfg$data$processed_real_dir, mode = "real")
  cat(" DONE.\n")

  # Precompute DuckDB monthly aggregates
  cat("Precomputing DuckDB monthly aggregates for real data...")
  monthly_real <- compute_monthly_aggregates_duckdb(cleaned_real, min_coverage = cfg$quality$min_monthly_coverage)
  saveRDS(monthly_real, file.path(cfg$data$processed_real_dir, "monthly_aggregates.rds"))
  cat(" DONE.\n")

  cat("\nSUCCESS: Real historical dataset pipeline complete.\n")
} else {
  cat("\nNo real data was retrieved. Check network connectivity or use offline synthetic demo data.\n")
}

if (length(failed_locations) > 0) {
  cat("Failed locations: ", paste(failed_locations, collapse = ", "), "\n")
}
