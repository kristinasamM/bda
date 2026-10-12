# R/config.R - Configuration and Metadata Loader for ClimateWatch

#' Load Application Settings
#'
#' Reads configuration from YAML file with fallback defaults.
#' @param config_path Path to settings.yml
#' @return List of configuration options
load_config <- function(config_path = "config/settings.yml") {
  if (file.exists(config_path)) {
    if (requireNamespace("yaml", quietly = TRUE)) {
      cfg <- yaml::read_yaml(config_path)
      return(cfg)
    }
  }
  
  # Default fallback configuration if YAML is missing
  list(
    app = list(
      name = "ClimateWatch",
      title = "Spatio-Temporal Climate Anomaly Detection and Forecasting Using R",
      version = "1.0.0"
    ),
    data = list(
      mode = "demo",
      data_dir = "data",
      raw_dir = "data/raw",
      processed_real_dir = "data/processed/real",
      processed_demo_dir = "data/processed/demo",
      metadata_dir = "data/metadata"
    ),
    historical_period = list(start_year = 2001, end_year = 2024),
    baseline_period = list(start_year = 2001, end_year = 2015),
    analysis_period = list(start_year = 2016, end_year = 2024),
    quality = list(
      min_monthly_coverage = 0.90,
      min_baseline_years = 5,
      temp_valid_range = c(-20.0, 60.0),
      precip_valid_range = c(0.0, 1500.0),
      humidity_valid_range = c(0.0, 100.0),
      sentinel_values = c(-999, -999.0, -99, -99.0)
    ),
    anomaly = list(
      z_mild = 1.5,
      z_moderate = 2.0,
      z_severe = 3.0,
      rainfall_dry_percentile = 0.10,
      rainfall_wet_percentile = 0.90,
      humidity_z_high = 1.5,
      humidity_z_low = -1.5
    ),
    forecasting = list(
      horizons = c(3, 6, 12),
      holdout_months = 12,
      models = c("snaive", "ets", "arima")
    ),
    nasa_power = list(
      base_url = "https://power.larc.nasa.gov/api/temporal/daily/point",
      parameters = c("T2M", "PRECTOTCORR", "RH2M"),
      community = "AG",
      rate_limit_delay_sec = 1.2,
      max_retries = 3,
      backoff_factor = 2.0,
      request_timeout_sec = 45
    )
  )
}

#' Load Monitored Locations
#'
#' Reads reviewed locations from CSV.
#' @param locations_path Path to locations.csv
#' @return Data frame with location metadata
load_locations <- function(locations_path = "config/locations.csv") {
  if (!file.exists(locations_path)) {
    stop("Locations file not found at: ", locations_path)
  }
  df <- utils::read.csv(locations_path, stringsAsFactors = FALSE)
  # Standardize types
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  df$elevation_m <- as.numeric(df$elevation_m)
  df
}

#' Resolve Active Data Directory
#'
#' Returns directory path based on mode ("demo" or "real")
#' @param config App configuration list
#' @param mode Character, "demo" or "real" (optional override)
#' @return Path string
get_data_dir <- function(config = load_config(), mode = NULL) {
  selected_mode <- if (!is.null(mode)) mode else config$data$mode
  if (selected_mode == "real") {
    return(config$data$processed_real_dir)
  } else {
    return(config$data$processed_demo_dir)
  }
}

#' Ensure Required Directory Tree Exists
#'
#' Creates necessary folders if missing
ensure_directories <- function(config = load_config()) {
  dirs <- c(
    "config",
    config$data$data_dir,
    config$data$raw_dir,
    config$data$processed_real_dir,
    config$data$processed_demo_dir,
    config$data$metadata_dir,
    "R",
    "scripts",
    "tests/testthat",
    "www"
  )
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
}
