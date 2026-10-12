# scripts/generate_demo_data.R - Synthetic Demonstration Data Generator for ClimateWatch
# Generates realistic daily meteorological data (2001-2024) across 15 Indian locations
# Uses fixed random seed (42) and injects documented anomalies and test conditions.

source("R/config.R")
source("R/data_cleaning.R")
source("R/storage.R")

cat("=== Generating Offline Synthetic Demonstration Data ===\n")
set.seed(42)

ensure_directories()
locations <- load_locations("config/locations.csv")
dates <- seq.Date(from = as.Date("2001-01-01"), to = as.Date("2024-12-31"), by = "day")
n_days <- length(dates)
d_years <- as.integer(format(dates, "%Y"))
d_months <- as.integer(format(dates, "%m"))
d_yday <- as.integer(format(dates, "%j"))

all_dfs <- list()

# Regional climatology parameters
get_climatology <- function(loc_id, lat) {
  # Base annual temperature mean and seasonal swing
  if (loc_id %in% c("delhi", "jaipur", "lucknow", "bhopal", "patna")) {
    # North / Central continental
    base_t <- 25.5
    amp_t <- 11.0 # winter ~14, summer ~35
    peak_day_t <- 145 # late May
    base_hum <- 55
    hum_amp <- 25
    monsoon_months <- c(7, 8, 9)
    monsoon_rate <- 12.0
  } else if (loc_id %in% c("mumbai", "pune", "ahmedabad", "nagpur")) {
    # West / Central Deccan
    base_t <- 27.5
    amp_t <- 5.5
    peak_day_t <- 135
    base_hum <- 68
    hum_amp <- 22
    monsoon_months <- c(6, 7, 8, 9)
    monsoon_rate <- if (loc_id == "mumbai") 24.0 else 9.0
  } else if (loc_id %in% c("kolkata", "guwahati")) {
    # East / Northeast humid
    base_t <- 26.0
    amp_t <- 8.0
    peak_day_t <- 150
    base_hum <- 75
    hum_amp <- 18
    monsoon_months <- c(5, 6, 7, 8, 9)
    monsoon_rate <- 14.0
  } else if (loc_id %in% c("chennai")) {
    # Southeast coastal (NE monsoon dominant)
    base_t <- 29.0
    amp_t <- 4.5
    peak_day_t <- 150
    base_hum <- 72
    hum_amp <- 15
    monsoon_months <- c(10, 11, 12)
    monsoon_rate <- 16.0
  } else if (loc_id %in% c("bengaluru")) {
    # Southern plateau (elevation 920m)
    base_t <- 23.8
    amp_t <- 3.8
    peak_day_t <- 110
    base_hum <- 65
    hum_amp <- 20
    monsoon_months <- c(6, 7, 8, 9, 10)
    monsoon_rate <- 7.0
  } else {
    # Southern maritime (Thiruvananthapuram, Hyderabad)
    base_t <- 27.8
    amp_t <- 2.5
    peak_day_t <- 100
    base_hum <- 76
    hum_amp <- 14
    monsoon_months <- c(5, 6, 7, 8, 10, 11)
    monsoon_rate <- 11.0
  }
  
  list(base_t = base_t, amp_t = amp_t, peak_day_t = peak_day_t,
       base_hum = base_hum, hum_amp = hum_amp,
       monsoon_months = monsoon_months, monsoon_rate = monsoon_rate)
}

for (i in seq_len(nrow(locations))) {
  loc <- locations[i, ]
  loc_id <- loc$location_id
  clim <- get_climatology(loc_id, loc$latitude)

  # 1. Temperature cycle with daily AR(1) autocorrelation
  seasonal_t <- clim$base_t + clim$amp_t * sin(2 * pi * (d_yday - clim$peak_day_t + 90) / 365.25)
  t_noise <- as.numeric(stats::arima.sim(model = list(ar = 0.6), n = n_days, sd = 1.6))
  sim_t <- seasonal_t + t_noise

  # 2. Rainfall model (Zero-inflated Gamma / Exponential)
  is_monsoon <- d_months %in% clim$monsoon_months
  rain_prob <- ifelse(is_monsoon, 0.48, 0.05)
  rain_event <- stats::rbinom(n_days, size = 1, prob = rain_prob)
  rain_amounts <- stats::rexp(n_days, rate = 1 / clim$monsoon_rate)
  sim_precip <- round(rain_event * rain_amounts, 1)

  # 3. Humidity cycle (higher in monsoon, lower in winter/spring)
  seasonal_hum <- clim$base_hum + clim$hum_amp * ifelse(is_monsoon, 0.8, -0.4)
  hum_noise <- as.numeric(stats::arima.sim(model = list(ar = 0.5), n = n_days, sd = 4.0))
  sim_hum <- pmin(pmax(seasonal_hum + hum_noise, 15), 98)

  # -----------------------------------------------------------------
  # INJECT KNOWN DOCUMENTED ANOMALIES FOR RIGOROUS VALIDATION
  # -----------------------------------------------------------------
  # 1. May 2022 Heatwave (Delhi, Jaipur, Bhopal): +4.0°C
  if (loc_id %in% c("delhi", "jaipur", "bhopal")) {
    mask_may22 <- (d_years == 2022 & d_months == 5)
    sim_t[mask_may22] <- sim_t[mask_may22] + 4.2
  }

  # 2. Chennai Dec 2015 & Dec 2023 Deluge: Extreme rainfall
  if (loc_id == "chennai") {
    mask_dec15 <- (d_years == 2015 & d_months == 12 & d_yday %in% 335:345)
    sim_precip[mask_dec15] <- sim_precip[mask_dec15] + stats::rgamma(sum(mask_dec15), shape = 3, scale = 40)
    
    mask_dec23 <- (d_years == 2023 & d_months == 12 & d_yday %in% 336:340)
    sim_precip[mask_dec23] <- sim_precip[mask_dec23] + stats::rgamma(sum(mask_dec23), shape = 4, scale = 35)
  }

  # 3. Jan 2023 Cold Wave (Delhi, Lucknow): -4.0°C
  if (loc_id %in% c("delhi", "lucknow")) {
    mask_jan23 <- (d_years == 2023 & d_months == 1)
    sim_t[mask_jan23] <- sim_t[mask_jan23] - 4.1
  }

  # 4. Aug 2023 Dry Monsoon (Bengaluru, Mumbai): virtually zero rain
  if (loc_id %in% c("bengaluru", "mumbai")) {
    mask_aug23 <- (d_years == 2023 & d_months == 8)
    sim_precip[mask_aug23] <- sim_precip[mask_aug23] * 0.05
  }

  # 5. May 2021 High Humidity (Mumbai): +16%
  if (loc_id == "mumbai") {
    mask_may21 <- (d_years == 2021 & d_months == 5)
    sim_hum[mask_may21] <- pmin(sim_hum[mask_may21] + 16, 96)
  }

  # -----------------------------------------------------------------
  # INJECT TESTING ARTIFACTS (Sentinels, Missingness, Coverage Drops)
  # -----------------------------------------------------------------
  # A. A few sentinel values (-999) to verify cleaning
  if (loc_id == "pune") {
    sim_t[100:102] <- -999.0
    sim_precip[103:104] <- -999.0
  }

  # B. An intentionally low-coverage month (<90%) for quality filter tests
  # e.g., Nagpur in Feb 2019: only 8 days observed out of 28 (< 30% coverage)
  if (loc_id == "nagpur") {
    mask_feb19_drop <- (d_years == 2019 & d_months == 2 & as.integer(format(dates, "%d")) > 8)
    sim_t[mask_feb19_drop] <- NA
    sim_precip[mask_feb19_drop] <- NA
    sim_hum[mask_feb19_drop] <- NA
  }

  # Build data frame
  loc_df <- data.frame(
    location_id = loc_id,
    location_name = loc$location_name,
    latitude = loc$latitude,
    longitude = loc$longitude,
    date = dates,
    temperature_c = round(sim_t, 2),
    precipitation_mm = round(sim_precip, 2),
    humidity_pct = round(sim_hum, 2),
    source = "SYNTHETIC_GENERATOR",
    is_synthetic = TRUE,
    quality_flag = "VALID",
    stringsAsFactors = FALSE
  )

  # C. Inject duplicate test rows (2 duplicates for Ahmedabad)
  if (loc_id == "ahmedabad") {
    dup_rows <- loc_df[1:2, ]
    loc_df <- rbind(loc_df, dup_rows)
  }

  all_dfs[[loc_id]] <- loc_df
}

raw_combined <- do.call(rbind, all_dfs)
cat(sprintf("Raw synthetic observations generated: %d records across %d locations.\n",
            nrow(raw_combined), length(all_dfs)))

# Clean generated data using standard cleaning pipeline
cat("Passing synthetic data through cleaning pipeline...\n")
cleaned_demo <- clean_daily_weather_data(raw_combined)
clean_stats <- summarize_cleaning_metrics(raw_combined, cleaned_demo)

cat(sprintf("Cleaning complete:\n  Cleaned records: %d\n  Duplicates removed: %d\n  Missing temp: %d\n  Missing precip: %d\n",
            clean_stats$cleaned_record_count,
            clean_stats$duplicates_removed,
            clean_stats$missing_temp_count,
            clean_stats$missing_precip_count))

# Save to partitioned Parquet
cat("Saving cleaned demo data to partitioned Parquet (data/processed/demo/)...")
save_daily_data_parquet(cleaned_demo, output_dir = "data/processed/demo", mode = "demo")
cat(" DONE.\n")

# Compute and cache monthly aggregates for responsive dashboard
cat("Precomputing DuckDB monthly aggregates...")
source("R/aggregation.R")
monthly_demo <- compute_monthly_aggregates_duckdb(cleaned_demo, min_coverage = 0.90)
saveRDS(monthly_demo, "data/processed/demo/monthly_aggregates.rds")
cat(sprintf(" DONE. (Computed %d monthly records)\n", nrow(monthly_demo)))

cat("\nSUCCESS: Synthetic demonstration dataset is ready.\n")
