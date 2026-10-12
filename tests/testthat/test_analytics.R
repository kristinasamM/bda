# tests/testthat/test_analytics.R - Comprehensive Test Suite for ClimateWatch Analytics

library(testthat)

# Source core analytics modules
source("../../R/config.R")
source("../../R/data_cleaning.R")
source("../../R/storage.R")
source("../../R/aggregation.R")
source("../../R/anomaly_detection.R")
source("../../R/explanations.R")
source("../../R/forecasting.R")

test_that("Missing sentinel values are converted to NA and not zero", {
  raw_test <- data.frame(
    location_id = "test_loc",
    location_name = "Test City",
    latitude = 19.0,
    longitude = 72.8,
    date = as.Date("2020-01-01"),
    temperature_c = -999.0,
    precipitation_mm = -999.0,
    humidity_pct = -99.0,
    source = "TEST",
    is_synthetic = TRUE,
    quality_flag = "VALID",
    stringsAsFactors = FALSE
  )

  cleaned <- clean_daily_weather_data(raw_test)

  expect_true(is.na(cleaned$temperature_c[1]))
  expect_true(is.na(cleaned$precipitation_mm[1]))
  expect_false(identical(cleaned$precipitation_mm[1], 0)) # Rainfall must NOT be converted to 0
  expect_true(is.na(cleaned$humidity_pct[1]))
  expect_equal(cleaned$quality_flag[1], "MISSING")
})

test_that("Duplicate location-date records are strictly removed", {
  raw_dups <- data.frame(
    location_id = c("mumbai", "mumbai", "delhi"),
    location_name = c("Mumbai", "Mumbai", "Delhi"),
    latitude = c(19.07, 19.07, 28.61),
    longitude = c(72.87, 72.87, 77.20),
    date = as.Date(c("2021-05-15", "2021-05-15", "2021-05-15")),
    temperature_c = c(31.5, 31.9, 41.2),
    precipitation_mm = c(0, 0, 5),
    humidity_pct = c(75, 75, 30),
    source = "TEST",
    is_synthetic = TRUE,
    quality_flag = "VALID",
    stringsAsFactors = FALSE
  )

  cleaned <- clean_daily_weather_data(raw_dups)
  expect_equal(nrow(cleaned), 2)
  expect_equal(cleaned$temperature_c[cleaned$location_id == "mumbai"], 31.5)
})

test_that("Plausible-value validation flags outliers as NA and SUSPECT", {
  raw_outliers <- data.frame(
    location_id = "test",
    location_name = "Test",
    latitude = 20.0,
    longitude = 80.0,
    date = as.Date("2021-06-01"),
    temperature_c = 75.0, # Impossible > 60°C
    precipitation_mm = -5.0, # Negative rain
    humidity_pct = 150.0, # > 100%
    source = "TEST",
    is_synthetic = TRUE,
    quality_flag = "VALID",
    stringsAsFactors = FALSE
  )

  cleaned <- clean_daily_weather_data(raw_outliers)
  expect_true(is.na(cleaned$temperature_c[1]))
  expect_true(is.na(cleaned$precipitation_mm[1]))
  expect_true(is.na(cleaned$humidity_pct[1]))
  expect_equal(cleaned$quality_flag[1], "SUSPECT")
})

test_that("Monthly aggregation enforces minimum coverage rule and preserves incomplete rainfall as NA", {
  # Build a 31-day month with only 10 days of valid rain (< 90% coverage)
  dates_jan <- seq.Date(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "day")
  df_low_cov <- data.frame(
    location_id = "test_cov",
    location_name = "Coverage Station",
    latitude = 15.0,
    longitude = 75.0,
    date = dates_jan,
    temperature_c = rep(25.0, 31),
    precipitation_mm = c(rep(10.0, 10), rep(NA_real_, 21)), # 10/31 = 32.2% coverage
    humidity_pct = rep(60.0, 31),
    source = "TEST",
    is_synthetic = TRUE,
    quality_flag = "VALID",
    stringsAsFactors = FALSE
  )

  monthly <- compute_monthly_aggregates_duckdb(df_low_cov, min_coverage = 0.90)

  expect_equal(nrow(monthly), 1)
  expect_equal(monthly$temp_coverage_pct[1], 100.0)
  expect_equal(monthly$mean_temperature_c[1], 25.0)
  expect_equal(monthly$is_usable_temp[1], TRUE)

  # Rainfall coverage is 32.26% (< 90%), so total_precipitation_mm must be NA
  expect_true(monthly$precip_coverage_pct[1] < 90.0)
  expect_true(is.na(monthly$total_precipitation_mm[1]))
  expect_equal(monthly$is_usable_precip[1], FALSE)
  expect_equal(monthly$quality_status[1], "INSUFFICIENT_COVERAGE")
})

test_that("Seasonal baseline strictly separates baseline from analysis periods", {
  # Synthetic 20-year monthly data for 1 station
  base_years <- 2001:2015
  an_years <- 2016:2024
  all_years <- c(base_years, an_years)

  m_data <- expand.grid(year = all_years, month = 1:12)
  m_data$location_id <- "loc_test"
  m_data$location_name <- "Loc Test"
  m_data$latitude <- 20.0
  m_data$longitude <- 75.0
  m_data$year_month <- as.Date(sprintf("%04d-%02d-01", m_data$year, m_data$month))
  m_data$total_days_in_month <- 30
  m_data$temp_observed_days <- 30
  m_data$precip_observed_days <- 30
  m_data$humidity_observed_days <- 30
  m_data$temp_coverage_pct <- 100
  m_data$precip_coverage_pct <- 100
  m_data$humidity_coverage_pct <- 100
  # Controlled temperature: baseline mean = 25.0, analysis mean = 30.0
  m_data$mean_temperature_c <- ifelse(m_data$year <= 2015, 25.0 + rnorm(nrow(m_data), 0, 1.0), 30.0)
  m_data$total_precipitation_mm <- ifelse(m_data$month %in% 7:8, 200.0, 10.0)
  m_data$mean_humidity_pct <- 65.0
  m_data$quality_status <- "ADEQUATE_COVERAGE"
  m_data$is_usable_temp <- TRUE
  m_data$is_usable_precip <- TRUE
  m_data$is_usable_humidity <- TRUE
  m_data$is_synthetic <- TRUE

  baseline <- compute_seasonal_baseline(m_data, baseline_start = 2001, baseline_end = 2015, min_baseline_years = 5)
  expect_equal(nrow(baseline), 12)
  # All baseline records must use only N=15 years
  expect_true(all(baseline$temp_n == 15))
  expect_true(all(abs(baseline$temp_baseline_mean - 25.0) < 1.0))

  # Anomaly detection for 2016-2024
  anomalies <- detect_climate_anomalies(m_data, baseline, analysis_start = 2016, analysis_end = 2024)
  expect_true(all(anomalies$year >= 2016))
  expect_true(all(anomalies$year <= 2024))
  # With observed temp ~30 and baseline ~25, all should be positive deviations
  expect_true(all(anomalies$temp_deviation_c > 3.0))
})

test_that("Zero-variation and insufficient baseline samples are safely handled", {
  # Baseline with only 2 years (< 5 min required)
  m_short <- expand.grid(year = 2001:2002, month = 7)
  m_short$location_id <- "short"
  m_short$location_name <- "Short"
  m_short$latitude <- 20
  m_short$longitude <- 75
  m_short$mean_temperature_c <- c(28.0, 28.5)
  m_short$total_precipitation_mm <- c(150, 160)
  m_short$mean_humidity_pct <- c(70, 72)
  m_short$is_usable_temp <- TRUE
  m_short$is_usable_precip <- TRUE
  m_short$is_usable_humidity <- TRUE

  base_short <- compute_seasonal_baseline(m_short, baseline_start = 2001, baseline_end = 2002, min_baseline_years = 5)
  expect_true(is.na(base_short$temp_baseline_mean[base_short$month == 7]))

  # Test anomaly evaluation handles this without crashing
  obs_an <- data.frame(
    location_id = "short",
    location_name = "Short",
    latitude = 20,
    longitude = 75,
    year = 2020,
    month = 7,
    year_month = as.Date("2020-07-01"),
    temp_coverage_pct = 100,
    precip_coverage_pct = 100,
    humidity_coverage_pct = 100,
    quality_status = "ADEQUATE_COVERAGE",
    mean_temperature_c = 30.0,
    total_precipitation_mm = 200,
    mean_humidity_pct = 80,
    is_usable_temp = TRUE,
    is_usable_precip = TRUE,
    is_usable_humidity = TRUE,
    is_synthetic = TRUE,
    stringsAsFactors = FALSE
  )

  an_res <- detect_climate_anomalies(obs_an, base_short, analysis_start = 2020, analysis_end = 2020)
  expect_equal(an_res$temp_anomaly_type[1], "Insufficient Baseline Samples")
  expect_true(is.na(an_res$temp_z_score[1]))
})

test_that("Severity boundary thresholds match project specifications", {
  # Config thresholds: 1.5 mild, 2.0 moderate, 3.0 severe
  base_df <- data.frame(
    location_id = "thresh",
    location_name = "Thresh",
    latitude = 20,
    longitude = 75,
    month = 5,
    baseline_start_year = 2001,
    baseline_end_year = 2015,
    temp_n = 15,
    temp_baseline_mean = 30.0,
    temp_baseline_sd = 1.0,
    precip_n = 15,
    precip_baseline_mean = 50.0,
    precip_baseline_p10 = 10.0,
    precip_baseline_p90 = 90.0,
    humidity_n = 15,
    humidity_baseline_mean = 60.0,
    humidity_baseline_sd = 5.0,
    stringsAsFactors = FALSE
  )

  # Test exact z values: z = 1.4 (Normal), z = 1.8 (Mild), z = 2.5 (Moderate), z = 3.5 (Severe)
  test_temps <- c(31.4, 31.8, 32.5, 33.5, 28.2) # z = +1.4, +1.8, +2.5, +3.5, -1.8
  obs_rows <- data.frame(
    location_id = "thresh",
    location_name = "Thresh",
    latitude = 20,
    longitude = 75,
    year = 2020:2024,
    month = 5,
    year_month = as.Date(sprintf("%d-05-01", 2020:2024)),
    temp_coverage_pct = 100,
    precip_coverage_pct = 100,
    humidity_coverage_pct = 100,
    quality_status = "ADEQUATE_COVERAGE",
    mean_temperature_c = test_temps,
    total_precipitation_mm = 50,
    mean_humidity_pct = 60,
    is_usable_temp = TRUE,
    is_usable_precip = TRUE,
    is_usable_humidity = TRUE,
    is_synthetic = TRUE,
    stringsAsFactors = FALSE
  )

  res <- detect_climate_anomalies(obs_rows, base_df, analysis_start = 2020, analysis_end = 2024)

  expect_equal(res$temp_severity[1], "Normal")
  expect_equal(res$temp_severity[2], "Mild")
  expect_equal(res$temp_anomaly_type[2], "Unusually Hot (Mild)")
  expect_equal(res$temp_severity[3], "Moderate")
  expect_equal(res$temp_anomaly_type[3], "Unusually Hot (Moderate)")
  expect_equal(res$temp_severity[4], "Severe")
  expect_equal(res$temp_anomaly_type[4], "Unusually Hot (Severe)")
  expect_equal(res$temp_severity[5], "Mild")
  expect_equal(res$temp_anomaly_type[5], "Unusually Cold (Mild)")
})

test_that("Rainfall percentile ties and zero-heavy distributions are explicitly handled", {
  # Tied at zero: dry winter month where p10 = 0 and p90 = 2.0
  base_tied <- data.frame(
    location_id = "dry_loc",
    location_name = "Dry Station",
    latitude = 20,
    longitude = 75,
    month = 1,
    baseline_start_year = 2001,
    baseline_end_year = 2015,
    temp_n = 15, temp_baseline_mean = 20.0, temp_baseline_sd = 1.0,
    precip_n = 15, precip_baseline_mean = 1.0, precip_baseline_p10 = 0.0, precip_baseline_p90 = 2.0,
    humidity_n = 15, humidity_baseline_mean = 40.0, humidity_baseline_sd = 5.0,
    stringsAsFactors = FALSE
  )

  # When observed rain is 0, it must be flagged Normal, NOT Unusually Dry!
  obs_rain <- data.frame(
    location_id = "dry_loc",
    location_name = "Dry Station",
    latitude = 20, longitude = 75,
    year = 2022, month = 1,
    year_month = as.Date("2022-01-01"),
    temp_coverage_pct = 100, precip_coverage_pct = 100, humidity_coverage_pct = 100,
    quality_status = "ADEQUATE_COVERAGE",
    mean_temperature_c = 20.0,
    total_precipitation_mm = 0.0,
    mean_humidity_pct = 40.0,
    is_usable_temp = TRUE, is_usable_precip = TRUE, is_usable_humidity = TRUE,
    is_synthetic = TRUE,
    stringsAsFactors = FALSE
  )

  res_rain <- detect_climate_anomalies(obs_rain, base_tied, analysis_start = 2022, analysis_end = 2022)
  expect_equal(res_rain$precip_anomaly_flag[1], "Normal")
  expect_false(res_rain$precip_is_anomaly[1])

  # When rain exceeds p90, it must be flagged Unusually Wet
  obs_rain$total_precipitation_mm <- 15.0
  res_wet <- detect_climate_anomalies(obs_rain, base_tied, analysis_start = 2022, analysis_end = 2022)
  expect_equal(res_wet$precip_anomaly_flag[1], "Unusually Wet")
  expect_true(res_wet$precip_is_anomaly[1])
})

test_that("Explainable anomaly card content directly matches computed statistics", {
  rec <- data.frame(
    location_id = "mumbai",
    location_name = "Mumbai",
    latitude = 19.07, longitude = 72.87,
    year = 2022, month = 7,
    year_month = as.Date("2022-07-01"),
    temp_coverage_pct = 100.0, precip_coverage_pct = 100.0, humidity_coverage_pct = 100.0,
    quality_status = "ADEQUATE_COVERAGE",
    temperature_c = 30.8,
    temp_baseline_mean = 28.0,
    temp_baseline_sd = 1.0,
    temp_deviation_c = 2.8,
    temp_z_score = 2.8,
    temp_anomaly_type = "Unusually Hot (Moderate)",
    temp_severity = "Moderate",
    temp_is_anomaly = TRUE,
    precipitation_mm = 850.0,
    precip_baseline_mean = 700.0,
    precip_baseline_p10 = 500.0,
    precip_baseline_p90 = 950.0,
    precip_deviation_mm = 150.0,
    precip_anomaly_flag = "Normal",
    precip_is_anomaly = FALSE,
    humidity_pct = 85.0,
    humidity_baseline_mean = 80.0,
    humidity_baseline_sd = 3.0,
    humidity_deviation_pct = 5.0,
    humidity_z_score = 1.67,
    humidity_anomaly_flag = "Unusually High Humidity",
    humidity_is_anomaly = TRUE,
    baseline_period_label = "2001–2015 (N=15)",
    is_synthetic = TRUE,
    stringsAsFactors = FALSE
  )

  card_t <- generate_anomaly_card_data(rec, variable = "temperature")
  expect_equal(card_t$location, "Mumbai")
  expect_equal(card_t$month_year, "July 2022")
  expect_equal(card_t$observed_value, "30.8 °C")
  expect_equal(card_t$deviation, "+2.8 °C")
  expect_equal(card_t$severity, "Moderate")
  expect_match(card_t$explanation, "2.8°C above the historical July average")

  card_p <- generate_anomaly_card_data(rec, variable = "precipitation")
  expect_equal(card_p$observed_value, "850.0 mm")
  expect_match(card_p$baseline_reference, "P10: 500.0")

  card_h <- generate_anomaly_card_data(rec, variable = "humidity")
  expect_equal(card_h$anomaly_type, "Unusually High Humidity")
  expect_match(card_h$explanation, "unusually high humidity")
})

test_that("Forecasting strictly reserves the latest 12 consecutive months for chronological evaluation", {
  # Build 10 years of monthly data (120 months)
  dates_10yr <- seq.Date(as.Date("2010-01-01"), as.Date("2019-12-01"), by = "month")
  m_df <- data.frame(
    location_id = "fc_test",
    location_name = "Forecast City",
    latitude = 25.0, longitude = 80.0,
    year = as.integer(format(dates_10yr, "%Y")),
    month = as.integer(format(dates_10yr, "%m")),
    year_month = dates_10yr,
    mean_temperature_c = 25.0 + 8.0 * sin(2 * pi * as.integer(format(dates_10yr, "%m")) / 12),
    is_usable_temp = TRUE,
    stringsAsFactors = FALSE
  )

  fc_res <- forecast_temperature(m_df, location_id = "fc_test", horizon = 6, holdout_months = 12)

  expect_equal(fc_res$status, "SUCCESS")
  expect_equal(nrow(fc_res$holdout_df), 12)
  expect_equal(nrow(fc_res$future_df), 6)
  expect_equal(fc_res$future_df$date[1], as.Date("2020-01-01"))
  expect_equal(fc_res$future_df$date[6], as.Date("2020-06-01"))
  expect_equal(nrow(fc_res$comparison_table), 3) # snaive, ets, arima
  expect_true("Selected" %in% names(fc_res$comparison_table))
  expect_true(sum(grepl("Best", fc_res$comparison_table$Selected)) == 1)
})
