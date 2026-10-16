# tests/testthat/test_novelty_features.R - Test Suite for Anomaly Detective, Climate Twins, and Relationships

library(testthat)

source_module <- function(mod_name) {
  candidates <- c(
    file.path("R", mod_name),
    file.path("../R", mod_name),
    file.path("../../R", mod_name)
  )
  for (p in candidates) {
    if (file.exists(p)) {
      source(p)
      return(invisible(TRUE))
    }
  }
  stop("Module not found: ", mod_name)
}

source_module("config.R")
source_module("storage.R")
source_module("aggregation.R")
source_module("anomaly_detection.R")
source_module("forecasting.R")
source_module("dataset_manager.R")
source_module("anomaly_detective.R")
source_module("climate_twins.R")
source_module("relationships.R")

test_that("Anomaly Detective calculates rainfall top 3 daily contributions and share correctly", {
  # Build a 31-day month of daily rainfall
  dates_aug <- seq.Date(as.Date("2020-08-01"), as.Date("2020-08-31"), by = "day")
  # 28 days with 5 mm, and 3 heavy days with 50 mm, 40 mm, 30 mm
  # Total: (28 * 5) + 50 + 40 + 30 = 140 + 120 = 260 mm
  # Top 3 days: 50 + 40 + 30 = 120 mm
  # Top 3 share: 120 / 260 = 46.15% (round to 46.2%)
  rain_vals <- c(50.0, 40.0, 30.0, rep(5.0, 28))
  daily_df <- data.frame(
    date = dates_aug,
    day = 1:31,
    location_id = "STN_TEST",
    location_name = "Test Station",
    precipitation_mm = rain_vals,
    temperature_c = rep(28.0, 31),
    humidity_pct = rep(80.0, 31),
    stringsAsFactors = FALSE
  )

  monthly_row <- list(total_precipitation_mm = 260.0)
  baseline_row <- list(precip_baseline_mean = 180.0, precip_baseline_p10 = 100.0, precip_baseline_p90 = 220.0)

  res <- analyze_monthly_anomaly_details(daily_df, monthly_row, baseline_row, variable = "precipitation_mm")

  expect_equal(res$status, "SUCCESS")
  expect_equal(res$valid_days, 31)
  expect_equal(res$missing_days, 0)
  expect_equal(res$coverage_pct, 100.0)
  expect_equal(res$monthly_observed, 260.0)
  expect_equal(res$top3_share_pct, 46.2)
  expect_equal(res$highest_day$value, 50.0)
  expect_true(nzchar(res$narrative))
  expect_match(res$disclaimer, "not confirmed civic flooding")
})

test_that("Anomaly Detective diagnoses temperature departures from baseline reference", {
  dates_may <- seq.Date(as.Date("2020-05-01"), as.Date("2020-05-31"), by = "day")
  # 25 days warmer than 32.0 baseline, 6 days cooler -> 25/31 = 80.6% (>60% widespread)
  temps <- c(rep(35.0, 25), rep(30.0, 6))
  daily_df <- data.frame(
    date = dates_may,
    day = 1:31,
    location_id = "STN_HEAT",
    location_name = "Heat City",
    temperature_c = temps,
    precipitation_mm = rep(0, 31),
    humidity_pct = rep(45, 31),
    stringsAsFactors = FALSE
  )

  monthly_row <- list(mean_temperature_c = mean(temps))
  baseline_row <- list(temp_baseline_mean = 32.0, temp_baseline_sd = 1.2)

  res <- analyze_monthly_anomaly_details(daily_df, monthly_row, baseline_row, variable = "temperature_c")

  expect_equal(res$status, "SUCCESS")
  expect_equal(res$days_exceeding_baseline, 25)
  expect_equal(res$days_exceeding_pct, 80.6)
  expect_equal(res$diagnosis_type, "Widespread Sustained Departure")
  expect_match(res$narrative, "widespread sustained departure")
})

test_that("Sensitivity check temporarily excludes days and enforces 90% monthly coverage rule without modifying data", {
  dates_july <- seq.Date(as.Date("2020-07-01"), as.Date("2020-07-31"), by = "day")
  rain_vals <- c(150.0, 100.0, rep(10.0, 29)) # Total = 150 + 100 + 290 = 540 mm
  daily_df <- data.frame(
    date = dates_july,
    day = 1:31,
    location_id = "STN_MUMBAI",
    precipitation_mm = rain_vals,
    stringsAsFactors = FALSE
  )
  orig_vals_copy <- daily_df$precipitation_mm

  # Exclude 1 day (July 1st: 150mm). Retains 30/31 = 96.8% coverage >= 90% -> Eligible!
  sens_1 <- run_sensitivity_check(
    daily_df = daily_df,
    excluded_dates = as.Date("2020-07-01"),
    variable = "precipitation_mm",
    original_monthly_val = 540.0
  )

  expect_true(sens_1$is_eligible)
  expect_equal(sens_1$valid_retained_days, 30)
  expect_equal(sens_1$exploratory_value, 390.0) # 540 - 150 = 390 mm
  expect_equal(sens_1$difference, -150.0)

  # Original data must NOT be modified
  expect_equal(daily_df$precipitation_mm, orig_vals_copy)

  # Exclude 4 days. Retains 27/31 = 87.1% coverage < 90% -> Ineligible under coverage rule!
  sens_4 <- run_sensitivity_check(
    daily_df = daily_df,
    excluded_dates = as.Date(c("2020-07-01", "2020-07-02", "2020-07-03", "2020-07-04")),
    variable = "precipitation_mm",
    original_monthly_val = 540.0
  )

  expect_false(sens_4$is_eligible)
  expect_match(sens_4$status_description, "Ineligible")
  expect_equal(sens_4$valid_retained_days, 27)
})

test_that("Climate Twin Finder profiles locations, uses log1p for rain, and computes RMSD distance", {
  # Build a 2-year monthly dataset for 4 stations
  months_seq <- 1:12
  years_seq <- 2018:2019
  grid <- expand.grid(year = years_seq, month = months_seq)
  
  # Station A (Reference): Mumbai-like
  stn_a <- grid
  stn_a$location_id <- "LOC_MUMBAI"
  stn_a$location_name <- "Mumbai Coastal"
  stn_a$mean_temperature_c <- 27 + sin((stn_a$month - 4)/12 * 2 * pi) * 4
  stn_a$total_precipitation_mm <- ifelse(stn_a$month %in% 6:9, 600, 10)
  stn_a$is_usable_temp <- TRUE
  stn_a$is_usable_precip <- TRUE

  # Station B (Twin candidate): Goa-like (very close to Mumbai)
  stn_b <- grid
  stn_b$location_id <- "LOC_GOA"
  stn_b$location_name <- "Goa Coastal"
  stn_b$mean_temperature_c <- 27 + sin((stn_b$month - 4)/12 * 2 * pi) * 3.8
  stn_b$total_precipitation_mm <- ifelse(stn_b$month %in% 6:9, 580, 15)
  stn_b$is_usable_temp <- TRUE
  stn_b$is_usable_precip <- TRUE

  # Station C (Twin candidate): Delhi-like (very different: cold winter, hot dry summer)
  stn_c <- grid
  stn_c$location_id <- "LOC_DELHI"
  stn_c$location_name <- "Delhi Inland"
  stn_c$mean_temperature_c <- 25 + sin((stn_c$month - 4)/12 * 2 * pi) * 12 # high range
  stn_c$total_precipitation_mm <- ifelse(stn_c$month %in% 7:8, 200, 20)
  stn_c$is_usable_temp <- TRUE
  stn_c$is_usable_precip <- TRUE

  all_monthly <- rbind(stn_a, stn_b, stn_c)

  profiles <- compute_climatological_profiles(
    monthly_df = all_monthly,
    location_ids = c("LOC_MUMBAI", "LOC_GOA", "LOC_DELHI"),
    start_year = 2018,
    end_year = 2019,
    variables = c("mean_temperature_c", "total_precipitation_mm")
  )

  expect_equal(length(profiles$eligible), 3)

  twins_res <- calculate_climate_twins(
    eligible_profiles = profiles$eligible,
    reference_loc_id = "LOC_MUMBAI",
    variables = c("mean_temperature_c", "total_precipitation_mm")
  )

  expect_equal(twins_res$status, "SUCCESS")
  expect_equal(twins_res$reference_location_id, "LOC_MUMBAI")
  # Goa must be closer to Mumbai than Delhi
  top_matches <- twins_res$top_matches_table
  expect_equal(top_matches$location_id[1], "LOC_GOA")
  expect_true(top_matches$distance_score[1] < top_matches$distance_score[2])
  expect_false("LOC_MUMBAI" %in% top_matches$location_id) # Reference excluded from its own matches!
})

test_that("Weather variable relationships strictly exclude identifier codes", {
  daily_df <- data.frame(
    id = 1:50,
    state_code = rep(27, 50),
    district_code = rep(518, 50),
    elevation = rep(14.0, 50),
    latitude = rep(19.07, 50),
    longitude = rep(72.87, 50),
    temperature_c = 25 + rnorm(50, 5, 2),
    precipitation_mm = pmax(0, rnorm(50, 20, 15)),
    humidity_pct = 70 + rnorm(50, 10, 5),
    stringsAsFactors = FALSE
  )

  rel <- compute_weather_correlations(daily_df)

  expect_equal(rel$status, "SUCCESS")
  # Strictly only temperature_c, precipitation_mm, humidity_pct
  expect_true(all(rel$variables %in% c("temperature_c", "precipitation_mm", "humidity_pct")))
  expect_false("id" %in% rel$variables)
  expect_false("state_code" %in% rel$variables)
  expect_false("district_code" %in% rel$variables)
  expect_false("elevation" %in% rel$variables)
  expect_equal(nrow(rel$cor_matrix), 3)
})
