testthat::context("Automated Climate Risk Index & Advisory Engine")

source("../../R/config.R")
source("../../R/anomaly_detection.R")
source("../../R/climate_risk_engine.R")

testthat::test_that("calculate_station_risk_profile evaluates scores, tiers, and playbook", {
  # Synthetic monthly series for 10 years
  years <- 2010:2020
  months <- 1:12
  grid <- expand.grid(year = years, month = months)
  grid$location_id <- "LOC_TEST"
  grid$location_name <- "Test Station"
  grid$is_usable_precip <- TRUE
  grid$is_usable_temp <- TRUE
  grid$mean_temperature_c <- 25 + sin(grid$month / 12 * 2 * pi) * 10
  grid$total_precipitation_mm <- pmax(0, 50 + rnorm(nrow(grid), mean = 30, sd = 40))
  grid$total_precipitation_mm[grid$month %in% 7:8] <- grid$total_precipitation_mm[grid$month %in% 7:8] + 200

  prof <- calculate_station_risk_profile(grid, "LOC_TEST")
  testthat::expect_false(is.null(prof))
  testthat::expect_true(prof$composite_score >= 0 && prof$composite_score <= 100)
  testthat::expect_true(prof$flood_score >= 0 && prof$flood_score <= 100)
  testthat::expect_true(prof$drought_score >= 0 && prof$drought_score <= 100)
  testthat::expect_true(nzchar(prof$risk_cat$tier))
  testthat::expect_true(nzchar(prof$narrative))
  testthat::expect_equal(length(prof$playbook), 3)

  ui <- render_climate_risk_ui(prof)
  testthat::expect_false(is.null(ui))
})
