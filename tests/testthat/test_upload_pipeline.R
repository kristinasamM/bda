# tests/testthat/test_upload_pipeline.R - Test Suite for Actual Dataset Upload, Validation & Pipeline

library(testthat)

source("../../R/config.R")
source("../../R/data_cleaning.R")
source("../../R/storage.R")
source("../../R/aggregation.R")
source("../../R/anomaly_detection.R")
source("../../R/forecasting.R")
source("../../R/dataset_manager.R")

test_that("Column mapping and unit conversions correctly convert F to C, in to mm, and fraction to percent", {
  test_dir <- tempfile("upload_test_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Create a synthetic CSV with Fahrenheit, inches, and fraction humidity
  test_csv <- file.path(test_dir, "raw_units.csv")
  df_input <- data.frame(
    obs_date = c("2021-01-01", "2021-01-02", "2021-01-03"),
    station = c("S1", "S1", "S1"),
    temp_f = c(68.0, 86.0, 32.0), # 68°F = 20°C, 86°F = 30°C, 32°F = 0°C
    rain_in = c(0.0, 1.0, 2.0),   # 1 in = 25.4 mm, 2 in = 50.8 mm
    hum_ratio = c(0.50, 0.75, 1.0), # 50%, 75%, 100%
    stringsAsFactors = FALSE
  )
  write.csv(df_input, test_csv, row.names = FALSE)

  prev <- preview_uploaded_file(test_csv)
  expect_equal(prev$file_format, "csv")
  expect_true(all(c("obs_date", "station", "temp_f", "rain_in", "hum_ratio") %in% prev$columns))

  col_map <- list(
    col_date = "obs_date",
    col_location_id = "station",
    col_temperature = "temp_f",
    col_precipitation = "rain_in",
    col_humidity = "hum_ratio"
  )
  unit_map <- list(
    temp_unit = "fahrenheit",
    precip_unit = "inches",
    hum_unit = "fraction"
  )

  val_res <- validate_and_prepare_dataset(
    file_path = test_csv,
    dataset_name = "Unit Test",
    column_mapping = col_map,
    unit_mapping = unit_map,
    frequency_choice = "daily"
  )

  expect_true(val_res$is_valid)
  c_df <- val_res$cleaned_df

  # Verify conversions
  expect_equal(c_df$temperature_c[1], 20.0)
  expect_equal(c_df$temperature_c[2], 30.0)
  expect_equal(c_df$temperature_c[3], 0.0)

  expect_equal(c_df$precipitation_mm[1], 0.0)
  expect_equal(c_df$precipitation_mm[2], 25.4)
  expect_equal(c_df$precipitation_mm[3], 50.8)

  expect_equal(c_df$humidity_pct[1], 50.0)
  expect_equal(c_df$humidity_pct[2], 75.0)
  expect_equal(c_df$humidity_pct[3], 100.0)
})

test_that("Conflicting location-date duplicates strictly block activation while exact duplicates are removed with count", {
  test_dir <- tempfile("dup_test_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # CSV with 1 exact duplicate and 1 conflicting duplicate
  test_csv <- file.path(test_dir, "dups.csv")
  df_dups <- data.frame(
    date = c("2021-05-01", "2021-05-01", "2021-05-02", "2021-05-02"),
    loc = c("locA", "locA", "locA", "locA"),
    temp = c(25.0, 25.0, 28.0, 32.5), # Row 1 & 2 exact; Row 3 & 4 conflicting (same date, different temp)
    rain = c(0, 0, 5, 5),
    hum = c(50, 50, 60, 60),
    stringsAsFactors = FALSE
  )
  write.csv(df_dups, test_csv, row.names = FALSE)

  col_map <- list(col_date = "date", col_location_id = "loc", col_temperature = "temp", col_precipitation = "rain", col_humidity = "hum")

  val_res <- validate_and_prepare_dataset(
    file_path = test_csv,
    column_mapping = col_map,
    frequency_choice = "daily"
  )

  # Must block activation because of conflicting duplicates!
  expect_false(val_res$is_valid)
  expect_true(any(grepl("Activation blocked", val_res$errors)))
  expect_equal(val_res$exact_duplicates_count, 1) # Exact duplicate identified
})

test_that("Negative rainfall is strictly flagged and rejected", {
  test_dir <- tempfile("neg_rain_test_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  test_csv <- file.path(test_dir, "neg_rain.csv")
  df_neg <- data.frame(
    date = c("2021-06-01", "2021-06-02"),
    loc = c("MUM", "MUM"),
    temp = c(28.0, 29.0),
    rain = c(10.0, -5.0), # Negative rain!
    hum = c(80, 80),
    stringsAsFactors = FALSE
  )
  write.csv(df_neg, test_csv, row.names = FALSE)

  col_map <- list(col_date = "date", col_location_id = "loc", col_temperature = "temp", col_precipitation = "rain", col_humidity = "hum")
  val_res <- validate_and_prepare_dataset(test_csv, column_mapping = col_map, frequency_choice = "daily")

  expect_false(val_res$is_valid)
  expect_true(any(grepl("negative rainfall", val_res$errors, ignore.case = TRUE)))
})

test_that("Failed upload preserves the previously active dataset intact", {
  test_base <- tempfile("base_uploads_")
  ensure_upload_directories(test_base)
  on.exit(unlink(test_base, recursive = TRUE), add = TRUE)

  # 1. Activate a valid dataset
  valid_csv <- file.path(test_base, "valid.csv")
  df_valid <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-03-31"), by = "day"),
    loc = "CITY1",
    temperature_c = 25.0,
    precipitation_mm = 0.0,
    humidity_pct = 50.0,
    stringsAsFactors = FALSE
  )
  write.csv(df_valid, valid_csv, row.names = FALSE)
  col_map <- list(col_date = "date", col_location_id = "loc", col_temperature = "temperature_c", col_precipitation = "precipitation_mm", col_humidity = "humidity_pct")

  v_res <- validate_and_prepare_dataset(valid_csv, dataset_name = "Valid DS 1", column_mapping = col_map, frequency_choice = "daily")
  expect_true(v_res$is_valid)

  act1 <- activate_uploaded_dataset(v_res, valid_csv, base_dir = test_base)
  id1 <- act1$dataset_id
  expect_equal(get_active_dataset_id(test_base), id1)

  # 2. Attempt an invalid upload (conflicting duplicates)
  bad_csv <- file.path(test_base, "bad.csv")
  df_bad <- data.frame(
    date = c("2021-01-01", "2021-01-01"),
    loc = c("CITY2", "CITY2"),
    temperature_c = c(20.0, 30.0), # Conflict
    precipitation_mm = c(0, 0),
    humidity_pct = c(50, 50),
    stringsAsFactors = FALSE
  )
  write.csv(df_bad, bad_csv, row.names = FALSE)
  bad_res <- validate_and_prepare_dataset(bad_csv, column_mapping = col_map, frequency_choice = "daily")
  expect_false(bad_res$is_valid)

  # Active dataset must remain id1
  expect_equal(get_active_dataset_id(test_base), id1)
  loaded <- load_active_dataset(test_base)
  expect_equal(loaded$dataset_id, id1)
  expect_equal(loaded$meta$dataset_name, "Valid DS 1")
})

test_that("Dataset switching updates metadata without stale remnants", {
  test_base <- tempfile("switch_test_")
  ensure_upload_directories(test_base)
  on.exit(unlink(test_base, recursive = TRUE), add = TRUE)

  # Dataset 1
  csv1 <- file.path(test_base, "ds1.csv")
  df1 <- data.frame(
    date = seq.Date(as.Date("2018-01-01"), as.Date("2018-06-30"), by = "day"),
    loc = "LOC_ALPHA",
    temperature_c = 22.0,
    precipitation_mm = 5.0,
    humidity_pct = 55.0,
    stringsAsFactors = FALSE
  )
  write.csv(df1, csv1, row.names = FALSE)
  col_map <- list(col_date = "date", col_location_id = "loc", col_temperature = "temperature_c", col_precipitation = "precipitation_mm", col_humidity = "humidity_pct")

  v1 <- validate_and_prepare_dataset(csv1, dataset_name = "Alpha Dataset", column_mapping = col_map)
  act1 <- activate_uploaded_dataset(v1, csv1, base_dir = test_base)

  # Dataset 2
  csv2 <- file.path(test_base, "ds2.csv")
  df2 <- data.frame(
    date = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"),
    loc = "LOC_BETA",
    temperature_c = 31.0,
    precipitation_mm = 12.0,
    humidity_pct = 70.0,
    stringsAsFactors = FALSE
  )
  write.csv(df2, csv2, row.names = FALSE)

  v2 <- validate_and_prepare_dataset(csv2, dataset_name = "Beta Dataset", column_mapping = col_map)
  act2 <- activate_uploaded_dataset(v2, csv2, base_dir = test_base)

  # Verify act2 is active
  expect_equal(get_active_dataset_id(test_base), act2$dataset_id)
  cur <- load_active_dataset(test_base)
  expect_equal(cur$meta$dataset_name, "Beta Dataset")
  expect_equal(unique(cur$monthly_data$location_id), "LOC_BETA")

  # Switch back to act1
  set_active_dataset_id(act1$dataset_id, base_dir = test_base)
  cur_switched <- load_active_dataset(test_base)
  expect_equal(cur_switched$meta$dataset_name, "Alpha Dataset")
  expect_equal(unique(cur_switched$monthly_data$location_id), "LOC_ALPHA")
  expect_false("LOC_BETA" %in% cur_switched$monthly_data$location_id)
})

test_that("Conflicting duplicates are automatically resolved when auto_resolve_duplicates = TRUE", {
  test_dir <- tempfile("auto_resolve_test_")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  test_csv <- file.path(test_dir, "conflicts.csv")
  df_conf <- data.frame(
    date = c("2021-05-01", "2021-05-01", "2021-05-02"),
    loc = c("locA", "locA", "locA"),
    temp = c(20.0, 30.0, 25.0), # Row 1 & 2 conflict: mean should be 25.0
    rain = c(2.0, 4.0, 1.0),
    hum = c(50.0, 70.0, 60.0),
    stringsAsFactors = FALSE
  )
  write.csv(df_conf, test_csv, row.names = FALSE)

  col_map <- list(col_date = "date", col_location_id = "loc", col_temperature = "temp", col_precipitation = "rain", col_humidity = "hum")

  val_res <- validate_and_prepare_dataset(
    file_path = test_csv,
    column_mapping = col_map,
    frequency_choice = "daily",
    auto_resolve_duplicates = TRUE
  )

  expect_true(val_res$is_valid)
  expect_equal(nrow(val_res$cleaned_df), 2)
  expect_equal(val_res$cleaned_df$temperature_c[val_res$cleaned_df$date == as.Date("2021-05-01")], 25.0)
  expect_equal(val_res$cleaned_df$humidity_pct[val_res$cleaned_df$date == as.Date("2021-05-01")], 60.0)
})

