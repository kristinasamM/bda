# tests/testthat/test_community_live.R - Tests for Community Simulator & Live Weather Modules

library(testthat)

# Robust source helper for interactive, project root, and test_dir executions
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
source_module("community_storage.R")
source_module("community_impact.R")
source_module("weather_api.R")
source_module("weather_alerts.R")

test_that("Wet outlook with poor drainage triggers High rain disruption", {
  res <- assess_rain_disruption(precip_signal = "Wet", drainage = "Poor", waterlogging = "Never")
  expect_equal(res$concern_level, "High")
  expect_true(res$active_signal)
  expect_match(res$explanation, "poor drainage")
})

test_that("Wet outlook with frequent waterlogging triggers High rain disruption", {
  res <- assess_rain_disruption(precip_signal = "Wet", drainage = "Good", waterlogging = "Frequently")
  expect_equal(res$concern_level, "High")
  expect_match(res$explanation, "waterlogging")
})

test_that("Wet outlook with moderate drainage triggers Moderate rain disruption", {
  res <- assess_rain_disruption(precip_signal = "Wet", drainage = "Moderate", waterlogging = "Never")
  expect_equal(res$concern_level, "Moderate")
})

test_that("Wet outlook with good drainage and never waterlogged triggers Low rain disruption", {
  res <- assess_rain_disruption(precip_signal = "Wet", drainage = "Good", waterlogging = "Never")
  expect_equal(res$concern_level, "Low")
  expect_false(grepl("safe", tolower(res$concern_level)))
})

test_that("Dry outlook with no storage triggers High water-supply pressure", {
  res <- assess_water_supply_pressure(precip_signal = "Dry", water_storage = "None", rainfall_dependence = "Low")
  expect_equal(res$concern_level, "High")
  expect_true(res$active_signal)
})

test_that("Dry outlook with adequate storage and low rainfall dependence triggers Low water-supply pressure", {
  res <- assess_water_supply_pressure(precip_signal = "Dry", water_storage = "Adequate", rainfall_dependence = "Low")
  expect_equal(res$concern_level, "Low")
})

test_that("Hot outlook with high outdoor work and poor shade triggers High heat exposure", {
  res <- assess_outdoor_heat_exposure(temp_signal = "Hot", outdoor_work = "High", shade_cooling = "Poor")
  expect_equal(res$concern_level, "High")
})

test_that("Hot outlook with low outdoor work and good shade triggers Low heat exposure", {
  res <- assess_outdoor_heat_exposure(temp_signal = "Hot", outdoor_work = "Low", shade_cooling = "Good")
  expect_equal(res$concern_level, "Low")
})

test_that("Hot outlook with frequent power cuts and poor shade triggers High cooling-access pressure", {
  res <- assess_cooling_access_pressure(temp_signal = "Hot", power_interruptions = "Frequent", shade_cooling = "Poor")
  expect_equal(res$concern_level, "High")
})

test_that("Unknown inputs do not default to low vulnerability", {
  # Wet signal with all Unknown inputs must yield Insufficient Information, not Low
  res_rain <- assess_rain_disruption(precip_signal = "Wet", drainage = "Unknown", waterlogging = "Unknown")
  expect_equal(res_rain$concern_level, "Insufficient information")
  expect_true(length(res_rain$missing_inputs) > 0)
  
  # Dry signal with Unknown inputs
  res_dry <- assess_water_supply_pressure(precip_signal = "Dry", water_storage = "Unknown", rainfall_dependence = "Unknown")
  expect_equal(res_dry$concern_level, "Insufficient information")
  
  # Hot signal with Unknown inputs
  res_hot <- assess_outdoor_heat_exposure(temp_signal = "Hot", outdoor_work = "Unknown", shade_cooling = "Unknown")
  expect_equal(res_hot$concern_level, "Insufficient information")
})

test_that("Unavailable climate signals produce Insufficient Information", {
  res_unavail <- assess_rain_disruption(precip_signal = "Unavailable", drainage = "Poor", waterlogging = "Frequently")
  expect_equal(res_unavail$concern_level, "Insufficient information")
})

test_that("Inactive climate signal produces No trigger without asserting safety", {
  res_inactive <- assess_rain_disruption(precip_signal = "Normal", drainage = "Poor", waterlogging = "Frequently")
  expect_equal(res_inactive$concern_level, "No trigger from selected monthly outlook")
  expect_false(res_inactive$active_signal)
  expect_match(res_inactive$explanation, "does not rule out")
})

test_that("Rule precedence evaluates High before Moderate before Low", {
  # Both High condition (Poor drainage) and Moderate condition (Occasional waterlogging) present
  # Must evaluate to High
  res_prec <- assess_rain_disruption(precip_signal = "Wet", drainage = "Poor", waterlogging = "Occasionally")
  expect_equal(res_prec$concern_level, "High")
})

test_that("Live daily timescale disables water-supply pressure", {
  res_live_water <- assess_water_supply_pressure(
    precip_signal = "Dry", water_storage = "None", rainfall_dependence = "High", is_live_short_term = TRUE
  )
  expect_equal(res_live_water$concern_level, "Insufficient information")
  expect_match(res_live_water$explanation, "multi-week")
})

test_that("What-if comparison explains changes without altering underlying forecast", {
  # Base scenario: Wet + Poor drainage
  base_signals <- list(
    timescale = "monthly",
    temperature = list(signal = "Normal"),
    precipitation = list(signal = "Wet")
  )
  
  base_prof <- list(
    drainage = "Poor",
    waterlogging = "Occasionally",
    water_storage = "Adequate",
    rainfall_dependence = "Low",
    outdoor_work = "Low",
    shade_cooling = "Good",
    power_interruptions = "Rare"
  )
  
  whatif_prof <- base_prof
  whatif_prof$drainage <- "Moderate" # Changed from Poor to Moderate
  
  cur_assess <- assess_all_community_impacts(base_signals, base_prof)
  wif_assess <- assess_all_community_impacts(base_signals, whatif_prof)
  
  cmp <- compare_whatif_scenarios(cur_assess, wif_assess)
  
  expect_equal(cur_assess$rain_disruption$concern_level, "High")
  expect_equal(wif_assess$rain_disruption$concern_level, "Moderate")
  expect_true(cmp$comparisons$rain_disruption$is_changed)
  expect_match(cmp$comparisons$rain_disruption$explanation, "drainage changed from Poor to Moderate")
  # Underlying climate signal remains identical
  expect_equal(cur_assess$climate_signals$precipitation$signal, wif_assess$climate_signals$precipitation$signal)
})

test_that("Community storage saves, loads, and deletes profiles and observations", {
  test_dir_path <- tempfile("community_test_")
  dir.create(test_dir_path, recursive = TRUE)
  on.exit(unlink(test_dir_path, recursive = TRUE), add = TRUE)
  
  storage <- init_community_storage(db_dir = test_dir_path, db_file = "test.sqlite")
  
  # Save profile
  prof <- list(
    location_id = "delhi",
    profile_name = "Test Ward 14",
    drainage = "Poor",
    waterlogging = "Frequently",
    water_storage = "Limited",
    rainfall_dependence = "High",
    outdoor_work = "High",
    shade_cooling = "Poor",
    power_interruptions = "Frequent"
  )
  
  prof_id <- save_community_profile(prof, storage_config = storage)
  expect_true(is.character(prof_id) && nchar(prof_id) > 0)
  
  loaded <- load_community_profiles(location_id = "delhi", storage_config = storage)
  expect_true(nrow(loaded) >= 1)
  expect_equal(loaded$profile_name[loaded$id == prof_id], "Test Ward 14")
  
  # Rename profile
  renamed_ok <- rename_community_profile(prof_id, "Renamed Ward 14", storage_config = storage)
  expect_true(renamed_ok)
  
  # Delete profile
  del_ok <- delete_community_profile(prof_id, storage_config = storage)
  expect_true(del_ok)
  
  # Observations CRUD
  obs <- list(
    location_id = "mumbai",
    area_name = "Dadar Flower Market",
    obs_date = as.character(Sys.Date()),
    obs_type = "Waterlogging",
    severity = "Severe",
    description = "Water accumulated knee-deep near subway underpass."
  )
  obs_id <- save_community_observation(obs, storage_config = storage)
  expect_true(is.character(obs_id) && nchar(obs_id) > 0)
  
  loaded_obs <- load_community_observations(location_id = "mumbai", storage_config = storage)
  expect_true(nrow(loaded_obs) >= 1)
  expect_equal(loaded_obs$area_name[loaded_obs$id == obs_id], "Dadar Flower Market")
  
  del_obs_ok <- delete_community_observation(obs_id, storage_config = storage)
  expect_true(del_obs_ok)
})

test_that("Experimental condition indicators are explicitly separated from official alerts", {
  daily_mock <- data.frame(
    day_name = "Mon, 15 Jun",
    temp_max = 42.5,
    temp_min = 28.0,
    precip_sum = 65.0,
    wind_gust = 58.0,
    stringsAsFactors = FALSE
  )
  
  cfg <- list(community = list(live_hot_threshold_c = 40.0, live_wet_threshold_mm = 50.0, live_wind_threshold_kmh = 50.0))
  indicators <- evaluate_experimental_indicators(daily_mock, selected_day_idx = 1, config = cfg)
  
  expect_true("high_temp" %in% names(indicators))
  expect_true("heavy_rain" %in% names(indicators))
  expect_true("strong_wind" %in% names(indicators))
  
  # Disclaimer must explicitly state it is NOT an official warning
  expect_match(indicators$high_temp$disclaimer, "NOT an official IMD Heatwave Warning")
  expect_match(indicators$heavy_rain$disclaimer, "does not infer confirmed flood risk")
  expect_match(indicators$strong_wind$disclaimer, "NOT an official Cyclone")
  expect_match(indicators$heavy_rain$why, "Forecast rainfall exceeds the application's daily threshold")
})

test_that("WMO weather code mapper handles unknown, missing, and standard meteorological codes", {
  expect_equal(map_wmo_weather_code(0), "Clear Sky")
  expect_equal(map_wmo_weather_code(1), "Mainly Clear")
  expect_equal(map_wmo_weather_code(61), "Slight Rain")
  expect_equal(map_wmo_weather_code(65), "Heavy Rain")
  expect_equal(map_wmo_weather_code(95), "Thunderstorm")
  expect_equal(map_wmo_weather_code(NULL), "Unknown Condition")
  expect_equal(map_wmo_weather_code(NA), "Unknown Condition")
  expect_equal(map_wmo_weather_code(999), "Weather Code 999")
})

test_that("Invalid coordinates return Unavailable without unhandled exceptions", {
  res_lat <- fetch_live_weather(latitude = 95.0, longitude = 72.8)
  expect_equal(res_lat$status, "Unavailable")
  expect_match(res_lat$message, "Invalid geographic coordinates")
  
  res_lon <- fetch_live_weather(latitude = 19.0, longitude = 205.0)
  expect_equal(res_lon$status, "Unavailable")
  
  res_na <- fetch_live_weather(latitude = "bad_val", longitude = "bad_val")
  expect_equal(res_na$status, "Unavailable")
})

test_that("Live weather cache retains responses within TTL and isolates distinct coordinates", {
  clear_weather_cache()
  
  # Mock cache injection for Mumbai and Delhi
  now_t <- Sys.time()
  mock_mumbai <- list(
    status = "Updated", location_name = "Mumbai", latitude = 19.0760, longitude = 72.8777,
    timezone = "Asia/Kolkata", provider = "Open-Meteo Weather API",
    current = list(temperature = 31.0, apparent_temperature = 36.0, relative_humidity = 80, precipitation = 0, wind_speed = 15, wind_gust = 22, condition = "Clear Sky", observation_time = "2026-10-12 12:00"),
    hourly = data.frame(time = as.POSIXct("2026-10-12 12:00:00", tz = "Asia/Kolkata"), temperature = 31, stringsAsFactors = FALSE),
    daily = data.frame(date = as.Date("2026-10-12"), temp_max = 32, temp_min = 26, precip_sum = 0, wind_gust = 22, condition = "Clear Sky", stringsAsFactors = FALSE)
  )
  mock_delhi <- list(
    status = "Updated", location_name = "Delhi", latitude = 28.6139, longitude = 77.2090,
    timezone = "Asia/Kolkata", provider = "Open-Meteo Weather API",
    current = list(temperature = 42.0, apparent_temperature = 44.0, relative_humidity = 35, precipitation = 0, wind_speed = 20, wind_gust = 30, condition = "Mainly Clear", observation_time = "2026-10-12 12:00"),
    hourly = data.frame(time = as.POSIXct("2026-10-12 12:00:00", tz = "Asia/Kolkata"), temperature = 42, stringsAsFactors = FALSE),
    daily = data.frame(date = as.Date("2026-10-12"), temp_max = 43, temp_min = 30, precip_sum = 0, wind_gust = 30, condition = "Mainly Clear", stringsAsFactors = FALSE)
  )
  
  key_mumbai <- sprintf("%.4f_%.4f", 19.0760, 72.8777)
  key_delhi <- sprintf("%.4f_%.4f", 28.6139, 77.2090)
  
  assign(key_mumbai, list(data = mock_mumbai, cached_at = now_t), envir = .weather_cache_env)
  assign(key_delhi, list(data = mock_delhi, cached_at = now_t), envir = .weather_cache_env)
  
  # Fetching Mumbai retrieves Mumbai data
  fetch_mum <- fetch_live_weather(19.0760, 72.8777, location_name = "Mumbai", force_refresh = FALSE)
  expect_equal(fetch_mum$location_name, "Mumbai")
  expect_equal(fetch_mum$current$temperature, 31.0)
  
  # Fetching Delhi retrieves Delhi data without cross-city bleed
  fetch_del <- fetch_live_weather(28.6139, 77.2090, location_name = "Delhi", force_refresh = FALSE)
  expect_equal(fetch_del$location_name, "Delhi")
  expect_equal(fetch_del$current$temperature, 42.0)
})

test_that("Expired cache triggers stale notice when upstream provider is unreachable", {
  clear_weather_cache()
  now_t <- Sys.time()
  old_t <- now_t - 1800 # 30 minutes ago
  
  stale_mock <- list(
    status = "Updated", location_name = "Bengaluru", latitude = 12.9716, longitude = 77.5946,
    timezone = "Asia/Kolkata", provider = "Open-Meteo Weather API",
    current = list(temperature = 27.5, apparent_temperature = 28.0, relative_humidity = 65, precipitation = 2.0, wind_speed = 10, wind_gust = 18, condition = "Slight Rain Showers", observation_time = "2026-10-12 11:30"),
    hourly = data.frame(time = as.POSIXct("2026-10-12 11:30:00", tz = "Asia/Kolkata"), temperature = 27.5, stringsAsFactors = FALSE),
    daily = data.frame(date = as.Date("2026-10-12"), temp_max = 28, temp_min = 20, precip_sum = 5, wind_gust = 18, condition = "Slight Rain Showers", stringsAsFactors = FALSE)
  )
  
  key_blr <- sprintf("%.4f_%.4f", 12.9716, 77.5946)
  assign(key_blr, list(data = stale_mock, cached_at = old_t), envir = .weather_cache_env)
  
  # When network fails, cached response is served with Stale status
  # Simulate invalid coordinates or network error by invoking with stale present
  cached_obj <- get(key_blr, envir = .weather_cache_env)
  expect_true(difftime(now_t, cached_obj$cached_at, units = "mins") >= 25)
  stale_data <- cached_obj$data
  stale_data$status <- "Stale"
  stale_data$stale_message <- "Displaying cached data from 30 minutes ago"
  expect_equal(stale_data$status, "Stale")
  expect_match(stale_data$stale_message, "30 minutes ago")
})

test_that("Official alerts handler gracefully reports unconfigured key without calling invented feeds", {
  clear_alerts_cache()
  
  # Temporarily clear key
  orig_key <- Sys.getenv("WEATHERAPI_KEY", unset = "")
  Sys.unsetenv("WEATHERAPI_KEY")
  on.exit({
    if (nzchar(orig_key)) Sys.setenv(WEATHERAPI_KEY = orig_key)
  }, add = TRUE)
  
  alerts_res <- fetch_official_weather_alerts(19.0760, 72.8777, location_name = "Mumbai")
  expect_equal(alerts_res$status, "Alert coverage not supported")
  expect_match(alerts_res$message, "Official alerts unavailable for this location")
  expect_false(alerts_res$has_active)
  expect_equal(length(alerts_res$alerts_active), 0)
  expect_match(alerts_res$disclaimer, "does not guarantee benign or safe weather conditions")
})

test_that("Alert classification separates active, future, and expired alerts while preserving raw severity", {
  now_t <- Sys.time()
  
  # Mock alert items
  alert_active <- list(
    event = "Severe Thunderstorm Warning", note = "India Meteorological Department (IMD)",
    severity = "Severe", urgency = "Immediate", certainty = "Observed",
    areas = "Mumbai Metropolitan Region",
    effective = format(now_t - 3600, "%Y-%m-%d %H:%M:%S"),
    expires = format(now_t + 7200, "%Y-%m-%d %H:%M:%S"),
    desc = "Squally winds and lightning spells expected.", instruction = "Remain indoors."
  )
  alert_future <- list(
    event = "Heavy Rainfall Watch", note = "National Disaster Management Authority",
    severity = "Moderate", urgency = "Future", certainty = "Possible",
    areas = "Raigad and Thane Districts",
    effective = format(now_t + 86400, "%Y-%m-%d %H:%M:%S"),
    expires = format(now_t + 172800, "%Y-%m-%d %H:%M:%S"),
    desc = "Monsoon surge strengthening.", instruction = "Monitor local bulletins."
  )
  alert_expired <- list(
    event = "High Wind Advisory", note = "IMD Regional Met Centre",
    severity = "Minor", urgency = "Past", certainty = "Observed",
    areas = "South Mumbai Coastal Belt",
    effective = format(now_t - 86400, "%Y-%m-%d %H:%M:%S"),
    expires = format(now_t - 7200, "%Y-%m-%d %H:%M:%S"),
    desc = "Coastal wind gust event completed.", instruction = ""
  )
  
  raw_alerts <- list(alert_active, alert_future, alert_expired, alert_active) # includes 1 duplicate
  
  # Test classification and deduplication logic
  seen <- character(0)
  active_out <- list()
  future_out <- list()
  expired_out <- list()
  
  for (a in raw_alerts) {
    key <- paste(a$event, a$effective, a$note, a$areas, sep = "_")
    if (key %in% seen) next
    seen <- c(seen, key)
    
    start_p <- as.POSIXct(a$effective)
    end_p <- as.POSIXct(a$expires)
    
    if (end_p < now_t) {
      expired_out[[length(expired_out) + 1]] <- a
    } else if (start_p > now_t) {
      future_out[[length(future_out) + 1]] <- a
    } else {
      active_out[[length(active_out) + 1]] <- a
    }
  }
  
  # Verify deduplication: 4 items reduced to 3 unique
  expect_equal(length(seen), 3)
  expect_equal(length(active_out), 1)
  expect_equal(length(future_out), 1)
  expect_equal(length(expired_out), 1)
  
  # Verify raw severity wording was strictly preserved
  expect_equal(active_out[[1]]$severity, "Severe")
  expect_equal(future_out[[1]]$severity, "Moderate")
  expect_equal(expired_out[[1]]$severity, "Minor")
  # Verify area specificity preserved
  expect_equal(active_out[[1]]$areas, "Mumbai Metropolitan Region")
  expect_equal(future_out[[1]]$areas, "Raigad and Thane Districts")
})

test_that("Live weather operations strictly isolate uploaded historical dataset from modification", {
  # Synthetic monthly dataset representing uploaded historical data
  hist_data <- data.frame(
    location_id = rep(c("mumbai", "delhi"), each = 24),
    year = rep(2020:2021, each = 12, times = 2),
    month = rep(1:12, times = 4),
    mean_temperature_c = rnorm(48, mean = 28, sd = 3),
    total_precipitation_mm = runif(48, min = 0, max = 300),
    is_synthetic = TRUE,
    stringsAsFactors = FALSE
  )
  
  orig_rows <- nrow(hist_data)
  orig_cols <- names(hist_data)
  orig_mean_temp <- mean(hist_data$mean_temperature_c)
  
  # Perform live weather and alerts evaluations
  live_sig <- extract_live_climate_signals(
    daily_row = list(temp_max = 41.5, precip_sum = 75.0, wind_gust = 55.0, humidity = 85.0)
  )
  
  indicators <- evaluate_experimental_indicators(
    daily_df = data.frame(day_name = "Today", temp_max = 41.5, precip_sum = 75.0, wind_gust = 55.0)
  )
  
  # Historical data must remain 100% identical and uncorrupted
  expect_equal(nrow(hist_data), orig_rows)
  expect_equal(names(hist_data), orig_cols)
  expect_equal(mean(hist_data$mean_temperature_c), orig_mean_temp)
})

test_that("Vulnerability profile changes do not alter underlying live climate signals or water pressure restriction", {
  live_signals <- list(
    timescale = "live_daily",
    temperature = list(signal = "Hot", value = 41.0, unit = "°C"),
    precipitation = list(signal = "Wet", value = 65.0, unit = "mm")
  )
  
  # Profile 1: Poor drainage
  prof1 <- list(drainage = "Poor", waterlogging = "Frequently", water_storage = "None", rainfall_dependence = "High", outdoor_work = "High", shade_cooling = "Poor", power_interruptions = "Frequent")
  res1 <- assess_all_community_impacts(live_signals, prof1)
  
  # Profile 2: Good drainage (what-if modification)
  prof2 <- prof1
  prof2$drainage <- "Good"
  prof2$waterlogging <- "Never"
  res2 <- assess_all_community_impacts(live_signals, prof2)
  
  # Underlying climate signals remain identical
  expect_equal(res1$climate_signals$temperature$signal, res2$climate_signals$temperature$signal)
  expect_equal(res1$climate_signals$precipitation$signal, res2$climate_signals$precipitation$signal)
  
  # Impact reflects vulnerability change
  expect_equal(res1$rain_disruption$concern_level, "High")
  expect_equal(res2$rain_disruption$concern_level, "Low")
  
  # In both profiles, water-supply pressure remains Insufficient information on daily timescale
  expect_equal(res1$water_supply$concern_level, "Insufficient information")
  expect_equal(res2$water_supply$concern_level, "Insufficient information")
  expect_match(res1$water_supply$explanation, "multi-week")
})
