# R/anomaly_detective.R - Detailed Daily Anomaly Investigation & Sensitivity Analysis
# Allows users to inspect daily observations behind a flagged monthly anomaly.
# Calculates top daily rainfall contributions, daily temperature departures from baseline,
# and exploratory sensitivity checks without modifying original data.

#' Load Daily Records for a Specific Location and Month
#'
#' Retrieves valid daily observations for the specified location, year, and month.
#'
#' @param active_dataset Active dataset package from load_active_dataset()
#' @param location_id Character identifier of the station/location
#' @param target_year Integer year (e.g. 2020)
#' @param target_month Integer month (1 to 12)
#' @return Data frame of daily records for that month, with missing dates filled as NA
get_daily_records_for_month <- function(active_dataset, location_id, target_year, target_month) {
  if (is.null(active_dataset) || is.null(active_dataset$processed_dir)) {
    return(data.frame())
  }
  
  target_year <- as.integer(target_year)
  target_month <- as.integer(target_month)
  
  # Load daily observations for that year
  daily_df <- tryCatch({
    load_daily_data_parquet(
      input_dir = active_dataset$processed_dir,
      location_ids = location_id,
      start_year = target_year,
      end_year = target_year
    )
  }, error = function(e) data.frame())
  
  if (nrow(daily_df) == 0) return(data.frame())
  
  daily_df$date <- as.Date(daily_df$date)
  daily_df$month <- as.integer(format(daily_df$date, "%m"))
  
  month_sub <- daily_df[daily_df$month == target_month, ]
  if (nrow(month_sub) == 0) return(data.frame())
  
  # Construct complete calendar sequence for the month
  start_date <- as.Date(sprintf("%04d-%02d-01", target_year, target_month))
  # Days in month: jump to next month day 1 minus 1 day
  next_m <- if (target_month == 12) 1 else target_month + 1
  next_y <- if (target_month == 12) target_year + 1 else target_year
  end_date <- as.Date(sprintf("%04d-%02d-01", next_y, next_m)) - 1
  
  all_dates <- seq.Date(start_date, end_date, by = "day")
  cal_df <- data.frame(date = all_dates, stringsAsFactors = FALSE)
  
  merged_daily <- merge(cal_df, month_sub, by = "date", all.x = TRUE)
  merged_daily$day <- as.integer(format(merged_daily$date, "%d"))
  merged_daily$location_id <- location_id
  loc_name <- if ("location_name" %in% names(month_sub) && any(!is.na(month_sub$location_name))) {
    month_sub$location_name[!is.na(month_sub$location_name)][1]
  } else {
    location_id
  }
  merged_daily$location_name <- loc_name
  merged_daily <- merged_daily[order(merged_daily$date), ]
  
  merged_daily
}

#' Analyze Daily Observations Behind a Monthly Anomaly
#'
#' Computes daily metrics, top contributors, and baseline departure diagnostics.
#'
#' @param daily_df Complete daily data frame for the target month from get_daily_records_for_month()
#' @param monthly_row 1-row data frame or list with observed monthly results and metadata
#' @param baseline_row 1-row data frame or list with seasonal baseline for that calendar month
#' @param variable Selected climate variable ("precipitation_mm" or "temperature_c" or "humidity_pct")
#' @return List of detailed diagnostic indicators and narrative
analyze_monthly_anomaly_details <- function(daily_df, monthly_row, baseline_row, variable = "precipitation_mm") {
  if (is.null(daily_df) || nrow(daily_df) == 0) {
    return(list(status = "NO_DATA", message = "No daily records available for this month."))
  }
  
  total_days <- nrow(daily_df)
  
  if (variable == "precipitation_mm") {
    vals <- daily_df$precipitation_mm
    valid_mask <- !is.na(vals)
    valid_count <- sum(valid_mask)
    missing_count <- total_days - valid_count
    coverage_pct <- round((valid_count / total_days) * 100, 1)
    
    total_obs <- sum(vals[valid_mask], na.rm = TRUE)
    b_mean <- if (!is.null(baseline_row$precip_baseline_mean)) as.numeric(baseline_row$precip_baseline_mean) else NA_real_
    b_p10 <- if (!is.null(baseline_row$precip_baseline_p10)) as.numeric(baseline_row$precip_baseline_p10) else NA_real_
    b_p90 <- if (!is.null(baseline_row$precip_baseline_p90)) as.numeric(baseline_row$precip_baseline_p90) else NA_real_
    
    # Sort valid days descending
    valid_daily <- daily_df[valid_mask, ]
    sorted_idx <- order(valid_daily$precipitation_mm, decreasing = TRUE)
    top_days <- valid_daily[sorted_idx, c("date", "day", "precipitation_mm")]
    
    top3 <- head(top_days, 3)
    top3_sum <- sum(top3$precipitation_mm, na.rm = TRUE)
    top3_share_pct <- if (total_obs > 0) round((top3_sum / total_obs) * 100, 1) else 0.0
    
    top1_val <- if (nrow(top3) >= 1) top3$precipitation_mm[1] else 0.0
    top1_date <- if (nrow(top3) >= 1) as.character(top3$date[1]) else "N/A"
    
    zero_rain_days <- sum(vals[valid_mask] == 0, na.rm = TRUE)
    rainy_days <- sum(vals[valid_mask] > 0, na.rm = TRUE)
    
    # Diagnosis
    if (top3_share_pct >= 60.0 && total_obs > 0) {
      concentration_diag <- "High rainfall concentration: The top 3 days accounted for a substantial majority of the monthly total."
      concentration_type <- "Concentrated Downpours"
    } else if (rainy_days >= 15) {
      concentration_diag <- "Distributed rainfall: Precipitation was spread across numerous rainy days throughout the month."
      concentration_type <- "Persistent Rainfall"
    } else {
      concentration_diag <- "Moderate concentration: Typical distribution of rainfall events across the month."
      concentration_type <- "Moderate Distribution"
    }
    
    narrative <- sprintf(
      "During this month, %.1f mm of total rainfall was observed across %d valid recorded days (%.1f%% coverage, %d missing days). The top 3 wettest days contributed %.1f mm, representing %.1f%% of the total monthly rainfall. The highest single-day rainfall was %.1f mm on %s. Note: High daily concentration indicates localized downpours; it does not confirm surface flooding without hydrological catchment data.",
      total_obs, valid_count, coverage_pct, missing_count, top3_sum, top3_share_pct, top1_val, top1_date
    )
    
    list(
      status = "SUCCESS",
      variable = "precipitation_mm",
      variable_label = "Total Rainfall (mm)",
      unit = "mm",
      total_days_in_month = total_days,
      valid_days = valid_count,
      missing_days = missing_count,
      coverage_pct = coverage_pct,
      monthly_observed = round(total_obs, 1),
      baseline_mean = b_mean,
      baseline_p10 = b_p10,
      baseline_p90 = b_p90,
      top3_days = top3,
      top3_share_pct = top3_share_pct,
      highest_day = list(date = top1_date, value = top1_val),
      lowest_day = list(
        date = if (nrow(top_days) > 0) as.character(tail(top_days$date, 1)) else "N/A",
        value = if (nrow(top_days) > 0) tail(top_days$precipitation_mm, 1) else 0.0
      ),
      zero_rain_days = zero_rain_days,
      rainy_days = rainy_days,
      concentration_type = concentration_type,
      diagnosis = concentration_diag,
      narrative = narrative,
      disclaimer = "Statistical rainfall concentration indicates meteorological downpours, not confirmed civic flooding."
    )
    
  } else if (variable == "temperature_c") {
    vals <- daily_df$temperature_c
    valid_mask <- !is.na(vals)
    valid_count <- sum(valid_mask)
    missing_count <- total_days - valid_count
    coverage_pct <- round((valid_count / total_days) * 100, 1)
    
    mean_obs <- if (valid_count > 0) mean(vals[valid_mask], na.rm = TRUE) else NA_real_
    b_mean <- if (!is.null(baseline_row$temp_baseline_mean)) as.numeric(baseline_row$temp_baseline_mean) else NA_real_
    b_sd <- if (!is.null(baseline_row$temp_baseline_sd)) as.numeric(baseline_row$temp_baseline_sd) else NA_real_
    
    valid_daily <- daily_df[valid_mask, ]
    sorted_desc <- valid_daily[order(valid_daily$temperature_c, decreasing = TRUE), ]
    
    highest_day <- if (nrow(sorted_desc) > 0) list(date = as.character(sorted_desc$date[1]), value = round(sorted_desc$temperature_c[1], 1)) else list(date = "N/A", value = NA_real_)
    lowest_day <- if (nrow(sorted_desc) > 0) list(date = as.character(tail(sorted_desc$date, 1)), value = round(tail(sorted_desc$temperature_c, 1), 1)) else list(date = "N/A", value = NA_real_)
    
    # Days exceeding baseline reference
    days_exceeding <- if (!is.na(b_mean)) sum(vals[valid_mask] > b_mean, na.rm = TRUE) else 0
    days_exceeding_pct <- if (valid_count > 0) round((days_exceeding / valid_count) * 100, 1) else 0.0
    
    # Diagnosis of departure spread
    if (days_exceeding_pct >= 60.0) {
      temp_diag <- "Widespread sustained warming: A solid majority of days experienced temperatures above the seasonal climatological reference."
      diag_type = "Widespread Sustained Departure"
    } else if (days_exceeding_pct <= 20.0) {
      temp_diag <- "Predominantly cooler: Most days remained below the baseline reference mean."
      diag_type = "Widespread Cooler Departure"
    } else {
      temp_diag <- "Acute episodic variation: Driven by a small number of prominent extreme temperature days rather than uniform warming."
      diag_type = "Acute Episodic Variation"
    }
    
    narrative <- sprintf(
      "Observed monthly mean temperature was %.2f°C across %d valid days (%.1f%% coverage, %d missing days). Compared to the seasonal calendar-month reference mean of %.2f°C, %d out of %d valid days (%.1f%%) were warmer than the baseline reference. Peak daily temperature reached %.1f°C on %s, while the lowest daily temperature was %.1f°C on %s. Diagnosis indicates: %s.",
      mean_obs, valid_count, coverage_pct, missing_count, b_mean, days_exceeding, valid_count, days_exceeding_pct,
      highest_day$value, highest_day$date, lowest_day$value, lowest_day$date, tolower(diag_type)
    )
    
    list(
      status = "SUCCESS",
      variable = "temperature_c",
      variable_label = "Mean Temperature (°C)",
      unit = "°C",
      total_days_in_month = total_days,
      valid_days = valid_count,
      missing_days = missing_count,
      coverage_pct = coverage_pct,
      monthly_observed = round(mean_obs, 2),
      baseline_mean = b_mean,
      baseline_sd = b_sd,
      days_exceeding_baseline = days_exceeding,
      days_exceeding_pct = days_exceeding_pct,
      highest_day = highest_day,
      lowest_day = lowest_day,
      diagnosis_type = diag_type,
      diagnosis = temp_diag,
      narrative = narrative,
      disclaimer = "The calendar-month baseline mean serves as a benchmark reference, not an expected identical target for each individual day."
    )
    
  } else {
    # Humidity
    vals <- daily_df$humidity_pct
    valid_mask <- !is.na(vals)
    valid_count <- sum(valid_mask)
    missing_count <- total_days - valid_count
    coverage_pct <- round((valid_count / total_days) * 100, 1)
    mean_obs <- if (valid_count > 0) mean(vals[valid_mask], na.rm = TRUE) else NA_real_
    b_mean <- if (!is.null(baseline_row$humidity_baseline_mean)) as.numeric(baseline_row$humidity_baseline_mean) else NA_real_
    
    list(
      status = "SUCCESS",
      variable = "humidity_pct",
      variable_label = "Mean Relative Humidity (%)",
      unit = "%",
      total_days_in_month = total_days,
      valid_days = valid_count,
      missing_days = missing_count,
      coverage_pct = coverage_pct,
      monthly_observed = round(mean_obs, 1),
      baseline_mean = b_mean,
      highest_day = list(date = "N/A", value = max(vals, na.rm = TRUE)),
      lowest_day = list(date = "N/A", value = min(vals, na.rm = TRUE)),
      narrative = sprintf("Observed mean humidity was %.1f%% across %d valid recorded days.", mean_obs, valid_count),
      disclaimer = "Exploratory daily humidity analysis."
    )
  }
}

#' Run Sensitivity Check Excluding Selected Days
#'
#' Temporarily removes chosen dates, recalculates exploratory monthly result,
#' and strictly enforces the 90% monthly coverage rule. Never modifies original data.
#'
#' @param daily_df Complete daily data frame for the target month
#' @param excluded_dates Vector of Date or character dates to temporarily exclude
#' @param variable Selected climate variable
#' @param original_monthly_val Original monthly aggregate value
#' @param baseline_val Seasonal baseline reference value
#' @return List with exploratory values, coverage checks, and explanation
run_sensitivity_check <- function(daily_df, excluded_dates, variable = "precipitation_mm", original_monthly_val = NULL, baseline_val = NULL) {
  if (is.null(daily_df) || nrow(daily_df) == 0) {
    return(list(status = "NO_DATA", message = "No daily records provided."))
  }
  
  total_days_in_month <- nrow(daily_df)
  excluded_dates <- as.Date(excluded_dates)
  
  # Filter out excluded dates (TEMPORARILY - NEVER modifies original data)
  retained_df <- daily_df[!daily_df$date %in% excluded_dates, ]
  
  vals <- retained_df[[variable]]
  valid_retained <- sum(!is.na(vals))
  days_excluded_count <- length(excluded_dates)
  
  # Recalculate coverage percentage relative to the complete calendar month
  coverage_pct <- round((valid_retained / total_days_in_month) * 100, 1)
  is_eligible <- coverage_pct >= 90.0
  
  if (variable == "precipitation_mm") {
    new_monthly_val <- if (valid_retained > 0) round(sum(vals, na.rm = TRUE), 1) else 0.0
    unit <- "mm"
    diff_val <- if (!is.null(original_monthly_val)) round(new_monthly_val - original_monthly_val, 1) else NA_real_
  } else {
    new_monthly_val <- if (valid_retained > 0) round(mean(vals, na.rm = TRUE), 2) else NA_real_
    unit <- if (variable == "temperature_c") "°C" else "%"
    diff_val <- if (!is.null(original_monthly_val) && !is.na(new_monthly_val)) round(new_monthly_val - original_monthly_val, 2) else NA_real_
  }
  
  status_desc <- if (is_eligible) {
    sprintf("Retained %d days (%.1f%% coverage ≥ 90%% rule). Result is analytically eligible.", valid_retained, coverage_pct)
  } else {
    sprintf("Ineligible: Retained %d days drops coverage to %.1f%% (< 90%% coverage requirement). Incomplete aggregate cannot be used in official analysis.", valid_retained, coverage_pct)
  }
  
  list(
    status = "SUCCESS",
    variable = variable,
    unit = unit,
    total_days_in_month = total_days_in_month,
    days_excluded_count = days_excluded_count,
    valid_retained_days = valid_retained,
    coverage_pct = coverage_pct,
    is_eligible = is_eligible,
    original_value = original_monthly_val,
    exploratory_value = new_monthly_val,
    difference = diff_val,
    baseline_value = baseline_val,
    status_description = status_desc,
    disclaimer = "Sensitivity analysis is exploratory and does not establish the true cause of an anomaly. Original dataset remains unmodified."
  )
}
