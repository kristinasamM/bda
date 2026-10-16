# R/anomaly_detection.R - Season-Aware Anomaly Detection for ClimateWatch

#' Compute Baseline Statistics for Each Location and Calendar Month
#'
#' Evaluates historical statistics using strictly non-overlapping baseline years.
#' Enforces minimum sample size and non-zero standard deviation constraints.
#'
#' @param monthly_df Monthly aggregated data frame
#' @param baseline_start Integer start year for baseline (default 2001)
#' @param baseline_end Integer end year for baseline (default 2015)
#' @param min_baseline_years Minimum number of valid covered years required (default 5)
#' @return List of baseline data frames by variable or aggregated reference table
compute_seasonal_baseline <- function(monthly_df,
                                      baseline_start = 2001,
                                      baseline_end = 2015,
                                      min_baseline_years = 5) {
  if (is.null(monthly_df) || nrow(monthly_df) == 0) {
    return(data.frame())
  }

  # Filter to strictly baseline period
  base_df <- monthly_df[monthly_df$year >= baseline_start & monthly_df$year <= baseline_end, ]
  if (nrow(base_df) == 0) return(data.frame())

  locs <- unique(monthly_df$location_id)
  months <- 1:12

  records <- list()

  for (loc in locs) {
    loc_sub <- base_df[base_df$location_id == loc, ]
    loc_name <- if (nrow(loc_sub) > 0 && "location_name" %in% names(loc_sub) && length(loc_sub$location_name) > 0) loc_sub$location_name[1] else as.character(loc)
    lat_val <- if (nrow(loc_sub) > 0 && "latitude" %in% names(loc_sub) && length(loc_sub$latitude) > 0) loc_sub$latitude[1] else NA_real_
    lon_val <- if (nrow(loc_sub) > 0 && "longitude" %in% names(loc_sub) && length(loc_sub$longitude) > 0) loc_sub$longitude[1] else NA_real_

    for (m in months) {
      m_sub <- loc_sub[loc_sub$month == m, ]

      # Temperature baseline
      valid_t <- m_sub$mean_temperature_c[!is.na(m_sub$mean_temperature_c) & m_sub$is_usable_temp]
      n_t <- length(valid_t)
      t_mean <- if (n_t >= min_baseline_years) mean(valid_t) else NA_real_
      t_sd <- if (n_t >= min_baseline_years) stats::sd(valid_t) else NA_real_

      # Rainfall baseline (percentiles)
      valid_p <- m_sub$total_precipitation_mm[!is.na(m_sub$total_precipitation_mm) & m_sub$is_usable_precip]
      n_p <- length(valid_p)
      p_p10 <- if (n_p >= min_baseline_years) stats::quantile(valid_p, probs = 0.10, na.rm = TRUE, names = FALSE) else NA_real_
      p_p90 <- if (n_p >= min_baseline_years) stats::quantile(valid_p, probs = 0.90, na.rm = TRUE, names = FALSE) else NA_real_
      p_mean <- if (n_p >= min_baseline_years) mean(valid_p) else NA_real_

      # Humidity baseline
      valid_h <- m_sub$mean_humidity_pct[!is.na(m_sub$mean_humidity_pct) & m_sub$is_usable_humidity]
      n_h <- length(valid_h)
      h_mean <- if (n_h >= min_baseline_years) mean(valid_h) else NA_real_
      h_sd <- if (n_h >= min_baseline_years) stats::sd(valid_h) else NA_real_

      records[[length(records) + 1]] <- data.frame(
        location_id = loc,
        location_name = loc_name,
        latitude = lat_val,
        longitude = lon_val,
        month = m,
        baseline_start_year = baseline_start,
        baseline_end_year = baseline_end,
        # Temperature stats
        temp_n = n_t,
        temp_baseline_mean = if (!is.na(t_mean)) round(t_mean, 2) else NA_real_,
        temp_baseline_sd = if (!is.na(t_sd)) round(t_sd, 3) else NA_real_,
        # Rainfall stats
        precip_n = n_p,
        precip_baseline_mean = if (!is.na(p_mean)) round(p_mean, 2) else NA_real_,
        precip_baseline_p10 = if (!is.na(p_p10)) round(p_p10, 2) else NA_real_,
        precip_baseline_p90 = if (!is.na(p_p90)) round(p_p90, 2) else NA_real_,
        # Humidity stats
        humidity_n = n_h,
        humidity_baseline_mean = if (!is.na(h_mean)) round(h_mean, 2) else NA_real_,
        humidity_baseline_sd = if (!is.na(h_sd)) round(h_sd, 3) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, records)
}

#' Detect Climate Anomalies for Analysis Period
#'
#' Compares analysis period observations against strictly historical baseline statistics.
#' Never includes an analysed observation in its own baseline.
#'
#' @param monthly_df Monthly aggregated data frame
#' @param baseline_df Computed baseline statistics from compute_seasonal_baseline()
#' @param analysis_start Integer start year for analysis (default 2016)
#' @param analysis_end Integer end year for analysis (default 2024)
#' @param config List of configuration parameters (optional)
#' @return Data frame of detailed anomaly records
detect_climate_anomalies <- function(monthly_df,
                                     baseline_df,
                                     analysis_start = 2016,
                                     analysis_end = 2024,
                                     config = NULL) {
  if (is.null(monthly_df) || nrow(monthly_df) == 0 || is.null(baseline_df) || nrow(baseline_df) == 0) {
    return(data.frame())
  }

  # Filter analysis observations
  an_df <- monthly_df[monthly_df$year >= analysis_start & monthly_df$year <= analysis_end, ]
  if (nrow(an_df) == 0) return(data.frame())

  # Merge with baseline on location_id and month
  merged <- merge(
    an_df,
    baseline_df,
    by = c("location_id", "month"),
    suffixes = c("", "_base"),
    all.x = TRUE
  )

  # Sort chronologically
  merged <- merged[order(merged$location_id, merged$year, merged$month), ]

  # Threshold parameters
  z_mild <- if (!is.null(config$anomaly$z_mild)) config$anomaly$z_mild else 1.5
  z_mod <- if (!is.null(config$anomaly$z_moderate)) config$anomaly$z_moderate else 2.0
  z_sev <- if (!is.null(config$anomaly$z_severe)) config$anomaly$z_severe else 3.0

  # -------------------------------------------------------------
  # 1. TEMPERATURE ANOMALY EVALUATION
  # -------------------------------------------------------------
  temp_dev <- merged$mean_temperature_c - merged$temp_baseline_mean
  temp_z <- rep(NA_real_, nrow(merged))
  temp_type <- rep("Insufficient Data", nrow(merged))
  temp_sev <- rep("None", nrow(merged))

  for (i in seq_len(nrow(merged))) {
    if (!merged$is_usable_temp[i]) {
      temp_type[i] <- "Insufficient Coverage"
    } else if (is.na(merged$temp_n[i]) || merged$temp_n[i] < 5) {
      temp_type[i] <- "Insufficient Baseline Samples"
    } else if (is.na(merged$temp_baseline_sd[i]) || merged$temp_baseline_sd[i] < 1e-6) {
      temp_type[i] <- "Zero Baseline SD"
    } else {
      z <- (merged$mean_temperature_c[i] - merged$temp_baseline_mean[i]) / merged$temp_baseline_sd[i]
      temp_z[i] <- round(z, 2)
      abs_z <- abs(z)

      if (abs_z < z_mild) {
        temp_type[i] <- "Normal"
        temp_sev[i] <- "Normal"
      } else if (abs_z < z_mod) {
        temp_type[i] <- if (z > 0) "Unusually Hot (Mild)" else "Unusually Cold (Mild)"
        temp_sev[i] <- "Mild"
      } else if (abs_z < z_sev) {
        temp_type[i] <- if (z > 0) "Unusually Hot (Moderate)" else "Unusually Cold (Moderate)"
        temp_sev[i] <- "Moderate"
      } else {
        temp_type[i] <- if (z > 0) "Unusually Hot (Severe)" else "Unusually Cold (Severe)"
        temp_sev[i] <- "Severe"
      }
    }
  }

  # -------------------------------------------------------------
  # 2. RAINFALL ANOMALY EVALUATION (Seasonal Percentiles)
  # -------------------------------------------------------------
  precip_dev <- merged$total_precipitation_mm - merged$precip_baseline_mean
  precip_flag <- rep("Insufficient Data", nrow(merged))

  for (i in seq_len(nrow(merged))) {
    if (!merged$is_usable_precip[i]) {
      precip_flag[i] <- "Insufficient Coverage"
    } else if (is.na(merged$precip_n[i]) || merged$precip_n[i] < 5) {
      precip_flag[i] <- "Insufficient Baseline Samples"
    } else {
      obs_p <- merged$total_precipitation_mm[i]
      p10 <- merged$precip_baseline_p10[i]
      p90 <- merged$precip_baseline_p90[i]

      # Handle zero-heavy dry season and tied thresholds
      if (p10 == p90) {
        if (obs_p > p90) {
          precip_flag[i] <- "Unusually Wet"
        } else {
          precip_flag[i] <- "Normal"
        }
      } else if (p10 == 0 && obs_p == 0) {
        # Tied at zero in dry season is expected seasonal condition
        precip_flag[i] <- "Normal"
      } else if (obs_p < p10) {
        precip_flag[i] <- "Unusually Dry"
      } else if (obs_p > p90) {
        precip_flag[i] <- "Unusually Wet"
      } else {
        precip_flag[i] <- "Normal"
      }
    }
  }

  # -------------------------------------------------------------
  # 3. HUMIDITY ANOMALY EVALUATION (Standardised Deviations)
  # -------------------------------------------------------------
  hum_dev <- merged$mean_humidity_pct - merged$humidity_baseline_mean
  hum_z <- rep(NA_real_, nrow(merged))
  hum_flag <- rep("Insufficient Data", nrow(merged))

  for (i in seq_len(nrow(merged))) {
    if (!merged$is_usable_humidity[i]) {
      hum_flag[i] <- "Insufficient Coverage"
    } else if (is.na(merged$humidity_n[i]) || merged$humidity_n[i] < 5) {
      hum_flag[i] <- "Insufficient Baseline Samples"
    } else if (is.na(merged$humidity_baseline_sd[i]) || merged$humidity_baseline_sd[i] < 1e-6) {
      hum_flag[i] <- "Zero Baseline SD"
    } else {
      hz <- (merged$mean_humidity_pct[i] - merged$humidity_baseline_mean[i]) / merged$humidity_baseline_sd[i]
      hum_z[i] <- round(hz, 2)

      if (hz >= 1.5) {
        hum_flag[i] <- "Unusually High Humidity"
      } else if (hz <= -1.5) {
        hum_flag[i] <- "Unusually Low Humidity"
      } else {
        hum_flag[i] <- "Normal"
      }
    }
  }

  # Build comprehensive anomaly dataframe
  data.frame(
    location_id = merged$location_id,
    location_name = merged$location_name,
    latitude = merged$latitude,
    longitude = merged$longitude,
    year = merged$year,
    month = merged$month,
    year_month = merged$year_month,
    # Coverage
    temp_coverage_pct = merged$temp_coverage_pct,
    precip_coverage_pct = merged$precip_coverage_pct,
    humidity_coverage_pct = merged$humidity_coverage_pct,
    quality_status = merged$quality_status,
    # Temperature results
    temperature_c = merged$mean_temperature_c,
    temp_baseline_mean = merged$temp_baseline_mean,
    temp_baseline_sd = merged$temp_baseline_sd,
    temp_deviation_c = round(temp_dev, 2),
    temp_z_score = temp_z,
    temp_anomaly_type = temp_type,
    temp_severity = temp_sev,
    temp_is_anomaly = temp_sev %in% c("Mild", "Moderate", "Severe"),
    # Rainfall results
    precipitation_mm = merged$total_precipitation_mm,
    precip_baseline_mean = merged$precip_baseline_mean,
    precip_baseline_p10 = merged$precip_baseline_p10,
    precip_baseline_p90 = merged$precip_baseline_p90,
    precip_deviation_mm = round(precip_dev, 2),
    precip_anomaly_flag = precip_flag,
    precip_is_anomaly = precip_flag %in% c("Unusually Dry", "Unusually Wet"),
    # Humidity results
    humidity_pct = merged$mean_humidity_pct,
    humidity_baseline_mean = merged$humidity_baseline_mean,
    humidity_baseline_sd = merged$humidity_baseline_sd,
    humidity_deviation_pct = round(hum_dev, 2),
    humidity_z_score = hum_z,
    humidity_anomaly_flag = hum_flag,
    humidity_is_anomaly = hum_flag %in% c("Unusually High Humidity", "Unusually Low Humidity"),
    # Baseline metadata
    baseline_period_label = sprintf("%d–%d (N=%d)", merged$baseline_start_year, merged$baseline_end_year, merged$temp_n),
    is_synthetic = merged$is_synthetic,
    stringsAsFactors = FALSE
  )
}
