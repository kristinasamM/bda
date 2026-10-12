# R/explanations.R - Explainable Anomaly Cards Generator for ClimateWatch

#' Generate Natural-Language Explanation and Structured Card Data
#'
#' Generates explainable cards derived strictly from computed statistics.
#' Never hardcodes findings or speculates unsupported causal mechanisms.
#'
#' @param row A single record from detect_climate_anomalies()
#' @param variable Selected climate variable: "temperature", "precipitation", or "humidity"
#' @return List containing structured card metadata and human-readable explanation
generate_anomaly_card_data <- function(row, variable = "temperature") {
  if (is.null(row) || nrow(row) == 0) return(NULL)

  m_name <- month.name[as.integer(row$month[1])]
  yr <- row$year[1]
  loc <- row$location_name[1]

  if (variable == "temperature") {
    obs <- row$temperature_c[1]
    base_ref <- row$temp_baseline_mean[1]
    dev <- row$temp_deviation_c[1]
    z <- row$temp_z_score[1]
    a_type <- row$temp_anomaly_type[1]
    sev <- row$temp_severity[1]
    cov <- row$temp_coverage_pct[1]
    unit <- "°C"

    # Natural explanation phrasing
    if (a_type %in% c("Insufficient Data", "Insufficient Coverage", "Insufficient Baseline Samples", "Zero Baseline SD")) {
      explanation <- sprintf(
        "%s %d at %s's selected coordinates has %s; no reliable temperature anomaly score could be determined.",
        m_name, yr, loc, tolower(a_type)
      )
    } else if (sev == "Normal") {
      explanation <- sprintf(
        "%s %d mean temperature at %s's selected coordinates (%.1f°C) remained within the historical baseline normal range (%.1f°C, deviation %+.1f°C, z-score: %+.2f).",
        m_name, yr, loc, obs, base_ref, dev, z
      )
    } else {
      direction <- if (dev > 0) "above" else "below"
      explanation <- sprintf(
        "%s %d mean temperature at %s's selected coordinates was %.1f°C %s the historical %s average (baseline: %.1f°C, z-score: %+.2f, severity: %s).",
        m_name, yr, loc, abs(dev), direction, m_name, base_ref, z, tolower(sev)
      )
    }

    badge_class <- switch(sev,
      "Normal" = "badge-normal",
      "Mild" = "badge-mild",
      "Moderate" = "badge-moderate",
      "Severe" = "badge-severe",
      "badge-neutral"
    )

    list(
      location = loc,
      month_year = paste(m_name, yr),
      variable = "Mean Temperature",
      observed_value = if (!is.na(obs)) sprintf("%.1f %s", obs, unit) else "N/A",
      baseline_reference = if (!is.na(base_ref)) sprintf("%.1f %s (mean)", base_ref, unit) else "N/A",
      deviation = if (!is.na(dev)) sprintf("%+.1f %s", dev, unit) else "N/A",
      standardized_score = if (!is.na(z)) sprintf("%+.2f z", z) else "N/A",
      anomaly_type = a_type,
      severity = sev,
      coverage = sprintf("%.1f%%", cov),
      badge_class = badge_class,
      explanation = explanation,
      is_anomaly = row$temp_is_anomaly[1]
    )

  } else if (variable == "precipitation") {
    obs <- row$precipitation_mm[1]
    p10 <- row$precip_baseline_p10[1]
    p90 <- row$precip_baseline_p90[1]
    base_mean <- row$precip_baseline_mean[1]
    dev <- row$precip_deviation_mm[1]
    a_flag <- row$precip_anomaly_flag[1]
    cov <- row$precip_coverage_pct[1]
    unit <- "mm"

    if (a_flag %in% c("Insufficient Data", "Insufficient Coverage", "Insufficient Baseline Samples")) {
      explanation <- sprintf(
        "%s %d at %s's selected coordinates has %s; incomplete monthly rainfall is excluded to avoid distortion.",
        m_name, yr, loc, tolower(a_flag)
      )
    } else if (a_flag == "Unusually Dry") {
      explanation <- sprintf(
        "%s %d monthly rainfall at %s's selected coordinates (%.1f mm) was below the historical 10th-percentile threshold (%.1f mm; baseline mean: %.1f mm).",
        m_name, yr, loc, obs, p10, base_mean
      )
    } else if (a_flag == "Unusually Wet") {
      explanation <- sprintf(
        "%s %d monthly rainfall at %s's selected coordinates (%.1f mm) exceeded the historical 90th-percentile threshold (%.1f mm; baseline mean: %.1f mm).",
        m_name, yr, loc, obs, p90, base_mean
      )
    } else {
      explanation <- sprintf(
        "%s %d monthly rainfall at %s's selected coordinates (%.1f mm) fell within historical seasonal bounds (10th pct: %.1f mm, 90th pct: %.1f mm).",
        m_name, yr, loc, obs, p10, p90
      )
    }

    badge_class <- switch(a_flag,
      "Unusually Wet" = "badge-wet",
      "Unusually Dry" = "badge-dry",
      "Normal" = "badge-normal",
      "badge-neutral"
    )

    list(
      location = loc,
      month_year = paste(m_name, yr),
      variable = "Total Precipitation",
      observed_value = if (!is.na(obs)) sprintf("%.1f %s", obs, unit) else "N/A",
      baseline_reference = sprintf("P10: %.1f | P90: %.1f %s", p10, p90, unit),
      deviation = if (!is.na(dev)) sprintf("%+.1f %s (vs mean)", dev, unit) else "N/A",
      standardized_score = "Seasonal Percentiles",
      anomaly_type = a_flag,
      severity = if (a_flag %in% c("Unusually Wet", "Unusually Dry")) "Flagged" else "Normal",
      coverage = sprintf("%.1f%%", cov),
      badge_class = badge_class,
      explanation = explanation,
      is_anomaly = row$precip_is_anomaly[1]
    )

  } else { # Humidity
    obs <- row$humidity_pct[1]
    base_ref <- row$humidity_baseline_mean[1]
    dev <- row$humidity_deviation_pct[1]
    z <- row$humidity_z_score[1]
    a_flag <- row$humidity_anomaly_flag[1]
    cov <- row$humidity_coverage_pct[1]
    unit <- "%"

    if (a_flag %in% c("Insufficient Data", "Insufficient Coverage", "Insufficient Baseline Samples", "Zero Baseline SD")) {
      explanation <- sprintf(
        "%s %d relative humidity at %s's selected coordinates has %s.",
        m_name, yr, loc, tolower(a_flag)
      )
    } else if (a_flag == "Normal") {
      explanation <- sprintf(
        "%s %d relative humidity at %s's selected coordinates (%.1f%%) remained within normal historical levels (baseline mean: %.1f%%).",
        m_name, yr, loc, obs, base_ref
      )
    } else {
      explanation <- sprintf(
        "%s %d relative humidity at %s's selected coordinates (%.1f%%) was %s compared with historical values for this month (baseline mean: %.1f%%, z-score: %+.2f).",
        m_name, yr, loc, obs, tolower(a_flag), base_ref, z
      )
    }

    badge_class <- switch(a_flag,
      "Unusually High Humidity" = "badge-high-hum",
      "Unusually Low Humidity" = "badge-low-hum",
      "Normal" = "badge-normal",
      "badge-neutral"
    )

    list(
      location = loc,
      month_year = paste(m_name, yr),
      variable = "Mean Relative Humidity",
      observed_value = if (!is.na(obs)) sprintf("%.1f %s", obs, unit) else "N/A",
      baseline_reference = if (!is.na(base_ref)) sprintf("%.1f %s (mean)", base_ref, unit) else "N/A",
      deviation = if (!is.na(dev)) sprintf("%+.1f %s", dev, unit) else "N/A",
      standardized_score = if (!is.na(z)) sprintf("%+.2f z", z) else "N/A",
      anomaly_type = a_flag,
      severity = if (a_flag != "Normal") "Flagged" else "Normal",
      coverage = sprintf("%.1f%%", cov),
      badge_class = badge_class,
      explanation = explanation,
      is_anomaly = row$humidity_is_anomaly[1]
    )
  }
}
