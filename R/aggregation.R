# R/aggregation.R - DuckDB Monthly Aggregation and Coverage Calculation

#' Aggregate Daily Meteorological Data into Monthly Series Using DuckDB
#'
#' Computes monthly metrics: mean temperature, total precipitation, mean humidity,
#' observed-day counts, coverage percentages, and strict quality flags.
#' Enforces the rule: if coverage < min_coverage (default 90%), do not compute
#' or display incomplete monthly aggregates.
#'
#' @param daily_df Cleaned daily data frame
#' @param min_coverage Minimum coverage proportion required (default 0.90)
#' @return Data frame of monthly climate aggregates
compute_monthly_aggregates_duckdb <- function(daily_df, min_coverage = 0.90, is_monthly = FALSE) {
  if (is.null(daily_df) || nrow(daily_df) == 0) {
    return(data.frame(
      location_id = character(0),
      location_name = character(0),
      latitude = numeric(0),
      longitude = numeric(0),
      year = integer(0),
      month = integer(0),
      year_month = as.Date(character(0)),
      total_days_in_month = integer(0),
      temp_observed_days = integer(0),
      precip_observed_days = integer(0),
      humidity_observed_days = integer(0),
      temp_coverage_pct = numeric(0),
      precip_coverage_pct = numeric(0),
      humidity_coverage_pct = numeric(0),
      mean_temperature_c = numeric(0),
      total_precipitation_mm = numeric(0),
      mean_humidity_pct = numeric(0),
      quality_status = character(0),
      is_usable_temp = logical(0),
      is_usable_precip = logical(0),
      is_usable_humidity = logical(0),
      is_synthetic = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  min_cov_pct <- min_coverage * 100.0

  # Ensure date is standard Date
  daily_df$date <- as.Date(daily_df$date)

  # Check which climate variables are mapped and present in data
  has_temp_var <- "temperature_c" %in% names(daily_df) && any(!is.na(daily_df$temperature_c))
  has_precip_var <- "precipitation_mm" %in% names(daily_df) && any(!is.na(daily_df$precipitation_mm))
  has_hum_var <- "humidity_pct" %in% names(daily_df) && any(!is.na(daily_df$humidity_pct))

  # Check if DuckDB is available
  has_duckdb <- requireNamespace("duckdb", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE)

  if (has_duckdb) {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    # Register temporary table in DuckDB
    duckdb::duckdb_register(con, "daily_weather", daily_df)

    if (isTRUE(is_monthly)) {
      query <- "
        WITH monthly_prep AS (
          SELECT
            location_id,
            location_name,
            latitude,
            longitude,
            EXTRACT(YEAR FROM date)::INTEGER AS year,
            EXTRACT(MONTH FROM date)::INTEGER AS month,
            DAY(LAST_DAY(date))::INTEGER AS days_in_month,
            temperature_c,
            precipitation_mm,
            humidity_pct,
            is_synthetic
          FROM daily_weather
        )
        SELECT
          location_id,
          FIRST(location_name) AS location_name,
          FIRST(latitude) AS latitude,
          FIRST(longitude) AS longitude,
          year,
          month,
          FIRST(days_in_month) AS total_days_in_month,
          COUNT(temperature_c)::INTEGER AS temp_observed_days,
          COUNT(precipitation_mm)::INTEGER AS precip_observed_days,
          COUNT(humidity_pct)::INTEGER AS humidity_observed_days,
          CASE WHEN COUNT(temperature_c) > 0 THEN 100.0 ELSE 0.0 END AS temp_coverage_pct,
          CASE WHEN COUNT(precipitation_mm) > 0 THEN 100.0 ELSE 0.0 END AS precip_coverage_pct,
          CASE WHEN COUNT(humidity_pct) > 0 THEN 100.0 ELSE 0.0 END AS humidity_coverage_pct,
          ROUND(AVG(temperature_c), 2) AS mean_temperature_c,
          ROUND(SUM(precipitation_mm), 2) AS total_precipitation_mm,
          ROUND(AVG(humidity_pct), 2) AS mean_humidity_pct,
          'ADEQUATE_COVERAGE' AS quality_status,
          (COUNT(temperature_c) > 0) AS is_usable_temp,
          (COUNT(precipitation_mm) > 0) AS is_usable_precip,
          (COUNT(humidity_pct) > 0) AS is_usable_humidity,
          BOOL_OR(is_synthetic) AS is_synthetic
        FROM monthly_prep
        GROUP BY location_id, year, month
        ORDER BY location_id, year, month
      "
      res <- DBI::dbGetQuery(con, query)
    } else {
      # SQL strings for absent variables: if variable is absent, evaluate to TRUE
      t_skip <- if (!has_temp_var) "TRUE" else "FALSE"
      p_skip <- if (!has_precip_var) "TRUE" else "FALSE"
      h_skip <- if (!has_hum_var) "TRUE" else "FALSE"

      query <- sprintf("
        WITH daily_prep AS (
          SELECT
            location_id,
            location_name,
            latitude,
            longitude,
            EXTRACT(YEAR FROM date)::INTEGER AS year,
            EXTRACT(MONTH FROM date)::INTEGER AS month,
            DAY(LAST_DAY(date))::INTEGER AS days_in_month,
            temperature_c,
            precipitation_mm,
            humidity_pct,
            is_synthetic
          FROM daily_weather
        ),
        agg AS (
          SELECT
            location_id,
            FIRST(location_name) AS location_name,
            FIRST(latitude) AS latitude,
            FIRST(longitude) AS longitude,
            year,
            month,
            FIRST(days_in_month) AS total_days_in_month,
            COUNT(temperature_c)::INTEGER AS temp_observed_days,
            COUNT(precipitation_mm)::INTEGER AS precip_observed_days,
            COUNT(humidity_pct)::INTEGER AS humidity_observed_days,
            ROUND(100.0 * COUNT(temperature_c) / FIRST(days_in_month), 2) AS temp_coverage_pct,
            ROUND(100.0 * COUNT(precipitation_mm) / FIRST(days_in_month), 2) AS precip_coverage_pct,
            ROUND(100.0 * COUNT(humidity_pct) / FIRST(days_in_month), 2) AS humidity_coverage_pct,
            AVG(temperature_c) AS raw_mean_temp,
            SUM(precipitation_mm) AS raw_total_precip,
            AVG(humidity_pct) AS raw_mean_humidity,
            BOOL_OR(is_synthetic) AS is_synthetic
          FROM daily_prep
          GROUP BY location_id, year, month
        )
        SELECT
          location_id,
          location_name,
          latitude,
          longitude,
          year,
          month,
          total_days_in_month,
          temp_observed_days,
          precip_observed_days,
          humidity_observed_days,
          temp_coverage_pct,
          precip_coverage_pct,
          humidity_coverage_pct,
          CASE
            WHEN temp_coverage_pct >= %f THEN ROUND(raw_mean_temp, 2)
            ELSE NULL
          END AS mean_temperature_c,
          CASE
            WHEN precip_coverage_pct >= %f THEN ROUND(raw_total_precip, 2)
            ELSE NULL
          END AS total_precipitation_mm,
          CASE
            WHEN humidity_coverage_pct >= %f THEN ROUND(raw_mean_humidity, 2)
            ELSE NULL
          END AS mean_humidity_pct,
          CASE
            WHEN (%s OR temp_coverage_pct >= %f)
             AND (%s OR precip_coverage_pct >= %f)
             AND (%s OR humidity_coverage_pct >= %f)
              THEN 'ADEQUATE_COVERAGE'
            ELSE 'INSUFFICIENT_COVERAGE'
          END AS quality_status,
          (temp_coverage_pct >= %f) AS is_usable_temp,
          (precip_coverage_pct >= %f) AS is_usable_precip,
          (humidity_coverage_pct >= %f) AS is_usable_humidity,
          is_synthetic
        FROM agg
        ORDER BY location_id, year, month
      ", min_cov_pct, min_cov_pct, min_cov_pct,
         t_skip, min_cov_pct,
         p_skip, min_cov_pct,
         h_skip, min_cov_pct,
         min_cov_pct, min_cov_pct, min_cov_pct)

      res <- DBI::dbGetQuery(con, query)
    }
  } else {
    # R fallback aggregation if DuckDB is not yet loaded
    res <- aggregate_monthly_r(daily_df, min_coverage = min_coverage, is_monthly = is_monthly)
  }

  # Build year_month date column (first day of the month)
  res$year_month <- as.Date(sprintf("%04d-%02d-01", res$year, res$month))

  # Ensure types
  res$mean_temperature_c <- as.numeric(res$mean_temperature_c)
  res$total_precipitation_mm <- as.numeric(res$total_precipitation_mm)
  res$mean_humidity_pct <- as.numeric(res$mean_humidity_pct)
  res$is_usable_temp <- as.logical(res$is_usable_temp)
  res$is_usable_precip <- as.logical(res$is_usable_precip)
  res$is_usable_humidity <- as.logical(res$is_usable_humidity)
  res$is_synthetic <- as.logical(res$is_synthetic)

  res
}

#' Fallback R Implementation for Monthly Aggregation
#'
#' @param df Cleaned daily data frame
#' @param min_coverage Minimum coverage proportion
#' @param is_monthly Logical indicating if input is already monthly
#' @return Monthly aggregated data frame
aggregate_monthly_r <- function(df, min_coverage = 0.90, is_monthly = FALSE) {
  min_cov_pct <- min_coverage * 100.0
  df$year <- as.integer(format(df$date, "%Y"))
  df$month <- as.integer(format(df$date, "%m"))

  has_t_var <- any(!is.na(df$temperature_c))
  has_p_var <- any(!is.na(df$precipitation_mm))
  has_h_var <- any(!is.na(df$humidity_pct))

  # Days in month calculation
  get_dim <- function(y, m) {
    as.integer(difftime(
      as.Date(sprintf("%04d-%02d-01", ifelse(m == 12, y + 1, y), ifelse(m == 12, 1, m + 1))),
      as.Date(sprintf("%04d-%02d-01", y, m)),
      units = "days"
    ))
  }

  groups <- split(df, list(df$location_id, df$year, df$month), drop = TRUE)
  rows <- lapply(groups, function(sub) {
    y <- sub$year[1]
    m <- sub$month[1]
    dim_val <- get_dim(y, m)
    
    t_obs <- sum(!is.na(sub$temperature_c))
    p_obs <- sum(!is.na(sub$precipitation_mm))
    h_obs <- sum(!is.na(sub$humidity_pct))
    
    if (isTRUE(is_monthly)) {
      t_cov <- if (t_obs > 0) 100.0 else 0.0
      p_cov <- if (p_obs > 0) 100.0 else 0.0
      h_cov <- if (h_obs > 0) 100.0 else 0.0
      mean_t <- if (t_obs > 0) round(mean(sub$temperature_c, na.rm = TRUE), 2) else NA_real_
      total_p <- if (p_obs > 0) round(sum(sub$precipitation_mm, na.rm = TRUE), 2) else NA_real_
      mean_h <- if (h_obs > 0) round(mean(sub$humidity_pct, na.rm = TRUE), 2) else NA_real_
      q_stat <- "ADEQUATE_COVERAGE"
    } else {
      t_cov <- round(100.0 * t_obs / dim_val, 2)
      p_cov <- round(100.0 * p_obs / dim_val, 2)
      h_cov <- round(100.0 * h_obs / dim_val, 2)
      
      mean_t <- if (t_cov >= min_cov_pct) round(mean(sub$temperature_c, na.rm = TRUE), 2) else NA_real_
      total_p <- if (p_cov >= min_cov_pct) round(sum(sub$precipitation_mm, na.rm = TRUE), 2) else NA_real_
      mean_h <- if (h_cov >= min_cov_pct) round(mean(sub$humidity_pct, na.rm = TRUE), 2) else NA_real_
      
      q_adequate <- (!has_t_var || t_cov >= min_cov_pct) &&
                    (!has_p_var || p_cov >= min_cov_pct) &&
                    (!has_h_var || h_cov >= min_cov_pct)
      q_stat <- if (q_adequate) "ADEQUATE_COVERAGE" else "INSUFFICIENT_COVERAGE"
    }
    
    data.frame(
      location_id = sub$location_id[1],
      location_name = sub$location_name[1],
      latitude = sub$latitude[1],
      longitude = sub$longitude[1],
      year = y,
      month = m,
      total_days_in_month = dim_val,
      temp_observed_days = t_obs,
      precip_observed_days = p_obs,
      humidity_observed_days = h_obs,
      temp_coverage_pct = t_cov,
      precip_coverage_pct = p_cov,
      humidity_coverage_pct = h_cov,
      mean_temperature_c = mean_t,
      total_precipitation_mm = total_p,
      mean_humidity_pct = mean_h,
      quality_status = q_stat,
      is_usable_temp = if (isTRUE(is_monthly)) (t_obs > 0) else (t_cov >= min_cov_pct),
      is_usable_precip = if (isTRUE(is_monthly)) (p_obs > 0) else (p_cov >= min_cov_pct),
      is_usable_humidity = if (isTRUE(is_monthly)) (h_obs > 0) else (h_cov >= min_cov_pct),
      is_synthetic = any(sub$is_synthetic),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
