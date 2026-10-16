# R/relationships.R - Weather Variables Correlation & Relationship Explorer
# Strictly analyzes true meteorological parameters (Temperature, Precipitation, Humidity).
# Strictly excludes identifier codes, indices, station numbers, and geographic coordinates.

#' Compute Weather Variable Correlations for a Specific Location
#'
#' @param daily_df Daily data frame for the active dataset
#' @param location_id Selected location identifier
#' @return List containing correlation matrix, pairwise counts, and educational explanation
compute_weather_correlations <- function(daily_df, location_id = NULL) {
  if (is.null(daily_df) || nrow(daily_df) == 0) {
    return(list(status = "NO_DATA", message = "No data available for correlation analysis."))
  }

  loc_df <- daily_df
  if (!is.null(location_id) && nzchar(location_id) && "location_id" %in% names(loc_df)) {
    loc_sub <- loc_df[loc_df$location_id == location_id, ]
    if (nrow(loc_sub) > 0) loc_df <- loc_sub
  }

  # Strictly include only true weather measurements
  weather_vars <- intersect(c("temperature_c", "precipitation_mm", "humidity_pct"), names(loc_df))
  
  # Filter to variables that have at least some non-NA values
  valid_vars <- character(0)
  for (v in weather_vars) {
    if (sum(!is.na(loc_df[[v]])) >= 5) {
      valid_vars <- c(valid_vars, v)
    }
  }

  if (length(valid_vars) < 2) {
    return(list(
      status = "INSUFFICIENT_VARIABLES",
      message = "At least two valid weather measurements (e.g. Temperature and Rainfall) are required for correlation analysis. Identifier columns are excluded."
    ))
  }

  sub_mat <- as.matrix(loc_df[, valid_vars, drop = FALSE])
  
  # Pearson correlation with pairwise complete observations
  cor_mat <- tryCatch({
    stats::cor(sub_mat, use = "pairwise.complete.obs", method = "pearson")
  }, error = function(e) matrix(NA_real_, nrow = length(valid_vars), ncol = length(valid_vars)))

  # Count pairwise observations
  n_mat <- matrix(0L, nrow = length(valid_vars), ncol = length(valid_vars), dimnames = list(valid_vars, valid_vars))
  for (i in seq_along(valid_vars)) {
    for (j in seq_along(valid_vars)) {
      v1 <- valid_vars[i]
      v2 <- valid_vars[j]
      n_mat[i, j] <- sum(!is.na(loc_df[[v1]]) & !is.na(loc_df[[v2]]))
    }
  }

  # Pretty labels
  var_labels <- c(
    temperature_c = "Temperature (°C)",
    precipitation_mm = "Rainfall (mm)",
    humidity_pct = "Relative Humidity (%)"
  )
  display_names <- var_labels[valid_vars]

  list(
    status = "SUCCESS",
    location_id = location_id,
    variables = valid_vars,
    display_names = display_names,
    cor_matrix = round(cor_mat, 3),
    n_matrix = n_mat,
    educational_note = "Correlation describes association, not causation. Seasonal patterns (such as monsoon cooling or summer heat) strongly influence these relationships."
  )
}

#' Generate Weather Pair Scatterplot Data
#'
#' @param daily_df Daily data frame
#' @param location_id Selected location ID
#' @param var_x Variable name for X axis
#' @param var_y Variable name for Y axis
#' @return List with aligned points, linear regression slope/intercept, r value, and p value
generate_weather_pair_data <- function(daily_df, location_id, var_x, var_y) {
  if (is.null(daily_df) || nrow(daily_df) == 0) return(list(status = "NO_DATA"))
  
  loc_df <- daily_df
  if (!is.null(location_id) && nzchar(location_id) && "location_id" %in% names(loc_df)) {
    loc_sub <- loc_df[loc_df$location_id == location_id, ]
    if (nrow(loc_sub) > 0) loc_df <- loc_sub
  }

  if (!all(c(var_x, var_y) %in% names(loc_df))) {
    return(list(status = "MISSING_COLUMNS"))
  }

  valid_mask <- !is.na(loc_df[[var_x]]) & !is.na(loc_df[[var_y]])
  points_df <- loc_df[valid_mask, c("date", var_x, var_y)]
  
  if (nrow(points_df) < 5) {
    return(list(status = "INSUFFICIENT_POINTS", n = nrow(points_df)))
  }

  fit <- stats::lm(points_df[[var_y]] ~ points_df[[var_x]])
  r_val <- stats::cor(points_df[[var_x]], points_df[[var_y]])
  
  list(
    status = "SUCCESS",
    data = points_df,
    n = nrow(points_df),
    r = round(r_val, 3),
    r_squared = round(r_val^2, 3),
    slope = round(stats::coef(fit)[2], 4),
    intercept = round(stats::coef(fit)[1], 3)
  )
}
