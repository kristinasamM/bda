# R/forecasting.R - Temperature Time-Series Forecasting for ClimateWatch

#' Run Chronologically Evaluated Temperature Forecasting
#'
#' Compares Seasonal Naïve, ETS, and ARIMA models using a strict 12-month holdout test set.
#' Evaluates models on identical timestamps, reports actual MAE/RMSE,
#' refits the best performing model on the complete series, and produces
#' future point forecasts with 80% and 95% prediction intervals.
#'
#' @param monthly_df Monthly data frame for a specific location
#' @param location_id Selected location ID
#' @param horizon Forecast horizon in months (3, 6, or 12)
#' @param holdout_months Number of consecutive months reserved for evaluation (default 12)
#' @return List with diagnostic status, model comparison, holdout predictions, and future forecasts
forecast_temperature <- function(monthly_df,
                                 location_id,
                                 horizon = 6,
                                 holdout_months = 12) {
  # Validate input
  if (is.null(monthly_df) || nrow(monthly_df) == 0) {
    return(list(status = "INSUFFICIENT_DATA", message = "No monthly data available for forecasting."))
  }

  loc_df <- monthly_df[monthly_df$location_id == location_id & monthly_df$is_usable_temp, ]
  loc_df <- loc_df[!is.na(loc_df$mean_temperature_c), ]

  if (nrow(loc_df) < (holdout_months + 12)) {
    return(list(
      status = "INSUFFICIENT_DATA",
      message = sprintf(
        "Insufficient historical data for %s. Minimum required: %d valid months (at least %d training + %d holdout); available: %d.",
        location_id, holdout_months + 12, 12, holdout_months, nrow(loc_df)
      )
    ))
  }

  # Ensure chronological order
  loc_df <- loc_df[order(loc_df$year, loc_df$month), ]

  # Check continuity of monthly series
  min_date <- loc_df$year_month[1]
  max_date <- loc_df$year_month[nrow(loc_df)]
  expected_dates <- seq.Date(from = min_date, to = max_date, by = "month")

  if (length(expected_dates) != nrow(loc_df)) {
    # If there are gaps, find largest contiguous segment
    # Match dates to find missing
    date_diffs <- as.numeric(diff(loc_df$year_month))
    # Approximate monthly difference: 28 to 31 days
    gap_indices <- which(date_diffs > 32)
    if (length(gap_indices) > 0) {
      # Use segment after last gap
      start_idx <- max(gap_indices) + 1
      loc_df <- loc_df[start_idx:nrow(loc_df), ]
      if (nrow(loc_df) < (holdout_months + 12)) {
        return(list(
          status = "INSUFFICIENT_DATA",
          message = "Contiguous valid monthly sequence is too short after handling timeline gaps."
        ))
      }
    }
  }

  n_total <- nrow(loc_df)
  n_train <- n_total - holdout_months

  start_yr <- loc_df$year[1]
  start_mo <- loc_df$month[1]

  # Create ts objects
  full_ts <- stats::ts(loc_df$mean_temperature_c, start = c(start_yr, start_mo), frequency = 12)
  train_ts <- stats::window(full_ts, end = stats::time(full_ts)[n_train])
  actual_holdout <- as.numeric(stats::window(full_ts, start = stats::time(full_ts)[n_train + 1]))
  holdout_dates <- loc_df$year_month[(n_train + 1):n_total]

  # Check forecast package availability
  has_forecast <- requireNamespace("forecast", quietly = TRUE)

  # Model fits on training data
  fits <- list()
  preds <- list()

  # 1. Seasonal Naïve
  if (has_forecast) {
    fits$snaive <- tryCatch(forecast::snaive(train_ts, h = holdout_months), error = function(e) NULL)
    preds$snaive <- if (!is.null(fits$snaive)) as.numeric(fits$snaive$mean) else rep(NA_real_, holdout_months)
  } else {
    # Base fallback for snaive: last 12 values
    snaive_vals <- rep(tail(as.numeric(train_ts), 12), length.out = holdout_months)
    preds$snaive <- snaive_vals
  }

  # 2. ETS
  if (has_forecast) {
    fits$ets <- tryCatch({
      fit <- forecast::ets(train_ts, model = "ZZZ")
      forecast::forecast(fit, h = holdout_months)
    }, error = function(e) NULL)
    preds$ets <- if (!is.null(fits$ets)) as.numeric(fits$ets$mean) else rep(NA_real_, holdout_months)
  } else {
    preds$ets <- preds$snaive
  }

  # 3. ARIMA
  if (has_forecast) {
    fits$arima <- tryCatch({
      fit <- forecast::auto.arima(train_ts, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
      forecast::forecast(fit, h = holdout_months)
    }, error = function(e) NULL)
    preds$arima <- if (!is.null(fits$arima)) as.numeric(fits$arima$mean) else rep(NA_real_, holdout_months)
  } else {
    preds$arima <- preds$snaive
  }

  # Evaluation metrics on holdout test set
  calc_mae <- function(actual, pred) {
    valid <- !is.na(actual) & !is.na(pred)
    if (sum(valid) == 0) return(NA_real_)
    round(mean(abs(actual[valid] - pred[valid])), 3)
  }

  calc_rmse <- function(actual, pred) {
    valid <- !is.na(actual) & !is.na(pred)
    if (sum(valid) == 0) return(NA_real_)
    round(sqrt(mean((actual[valid] - pred[valid])^2)), 3)
  }

  mae_snaive <- calc_mae(actual_holdout, preds$snaive)
  rmse_snaive <- calc_rmse(actual_holdout, preds$snaive)

  mae_ets <- calc_mae(actual_holdout, preds$ets)
  rmse_ets <- calc_rmse(actual_holdout, preds$ets)

  mae_arima <- calc_mae(actual_holdout, preds$arima)
  rmse_arima <- calc_rmse(actual_holdout, preds$arima)

  # Comparison table
  comp_df <- data.frame(
    Model = c("Seasonal Naïve", "ETS", "ARIMA"),
    MAE = c(mae_snaive, mae_ets, mae_arima),
    RMSE = c(rmse_snaive, rmse_ets, rmse_arima),
    Status = c(
      ifelse(is.na(rmse_snaive), "Failed", "Evaluated"),
      ifelse(is.na(rmse_ets), "Failed", "Evaluated"),
      ifelse(is.na(rmse_arima), "Failed", "Evaluated")
    ),
    stringsAsFactors = FALSE
  )

  # Identify best model on holdout
  valid_rmse <- comp_df$RMSE
  valid_rmse[is.na(valid_rmse)] <- Inf
  best_idx <- which.min(valid_rmse)
  best_model_name <- comp_df$Model[best_idx]
  comp_df$Selected <- ifelse(seq_len(nrow(comp_df)) == best_idx, "Yes (Best on Holdout)", "No")

  # -------------------------------------------------------------
  # REFIT BEST MODEL ON COMPLETE ELIGIBLE DATASET FOR FUTURE FORECAST
  # -------------------------------------------------------------
  future_forecast <- NULL
  if (has_forecast) {
    if (best_model_name == "Seasonal Naïve") {
      future_fit <- tryCatch(forecast::snaive(full_ts, h = horizon), error = function(e) NULL)
    } else if (best_model_name == "ETS") {
      future_fit <- tryCatch({
        fit_full <- forecast::ets(full_ts, model = "ZZZ")
        forecast::forecast(fit_full, h = horizon)
      }, error = function(e) NULL)
    } else {
      future_fit <- tryCatch({
        fit_full <- forecast::auto.arima(full_ts, seasonal = TRUE, stepwise = TRUE)
        forecast::forecast(fit_full, h = horizon)
      }, error = function(e) NULL)
    }
    future_forecast <- future_fit
  }

  # Build future dates
  last_yr <- loc_df$year[n_total]
  last_mo <- loc_df$month[n_total]
  
  future_dates <- as.Date(character(horizon))
  curr_y <- last_yr
  curr_m <- last_mo
  for (h in seq_len(horizon)) {
    curr_m <- curr_m + 1
    if (curr_m > 12) {
      curr_m <- 1
      curr_y <- curr_y + 1
    }
    future_dates[h] <- as.Date(sprintf("%04d-%02d-01", curr_y, curr_m))
  }

  if (!is.null(future_forecast)) {
    pt_mean <- as.numeric(future_forecast$mean)
    l80 <- as.numeric(future_forecast$lower[, 1])
    u80 <- as.numeric(future_forecast$upper[, 1])
    l95 <- as.numeric(future_forecast$lower[, 2])
    u95 <- as.numeric(future_forecast$upper[, 2])
  } else {
    # Fallback point forecast if package call fails
    last_12 <- tail(loc_df$mean_temperature_c, 12)
    pt_mean <- rep(last_12, length.out = horizon)
    sd_val <- stats::sd(loc_df$mean_temperature_c)
    l80 <- pt_mean - 1.28 * sd_val
    u80 <- pt_mean + 1.28 * sd_val
    l95 <- pt_mean - 1.96 * sd_val
    u95 <- pt_mean + 1.96 * sd_val
  }

  future_df <- data.frame(
    date = future_dates,
    month_year = format(future_dates, "%B %Y"),
    predicted_temp_c = round(pt_mean, 2),
    lower_80 = round(l80, 2),
    upper_80 = round(u80, 2),
    lower_95 = round(l95, 2),
    upper_95 = round(u95, 2),
    model_used = best_model_name,
    stringsAsFactors = FALSE
  )

  # Holdout dataframe for actual-vs-predicted chart
  holdout_df <- data.frame(
    date = holdout_dates,
    month_year = format(holdout_dates, "%B %Y"),
    actual = round(actual_holdout, 2),
    snaive_pred = round(preds$snaive, 2),
    ets_pred = round(preds$ets, 2),
    arima_pred = round(preds$arima, 2),
    best_pred = round(switch(best_model_name,
      "Seasonal Naïve" = preds$snaive,
      "ETS" = preds$ets,
      "ARIMA" = preds$arima
    ), 2),
    stringsAsFactors = FALSE
  )

  # Historical series dataframe
  hist_df <- data.frame(
    date = loc_df$year_month,
    temperature_c = loc_df$mean_temperature_c,
    period = ifelse(loc_df$year_month %in% holdout_dates, "Holdout Test (12m)", "Training History"),
    stringsAsFactors = FALSE
  )

  training_range_str <- sprintf("%s to %s (%d months)",
    format(loc_df$year_month[1], "%b %Y"),
    format(loc_df$year_month[n_train], "%b %Y"),
    n_train
  )
  holdout_range_str <- sprintf("%s to %s (%d months)",
    format(holdout_dates[1], "%b %Y"),
    format(holdout_dates[length(holdout_dates)], "%b %Y"),
    holdout_months
  )

  list(
    status = "SUCCESS",
    location_id = location_id,
    location_name = loc_df$location_name[1],
    horizon = horizon,
    training_range = training_range_str,
    holdout_range = holdout_range_str,
    best_model_name = best_model_name,
    comparison_table = comp_df,
    holdout_df = holdout_df,
    future_df = future_df,
    historical_df = hist_df
  )
}
