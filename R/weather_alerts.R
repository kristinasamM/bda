# R/weather_alerts.R - Official Weather Alerts & Experimental Condition Indicators
# Strictly separates authoritative weather alerts from user-configured experimental indicators.
# Preserves issuing authorities, original wording, and geographic applicability.
# Never invents missing alert endpoints or promises safety when no alerts are returned.

# In-memory session cache environment for alerts responses
.alerts_cache_env <- new.env(parent = emptyenv())

#' Clear Alerts Session Cache
#'
#' Clears all cached official alert responses. Useful for testing and manual resets.
clear_alerts_cache <- function() {
  rm(list = ls(envir = .alerts_cache_env), envir = .alerts_cache_env)
  invisible(TRUE)
}

#' Fetch and Parse Official Weather Alerts
#'
#' Connects to an authoritative alert feed (WeatherAPI government alerts feed when configured via WEATHERAPI_KEY).
#' When no key is configured, transparently reports that official alert coverage is not configured,
#' rather than querying invented or unofficial endpoints.
#'
#' Official Provider Documentation:
#' WeatherAPI Alerts: https://www.weatherapi.com/docs/
#'
#' @param latitude Latitude numeric
#' @param longitude Longitude numeric
#' @param location_name Character name of location
#' @param force_refresh Logical, bypass cache if TRUE
#' @param cache_ttl_sec Integer cache validity in seconds (default 900)
#' @return List with status, official alert lists (active, future, expired), and metadata
fetch_official_weather_alerts <- function(latitude,
                                          longitude,
                                          location_name = "Selected Location",
                                          force_refresh = FALSE,
                                          cache_ttl_sec = 900) {
  lat <- suppressWarnings(as.numeric(latitude))
  lon <- suppressWarnings(as.numeric(longitude))
  now_time <- Sys.time()
  
  if (is.na(lat) || is.na(lon) || lat < -90 || lat > 90 || lon < -180 || lon > 180) {
    return(list(
      status = "Alert coverage not supported",
      message = "Invalid coordinates provided for alert matching.",
      has_active = FALSE,
      alerts_active = list(),
      alerts_future = list(),
      alerts_expired = list(),
      provider = "Authoritative Alert Feeds",
      fetch_time = format(now_time, "%Y-%m-%d %H:%M:%S"),
      disclaimer = "The absence of active alerts returned by the provider does not guarantee benign or safe weather conditions."
    ))
  }
  
  cache_key <- sprintf("%.4f_%.4f", lat, lon)
  
  # Check in-memory cache
  if (!force_refresh && exists(cache_key, envir = .alerts_cache_env)) {
    cached_obj <- get(cache_key, envir = .alerts_cache_env)
    age_sec <- as.numeric(difftime(now_time, cached_obj$cached_at, units = "secs"))
    
    if (age_sec <= cache_ttl_sec) {
      cached_data <- cached_obj$data
      cached_data$cached_age_sec <- round(age_sec)
      cached_data$location_name <- location_name
      return(cached_data)
    }
  }
  
  # Check if WeatherAPI key exists in environment
  wapi_key <- trimws(Sys.getenv("WEATHERAPI_KEY", unset = ""))
  is_valid_key <- nzchar(wapi_key) && !grepl("^your_weatherapi", tolower(wapi_key))
  
  # If no authoritative provider key is configured:
  # Open-Meteo does not provide an official alerts feed for India; do not invent an endpoint.
  if (!is_valid_key) {
    result <- list(
      status = "Alert coverage not supported",
      message = "Official alerts unavailable for this location. Open-Meteo provides numerical weather prediction models but does not issue authoritative government emergency warnings. To connect authoritative alerts (CAP/IMD), set WEATHERAPI_KEY in .Renviron.",
      has_active = FALSE,
      alerts_active = list(),
      alerts_future = list(),
      alerts_expired = list(),
      provider = "Authoritative Alert Feed (Not Configured)",
      fetch_time = format(now_time, "%Y-%m-%d %H:%M:%S"),
      disclaimer = "The absence of active alerts returned by the provider does not guarantee benign or safe weather conditions."
    )
    return(result)
  }
  
  # Authoritative provider request: WeatherAPI Alerts endpoint
  provider_label <- "WeatherAPI Official Alerts Feed"
  wapi_url <- sprintf("https://api.weatherapi.com/v1/alerts.json?key=%s&q=%.4f,%.4f", wapi_key, lat, lon)
  
  alerts_list <- list()
  status_val <- "No active alerts returned by the provider."
  err_msg <- NULL
  
  tryCatch({
    if (requireNamespace("httr2", quietly = TRUE)) {
      req <- httr2::request(wapi_url)
      req <- httr2::req_timeout(req, 10)
      req <- httr2::req_retry(req, max_tries = 2, backoff = function(i) 1)
      resp <- httr2::req_perform(req)
      
      st_code <- httr2::resp_status(resp)
      if (st_code == 200) {
        json_out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
        if (!is.null(json_out$alerts) && !is.null(json_out$alerts$alert)) {
          alerts_list <- json_out$alerts$alert
        }
      } else if (st_code %in% c(401, 403)) {
        err_msg <- "Invalid or unauthorized API key for alert provider."
        status_val <- "Alert service unavailable"
      } else if (st_code == 429) {
        err_msg <- "Rate limit exceeded for alert provider."
        status_val <- "Alert service unavailable"
      } else {
        err_msg <- sprintf("HTTP status %d returned from alert provider", st_code)
        status_val <- "Alert service unavailable"
      }
    } else {
      json_out <- jsonlite::fromJSON(wapi_url, simplifyVector = FALSE)
      if (!is.null(json_out$alerts) && !is.null(json_out$alerts$alert)) {
        alerts_list <- json_out$alerts$alert
      }
    }
  }, error = function(e) {
    err_msg <<- e$message
    status_val <<- "Alert service unavailable"
  })
  
  # Handle service failure: fall back to cached alerts if present
  if (!is.null(err_msg) && exists(cache_key, envir = .alerts_cache_env)) {
    stale_obj <- get(cache_key, envir = .alerts_cache_env)
    stale_age <- round(as.numeric(difftime(now_time, stale_obj$cached_at, units = "mins")))
    stale_data <- stale_obj$data
    stale_data$status <- "Cached alert data is stale."
    stale_data$message <- sprintf("Provider connection error (%s). Displaying cached alert data from %d minute(s) ago.", err_msg, max(1, stale_age))
    return(stale_data)
  }
  
  if (!is.null(err_msg) && length(alerts_list) == 0 && status_val == "Alert service unavailable") {
    return(list(
      status = "Alert service unavailable",
      message = sprintf("Alert service unavailable: %s", err_msg),
      has_active = FALSE,
      alerts_active = list(),
      alerts_future = list(),
      alerts_expired = list(),
      provider = provider_label,
      fetch_time = format(now_time, "%Y-%m-%d %H:%M:%S"),
      disclaimer = "The absence of active alerts returned by the provider does not guarantee benign or safe weather conditions."
    ))
  }
  
  # Process and normalize raw alerts preserving all original fields
  active_alerts <- list()
  future_alerts <- list()
  expired_alerts <- list()
  seen_keys <- character(0)
  
  if (length(alerts_list) > 0) {
    for (a in alerts_list) {
      event <- if (!is.null(a$event) && nzchar(a$event)) a$event else if (!is.null(a$headline)) a$headline else "Weather Advisory"
      authority <- if (!is.null(a$note) && nzchar(a$note)) a$note else if (!is.null(a$sender_name)) a$sender_name else "National Meteorological Agency / Disaster Authority"
      desc <- if (!is.null(a$desc)) a$desc else if (!is.null(a$description)) a$description else ""
      instruction <- if (!is.null(a$instruction)) a$instruction else ""
      area <- if (!is.null(a$areas) && nzchar(a$areas)) a$areas else location_name
      severity <- if (!is.null(a$severity)) a$severity else "Unknown"
      urgency <- if (!is.null(a$urgency)) a$urgency else "Unknown"
      certainty <- if (!is.null(a$certainty)) a$certainty else "Unknown"
      source_link <- if (!is.null(a$url)) a$url else NULL
      
      start_t <- if (!is.null(a$effective)) a$effective else if (!is.null(a$starts)) a$starts else format(now_time, "%Y-%m-%d %H:%M:%S")
      end_t <- if (!is.null(a$expires)) a$expires else if (!is.null(a$ends)) a$ends else format(now_time + 86400, "%Y-%m-%d %H:%M:%S")
      
      # Deduplication key across event, start time, authority, and area
      dedup_key <- paste(event, start_t, authority, area, sep = "_")
      if (dedup_key %in% seen_keys) next
      seen_keys <- c(seen_keys, dedup_key)
      
      start_posix <- suppressWarnings(as.POSIXct(start_t))
      end_posix <- suppressWarnings(as.POSIXct(end_t))
      
      alert_obj <- list(
        event_name = event,
        issuing_authority = authority,
        provider = provider_label,
        issued_time = start_t,
        start_time = start_t,
        expiry_time = end_t,
        severity = severity,
        urgency = urgency,
        certainty = certainty,
        affected_area = area,
        description = desc,
        instructions = instruction,
        source_link = source_link
      )
      
      if (!is.na(end_posix) && end_posix < now_time) {
        expired_alerts[[length(expired_alerts) + 1]] <- alert_obj
      } else if (!is.na(start_posix) && start_posix > now_time) {
        future_alerts[[length(future_alerts) + 1]] <- alert_obj
      } else {
        active_alerts[[length(active_alerts) + 1]] <- alert_obj
      }
    }
  }
  
  if (length(active_alerts) > 0) {
    status_val <- sprintf("%d Active Official Alert(s)", length(active_alerts))
  } else if (length(future_alerts) > 0) {
    status_val <- "No active alerts (Future alerts scheduled)"
  } else {
    status_val <- "No active alerts returned by the provider."
  }
  
  result <- list(
    status = status_val,
    has_active = (length(active_alerts) > 0),
    alerts_active = active_alerts,
    alerts_future = future_alerts,
    alerts_expired = expired_alerts,
    provider = provider_label,
    fetch_time = format(now_time, "%Y-%m-%d %H:%M:%S"),
    disclaimer = "The absence of active alerts returned by the provider does not guarantee benign or safe weather conditions."
  )
  
  # Cache in in-memory session cache
  assign(cache_key, list(data = result, cached_at = now_time), envir = .alerts_cache_env)
  
  result
}

#' Generate Experimental Weather-Condition Indicators
#'
#' Evaluates user-configured or disclosed meteorological thresholds against
#' live daily forecast values. Explicitly separated from official warnings.
#'
#' @param daily_df Daily forecast data frame from fetch_live_weather()
#' @param selected_day_idx Integer index of target forecast day (1 = today, default 1)
#' @param config Application settings
#' @param custom_thresholds Optional list of user-configured thresholds (hot, rain, wind)
#' @return List of experimental indicators
evaluate_experimental_indicators <- function(daily_df,
                                             selected_day_idx = 1,
                                             config = NULL,
                                             custom_thresholds = NULL) {
  if (is.null(daily_df) || nrow(daily_df) == 0) return(list())
  
  idx <- max(1, min(nrow(daily_df), selected_day_idx))
  target_row <- daily_df[idx, ]
  
  # Disclosed application thresholds (custom override or config default)
  hot_thresh <- if (!is.null(custom_thresholds$hot)) {
    as.numeric(custom_thresholds$hot)
  } else if (!is.null(config$community$live_hot_threshold_c)) {
    config$community$live_hot_threshold_c
  } else {
    40.0
  }
  
  rain_thresh <- if (!is.null(custom_thresholds$rain)) {
    as.numeric(custom_thresholds$rain)
  } else if (!is.null(config$community$live_wet_threshold_mm)) {
    config$community$live_wet_threshold_mm
  } else {
    50.0
  }
  
  wind_thresh <- if (!is.null(custom_thresholds$wind)) {
    as.numeric(custom_thresholds$wind)
  } else if (!is.null(config$community$live_wind_threshold_kmh)) {
    config$community$live_wind_threshold_kmh
  } else {
    50.0
  }
  
  indicators <- list()
  
  # 1. High Temperature Indicator
  t_max <- target_row$temp_max
  if (!is.na(t_max) && t_max >= hot_thresh) {
    indicators$high_temp <- list(
      name = "High Temperature Indicator",
      value = t_max,
      unit = "°C",
      period = target_row$day_name,
      threshold = sprintf("≥ %.1f °C (Application scenario threshold)", hot_thresh),
      source = "Open-Meteo 7-Day Numerical Weather Prediction Forecast",
      why = sprintf("Forecast daily maximum temperature reaches or exceeds the application's %.1f °C benchmark.", hot_thresh),
      disclaimer = "Scenario indicator based on daily model maximum. This is NOT an official IMD Heatwave Warning."
    )
  }
  
  # 2. Heavy Rainfall Indicator
  p_sum <- target_row$precip_sum
  if (!is.na(p_sum) && p_sum >= rain_thresh) {
    indicators$heavy_rain <- list(
      name = "Heavy Rainfall Indicator",
      value = p_sum,
      unit = "mm / 24h",
      period = target_row$day_name,
      threshold = sprintf("≥ %.1f mm (Application scenario threshold)", rain_thresh),
      source = "Open-Meteo 7-Day Numerical Weather Prediction Forecast",
      why = sprintf("Forecast rainfall exceeds the application's daily threshold (%.1f mm).", rain_thresh),
      disclaimer = "Rainfall indicator reflecting aggregate model volume. This does not infer confirmed flood risk or substitute for official hydrological advisories."
    )
  }
  
  # 3. Strong Wind Gust Indicator
  w_gust <- target_row$wind_gust
  if (!is.na(w_gust) && w_gust >= wind_thresh) {
    indicators$strong_wind <- list(
      name = "Strong Winds Indicator",
      value = w_gust,
      unit = "km/h",
      period = target_row$day_name,
      threshold = sprintf("≥ %.1f km/h (Application scenario threshold)", wind_thresh),
      source = "Open-Meteo 7-Day Numerical Weather Prediction Forecast",
      why = sprintf("Forecast peak wind gust reaches or exceeds the application's %.1f km/h benchmark.", wind_thresh),
      disclaimer = "Wind gust indicator derived from gridded model peak estimates. This is NOT an official Cyclone or High Wind Warning."
    )
  }
  
  indicators
}
