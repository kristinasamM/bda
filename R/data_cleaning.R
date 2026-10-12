# R/data_cleaning.R - Data Cleaning and Validation for ClimateWatch

#' Clean Daily Weather Observations
#'
#' Enforces daily schema, removes duplicates, handles sentinels,
#' validates plausible values, and tracks data quality flags.
#'
#' @param raw_df Data frame containing raw daily meteorological data
#' @param config List of configuration parameters (optional)
#' @return Cleaned data frame adhering to ClimateWatch daily schema
clean_daily_weather_data <- function(raw_df, config = NULL) {
  if (is.null(raw_df) || nrow(raw_df) == 0) {
    return(data.frame(
      location_id = character(0),
      location_name = character(0),
      latitude = numeric(0),
      longitude = numeric(0),
      date = as.Date(character(0)),
      temperature_c = numeric(0),
      precipitation_mm = numeric(0),
      humidity_pct = numeric(0),
      source = character(0),
      is_synthetic = logical(0),
      quality_flag = character(0),
      stringsAsFactors = FALSE
    ))
  }

  df <- as.data.frame(raw_df, stringsAsFactors = FALSE)

  # Standardize column names if needed
  col_map <- c(
    "T2M" = "temperature_c",
    "PRECTOTCORR" = "precipitation_mm",
    "PRECTOT" = "precipitation_mm",
    "RH2M" = "humidity_pct",
    "temp" = "temperature_c",
    "precip" = "precipitation_mm",
    "humidity" = "humidity_pct"
  )
  for (nm in names(col_map)) {
    if (nm %in% names(df) && !(col_map[[nm]] %in% names(df))) {
      names(df)[names(df) == nm] <- col_map[[nm]]
    }
  }

  # Ensure core columns exist
  req_cols <- c("location_id", "location_name", "latitude", "longitude", "date",
                "temperature_c", "precipitation_mm", "humidity_pct")
  for (col in req_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }

  # Optional metadata columns
  if (!"source" %in% names(df)) df$source <- "UNKNOWN"
  if (!"is_synthetic" %in% names(df)) df$is_synthetic <- FALSE
  if (!"quality_flag" %in% names(df)) df$quality_flag <- "VALID"

  # Convert types
  df$location_id <- as.character(df$location_id)
  df$location_name <- as.character(df$location_name)
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  df$date <- as.Date(df$date)
  df$temperature_c <- as.numeric(df$temperature_c)
  df$precipitation_mm <- as.numeric(df$precipitation_mm)
  df$humidity_pct <- as.numeric(df$humidity_pct)
  df$is_synthetic <- as.logical(df$is_synthetic)
  df$source <- as.character(df$source)

  # Filter out rows with invalid dates or missing location_id
  df <- df[!is.na(df$date) & !is.na(df$location_id) & df$location_id != "", ]

  # Remove duplicate location-date records (keep first valid)
  # Deterministic sorting before deduplication
  df <- df[order(df$location_id, df$date), ]
  dup_mask <- duplicated(df[, c("location_id", "date")])
  df <- df[!dup_mask, ]

  # Missing-value sentinel conversion to NA (-999, -99, etc.)
  sentinels <- c(-999, -999.0, -99, -99.0, -9999)
  if (!is.null(config$quality$sentinel_values)) {
    sentinels <- union(sentinels, config$quality$sentinel_values)
  }

  df$temperature_c[df$temperature_c %in% sentinels] <- NA
  df$precipitation_mm[df$precipitation_mm %in% sentinels] <- NA
  df$humidity_pct[df$humidity_pct %in% sentinels] <- NA

  # Plausible-value validation
  # Temperature: -20 to 60 °C
  temp_range <- if (!is.null(config$quality$temp_valid_range)) config$quality$temp_valid_range else c(-20, 60)
  precip_range <- if (!is.null(config$quality$precip_valid_range)) config$quality$precip_valid_range else c(0, 1500)
  hum_range <- if (!is.null(config$quality$humidity_valid_range)) config$quality$humidity_valid_range else c(0, 100)

  temp_invalid <- !is.na(df$temperature_c) & (df$temperature_c < temp_range[1] | df$temperature_c > temp_range[2])
  precip_invalid <- !is.na(df$precipitation_mm) & (df$precipitation_mm < precip_range[1] | df$precipitation_mm > precip_range[2])
  hum_invalid <- !is.na(df$humidity_pct) & (df$humidity_pct < hum_range[1] | df$humidity_pct > hum_range[2])

  df$temperature_c[temp_invalid] <- NA
  df$precipitation_mm[precip_invalid] <- NA
  df$humidity_pct[hum_invalid] <- NA

  # Quality flag computation
  # Preserve missing observations without imputing or converting rainfall NA to 0
  q_flag <- rep("VALID", nrow(df))
  na_count <- is.na(df$temperature_c) + is.na(df$precipitation_mm) + is.na(df$humidity_pct)
  
  q_flag[na_count == 3] <- "MISSING"
  q_flag[na_count %in% c(1, 2)] <- "PARTIAL"
  q_flag[temp_invalid | precip_invalid | hum_invalid] <- "SUSPECT"
  df$quality_flag <- q_flag

  # Sort by location_id and date
  df <- df[order(df$location_id, df$date), ]
  rownames(df) <- NULL

  df[, c("location_id", "location_name", "latitude", "longitude", "date",
         "temperature_c", "precipitation_mm", "humidity_pct",
         "source", "is_synthetic", "quality_flag")]
}

#' Identify Missing Dates in a Historical Time Series
#'
#' Checks whether every calendar date between start_date and end_date is present.
#' @param df Cleaned data frame
#' @param start_date Date or string (optional, default min date in df)
#' @param end_date Date or string (optional, default max date in df)
#' @return Data frame listing missing dates per location
identify_missing_dates <- function(df, start_date = NULL, end_date = NULL) {
  if (nrow(df) == 0) return(data.frame(location_id = character(0), missing_date = as.Date(character(0))))

  locs <- unique(df$location_id)
  s_date <- if (!is.null(start_date)) as.Date(start_date) else min(df$date, na.rm = TRUE)
  e_date <- if (!is.null(end_date)) as.Date(end_date) else max(df$date, na.rm = TRUE)
  
  full_dates <- seq.Date(from = s_date, to = e_date, by = "day")
  
  missing_records <- list()
  for (loc in locs) {
    loc_dates <- df$date[df$location_id == loc]
    absent <- setdiff(full_dates, loc_dates)
    if (length(absent) > 0) {
      missing_records[[loc]] <- data.frame(
        location_id = loc,
        missing_date = as.Date(absent, origin = "1970-01-01"),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(missing_records) > 0) {
    do.call(rbind, missing_records)
  } else {
    data.frame(location_id = character(0), missing_date = as.Date(character(0)), stringsAsFactors = FALSE)
  }
}

#' Compute Cleaning and Validation Metrics
#'
#' @param raw_df Data frame before cleaning
#' @param cleaned_df Data frame after cleaning
#' @return List of cleaning statistics
summarize_cleaning_metrics <- function(raw_df, cleaned_df) {
  raw_count <- if (is.null(raw_df)) 0 else nrow(raw_df)
  cleaned_count <- nrow(cleaned_df)
  
  # Duplicates calculation
  dup_count <- 0
  if (!is.null(raw_df) && nrow(raw_df) > 0 && "location_id" %in% names(raw_df) && "date" %in% names(raw_df)) {
    dup_count <- sum(duplicated(raw_df[, c("location_id", "date")]))
  }
  
  list(
    raw_record_count = raw_count,
    cleaned_record_count = cleaned_count,
    duplicates_removed = dup_count,
    missing_temp_count = sum(is.na(cleaned_df$temperature_c)),
    missing_precip_count = sum(is.na(cleaned_df$precipitation_mm)),
    missing_humidity_count = sum(is.na(cleaned_df$humidity_pct)),
    suspect_count = sum(cleaned_df$quality_flag == "SUSPECT"),
    locations_count = length(unique(cleaned_df$location_id)),
    date_min = if (cleaned_count > 0) as.character(min(cleaned_df$date)) else NA,
    date_max = if (cleaned_count > 0) as.character(max(cleaned_df$date)) else NA
  )
}
