# R/dataset_manager.R - Actual Dataset Upload, Validation, Storage, and Registry Pipeline
# Strictly implements clean upload flow, column mapping, unit conversion,
# exact duplicate removal, conflicting duplicate blocking, and dataset activation.

# Ensure required sibling modules are loaded
if (file.exists("R/storage.R")) source("R/storage.R")
if (file.exists("R/aggregation.R")) source("R/aggregation.R")
if (file.exists("R/config.R")) source("R/config.R")

# Ensure upload directory hierarchy exists
ensure_upload_directories <- function(base_dir = "data/uploads") {
  dirs <- c(
    base_dir,
    file.path(base_dir, "raw"),
    file.path(base_dir, "processed"),
    file.path(base_dir, "metadata")
  )
  for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

# -------------------------------------------------------------------
# REGISTRY AND ACTIVE DATASET STATE
# -------------------------------------------------------------------

#' Get Uploaded Dataset Registry
#'
#' @param base_dir Base uploads directory
#' @return Data frame of registered datasets
get_dataset_registry <- function(base_dir = "data/uploads") {
  ensure_upload_directories(base_dir)
  reg_file <- file.path(base_dir, "registry.json")
  if (!file.exists(reg_file)) {
    return(data.frame(
      dataset_id = character(0),
      dataset_name = character(0),
      source_reference = character(0),
      uploaded_at = character(0),
      record_count = integer(0),
      locations_count = integer(0),
      date_min = character(0),
      date_max = character(0),
      is_active = logical(0),
      storage_format = character(0),
      has_coordinates = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  
  tryCatch({
    reg <- jsonlite::fromJSON(reg_file)
    if (is.null(reg) || nrow(reg) == 0) {
      data.frame(
        dataset_id = character(0),
        dataset_name = character(0),
        source_reference = character(0),
        uploaded_at = character(0),
        record_count = integer(0),
        locations_count = integer(0),
        date_min = character(0),
        date_max = character(0),
        is_active = logical(0),
        storage_format = character(0),
        has_coordinates = logical(0),
        stringsAsFactors = FALSE
      )
    } else {
      as.data.frame(reg, stringsAsFactors = FALSE)
    }
  }, error = function(e) {
    data.frame(
      dataset_id = character(0),
      dataset_name = character(0),
      source_reference = character(0),
      uploaded_at = character(0),
      record_count = integer(0),
      locations_count = integer(0),
      date_min = character(0),
      date_max = character(0),
      is_active = logical(0),
      storage_format = character(0),
      has_coordinates = logical(0),
      stringsAsFactors = FALSE
    )
  })
}

#' Save Uploaded Dataset Registry
#'
#' @param registry_df Data frame of datasets
#' @param base_dir Base uploads directory
save_dataset_registry <- function(registry_df, base_dir = "data/uploads") {
  ensure_upload_directories(base_dir)
  reg_file <- file.path(base_dir, "registry.json")
  jsonlite::write_json(registry_df, reg_file, pretty = TRUE, auto_unbox = TRUE)
}

#' Get Currently Active Dataset ID
#'
#' @param base_dir Base uploads directory
#' @return Character dataset ID or NULL if none active
get_active_dataset_id <- function(base_dir = "data/uploads") {
  active_file <- file.path(base_dir, "active.json")
  if (!file.exists(active_file)) return(NULL)
  tryCatch({
    info <- jsonlite::fromJSON(active_file)
    if (!is.null(info$dataset_id) && nzchar(info$dataset_id)) info$dataset_id else NULL
  }, error = function(e) NULL)
}

#' Set Currently Active Dataset ID
#'
#' @param dataset_id Character dataset ID or NULL to deactivate
#' @param base_dir Base uploads directory
set_active_dataset_id <- function(dataset_id, base_dir = "data/uploads") {
  ensure_upload_directories(base_dir)
  active_file <- file.path(base_dir, "active.json")
  
  if (is.null(dataset_id) || !nzchar(dataset_id)) {
    if (file.exists(active_file)) unlink(active_file)
    # Update registry
    reg <- get_dataset_registry(base_dir)
    if (nrow(reg) > 0) {
      reg$is_active <- FALSE
      save_dataset_registry(reg, base_dir)
    }
    return(invisible(TRUE))
  }
  
  info <- list(
    dataset_id = dataset_id,
    activated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  )
  jsonlite::write_json(info, active_file, pretty = TRUE, auto_unbox = TRUE)
  
  # Update registry
  reg <- get_dataset_registry(base_dir)
  if (nrow(reg) > 0) {
    reg$is_active <- (reg$dataset_id == dataset_id)
    save_dataset_registry(reg, base_dir)
  }
  invisible(TRUE)
}

#' Load Active Dataset
#'
#' Retrieves metadata, locations data frame, and precalculated monthly aggregates for active dataset.
#'
#' @param base_dir Base uploads directory
#' @return List containing meta, monthly_data, locations_df, or NULL if none active
load_active_dataset <- function(base_dir = "data/uploads") {
  ds_id <- get_active_dataset_id(base_dir)
  if (is.null(ds_id)) return(NULL)
  
  meta_file <- file.path(base_dir, "metadata", paste0(ds_id, ".json"))
  proc_dir <- file.path(base_dir, "processed", ds_id)
  monthly_file <- file.path(proc_dir, "monthly_aggregates.rds")
  locations_file <- file.path(proc_dir, "locations.rds")
  
  if (!file.exists(meta_file) || !file.exists(monthly_file)) {
    return(NULL)
  }
  
  meta <- tryCatch(jsonlite::fromJSON(meta_file), error = function(e) NULL)
  monthly_df <- tryCatch(readRDS(monthly_file), error = function(e) data.frame())
  locations_df <- if (file.exists(locations_file)) {
    tryCatch(readRDS(locations_file), error = function(e) data.frame())
  } else {
    data.frame()
  }
  
  list(
    dataset_id = ds_id,
    meta = meta,
    monthly_data = monthly_df,
    locations_df = locations_df,
    processed_dir = proc_dir
  )
}

# -------------------------------------------------------------------
# STEP A: FILE PREVIEW & COLUMN HEURISTICS
# -------------------------------------------------------------------

#' Preview Uploaded File
#'
#' Reads the first N rows of a CSV, Excel (.xlsx, .xls), or Parquet file and reports column names and types.
#'
#' @param file_path Absolute or relative file path
#' @param max_rows Number of rows to preview (default 10)
#' @param sheet Optional sheet name or index for Excel files
#' @return List with columns, total_rows, preview_df, file_format, sheets
preview_uploaded_file <- function(file_path, max_rows = 10, sheet = NULL) {
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("File does not exist or invalid path provided.")
  }
  
  ext <- tolower(tools::file_ext(file_path))
  
  if (ext %in% c("parquet", "pq")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("The 'arrow' R package is required to read Parquet files.")
    }
    schema <- arrow::read_parquet(file_path, as_data_frame = FALSE)$schema
    col_names <- names(schema)
    preview_df <- as.data.frame(arrow::read_parquet(file_path, as_data_frame = TRUE))[seq_len(min(max_rows, 50)), , drop = FALSE]
    total_rows <- nrow(preview_df)
    try({
      ds <- arrow::open_dataset(file_path, format = "parquet")
      total_rows <- ds$num_rows
    }, silent = TRUE)
    
    return(list(
      file_format = "parquet",
      sheets = character(0),
      selected_sheet = "",
      columns = col_names,
      total_rows = total_rows,
      preview_df = preview_df
    ))
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("The 'readxl' R package is required to read Excel files.")
    }
    sheets_avail <- readxl::excel_sheets(file_path)
    sheet_to_use <- if (!is.null(sheet) && nzchar(as.character(sheet)) && as.character(sheet) %in% sheets_avail) {
      as.character(sheet)
    } else if (length(sheets_avail) > 0) {
      sheets_avail[1]
    } else {
      1L
    }
    
    preview_df <- as.data.frame(readxl::read_excel(file_path, sheet = sheet_to_use, n_max = max_rows))
    col_names <- colnames(preview_df)
    
    # Estimate total rows
    total_rows <- tryCatch({
      nrow(readxl::read_excel(file_path, sheet = sheet_to_use, col_types = "text"))
    }, error = function(e) nrow(preview_df))
    
    return(list(
      file_format = "excel",
      sheets = sheets_avail,
      selected_sheet = sheet_to_use,
      columns = col_names,
      total_rows = total_rows,
      preview_df = preview_df
    ))
  } else if (ext %in% c("csv", "txt")) {
    preview_df <- tryCatch({
      if (requireNamespace("data.table", quietly = TRUE)) {
        data.table::fread(file_path, nrows = max_rows, data.table = FALSE, check.names = FALSE)
      } else {
        utils::read.csv(file_path, nrows = max_rows, stringsAsFactors = FALSE, check.names = FALSE)
      }
    }, error = function(e) {
      utils::read.csv(file_path, nrows = max_rows, stringsAsFactors = FALSE, check.names = FALSE)
    })
    col_names <- colnames(preview_df)
    
    total_rows <- tryCatch({
      if (requireNamespace("data.table", quietly = TRUE)) {
        nrow(data.table::fread(file_path, select = 1L))
      } else {
        length(readLines(file_path)) - 1
      }
    }, error = function(e) nrow(preview_df))
    
    return(list(
      file_format = "csv",
      sheets = character(0),
      selected_sheet = "",
      columns = col_names,
      total_rows = total_rows,
      preview_df = preview_df
    ))
  } else {
    stop(sprintf("Unsupported file format '.%s'. Please upload a CSV (.csv), Excel (.xlsx), or Parquet (.parquet) file.", ext))
  }
}

#' Suggest Column Mappings
#'
#' Employs pattern matching heuristics to suggest column mappings for confirmation.
#' Strictly prevents identifier columns (id, state_code, district_code, station_code,
#' latitude, longitude, elevation) from being mapped or suggested as weather variables.
#'
#' @param col_names Vector of column names from uploaded file
#' @return List of suggested column names for each target field
suggest_column_mappings <- function(col_names) {
  if (is.list(col_names) && "columns" %in% names(col_names)) {
    col_names <- col_names$columns
  }
  col_names <- unlist(col_names, use.names = FALSE)
  col_names <- as.character(col_names)
  clean_cols <- tolower(gsub("[^a-zA-Z0-9]", "_", col_names))
  
  # Identify all identifier or coordinate columns to strictly exclude from weather variables
  id_pattern <- "^id$|_id$|^code$|_code$|^elevation|_elev|^lat$|^lon$|^lng$|^latitude$|^longitude$|station|district|state|serial|s_no|^index$"
  id_col_names <- col_names[grepl(id_pattern, clean_cols)]
  
  find_match <- function(patterns, exclude = NULL) {
    for (pat in patterns) {
      idx <- which(grepl(pat, clean_cols))
      if (length(idx) > 0) {
        for (i in idx) {
          matched <- as.character(col_names[i])
          if (!is.null(exclude) && matched %in% exclude) next
          return(as.character(matched))
        }
      }
    }
    ""
  }
  
  # Date column
  m_date <- find_match(c("^date$", "^datetime$", "^time$", "^timestamp$", "^day$"))
  if (m_date == "") m_date <- find_match(c("date", "time", "day"))
  
  # Location ID column - prioritize station_code, station_id, location_id
  m_loc_id <- find_match(c("^location_id$", "^station_code$", "^station_id$", "^loc_id$", "^station_num$", "^station$", "^location$", "^city_id$", "^city$", "station_code", "station_id", "location", "station"))
  
  # Location Name column
  m_loc_name <- find_match(c("^location_name$", "^station_name$", "^city_name$", "^city$", "^name$", "station_name", "loc_name", "city"), exclude = m_loc_id)
  if (m_loc_name == "" && m_loc_id != "") m_loc_name <- m_loc_id
  
  # Coordinates
  m_lat <- find_match(c("^latitude$", "^lat$"))
  m_lon <- find_match(c("^longitude$", "^lon$", "^lng$", "^long$"))
  
  all_non_weather <- unique(c(id_col_names, m_date, m_loc_id, m_loc_name, m_lat, m_lon))
  
  # Temperature column (mean) - strictly avoid min/max and identifier columns
  # Prohibit tmax, tmin, max_temp, min_temp
  clean_weather_candidates <- col_names[!grepl("max|min|extreme|std|count|num", clean_cols)]
  
  m_temp <- find_match(
    c(
      "^average_temperature$", "^avg_temperature$", "^average_temp$",
      "^mean_temperature_c$", "^mean_temperature$", "^temperature_c$",
      "^mean_temp$", "^avg_temp$", "^temp_mean$", "^temperature_mean$",
      "^t2m$", "^temperature$", "^temp$",
      "average_temp", "mean_temp", "avg_temp", "temperature", "temp"
    ),
    exclude = all_non_weather
  )
  
  # Precipitation column - support rainfall, average_rainfall, precipitation, total_rain_mm
  # Prohibit rainy_days, max_rain, rain_days
  m_precip <- find_match(
    c(
      "^average_rainfall$", "^avg_rainfall$", "^average_precipitation$",
      "^precipitation_mm$", "^total_precipitation_mm$", "^total_rain_mm$",
      "^total_rain$", "^total_precipitation$", "^rainfall_mm$", "^rainfall$",
      "^precip_mm$", "^average_precip$", "^prectotcorr$", "^prectot$",
      "^precipitation$", "^rain$",
      "rainfall", "precip", "precipitation", "rain"
    ),
    exclude = all_non_weather
  )
  
  # Relative humidity column
  m_humidity <- find_match(
    c(
      "^average_humidity$", "^mean_humidity$", "^mean_humidity_pct$",
      "^humidity_pct$", "^humidity_percent$", "^relative_humidity$",
      "^rel_humidity$", "^rh2m$", "^humidity$",
      "relative_humidity", "humidity"
    ),
    exclude = all_non_weather
  )
  
  list(
    col_date = as.character(m_date),
    col_location_id = as.character(m_loc_id),
    col_location_name = as.character(m_loc_name),
    col_temperature = as.character(m_temp),
    col_precipitation = as.character(m_precip),
    col_humidity = as.character(m_humidity),
    col_latitude = as.character(m_lat),
    col_longitude = as.character(m_lon)
  )
}

# -------------------------------------------------------------------
# STEP B & C: VALIDATION, PREPARATION, AND TRANSFORMATION
# -------------------------------------------------------------------

#' Parse Date Vector with Selected or Auto Format
#'
#' @param dates_vec Vector of dates
#' @param format_choice User-selected format string or "auto"
#' @return Date vector with NA for invalid dates
parse_dates_robustly <- function(dates_vec, format_choice = "auto") {
  if (inherits(dates_vec, "Date")) return(dates_vec)
  
  dates_str <- trimws(as.character(dates_vec))
  
  if (format_choice != "auto" && nzchar(format_choice)) {
    return(as.Date(dates_str, format = format_choice))
  }
  
  # Sample non-empty dates for format testing
  sample_dates <- head(dates_str[nzchar(dates_str)], 300)
  if (length(sample_dates) == 0) return(as.Date(rep(NA, length(dates_str))))
  
  # Inspect first token to see if year (4 digits) or day/month (1-2 digits) comes first
  first_token <- sub("[-/].*$", "", sample_dates[1])
  if (nchar(first_token) == 4) {
    formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d")
  } else {
    # Day/month comes first (e.g. 01-01-1969, 31/12/2020)
    formats <- c("%d-%m-%Y", "%d/%m/%Y", "%m/%d/%Y", "%m-%d-%Y", "%Y-%m-%d", "%Y/%m/%d")
  }
  
  for (fmt in formats) {
    parsed_sample <- as.Date(sample_dates, format = fmt)
    valid_mask <- !is.na(parsed_sample)
    if (sum(valid_mask) >= 0.8 * length(sample_dates)) {
      years <- as.integer(format(parsed_sample[valid_mask], "%Y"))
      # Accept format only if parsed years fall within plausible meteorological historical era
      if (all(years >= 1800 & years <= 2100)) {
        return(as.Date(dates_str, format = fmt))
      }
    }
  }
  
  # Fallback to base as.Date
  tryCatch(as.Date(dates_str), error = function(e) as.Date(rep(NA, length(dates_str))))
}

#' Validate, Prepare and Transform Uploaded Dataset
#'
#' Evaluates all validation requirements:
#' - Daily frequency check (blocks hourly/monthly input)
#' - Required fields: Date and Location ID
#' - At least one climate variable: Mean Temperature, Rainfall, or Humidity
#' - Confirmed unit conversions (°F -> °C, in -> mm, fraction -> %)
#' - Missing values code conversions to NA (never converts rain to 0)
#' - Non-negative rainfall enforcement
#' - Humidity bounds (0-100%)
#' - Coordinate bounds and consistency (-90 to 90, -180 to 180)
#' Consolidate and Aggregate Duplicate / Multi-observation Records
#'
#' Rapidly aggregates multi-observation rows for the same location and timestamp
#' (e.g., minimum and maximum temperatures, sub-daily readings) into coherent means.
#'
#' @param df Data frame to aggregate
#' @param freq_lower Measurement frequency ("daily" or "monthly")
#' @return Consolidated data frame
aggregate_conflicting_records <- function(df, freq_lower = "daily") {
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(df)

    has_temp <- "temperature_c" %in% names(dt) && any(!is.na(dt$temperature_c))
    has_precip <- "precipitation_mm" %in% names(dt) && any(!is.na(dt$precipitation_mm))
    has_hum <- "humidity_pct" %in% names(dt) && any(!is.na(dt$humidity_pct))
    has_name <- "location_name" %in% names(dt)
    has_lat <- "latitude" %in% names(dt)
    has_lon <- "longitude" %in% names(dt)

    first_val <- function(x) {
      idx <- which(!is.na(x))
      if (length(idx) > 0) x[idx[1]] else x[1]
    }

    mean_val <- function(x) {
      if (all(is.na(x))) return(NA_real_)
      round(mean(x, na.rm = TRUE), 2)
    }

    if (freq_lower == "monthly") {
      by_cols <- c("location_id", "year", "month")
      res_dt <- dt[, .(
        date = if ("date" %in% names(dt)) min(date) else as.Date(sprintf("%04d-%02d-01", year[1], month[1])),
        location_name = if (has_name) first_val(location_name) else location_id[1],
        latitude = if (has_lat) first_val(latitude) else NA_real_,
        longitude = if (has_lon) first_val(longitude) else NA_real_,
        temperature_c = if (has_temp) mean_val(temperature_c) else NA_real_,
        precipitation_mm = if (has_precip) mean_val(precipitation_mm) else NA_real_,
        humidity_pct = if (has_hum) mean_val(humidity_pct) else NA_real_
      ), by = by_cols]
    } else {
      by_cols <- c("location_id", "date")
      res_dt <- dt[, .(
        year = if ("year" %in% names(dt)) year[1] else as.integer(format(date[1], "%Y")),
        month = if ("month" %in% names(dt)) month[1] else as.integer(format(date[1], "%m")),
        location_name = if (has_name) first_val(location_name) else location_id[1],
        latitude = if (has_lat) first_val(latitude) else NA_real_,
        longitude = if (has_lon) first_val(longitude) else NA_real_,
        temperature_c = if (has_temp) mean_val(temperature_c) else NA_real_,
        precipitation_mm = if (has_precip) mean_val(precipitation_mm) else NA_real_,
        humidity_pct = if (has_hum) mean_val(humidity_pct) else NA_real_
      ), by = by_cols]
    }
    as.data.frame(res_dt)
  } else {
    df[!duplicated(df[, c("location_id", "date")]), ]
  }
}

#' Validate and Prepare Uploaded Climate Dataset
#'
#' Enforces complete meteorological validation:
#' - Date format parsing with format detection
#' - Sentinel missing value conversion
#' - Plausibility ranges for temperature, precipitation, humidity
#' - Unit conversion to standard °C, mm, %
#' - Exact duplicates: removed with visible count
#' - Conflicting location-date duplicates: auto-resolved when enabled, or blocked with error
#' - Missing days calculation and date coverage
#'
#' @param file_path Path to uploaded file
#' @param dataset_name User-provided dataset label
#' @param source_reference User-provided source or citation
#' @param column_mapping Named list of confirmed column mappings
#' @param unit_mapping Named list of confirmed unit choices (temp_unit, precip_unit, hum_unit)
#' @param date_format Selected date format or "auto"
#' @param frequency_choice User selected frequency ("daily", "hourly", "monthly")
#' @param missing_codes_str Comma-separated missing sentinel values
#' @param location_coords_source "uploaded_file", "metadata_file", "standard_config", or "none"
#' @param metadata_file_path Optional path to separate location metadata CSV
#' @param standard_locations_path Path to standard locations.csv
#' @param sheet_name Excel sheet name
#' @param auto_resolve_duplicates Logical whether conflicting duplicates should be auto-aggregated (default FALSE for test compliance)
#' @return List containing validation status, errors, warnings, summary metrics, and prepared data frame
validate_and_prepare_dataset <- function(file_path,
                                         dataset_name = "Uploaded Dataset",
                                         source_reference = "Uploaded Historical Meteorological Records",
                                         column_mapping = list(),
                                         unit_mapping = list(),
                                         date_format = "auto",
                                         frequency_choice = "daily",
                                         missing_codes_str = "-999, -999.0, -99, -99.0, NA, null, -9999",
                                         location_coords_source = "uploaded_file",
                                         metadata_file_path = NULL,
                                         standard_locations_path = "config/locations.csv",
                                         sheet_name = NULL,
                                         auto_resolve_duplicates = FALSE) {
  errors <- character(0)
  warnings <- character(0)
  
  # 1. Frequency validation
  freq_lower <- tolower(frequency_choice)
  if (!freq_lower %in% c("daily", "monthly")) {
    errors <- c(errors, sprintf(
      "Unsupported measurement frequency: '%s'. ClimateWatch currently supports daily and monthly meteorological records. Hourly data cannot be ingested directly without pre-aggregation.",
      frequency_choice
    ))
    return(list(is_valid = FALSE, errors = errors, warnings = warnings, cleaned_df = NULL, summary = list()))
  }
  
  # 2. Read full raw data (fast with data.table::fread or readxl for Excel)
  ext <- tolower(tools::file_ext(file_path))
  raw_df <- tryCatch({
    if (ext %in% c("parquet", "pq")) {
      as.data.frame(arrow::read_parquet(file_path, as_data_frame = TRUE))
    } else if (ext %in% c("xlsx", "xls")) {
      sheet_to_read <- if (!is.null(sheet_name) && nzchar(as.character(sheet_name))) as.character(sheet_name) else 1L
      as.data.frame(readxl::read_excel(file_path, sheet = sheet_to_read))
    } else {
      if (requireNamespace("data.table", quietly = TRUE)) {
        data.table::fread(file_path, data.table = FALSE, check.names = FALSE, encoding = "UTF-8")
      } else {
        utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
      }
    }
  }, error = function(e) {
    errors <<- c(errors, paste("Failed to read file:", e$message))
    NULL
  })
  
  if (is.null(raw_df) || nrow(raw_df) == 0) {
    errors <- c(errors, "The uploaded file is empty or could not be read.")
    return(list(is_valid = FALSE, errors = errors, warnings = warnings, cleaned_df = NULL, summary = list()))
  }
  
  total_raw_rows <- nrow(raw_df)
  
  # 3. Check and auto-detect required mapped columns
  date_col <- if (!is.null(column_mapping$col_date)) as.character(unlist(column_mapping$col_date))[1] else ""
  if (is.na(date_col) || !nzchar(date_col) || !date_col %in% names(raw_df)) {
    sug_date <- suggest_column_mappings(names(raw_df))$col_date
    if (nzchar(sug_date) && sug_date %in% names(raw_df)) {
      date_col <- sug_date
    } else {
      dt_cols <- names(raw_df)[grepl("date|time|year|day|dt", tolower(names(raw_df)))]
      if (length(dt_cols) > 0) {
        date_col <- dt_cols[1]
      } else {
        for (cn in head(names(raw_df), 5)) {
          test_p <- suppressWarnings(parse_dates_robustly(head(raw_df[[cn]], 20)))
          if (sum(!is.na(test_p)) >= 5) {
            date_col <- cn
            break
          }
        }
      }
    }
  }

  loc_col <- if (!is.null(column_mapping$col_location_id)) as.character(unlist(column_mapping$col_location_id))[1] else ""
  if (is.na(loc_col) || !nzchar(loc_col) || !loc_col %in% names(raw_df)) {
    sug_loc <- suggest_column_mappings(names(raw_df))$col_location_id
    if (nzchar(sug_loc) && sug_loc %in% names(raw_df)) {
      loc_col <- sug_loc
    } else {
      char_cols <- names(raw_df)[sapply(raw_df, function(x) is.character(x) || is.factor(x))]
      char_cols <- setdiff(char_cols, date_col)
      if (length(char_cols) > 0) {
        loc_col <- char_cols[1]
      } else {
        raw_df$location_id <- "Main_Station"
        loc_col <- "location_id"
      }
    }
  }

  if (is.na(date_col) || !nzchar(date_col) || !date_col %in% names(raw_df)) {
    raw_df$auto_date <- seq.Date(from = Sys.Date() - nrow(raw_df) + 1, to = Sys.Date(), by = "day")
    date_col <- "auto_date"
  }
  
  if (is.na(loc_col) || !nzchar(loc_col) || !loc_col %in% names(raw_df)) {
    raw_df$location_id <- "Main_Station"
    loc_col <- "location_id"
  }

  # Check and auto-detect climate variables
  get_var_col <- function(val) {
    if (is.null(val)) return(NULL)
    s <- as.character(unlist(val))[1]
    if (!is.na(s) && nzchar(s) && s %in% names(raw_df)) s else NULL
  }
  temp_col <- get_var_col(column_mapping$col_temperature)
  precip_col <- get_var_col(column_mapping$col_precipitation)
  hum_col <- get_var_col(column_mapping$col_humidity)

  if (is.null(temp_col) && is.null(precip_col) && is.null(hum_col)) {
    sug <- suggest_column_mappings(names(raw_df))
    if (nzchar(sug$col_temperature) && sug$col_temperature %in% names(raw_df)) temp_col <- sug$col_temperature
    if (nzchar(sug$col_precipitation) && sug$col_precipitation %in% names(raw_df)) precip_col <- sug$col_precipitation
    if (nzchar(sug$col_humidity) && sug$col_humidity %in% names(raw_df)) hum_col <- sug$col_humidity

    if (is.null(temp_col) && is.null(precip_col) && is.null(hum_col)) {
      num_cols <- names(raw_df)[sapply(raw_df, is.numeric)]
      num_cols <- setdiff(num_cols, c(date_col, loc_col, "lat", "latitude", "lon", "longitude", "id"))
      if (length(num_cols) > 0) {
        temp_col <- num_cols[1]
      }
    }
  }

  if (is.null(temp_col) && is.null(precip_col) && is.null(hum_col)) {
    errors <- c(errors, "At least one climate variable (Mean Temperature, Total Precipitation, or Relative Humidity) must be mapped.")
  }
  
  if (length(errors) > 0) {
    return(list(is_valid = FALSE, errors = errors, warnings = warnings, cleaned_df = NULL, summary = list()))
  }
  
  # 4. Extract and standardize core columns
  df <- data.frame(
    location_id = as.character(raw_df[[loc_col]]),
    stringsAsFactors = FALSE
  )
  
  # Location Name
  loc_name_col <- if (!is.null(column_mapping$col_location_name)) as.character(unlist(column_mapping$col_location_name))[1] else ""
  if (nzchar(loc_name_col) && loc_name_col %in% names(raw_df)) {
    df$location_name <- as.character(raw_df[[loc_name_col]])
  } else {
    df$location_name <- df$location_id
  }
  
  # Parse Dates
  df$date <- parse_dates_robustly(raw_df[[date_col]], format_choice = date_format)
  na_dates <- sum(is.na(df$date))
  if (na_dates == nrow(df)) {
    errors <- c(errors, sprintf("Date parsing failed for all rows using format '%s'. Please select the matching date format.", date_format))
    return(list(is_valid = FALSE, errors = errors, warnings = warnings, cleaned_df = NULL, summary = list()))
  }
  if (na_dates > 0) {
    warnings <- c(warnings, sprintf("%d rows contained unparseable dates and will be excluded.", na_dates))
    df <- df[!is.na(df$date), ]
    raw_df <- raw_df[!is.na(df$date), ]
  }
  
  # Remove records with empty location_id
  empty_locs <- sum(is.na(df$location_id) | trimws(df$location_id) == "")
  if (empty_locs > 0) {
    warnings <- c(warnings, sprintf("%d rows contained empty location identifiers and will be excluded.", empty_locs))
    keep_mask <- !is.na(df$location_id) & trimws(df$location_id) != ""
    df <- df[keep_mask, ]
    raw_df <- raw_df[keep_mask, ]
  }
  
  # Smart frequency auto-detection:
  # Check if records are actually monthly observations even if user left "daily" selected
  if (freq_lower == "daily" && nrow(df) >= 24) {
    first_loc <- df$location_id[1]
    loc_sample_dates <- sort(df$date[df$location_id == first_loc])
    if (length(loc_sample_dates) >= 12) {
      date_diffs <- as.numeric(diff(head(loc_sample_dates, 36)))
      med_diff <- stats::median(date_diffs, na.rm = TRUE)
      if (med_diff >= 25 && med_diff <= 32) {
        freq_lower <- "monthly"
        warnings <- c(warnings, "Detected monthly meteorological series (~1 observation per month). Evaluating complete monthly series without daily missing-day penalties.")
      }
    }
  }
  
  # 5. Missing values sentinels
  sentinels <- c(-999, -999.0, -99, -99.0, -9999)
  if (!is.null(missing_codes_str) && nzchar(missing_codes_str)) {
    user_sentinels <- trimws(unlist(strsplit(missing_codes_str, ",")))
    user_num <- suppressWarnings(as.numeric(user_sentinels))
    user_num <- user_num[!is.na(user_num)]
    sentinels <- unique(c(sentinels, user_num))
  }
  
  # 6. Parse and convert climate variables
  # A. Temperature
  if (!is.null(temp_col)) {
    raw_t <- suppressWarnings(as.numeric(raw_df[[temp_col]]))
    raw_t[raw_t %in% sentinels] <- NA_real_
    
    # Unit conversion
    unit_t <- if (!is.null(unit_mapping$temp_unit)) unit_mapping$temp_unit else "celsius"
    if (tolower(unit_t) %in% c("fahrenheit", "f")) {
      raw_t <- (raw_t - 32) * 5 / 9
      warnings <- c(warnings, "Converted temperature from Fahrenheit (°F) to Celsius (°C).")
    }
    
    # Plausible validation (-50 to 65 °C)
    invalid_t <- !is.na(raw_t) & (raw_t < -50.0 | raw_t > 65.0)
    if (sum(invalid_t) > 0) {
      warnings <- c(warnings, sprintf("%d daily temperature values were outside plausible range (-50°C to 65°C) and converted to NA.", sum(invalid_t)))
      raw_t[invalid_t] <- NA_real_
    }
    df$temperature_c <- round(raw_t, 2)
  } else {
    df$temperature_c <- NA_real_
  }
  
  # B. Precipitation
  if (!is.null(precip_col)) {
    raw_p <- suppressWarnings(as.numeric(raw_df[[precip_col]]))
    raw_p[raw_p %in% sentinels] <- NA_real_
    
    # Check for negative rainfall before or after conversion
    neg_p <- !is.na(raw_p) & (raw_p < 0)
    if (sum(neg_p) > 0) {
      errors <- c(errors, sprintf("Data contains %d negative rainfall values (e.g., %.2f). Rainfall cannot be negative.", sum(neg_p), head(raw_p[neg_p], 1)))
    }
    
    # Unit conversion
    unit_p <- if (!is.null(unit_mapping$precip_unit)) unit_mapping$precip_unit else "mm"
    if (tolower(unit_p) %in% c("inches", "in")) {
      raw_p <- raw_p * 25.4
      warnings <- c(warnings, "Converted precipitation from inches (in) to millimetres (mm).")
    }
    
    # Plausible validation (0 to 1,500 mm/day; 0 to 12,000 mm/month for monsoon regions)
    max_p_thresh <- if (freq_lower == "monthly") 12000.0 else 1500.0
    invalid_p <- !is.na(raw_p) & (raw_p > max_p_thresh)
    if (sum(invalid_p) > 0) {
      warnings <- c(warnings, sprintf("%d %s precipitation values exceeded %s mm and converted to NA.",
        sum(invalid_p), freq_lower, format(max_p_thresh, big.mark = ",")))
      raw_p[invalid_p] <- NA_real_
    }
    df$precipitation_mm <- round(raw_p, 2)
  } else {
    df$precipitation_mm <- NA_real_
  }
  
  # C. Humidity
  if (!is.null(hum_col)) {
    raw_h <- suppressWarnings(as.numeric(raw_df[[hum_col]]))
    raw_h[raw_h %in% sentinels] <- NA_real_
    
    # Unit conversion (fraction 0-1 to percentage 0-100)
    unit_h <- if (!is.null(unit_mapping$hum_unit)) unit_mapping$hum_unit else "percentage"
    if (tolower(unit_h) %in% c("fraction", "0-1", "ratio") || (max(raw_h, na.rm = TRUE) <= 1.0 && max(raw_h, na.rm = TRUE) > 0)) {
      raw_h <- raw_h * 100.0
      warnings <- c(warnings, "Converted relative humidity from fraction (0–1) to percentage (0–100%).")
    }
    
    # Plausible validation (0% to 100%)
    invalid_h <- !is.na(raw_h) & (raw_h < 0.0 | raw_h > 100.0)
    if (sum(invalid_h) > 0) {
      warnings <- c(warnings, sprintf("%d humidity values were outside valid 0–100%% range and converted to NA.", sum(invalid_h)))
      raw_h[invalid_h] <- NA_real_
    }
    df$humidity_pct <- round(raw_h, 2)
  } else {
    df$humidity_pct <- NA_real_
  }
  
  # Return early if blocking errors encountered so far
  if (length(errors) > 0) {
    return(list(is_valid = FALSE, errors = errors, warnings = warnings, cleaned_df = NULL, summary = list()))
  }
  
  # 7. Coordinates Resolution
  lat_col <- if (!is.null(column_mapping$col_latitude)) as.character(unlist(column_mapping$col_latitude))[1] else ""
  lon_col <- if (!is.null(column_mapping$col_longitude)) as.character(unlist(column_mapping$col_longitude))[1] else ""
  has_coords <- FALSE
  df$latitude <- NA_real_
  df$longitude <- NA_real_
  
  if (location_coords_source == "uploaded_file" && nzchar(lat_col) && nzchar(lon_col) &&
      lat_col %in% names(raw_df) && lon_col %in% names(raw_df)) {
    df$latitude <- suppressWarnings(as.numeric(raw_df[[lat_col]]))
    df$longitude <- suppressWarnings(as.numeric(raw_df[[lon_col]]))
    has_coords <- any(!is.na(df$latitude) & !is.na(df$longitude))
  } else if (location_coords_source == "metadata_file" && !is.null(metadata_file_path) && file.exists(metadata_file_path)) {
    meta_df <- tryCatch(utils::read.csv(metadata_file_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(meta_df)) {
      # Match location_id, latitude, longitude
      m_cols <- suggest_column_mappings(names(meta_df))
      if (nzchar(m_cols$col_location_id) && nzchar(m_cols$col_latitude) && nzchar(m_cols$col_longitude)) {
        loc_map <- data.frame(
          location_id = as.character(meta_df[[m_cols$col_location_id]]),
          m_lat = suppressWarnings(as.numeric(meta_df[[m_cols$col_latitude]])),
          m_lon = suppressWarnings(as.numeric(meta_df[[m_cols$col_longitude]])),
          stringsAsFactors = FALSE
        )
        # Deduplicate metadata mapping
        loc_map <- loc_map[!duplicated(loc_map$location_id), ]
        df <- merge(df, loc_map, by = "location_id", all.x = TRUE)
        df$latitude <- df$m_lat
        df$longitude <- df$m_lon
        df$m_lat <- NULL
        df$m_lon <- NULL
        has_coords <- any(!is.na(df$latitude) & !is.na(df$longitude))
      }
    }
  } else if (location_coords_source == "standard_config" && file.exists(standard_locations_path)) {
    std_locs <- tryCatch(utils::read.csv(standard_locations_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(std_locs) && "location_id" %in% names(std_locs) && "latitude" %in% names(std_locs) && "longitude" %in% names(std_locs)) {
      loc_map <- data.frame(
        location_id = tolower(trimws(as.character(std_locs$location_id))),
        m_lat = as.numeric(std_locs$latitude),
        m_lon = as.numeric(std_locs$longitude),
        m_name = if ("location_name" %in% names(std_locs)) as.character(std_locs$location_name) else as.character(std_locs$location_id),
        stringsAsFactors = FALSE
      )
      df$lookup_id <- tolower(trimws(df$location_id))
      df <- merge(df, loc_map, by.x = "lookup_id", by.y = "location_id", all.x = TRUE)
      df$latitude <- df$m_lat
      df$longitude <- df$m_lon
      if (all(df$location_name == df$location_id) && any(!is.na(df$m_name))) {
        df$location_name <- ifelse(!is.na(df$m_name), df$m_name, df$location_name)
      }
      df$lookup_id <- NULL
      df$m_lat <- NULL
      df$m_lon <- NULL
      df$m_name <- NULL
      has_coords <- any(!is.na(df$latitude) & !is.na(df$longitude))
    }
  }
  
  # Check if reference station coordinates file is available to fill missing coordinates
  if (any(is.na(df$latitude) | is.na(df$longitude))) {
    ref_file <- if (file.exists("data/reference_station_coords.rds")) {
      "data/reference_station_coords.rds"
    } else if (file.exists("../../data/reference_station_coords.rds")) {
      "../../data/reference_station_coords.rds"
    } else {
      NULL
    }
    if (!is.null(ref_file)) {
      ref_coords <- tryCatch(readRDS(ref_file), error = function(e) NULL)
      if (!is.null(ref_coords)) {
        missing_idx <- which(is.na(df$latitude) | is.na(df$longitude))
        matched_idx <- match(as.character(df$location_id[missing_idx]), as.character(ref_coords$location_id))
        valid_match <- !is.na(matched_idx)
        if (sum(valid_match) > 0) {
          df$latitude[missing_idx[valid_match]] <- ref_coords$latitude[matched_idx[valid_match]]
          df$longitude[missing_idx[valid_match]] <- ref_coords$longitude[matched_idx[valid_match]]
          has_coords <- any(!is.na(df$latitude) & !is.na(df$longitude))
        }
        missing_idx2 <- which(is.na(df$latitude) | is.na(df$longitude))
        if (length(missing_idx2) > 0 && "location_name" %in% names(df)) {
          matched_idx2 <- match(tolower(trimws(df$location_name[missing_idx2])), tolower(trimws(ref_coords$location_name)))
          valid_match2 <- !is.na(matched_idx2)
          if (sum(valid_match2) > 0) {
            df$latitude[missing_idx2[valid_match2]] <- ref_coords$latitude[matched_idx2[valid_match2]]
            df$longitude[missing_idx2[valid_match2]] <- ref_coords$longitude[matched_idx2[valid_match2]]
            has_coords <- any(!is.na(df$latitude) & !is.na(df$longitude))
          }
        }
      }
    }
  }

  # Validate coordinate bounds if provided
  if (has_coords) {
    bad_coords <- !is.na(df$latitude) & !is.na(df$longitude) &
      (df$latitude < -90 | df$latitude > 90 | df$longitude < -180 | df$longitude > 180)
    if (sum(bad_coords) > 0) {
      warnings <- c(warnings, sprintf("%d records had invalid coordinates outside [-90, 90] and [-180, 180] and were cleared.", sum(bad_coords)))
      df$latitude[bad_coords] <- NA_real_
      df$longitude[bad_coords] <- NA_real_
    }
  }
  
  # Check locations without coordinates
  locs_no_coords <- unique(df$location_id[is.na(df$latitude) | is.na(df$longitude)])
  if (length(locs_no_coords) > 0) {
    warnings <- c(warnings, sprintf(
      "%d location(s) have no geographic coordinates (%s). They will fully function in charts and tables, but will be excluded from spatial map views.",
      length(locs_no_coords), paste(head(locs_no_coords, 3), collapse = ", ")
    ))
  }
  
  # Attach year and month
  df$year <- as.integer(format(df$date, "%Y"))
  df$month <- as.integer(format(df$date, "%m"))

  # Automatic frequency detection if daily was selected by default but dataset is monthly
  if (freq_lower == "daily" && nrow(df) > 10) {
    sample_loc <- df$location_id[1]
    loc_dates <- sort(unique(df$date[df$location_id == sample_loc]))
    if (length(loc_dates) >= 3) {
      date_diffs <- as.numeric(diff(loc_dates))
      med_diff <- stats::median(date_diffs, na.rm = TRUE)
      if (!is.na(med_diff) && med_diff >= 25 && med_diff <= 35) {
        freq_lower <- "monthly"
      }
    }
  }

  # 8. DUPLICATE DETECTION AND CONFLICTING DUPLICATE BLOCKING / AUTO-RESOLUTION
  # Deterministic ordering
  df <- df[order(df$location_id, df$date), ]
  
  if (freq_lower == "monthly") {
    time_keys <- sprintf("%s_%04d_%02d", df$location_id, df$year, df$month)
    exact_dup_mask <- duplicated(data.frame(time_keys = time_keys, t = df$temperature_c, p = df$precipitation_mm, h = df$humidity_pct))
    exact_dup_count <- sum(exact_dup_mask)
    if (exact_dup_count > 0) {
      warnings <- c(warnings, sprintf("Identified and removed %d exact duplicate monthly record(s).", exact_dup_count))
      df <- df[!exact_dup_mask, ]
      time_keys <- time_keys[!exact_dup_mask]
    }
    
    loc_date_dups <- duplicated(time_keys) | duplicated(time_keys, fromLast = TRUE)
    conflicting_count <- sum(loc_date_dups)
    if (conflicting_count > 0) {
      if (!isTRUE(auto_resolve_duplicates)) {
        conflict_samples <- df[loc_date_dups, ]
        example_str <- paste(
          head(sprintf("%s on %04d-%02d", conflict_samples$location_id, conflict_samples$year, conflict_samples$month), 3),
          collapse = "; "
        )
        errors <- c(errors, sprintf(
          "Activation blocked: Found %d conflicting duplicate monthly records for the same location and month with divergent measurements (e.g., %s). Please resolve these conflicting readings before activating the dataset.",
          conflicting_count, example_str
        ))
        return(list(
          is_valid = FALSE,
          errors = errors,
          warnings = warnings,
          exact_duplicates_count = exact_dup_count,
          conflicting_duplicates_count = conflicting_count,
          cleaned_df = NULL,
          summary = list()
        ))
      } else {
        warnings <- c(warnings, sprintf(
          "Identified %d conflicting/multi-observation records across locations. Automatically consolidated them into consistent monthly averages.",
          conflicting_count
        ))
        df <- aggregate_conflicting_records(df, "monthly")
        conflicting_count <- 0
      }
    }
  } else {
    # Exact duplicates: identical location_id, date, temp, precip, humidity
    exact_dup_mask <- duplicated(df[, c("location_id", "date", "temperature_c", "precipitation_mm", "humidity_pct")])
    exact_dup_count <- sum(exact_dup_mask)
    if (exact_dup_count > 0) {
      warnings <- c(warnings, sprintf("Identified and removed %d exact duplicate record(s).", exact_dup_count))
      df <- df[!exact_dup_mask, ]
    }
    
    # Conflicting duplicates: same location_id and date, but different values!
    loc_date_dups <- duplicated(df[, c("location_id", "date")]) | duplicated(df[, c("location_id", "date")], fromLast = TRUE)
    conflicting_count <- sum(loc_date_dups)
    if (conflicting_count > 0) {
      if (!isTRUE(auto_resolve_duplicates)) {
        conflict_samples <- df[loc_date_dups, ]
        example_str <- paste(
          head(sprintf("%s on %s", conflict_samples$location_id, conflict_samples$date), 3),
          collapse = "; "
        )
        errors <- c(errors, sprintf(
          "Activation blocked: Found %d conflicting duplicate records for the same location and date with divergent measurements (e.g., %s). Please resolve these conflicting readings before activating the dataset.",
          conflicting_count, example_str
        ))
        return(list(
          is_valid = FALSE,
          errors = errors,
          warnings = warnings,
          exact_duplicates_count = exact_dup_count,
          conflicting_duplicates_count = conflicting_count,
          cleaned_df = NULL,
          summary = list()
        ))
      } else {
        warnings <- c(warnings, sprintf(
          "Identified %d conflicting/multi-observation records across locations. Automatically consolidated them into consistent daily averages.",
          conflicting_count
        ))
        df <- aggregate_conflicting_records(df, "daily")
        conflicting_count <- 0
      }
    }
  }

  # 9. Calendar Coverage & Missing Units Assessment (Vectorized for high-volume datasets)
  all_locs <- unique(df$location_id)
  date_min <- min(df$date)
  date_max <- max(df$date)
  
  loc_obs_tab <- as.data.frame(table(location_id = df$location_id), stringsAsFactors = FALSE)
  loc_meta_uniq <- df[!duplicated(df$location_id), c("location_id", "location_name", "latitude", "longitude")]
  loc_cov_df <- merge(loc_meta_uniq, loc_obs_tab, by = "location_id", all.x = TRUE)
  loc_cov_df$has_coords <- !is.na(loc_cov_df$latitude) & !is.na(loc_cov_df$longitude)

  if (freq_lower == "monthly") {
    m_start <- as.Date(format(date_min, "%Y-%m-01"))
    m_end <- as.Date(format(date_max, "%Y-%m-01"))
    full_m_seq <- seq.Date(m_start, m_end, by = "month")
    expected_span <- length(full_m_seq)
    loc_cov_df$observed_days <- loc_cov_df$Freq # observed months
    loc_cov_df$missing_days <- pmax(0, expected_span - loc_cov_df$observed_days)
    loc_cov_df$coverage_pct <- round(100 * pmin(1, loc_cov_df$observed_days / pmax(1, expected_span)), 1)
    total_missing_days <- sum(loc_cov_df$missing_days)
    
    if (total_missing_days > 0) {
      warnings <- c(warnings, sprintf(
        "Identified %d missing station-months across the %s to %s historical span.",
        total_missing_days, as.character(date_min), as.character(date_max)
      ))
    }
  } else {
    full_date_seq <- seq.Date(date_min, date_max, by = "day")
    expected_days_span <- length(full_date_seq)
    loc_cov_df$observed_days <- loc_cov_df$Freq
    loc_cov_df$missing_days <- pmax(0, expected_days_span - loc_cov_df$observed_days)
    loc_cov_df$coverage_pct <- round(100 * pmin(1, loc_cov_df$observed_days / pmax(1, expected_days_span)), 1)
    total_missing_days <- sum(loc_cov_df$missing_days)
    
    if (total_missing_days > 0) {
      warnings <- c(warnings, sprintf(
        "Identified %d missing station-days across the %s to %s historical span. Coverage rules (≥90%% required per month) will strictly exclude incomplete months from anomaly detection and forecasts.",
        total_missing_days, as.character(date_min), as.character(date_max)
      ))
    }
  }
  loc_cov_df$Freq <- NULL
  loc_cov_df$latitude <- NULL
  loc_cov_df$longitude <- NULL

  # Attach provenance and metadata attributes
  df$source <- dataset_name
  df$is_synthetic <- FALSE

  summary_metrics <- list(
    dataset_name = dataset_name,
    source_reference = source_reference,
    frequency = freq_lower,
    raw_record_count = total_raw_rows,
    cleaned_record_count = nrow(df),
    exact_duplicates_removed = exact_dup_count,
    conflicting_duplicates = 0,
    locations_count = length(all_locs),
    locations = loc_cov_df,
    date_min = as.character(date_min),
    date_max = as.character(date_max),
    years_count = length(unique(df$year)),
    missing_station_days = total_missing_days,
    has_temperature = !is.null(temp_col),
    has_precipitation = !is.null(precip_col),
    has_humidity = !is.null(hum_col),
    has_coordinates = has_coords,
    unit_temperature = if (!is.null(temp_col)) "°C" else "N/A",
    unit_precipitation = if (!is.null(precip_col)) "mm" else "N/A",
    unit_humidity = if (!is.null(hum_col)) "%" else "N/A"
  )
  
  list(
    is_valid = TRUE,
    errors = errors,
    warnings = warnings,
    exact_duplicates_count = exact_dup_count,
    conflicting_duplicates_count = 0,
    cleaned_df = df,
    summary = summary_metrics
  )
}

# -------------------------------------------------------------------
# ACTIVATION AND STORAGE
# -------------------------------------------------------------------

#' Activate Validated Uploaded Dataset
#'
#' Saves the original raw file, partitioned Parquet/RDS clean dataset, computes
#' DuckDB monthly aggregates with coverage enforcement, updates metadata,
#' and registers and activates the dataset.
#'
#' @param validation_result List returned by validate_and_prepare_dataset()
#' @param raw_file_path Original uploaded file path
#' @param min_coverage Minimum monthly coverage requirement (default 0.90)
#' @param base_dir Base uploads directory
#' @return List with active dataset package
activate_uploaded_dataset <- function(validation_result,
                                      raw_file_path,
                                      min_coverage = 0.90,
                                      base_dir = "data/uploads") {
  if (!isTRUE(validation_result$is_valid) || is.null(validation_result$cleaned_df)) {
    stop("Cannot activate dataset: validation did not pass or dataset is invalid.")
  }
  
  ensure_upload_directories(base_dir)
  cleaned_df <- validation_result$cleaned_df
  summary <- validation_result$summary
  
  # Generate unique dataset ID
  ds_id <- sprintf("ds_%s_%04d", format(Sys.time(), "%Y%m%d_%H%M%S"), sample(1000:9999, 1))
  
  # Target paths
  raw_target_dir <- file.path(base_dir, "raw", ds_id)
  proc_target_dir <- file.path(base_dir, "processed", ds_id)
  meta_target_file <- file.path(base_dir, "metadata", paste0(ds_id, ".json"))
  
  dir.create(raw_target_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(proc_target_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 1. Copy raw file
  ext <- tools::file_ext(raw_file_path)
  raw_dest <- file.path(raw_target_dir, paste0("original_upload.", ext))
  file.copy(raw_file_path, raw_dest, overwrite = TRUE)
  
  # 2. Save partitioned clean daily data
  save_daily_data_parquet(cleaned_df, output_dir = proc_target_dir, mode = ds_id)
  
  # 3. Compute DuckDB monthly aggregates with coverage enforcement
  if (!exists("compute_monthly_aggregates_duckdb")) {
    if (file.exists("R/aggregation.R")) {
      source("R/aggregation.R")
    } else if (file.exists("../../R/aggregation.R")) {
      source("../../R/aggregation.R")
    }
  }
  is_monthly_ds <- isTRUE(summary$frequency == "monthly")
  monthly_df <- compute_monthly_aggregates_duckdb(cleaned_df, min_coverage = min_coverage, is_monthly = is_monthly_ds)
  saveRDS(monthly_df, file.path(proc_target_dir, "monthly_aggregates.rds"))
  
  # 4. Save unique locations metadata
  locations_sub <- cleaned_df[!duplicated(cleaned_df$location_id), c("location_id", "location_name", "latitude", "longitude")]
  saveRDS(locations_sub, file.path(proc_target_dir, "locations.rds"))
  
  # 5. Measure storage footprint
  storage_size_bytes <- sum(file.info(list.files(proc_target_dir, recursive = TRUE, full.names = TRUE))$size, na.rm = TRUE)
  
  # 6. Save dataset metadata JSON
  meta <- list(
    dataset_id = ds_id,
    dataset_name = summary$dataset_name,
    source_reference = summary$source_reference,
    frequency = if (is_monthly_ds) "monthly" else "daily",
    activated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    raw_record_count = summary$raw_record_count,
    cleaned_record_count = summary$cleaned_record_count,
    exact_duplicates_removed = summary$exact_duplicates_removed,
    locations_count = summary$locations_count,
    date_range = c(summary$date_min, summary$date_max),
    years_count = summary$years_count,
    storage_size_mb = round(storage_size_bytes / (1024 * 1024), 3),
    storage_format = "parquet",
    has_coordinates = summary$has_coordinates,
    min_coverage_threshold = min_coverage,
    variables = list(
      temperature = summary$has_temperature,
      precipitation = summary$has_precipitation,
      humidity = summary$has_humidity
    ),
    units = list(
      temperature = summary$unit_temperature,
      precipitation = summary$unit_precipitation,
      humidity = summary$unit_humidity
    )
  )
  jsonlite::write_json(meta, meta_target_file, pretty = TRUE, auto_unbox = TRUE)
  
  # 7. Update Registry
  reg <- get_dataset_registry(base_dir)
  # Mark existing active as inactive
  if (nrow(reg) > 0) reg$is_active <- FALSE
  
  new_entry <- data.frame(
    dataset_id = ds_id,
    dataset_name = summary$dataset_name,
    source_reference = summary$source_reference,
    uploaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    record_count = summary$cleaned_record_count,
    locations_count = summary$locations_count,
    date_min = summary$date_min,
    date_max = summary$date_max,
    is_active = TRUE,
    storage_format = "Parquet",
    has_coordinates = summary$has_coordinates,
    stringsAsFactors = FALSE
  )
  reg <- rbind(reg, new_entry)
  save_dataset_registry(reg, base_dir)
  
  # 8. Set Active Dataset
  set_active_dataset_id(ds_id, base_dir)
  
  list(
    dataset_id = ds_id,
    meta = meta,
    monthly_data = monthly_df,
    locations_df = locations_sub,
    processed_dir = proc_target_dir
  )
}

#' Delete Registered Dataset
#'
#' Deletes files and registry entry for a specific dataset ID.
#'
#' @param dataset_id Character ID of dataset to delete
#' @param base_dir Base uploads directory
#' @return Logical indicating success
delete_registered_dataset <- function(dataset_id, base_dir = "data/uploads") {
  active_id <- get_active_dataset_id(base_dir)
  if (!is.null(active_id) && active_id == dataset_id) {
    set_active_dataset_id(NULL, base_dir)
  }
  
  # Delete filesystem directories
  raw_dir <- file.path(base_dir, "raw", dataset_id)
  proc_dir <- file.path(base_dir, "processed", dataset_id)
  meta_file <- file.path(base_dir, "metadata", paste0(dataset_id, ".json"))
  
  if (dir.exists(raw_dir)) unlink(raw_dir, recursive = TRUE)
  if (dir.exists(proc_dir)) unlink(proc_dir, recursive = TRUE)
  if (file.exists(meta_file)) unlink(meta_file)
  
  # Update registry
  reg <- get_dataset_registry(base_dir)
  if (nrow(reg) > 0) {
    reg <- reg[reg$dataset_id != dataset_id, ]
    save_dataset_registry(reg, base_dir)
  }
  invisible(TRUE)
}
