# R/storage.R - Parquet Partitioned Storage for ClimateWatch

#' Save Cleaned Daily Data Partitioned by Year
#'
#' Stores daily meteorological records in Parquet format partitioned by year.
#' Also saves summary metadata in data/metadata.
#'
#' @param df Cleaned daily data frame
#' @param output_dir Target directory (e.g., data/processed/demo or data/processed/real)
#' @param mode Character label, "demo" or "real"
#' @return Logical indicating success
save_daily_data_parquet <- function(df, output_dir, mode = "demo") {
  if (is.null(df) || nrow(df) == 0) {
    warning("Attempted to save empty data frame to Parquet.")
    return(FALSE)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Add year column for partitioning if not already present
  df$year <- as.integer(format(as.Date(df$date), "%Y"))

  # Save via arrow dataset if arrow is available
  if (requireNamespace("arrow", quietly = TRUE)) {
    # Delete previous files in directory to ensure clean partitioned write
    tryCatch({
      arrow::write_dataset(
        dataset = df,
        path = output_dir,
        format = "parquet",
        partitioning = "year",
        existing_data_behavior = "overwrite"
      )
    }, error = function(e) {
      warning("arrow::write_dataset error: ", e$message, ". Using individual year parquet files.")
      years <- unique(df$year)
      for (yr in years) {
        yr_dir <- file.path(output_dir, paste0("year=", yr))
        if (!dir.exists(yr_dir)) dir.create(yr_dir, recursive = TRUE, showWarnings = FALSE)
        yr_file <- file.path(yr_dir, "part-0.parquet")
        arrow::write_parquet(df[df$year == yr, ], yr_file)
      }
    })
  } else {
    # Fallback to RDS partitions if arrow is unavailable
    years <- unique(df$year)
    for (yr in years) {
      yr_dir <- file.path(output_dir, paste0("year=", yr))
      if (!dir.exists(yr_dir)) dir.create(yr_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(df[df$year == yr, ], file.path(yr_dir, "data.rds"))
    }
  }

  # Write metadata summary
  meta_dir <- "data/metadata"
  if (!dir.exists(meta_dir)) dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)
  meta_file <- file.path(meta_dir, paste0("storage_summary_", mode, ".json"))

  # Measure storage size
  dir_size_bytes <- sum(file.info(list.files(output_dir, recursive = TRUE, full.names = TRUE))$size, na.rm = TRUE)

  meta <- list(
    mode = mode,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    record_count = nrow(df),
    locations_count = length(unique(df$location_id)),
    date_range = c(as.character(min(df$date)), as.character(max(df$date))),
    years = sort(unique(df$year)),
    storage_format = "parquet",
    partition_column = "year",
    storage_size_bytes = dir_size_bytes,
    storage_size_mb = round(dir_size_bytes / (1024 * 1024), 2),
    is_synthetic = any(df$is_synthetic)
  )

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(meta, meta_file, pretty = TRUE, auto_unbox = TRUE)
  }

  invisible(TRUE)
}

#' Load Daily Data from Parquet Dataset
#'
#' Reads partitioned Parquet data into a data frame with optional filters.
#'
#' @param input_dir Source directory
#' @param location_ids Optional character vector of location IDs to filter
#' @param start_year Optional integer start year
#' @param end_year Optional integer end year
#' @return Data frame of daily records
load_daily_data_parquet <- function(input_dir, location_ids = NULL, start_year = NULL, end_year = NULL) {
  if (!dir.exists(input_dir)) {
    warning("Directory does not exist: ", input_dir)
    return(data.frame())
  }

  parquet_files <- list.files(input_dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  if (length(parquet_files) > 0) {
    if (!is.null(start_year) || !is.null(end_year)) {
      file_years <- as.integer(sub(".*year=(\\d+).*", "\\1", parquet_files))
      valid_idx <- rep(TRUE, length(parquet_files))
      if (!is.null(start_year)) valid_idx <- valid_idx & (!is.na(file_years) & file_years >= as.integer(start_year))
      if (!is.null(end_year)) valid_idx <- valid_idx & (!is.na(file_years) & file_years <= as.integer(end_year))
      parquet_files <- parquet_files[valid_idx]
    }
    
    if (length(parquet_files) == 0) return(data.frame())

    dfs <- lapply(parquet_files, function(f) {
      d <- tryCatch(arrow::read_parquet(f), error = function(e) NULL)
      if (is.null(d) || nrow(d) == 0) return(NULL)
      if (!("year" %in% names(d))) {
        y_match <- sub(".*year=(\\d+).*", "\\1", f)
        if (grepl("^\\d+$", y_match)) d$year <- as.integer(y_match)
      }
      if (!is.null(location_ids) && length(location_ids) > 0 && "location_id" %in% names(d)) {
        d <- d[as.character(d$location_id) %in% as.character(location_ids), ]
      }
      d
    })
    
    dfs <- dfs[!sapply(dfs, is.null)]
    if (length(dfs) == 0) return(data.frame())

    combined <- if (requireNamespace("dplyr", quietly = TRUE)) {
      dplyr::bind_rows(dfs)
    } else {
      do.call(rbind, dfs)
    }

    if ("date" %in% names(combined)) combined$date <- as.Date(combined$date)
    return(as.data.frame(combined))
  }

  # Fallback to RDS files if parquet not present
  rds_files <- list.files(input_dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  rds_files <- grep("locations\\.rds|monthly_aggregates\\.rds", rds_files, value = TRUE, invert = TRUE)
  if (length(rds_files) > 0) {
    dfs <- lapply(rds_files, function(f) tryCatch(readRDS(f), error = function(e) NULL))
    dfs <- dfs[!sapply(dfs, is.null)]
    if (length(dfs) == 0) return(data.frame())
    combined <- dplyr::bind_rows(dfs)
    if (!is.null(location_ids) && length(location_ids) > 0 && "location_id" %in% names(combined)) {
      combined <- combined[as.character(combined$location_id) %in% as.character(location_ids), ]
    }
    if ("date" %in% names(combined)) combined$date <- as.Date(combined$date)
    return(as.data.frame(combined))
  }

  data.frame()
}
