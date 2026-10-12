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

  # If arrow is available
  if (requireNamespace("arrow", quietly = TRUE)) {
    tryCatch({
      ds <- arrow::open_dataset(input_dir, format = "parquet")
      
      # Build expression
      res <- ds
      if (!is.null(location_ids) && length(location_ids) > 0) {
        res <- dplyr::filter(res, location_id %in% location_ids)
      }
      if (!is.null(start_year)) {
        res <- dplyr::filter(res, year >= start_year)
      }
      if (!is.null(end_year)) {
        res <- dplyr::filter(res, year <= end_year)
      }
      df <- as.data.frame(dplyr::collect(res))
      if ("date" %in% names(df)) df$date <- as.Date(df$date)
      return(df)
    }, error = function(e) {
      # Fallback to direct file reading
      parquet_files <- list.files(input_dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
      if (length(parquet_files) > 0) {
        dfs <- lapply(parquet_files, arrow::read_parquet)
        combined <- do.call(rbind, dfs)
        if ("date" %in% names(combined)) combined$date <- as.Date(combined$date)
        if (!is.null(location_ids) && length(location_ids) > 0) {
          combined <- combined[combined$location_id %in% location_ids, ]
        }
        if (!is.null(start_year) && "year" %in% names(combined)) {
          combined <- combined[combined$year >= start_year, ]
        }
        if (!is.null(end_year) && "year" %in% names(combined)) {
          combined <- combined[combined$year <= end_year, ]
        }
        return(combined)
      }
    })
  }

  # Fallback to RDS files if parquet not present
  rds_files <- list.files(input_dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  if (length(rds_files) > 0) {
    dfs <- lapply(rds_files, readRDS)
    combined <- do.call(rbind, dfs)
    if (!is.null(location_ids) && length(location_ids) > 0) {
      combined <- combined[combined$location_id %in% location_ids, ]
    }
    return(combined)
  }

  data.frame()
}
