# R/community_storage.R - Persistent Storage for Community Simulator
# Persists Vulnerability Profiles and Community Observations using SQLite via DBI/RSQLite
# with a resilient JSON file fallback if RSQLite is unavailable.

#' Initialize Community Storage Engine
#'
#' Ensures the data/community directory exists and initializes the SQLite tables.
#' @param db_dir Directory for community database (default "data/community")
#' @param db_file Database filename (default "community.sqlite")
#' @return List with storage configuration and backend type
init_community_storage <- function(db_dir = "data/community", db_file = "community.sqlite") {
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  db_path <- file.path(db_dir, db_file)
  has_sqlite <- requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)
  
  backend <- if (has_sqlite) "sqlite" else "json"
  
  if (backend == "sqlite") {
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Table: Community Profiles
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS community_profiles (
          id TEXT PRIMARY KEY,
          location_id TEXT NOT NULL,
          dataset_id TEXT NOT NULL DEFAULT '',
          profile_name TEXT NOT NULL,
          drainage TEXT NOT NULL DEFAULT 'Unknown',
          waterlogging TEXT NOT NULL DEFAULT 'Unknown',
          water_storage TEXT NOT NULL DEFAULT 'Unknown',
          rainfall_dependence TEXT NOT NULL DEFAULT 'Unknown',
          outdoor_work TEXT NOT NULL DEFAULT 'Unknown',
          shade_cooling TEXT NOT NULL DEFAULT 'Unknown',
          power_interruptions TEXT NOT NULL DEFAULT 'Unknown',
          created_at TEXT NOT NULL,
          updated_at TEXT NOT NULL
        )
      ")
      
      tryCatch({
        DBI::dbExecute(con, "ALTER TABLE community_profiles ADD COLUMN dataset_id TEXT DEFAULT ''")
      }, error = function(e) NULL)
      
      # Table: Community Observations
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS community_observations (
          id TEXT PRIMARY KEY,
          location_id TEXT NOT NULL,
          area_name TEXT,
          obs_date TEXT NOT NULL,
          obs_type TEXT NOT NULL,
          severity TEXT NOT NULL,
          description TEXT NOT NULL,
          created_at TEXT NOT NULL
        )
      ")
      
      # Seed default profile if empty
      count_res <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM community_profiles")
      if (count_res$n[1] == 0) {
        now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        DBI::dbExecute(con, "
          INSERT INTO community_profiles (
            id, location_id, profile_name, drainage, waterlogging,
            water_storage, rainfall_dependence, outdoor_work,
            shade_cooling, power_interruptions, created_at, updated_at
          ) VALUES (
            'default_mumbai', 'mumbai', 'Default Baseline (Unknowns)',
            'Unknown', 'Unknown', 'Unknown', 'Unknown',
            'Unknown', 'Unknown', 'Unknown', ?, ?
          )
        ", params = list(now_str, now_str))
      }
    }, error = function(e) {
      warning("SQLite initialization failed, using JSON fallback: ", e$message)
      backend <<- "json"
    })
  }
  
  list(
    backend = backend,
    db_path = db_path,
    json_profiles = file.path(db_dir, "profiles.json"),
    json_observations = file.path(db_dir, "observations.json")
  )
}

# -------------------------------------------------------------------
# PROFILE MANAGEMENT
# -------------------------------------------------------------------

#' Save or Update a Vulnerability Profile
#'
#' @param profile List containing profile fields
#' @param storage_config Storage settings from init_community_storage()
#' @return Logical indicating success
save_community_profile <- function(profile, storage_config = init_community_storage()) {
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  if (is.null(profile$id) || nchar(profile$id) == 0) {
    profile$id <- paste0("prof_", sprintf("%.0f", as.numeric(Sys.time()) * 1000), "_", sample(1000:9999, 1))
    profile$created_at <- now_str
  }
  profile$updated_at <- now_str
  
  # Sanitize and default fields
  valid_levels <- list(
    drainage = c("Good", "Moderate", "Poor", "Unknown"),
    waterlogging = c("Never", "Occasionally", "Frequently", "Unknown"),
    water_storage = c("Adequate", "Limited", "None", "Unknown"),
    rainfall_dependence = c("Low", "Medium", "High", "Unknown"),
    outdoor_work = c("Low", "Medium", "High", "Unknown"),
    shade_cooling = c("Good", "Limited", "Poor", "Unknown"),
    power_interruptions = c("Rare", "Occasional", "Frequent", "Unknown")
  )
  
  for (f in names(valid_levels)) {
    val <- profile[[f]]
    if (is.null(val) || !(val %in% valid_levels[[f]])) {
      profile[[f]] <- "Unknown"
    }
  }
  
  if (is.null(profile$profile_name) || nchar(trimws(profile$profile_name)) == 0) {
    profile$profile_name <- paste("Scenario Profile", format(Sys.time(), "%b %d %H:%M"))
  }
  
  profile$dataset_id <- if (!is.null(profile$dataset_id)) as.character(profile$dataset_id) else ""

  if (storage_config$backend == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), storage_config$db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    # Check if exists
    chk <- DBI::dbGetQuery(con, "SELECT id, created_at FROM community_profiles WHERE id = ?", params = list(profile$id))
    if (nrow(chk) > 0) {
      created_time <- chk$created_at[1]
      DBI::dbExecute(con, "
        UPDATE community_profiles SET
          location_id = ?,
          dataset_id = ?,
          profile_name = ?,
          drainage = ?,
          waterlogging = ?,
          water_storage = ?,
          rainfall_dependence = ?,
          outdoor_work = ?,
          shade_cooling = ?,
          power_interruptions = ?,
          updated_at = ?
        WHERE id = ?
      ", params = list(
        profile$location_id, profile$dataset_id, profile$profile_name, profile$drainage, profile$waterlogging,
        profile$water_storage, profile$rainfall_dependence, profile$outdoor_work,
        profile$shade_cooling, profile$power_interruptions, now_str, profile$id
      ))
    } else {
      DBI::dbExecute(con, "
        INSERT INTO community_profiles (
          id, location_id, dataset_id, profile_name, drainage, waterlogging,
          water_storage, rainfall_dependence, outdoor_work,
          shade_cooling, power_interruptions, created_at, updated_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        profile$id, profile$location_id, profile$dataset_id, profile$profile_name, profile$drainage, profile$waterlogging,
        profile$water_storage, profile$rainfall_dependence, profile$outdoor_work,
        profile$shade_cooling, profile$power_interruptions, now_str, now_str
      ))
    }
    return(profile$id)
  } else {
    # JSON fallback
    profs <- load_community_profiles(location_id = NULL, storage_config = storage_config)
    idx <- which(profs$id == profile$id)
    new_df_row <- data.frame(
      id = profile$id,
      location_id = profile$location_id,
      dataset_id = profile$dataset_id,
      profile_name = profile$profile_name,
      drainage = profile$drainage,
      waterlogging = profile$waterlogging,
      water_storage = profile$water_storage,
      rainfall_dependence = profile$rainfall_dependence,
      outdoor_work = profile$outdoor_work,
      shade_cooling = profile$shade_cooling,
      power_interruptions = profile$power_interruptions,
      created_at = ifelse(length(idx) > 0, profs$created_at[idx], now_str),
      updated_at = now_str,
      stringsAsFactors = FALSE
    )
    if (length(idx) > 0) {
      profs[idx, ] <- new_df_row
    } else {
      profs <- rbind(profs, new_df_row)
    }
    jsonlite::write_json(profs, storage_config$json_profiles, pretty = TRUE)
    return(profile$id)
  }
}

#' Load Community Profiles
#'
#' @param location_id Filter by location_id (optional, if NULL returns all)
#' @param dataset_id Filter by dataset_id (optional)
#' @param storage_config Storage settings from init_community_storage()
#' @return Data frame of profiles
load_community_profiles <- function(location_id = NULL, dataset_id = NULL, storage_config = init_community_storage()) {
  cols <- c("id", "location_id", "dataset_id", "profile_name", "drainage", "waterlogging",
            "water_storage", "rainfall_dependence", "outdoor_work",
            "shade_cooling", "power_interruptions", "created_at", "updated_at")
  empty_df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  colnames(empty_df) <- cols
  
  if (storage_config$backend == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), storage_config$db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    where_clauses <- character(0)
    params <- list()
    if (!is.null(location_id) && nchar(location_id) > 0) {
      where_clauses <- c(where_clauses, "location_id = ?")
      params <- c(params, list(location_id))
    }
    if (!is.null(dataset_id) && nchar(dataset_id) > 0) {
      where_clauses <- c(where_clauses, "(dataset_id = ? OR dataset_id = '' OR dataset_id IS NULL)")
      params <- c(params, list(dataset_id))
    }
    
    query <- "SELECT * FROM community_profiles"
    if (length(where_clauses) > 0) {
      query <- paste(query, "WHERE", paste(where_clauses, collapse = " AND "))
    }
    query <- paste(query, "ORDER BY updated_at DESC")
    
    df <- if (length(params) > 0) DBI::dbGetQuery(con, query, params = params) else DBI::dbGetQuery(con, query)
    if (nrow(df) == 0) return(empty_df)
    return(df)
  } else {
    if (file.exists(storage_config$json_profiles)) {
      df <- tryCatch(jsonlite::fromJSON(storage_config$json_profiles), error = function(e) empty_df)
      if (!is.null(location_id) && nchar(location_id) > 0 && nrow(df) > 0) {
        df <- df[df$location_id == location_id, ]
      }
      return(df)
    }
    return(empty_df)
  }
}

#' Get Single Profile by ID
#'
#' @param profile_id Character ID
#' @param storage_config Storage settings
#' @return List of profile attributes or NULL
get_community_profile <- function(profile_id, storage_config = init_community_storage()) {
  profs <- load_community_profiles(location_id = NULL, storage_config = storage_config)
  match_idx <- which(profs$id == profile_id)
  if (length(match_idx) == 0) return(NULL)
  as.list(profs[match_idx[1], ])
}

#' Rename Community Profile
#'
#' @param profile_id Character ID
#' @param new_name New profile name
#' @param storage_config Storage settings
#' @return Logical indicating success
rename_community_profile <- function(profile_id, new_name, storage_config = init_community_storage()) {
  if (is.null(new_name) || nchar(trimws(new_name)) == 0) return(FALSE)
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  if (storage_config$backend == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), storage_config$db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    res <- DBI::dbExecute(con, "
      UPDATE community_profiles SET profile_name = ?, updated_at = ? WHERE id = ?
    ", params = list(trimws(new_name), now_str, profile_id))
    return(res > 0)
  } else {
    profs <- load_community_profiles(location_id = NULL, storage_config = storage_config)
    idx <- which(profs$id == profile_id)
    if (length(idx) == 0) return(FALSE)
    profs$profile_name[idx] <- trimws(new_name)
    profs$updated_at[idx] <- now_str
    jsonlite::write_json(profs, storage_config$json_profiles, pretty = TRUE)
    return(TRUE)
  }
}

#' Delete Community Profile
#'
#' @param profile_id Character ID
#' @param storage_config Storage settings
#' @return Logical indicating success
delete_community_profile <- function(profile_id, storage_config = init_community_storage()) {
  if (storage_config$backend == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), storage_config$db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    res <- DBI::dbExecute(con, "DELETE FROM community_profiles WHERE id = ?", params = list(profile_id))
    return(res > 0)
  } else {
    profs <- load_community_profiles(location_id = NULL, storage_config = storage_config)
    idx <- which(profs$id == profile_id)
    if (length(idx) == 0) return(FALSE)
    profs <- profs[-idx, ]
    jsonlite::write_json(profs, storage_config$json_profiles, pretty = TRUE)
    return(TRUE)
  }
}

# -------------------------------------------------------------------
# OBSERVATION MANAGEMENT
# -------------------------------------------------------------------

#' Save Community Observation
#'
#' @param observation List with observation details
#' @param storage_config Storage settings
#' @return Generated observation ID
save_community_observation <- function(observation, storage_config = init_community_storage()) {
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  obs_id <- paste0("obs_", sprintf("%.0f", as.numeric(Sys.time()) * 1000), "_", sample(100:999, 1))
  
  # Validate required fields
  if (is.null(observation$location_id) || nchar(observation$location_id) == 0) {
    stop("Observation location_id is required.")
  }
  if (is.null(observation$obs_date) || is.na(as.Date(observation$obs_date))) {
    observation$obs_date <- format(Sys.Date(), "%Y-%m-%d")
  } else {
    observation$obs_date <- as.character(as.Date(observation$obs_date))
  }
  
  valid_types <- c("Waterlogging", "Water shortage", "Power interruption", "Heat discomfort", "Other")
  obs_type <- if (observation$obs_type %in% valid_types) observation$obs_type else "Other"
  
  valid_sevs <- c("Minor", "Moderate", "Severe")
  obs_sev <- if (observation$severity %in% valid_sevs) observation$severity else "Minor"
  
  # Clean and limit description (max 300 characters, no PII)
  desc <- if (!is.null(observation$description)) substr(trimws(observation$description), 1, 300) else ""
  area <- if (!is.null(observation$area_name)) substr(trimws(observation$area_name), 1, 60) else ""
  
  if (storage_config$backend == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), storage_config$db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "
      INSERT INTO community_observations (
        id, location_id, area_name, obs_date, obs_type, severity, description, created_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      obs_id, observation$location_id, area, observation$obs_date, obs_type, obs_sev, desc, now_str
    ))
  } else {
    obs_df <- load_community_observations(storage_config = storage_config)
    new_row <- data.frame(
      id = obs_id,
      location_id = observation$location_id,
      area_name = area,
      obs_date = observation$obs_date,
      obs_type = obs_type,
      severity = obs_sev,
      description = desc,
      created_at = now_str,
      stringsAsFactors = FALSE
    )
    obs_df <- rbind(obs_df, new_row)
    jsonlite::write_json(obs_df, storage_config$json_observations, pretty = TRUE)
  }
  
  obs_id
}

#' Load Filtered Community Observations
#'
#' @param location_id Filter by location (optional)
#' @param obs_type Filter by observation type (optional)
#' @param start_date Filter by minimum date (optional)
#' @param end_date Filter by maximum date (optional)
#' @param storage_config Storage settings
#' @return Data frame of observations
load_community_observations <- function(location_id = NULL,
                                        obs_type = NULL,
                                        start_date = NULL,
                                        end_date = NULL,
                                        storage_config = init_community_storage()) {
  cols <- c("id", "location_id", "area_name", "obs_date", "obs_type", "severity", "description", "created_at")
  empty_df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  colnames(empty_df) <- cols
  
  if (storage_config$backend == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), storage_config$db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    where_clauses <- c()
    params <- list()
    
    if (!is.null(location_id) && location_id != "all" && nchar(location_id) > 0) {
      where_clauses <- c(where_clauses, "location_id = ?")
      params <- c(params, list(location_id))
    }
    if (!is.null(obs_type) && obs_type != "all" && nchar(obs_type) > 0) {
      where_clauses <- c(where_clauses, "obs_type = ?")
      params <- c(params, list(obs_type))
    }
    if (!is.null(start_date) && !is.na(as.Date(start_date))) {
      where_clauses <- c(where_clauses, "obs_date >= ?")
      params <- c(params, list(as.character(as.Date(start_date))))
    }
    if (!is.null(end_date) && !is.na(as.Date(end_date))) {
      where_clauses <- c(where_clauses, "obs_date <= ?")
      params <- c(params, list(as.character(as.Date(end_date))))
    }
    
    where_str <- if (length(where_clauses) > 0) paste("WHERE", paste(where_clauses, collapse = " AND ")) else ""
    query <- paste("SELECT * FROM community_observations", where_str, "ORDER BY obs_date DESC, created_at DESC")
    
    df <- if (length(params) > 0) DBI::dbGetQuery(con, query, params = params) else DBI::dbGetQuery(con, query)
    if (nrow(df) == 0) return(empty_df)
    return(df)
  } else {
    if (file.exists(storage_config$json_observations)) {
      df <- tryCatch(jsonlite::fromJSON(storage_config$json_observations), error = function(e) empty_df)
      if (nrow(df) > 0) {
        if (!is.null(location_id) && location_id != "all") df <- df[df$location_id == location_id, ]
        if (!is.null(obs_type) && obs_type != "all") df <- df[df$obs_type == obs_type, ]
        if (!is.null(start_date)) df <- df[as.Date(df$obs_date) >= as.Date(start_date), ]
        if (!is.null(end_date)) df <- df[as.Date(df$obs_date) <= as.Date(end_date), ]
      }
      return(df)
    }
    return(empty_df)
  }
}

#' Delete Community Observation
#'
#' @param obs_id Character observation ID
#' @param storage_config Storage settings
#' @return Logical indicating success
delete_community_observation <- function(obs_id, storage_config = init_community_storage()) {
  if (storage_config$backend == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), storage_config$db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    res <- DBI::dbExecute(con, "DELETE FROM community_observations WHERE id = ?", params = list(obs_id))
    return(res > 0)
  } else {
    obs_df <- load_community_observations(storage_config = storage_config)
    idx <- which(obs_df$id == obs_id)
    if (length(idx) == 0) return(FALSE)
    obs_df <- obs_df[-idx, ]
    jsonlite::write_json(obs_df, storage_config$json_observations, pretty = TRUE)
    return(TRUE)
  }
}
