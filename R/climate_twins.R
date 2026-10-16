# R/climate_twins.R - Climate Twin Finder
# Discovers monitored locations with similar seasonal climate profiles across shared periods.
# Uses 12-calendar-month climatological profiles, log1p transformation for rainfall,
# shared positive scaling across eligible candidates, and root-mean-square distance (RMSD).

#' Construct 12-Month Climatological Profiles for Locations
#'
#' Evaluates the average 12-calendar-month profile (Jan-Dec) for each candidate location
#' over a strictly shared historical period.
#' Requires adequate valid observations for every included calendar month (all 12 months present).
#'
#' @param monthly_df Monthly aggregated data frame
#' @param location_ids Character vector of candidate locations
#' @param start_year Integer start year
#' @param end_year Integer end year
#' @param variables Character vector of variables to include: "mean_temperature_c", "total_precipitation_mm", "mean_humidity_pct"
#' @return List of eligible location profiles and list of ineligible locations with reasons
compute_climatological_profiles <- function(monthly_df,
                                            location_ids,
                                            start_year,
                                            end_year,
                                            variables = c("mean_temperature_c", "total_precipitation_mm")) {
  if (is.null(monthly_df) || nrow(monthly_df) == 0) {
    return(list(eligible = list(), ineligible = list()))
  }

  # Filter to strictly shared historical period
  sub_df <- monthly_df[monthly_df$year >= start_year & monthly_df$year <= end_year, ]
  if (nrow(sub_df) == 0) {
    return(list(eligible = list(), ineligible = list()))
  }

  eligible_profiles <- list()
  ineligible_locs <- list()

  for (loc in location_ids) {
    loc_sub <- sub_df[sub_df$location_id == loc, ]
    if (nrow(loc_sub) == 0) {
      ineligible_locs[[loc]] <- "No records within the selected shared historical period."
      next
    }

    loc_name <- if ("location_name" %in% names(loc_sub) && any(!is.na(loc_sub$location_name))) {
      loc_sub$location_name[!is.na(loc_sub$location_name)][1]
    } else {
      loc
    }

    # Verify all 12 calendar months are represented
    months_present <- unique(loc_sub$month[!is.na(loc_sub$month)])
    if (length(months_present) < 12) {
      missing_months <- setdiff(1:12, months_present)
      ineligible_locs[[loc]] <- sprintf("Missing records for calendar month(s): %s.", paste(missing_months, collapse = ", "))
      next
    }

    # Build 12-month profile per variable
    prof_mat <- matrix(NA_real_, nrow = 12, ncol = length(variables), dimnames = list(1:12, variables))
    is_loc_valid <- TRUE

    for (v in variables) {
      # Check usability flag if available
      flag_col <- switch(v,
        "mean_temperature_c" = "is_usable_temp",
        "total_precipitation_mm" = "is_usable_precip",
        "mean_humidity_pct" = "is_usable_humidity",
        NULL
      )

      v_vals <- numeric(12)
      for (m in 1:12) {
        m_rows <- loc_sub[loc_sub$month == m, ]
        if (!is.null(flag_col) && flag_col %in% names(m_rows)) {
          m_rows <- m_rows[m_rows[[flag_col]] == TRUE, ]
        }
        valid_m_vals <- m_rows[[v]][!is.na(m_rows[[v]])]
        if (length(valid_m_vals) == 0) {
          is_loc_valid <- FALSE
          break
        }
        v_vals[m] <- mean(valid_m_vals)
      }

      if (!is_loc_valid) break
      prof_mat[, v] <- v_vals
    }

    if (!is_loc_valid) {
      ineligible_locs[[loc]] <- "One or more calendar months lack valid quality-assured observations for selected variables."
      next
    }

    eligible_profiles[[loc]] <- list(
      location_id = loc,
      location_name = loc_name,
      profile_matrix = prof_mat,
      years_span = sprintf("%d–%d", start_year, end_year)
    )
  }

  list(eligible = eligible_profiles, ineligible = ineligible_locs)
}

#' Calculate Climate Twins Using Transparent Similarity Distance
#'
#' @param eligible_profiles List of eligible profiles from compute_climatological_profiles()
#' @param reference_loc_id Character ID of reference station
#' @param variables Character vector of included variables
#' @param max_twins Maximum number of matches to return (default 3)
#' @return List with ranked twins, shared scales, distance scores, and comparison data
calculate_climate_twins <- function(eligible_profiles,
                                    reference_loc_id,
                                    variables = c("mean_temperature_c", "total_precipitation_mm"),
                                    max_twins = 3) {
  if (is.null(eligible_profiles) || length(eligible_profiles) == 0) {
    return(list(status = "NO_DATA", message = "No eligible location profiles provided."))
  }

  if (!reference_loc_id %in% names(eligible_profiles)) {
    return(list(
      status = "REFERENCE_NOT_ELIGIBLE",
      message = sprintf("Reference location '%s' does not have complete observations for all 12 calendar months across the selected period.", reference_loc_id)
    ))
  }

  candidate_ids <- setdiff(names(eligible_profiles), reference_loc_id)
  if (length(candidate_ids) == 0) {
    return(list(
      status = "INSUFFICIENT_CANDIDATES",
      message = "No other candidate locations with adequate shared data are available for comparison."
    ))
  }

  ref_prof <- eligible_profiles[[reference_loc_id]]

  # 1. Prepare transformed profile matrices for reference and all candidates
  # Apply log1p to rainfall to balance dry/wet seasons
  transform_matrix <- function(mat) {
    t_mat <- mat
    if ("total_precipitation_mm" %in% colnames(t_mat)) {
      t_mat[, "total_precipitation_mm"] <- log1p(pmax(0, t_mat[, "total_precipitation_mm"]))
    }
    t_mat
  }

  transformed_all <- lapply(eligible_profiles, function(p) transform_matrix(p$profile_matrix))

  # 2. Estimate shared positive scale across eligible candidate pool
  # (estimated across all candidates, not separately within each location)
  shared_scales <- numeric(length(variables))
  names(shared_scales) <- variables

  for (v in variables) {
    all_v_vals <- unlist(lapply(transformed_all, function(m) m[, v]))
    v_range <- max(all_v_vals, na.rm = TRUE) - min(all_v_vals, na.rm = TRUE)
    v_sd <- stats::sd(all_v_vals, na.rm = TRUE)
    
    # Use max(range, sd, default) to guarantee a strictly positive scale
    scale_val <- if (!is.na(v_range) && v_range > 1e-4) {
      v_range
    } else if (!is.na(v_sd) && v_sd > 1e-4) {
      v_sd
    } else {
      1.0
    }
    shared_scales[v] <- scale_val
  }

  # 3. Calculate Root-Mean-Square Distance (RMSD) for each candidate
  ref_t_mat <- transformed_all[[reference_loc_id]]
  scores <- data.frame(
    location_id = character(0),
    location_name = character(0),
    distance_score = numeric(0),
    stringsAsFactors = FALSE
  )

  for (cand_id in candidate_ids) {
    cand_t_mat <- transformed_all[[cand_id]]
    cand_info <- eligible_profiles[[cand_id]]

    # Scaled squared differences across all 12 calendar months and variables
    sq_diff_sum <- 0
    total_elements <- length(variables) * 12

    for (v in variables) {
      scale_v <- shared_scales[v]
      diff_v <- (ref_t_mat[, v] - cand_t_mat[, v]) / scale_v
      sq_diff_sum <- sq_diff_sum + sum(diff_v^2)
    }

    rmsd <- sqrt(sq_diff_sum / total_elements)
    scores <- rbind(scores, data.frame(
      location_id = cand_id,
      location_name = cand_info$location_name,
      distance_score = round(rmsd, 3),
      stringsAsFactors = FALSE
    ))
  }

  # Order by lowest distance (lower is closer)
  scores <- scores[order(scores$distance_score), ]
  top_n <- head(scores, max_twins)

  # 4. Generate structured breakdown of similarities and differences for each top twin
  twins_list <- list()
  for (i in seq_len(nrow(top_n))) {
    cand_id <- top_n$location_id[i]
    cand_prof <- eligible_profiles[[cand_id]]
    cand_orig_mat <- cand_prof$profile_matrix
    ref_orig_mat <- ref_prof$profile_matrix

    narratives <- character(0)
    
    if ("mean_temperature_c" %in% variables) {
      temp_diff <- round(mean(abs(ref_orig_mat[, "mean_temperature_c"] - cand_orig_mat[, "mean_temperature_c"])), 1)
      peak_ref_m <- which.max(ref_orig_mat[, "mean_temperature_c"])
      peak_cand_m <- which.max(cand_orig_mat[, "mean_temperature_c"])
      month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      
      narratives <- c(narratives, sprintf(
        "Temperature: Average monthly deviation of %.1f°C. Peak warm month is %s for %s vs %s for %s.",
        temp_diff, month_names[peak_ref_m], ref_prof$location_name, month_names[peak_cand_m], cand_prof$location_name
      ))
    }

    if ("total_precipitation_mm" %in% variables) {
      ref_annual <- round(sum(ref_orig_mat[, "total_precipitation_mm"]), 0)
      cand_annual <- round(sum(cand_orig_mat[, "total_precipitation_mm"]), 0)
      annual_diff <- abs(ref_annual - cand_annual)
      
      narratives <- c(narratives, sprintf(
        "Precipitation: Estimated annual climatological rainfall of %s mm (%s) vs %s mm (%s), a difference of %s mm.",
        format(ref_annual, big.mark = ","), ref_prof$location_name,
        format(cand_annual, big.mark = ","), cand_prof$location_name,
        format(annual_diff, big.mark = ",")
      ))
    }

    twins_list[[cand_id]] <- list(
      rank = i,
      location_id = cand_id,
      location_name = cand_prof$location_name,
      distance_score = top_n$distance_score[i],
      profile_matrix = cand_orig_mat,
      comparison_breakdown = narratives
    )
  }

  list(
    status = "SUCCESS",
    reference_location_id = reference_loc_id,
    reference_location_name = ref_prof$location_name,
    reference_profile = ref_prof$profile_matrix,
    shared_period = ref_prof$years_span,
    variables = variables,
    shared_scales = shared_scales,
    total_eligible_candidates = length(candidate_ids),
    twins = twins_list,
    top_matches_table = top_n,
    metric_label = "climate-profile distance — lower is closer",
    disclaimer = "Climate twins share seasonal patterns over the selected period. Distance is an index of seasonal curve shape, not an identity or hazard prediction."
  )
}
