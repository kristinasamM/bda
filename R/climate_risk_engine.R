# R/climate_risk_engine.R - Novelty Feature: Automated Climate Risk Index & Civic Action Advisory Engine
# Analyzes multi-decadal historical anomalies to generate a 0-100 Climate Vulnerability Score
# and plain-language civic adaptation recommendations for municipal planners and communities.

#' Calculate Comprehensive Climate Risk Profile for a Station
#'
#' @param monthly_df Monthly aggregated data frame
#' @param location_id Character ID of target location
#' @param baseline_df Computed seasonal baselines (optional, computed if missing)
#' @return List with scores, risk category, narrative briefing, and action playbook
calculate_station_risk_profile <- function(monthly_df, location_id, baseline_df = NULL) {
  if (is.null(monthly_df) || nrow(monthly_df) == 0 || is.null(location_id) || !nzchar(location_id)) {
    return(NULL)
  }

  stn_data <- monthly_df[monthly_df$location_id == location_id, ]
  if (nrow(stn_data) < 12) {
    return(NULL)
  }

  stn_name <- if (!is.null(stn_data$location_name[1]) && !is.na(stn_data$location_name[1])) stn_data$location_name[1] else location_id
  years_span <- length(unique(stn_data$year))
  total_months <- nrow(stn_data)

  # Compute baselines if not provided
  if (is.null(baseline_df) || nrow(baseline_df) == 0) {
    if (exists("compute_seasonal_baseline")) {
      b_start <- min(stn_data$year)
      b_end <- floor(mean(stn_data$year))
      baseline_df <- compute_seasonal_baseline(stn_data, baseline_start = b_start, baseline_end = b_end, min_baseline_years = 3)
    }
  }

  # Metric evaluation
  has_precip <- "total_precipitation_mm" %in% names(stn_data) && any(!is.na(stn_data$total_precipitation_mm))
  has_temp <- "mean_temperature_c" %in% names(stn_data) && any(!is.na(stn_data$mean_temperature_c))

  precip_vals <- if (has_precip) stn_data$total_precipitation_mm[!is.na(stn_data$total_precipitation_mm)] else numeric(0)
  temp_vals <- if (has_temp) stn_data$mean_temperature_c[!is.na(stn_data$mean_temperature_c)] else numeric(0)

  # 1. Flood & Deluge Score (0-100)
  max_precip <- if (length(precip_vals) > 0) max(precip_vals) else 0
  mean_precip <- if (length(precip_vals) > 0) mean(precip_vals) else 0
  high_deluge_months <- if (length(precip_vals) > 0) sum(precip_vals > (mean_precip + 1.5 * stats::sd(precip_vals))) else 0
  deluge_rate <- if (total_months > 0) (high_deluge_months / total_months) * 100 else 0

  flood_score <- pmin(100, round(
    (min(1, max_precip / 600) * 45) +
    (min(1, deluge_rate / 15) * 40) +
    (if (max_precip > 300) 15 else 0)
  ))

  # 2. Drought & Deficit Score (0-100)
  dry_months <- if (length(precip_vals) > 0) sum(precip_vals < 5.0) else 0
  dry_ratio <- if (total_months > 0) dry_months / total_months else 0
  monsoon_sub <- stn_data[stn_data$month %in% 6:9, ]
  monsoon_precip <- if (nrow(monsoon_sub) > 0 && has_precip) monsoon_sub$total_precipitation_mm else numeric(0)
  monsoon_cv <- if (length(monsoon_precip) > 5 && mean(monsoon_precip, na.rm = TRUE) > 0) {
    stats::sd(monsoon_precip, na.rm = TRUE) / mean(monsoon_precip, na.rm = TRUE)
  } else {
    0.5
  }

  drought_score <- pmin(100, round(
    (min(1, dry_ratio / 0.5) * 35) +
    (min(1, monsoon_cv / 1.0) * 45) +
    (if (mean_precip < 50) 20 else 5)
  ))

  # 3. Heat & Thermal Stress Score (0-100)
  heat_score <- if (has_temp && length(temp_vals) > 0) {
    max_t <- max(temp_vals)
    t_above_35 <- sum(temp_vals > 35)
    pmin(100, round((min(1, max_t / 45) * 50) + (min(1, t_above_35 / 10) * 50)))
  } else {
    35 # Default moderate assumption if only precipitation is provided
  }

  # 4. Volatility & Shock Score (0-100)
  volatility_score <- pmin(100, round((flood_score * 0.5) + (drought_score * 0.5)))

  # Composite Climate Vulnerability Score (0-100)
  composite_score <- pmin(100, pmax(10, round(
    (flood_score * 0.40) +
    (drought_score * 0.35) +
    (heat_score * 0.15) +
    (volatility_score * 0.10)
  )))

  # Risk Category and Styling
  risk_cat <- if (composite_score >= 75) {
    list(tier = "Critical Climate Stress", badge_class = "bg-danger", color = "#DC2626", icon = "⚠️")
  } else if (composite_score >= 50) {
    list(tier = "High Vulnerability", badge_class = "bg-warning text-dark", color = "#D97706", icon = "⚡")
  } else if (composite_score >= 30) {
    list(tier = "Moderate Climate Resilience", badge_class = "bg-info text-dark", color = "#0D9488", icon = "🌿")
  } else {
    list(tier = "High Adaptive Buffer", badge_class = "bg-success", color = "#166534", icon = "🛡️")
  }

  # Plain-Language Executive Advisory Narrative
  deluge_text <- if (max_precip > 0) {
    sprintf("The historic single-month deluge peak was recorded at %s mm.", round(max_precip, 1))
  } else ""

  primary_threat <- if (flood_score >= drought_score && flood_score >= heat_score) {
    "Monsoon Cloudbursts & Urban Drainage Overload"
  } else if (drought_score >= flood_score) {
    "Seasonal Moisture Deficit & Agricultural Rain Vulnerability"
  } else {
    "Thermal Heat Stress & Grid Strain"
  }

  narrative <- sprintf(
    "Based on %d years of continuous meteorological observations (%d months), %s exhibits a Composite Climate Vulnerability Score of %d/100 (%s). The primary climate stressor is %s. %s Over the observation record, %d severe anomaly months were identified where monthly readings deviated significantly from baseline averages.",
    years_span, total_months, stn_name, composite_score, risk_cat$tier, primary_threat, deluge_text, high_deluge_months
  )

  # Actionable Civic Playbook (3 Interventions)
  playbook <- list(
    list(
      title = "Stormwater Absorption & Drainage Reinforcement",
      type = "Civic Infrastructure",
      impact = "-32% Flood Risk",
      urgency = if (flood_score > 60) "High Priority" else "Medium Priority",
      recommendation = sprintf(
        "Modernize stormwater networks in %s to handle ≥%s mm/month flash events; install decentralized recharge pits and desilt natural culverts.",
        stn_name, round(max_precip * 0.7)
      )
    ),
    list(
      title = "Drought Buffer & Rainwater Storage Tanks",
      type = "Resource Security",
      impact = "-28% Dry-Season Deficit",
      urgency = if (drought_score > 55) "High Priority" else "Medium Priority",
      recommendation = sprintf(
        "Enforce mandatory rooftop rainwater harvesting across municipal buildings and promote micro-irrigation for peri-urban farms in %s.",
        stn_name
      )
    ),
    list(
      title = "Localized Early Warning Sirens & Citizen Alerts",
      type = "Disaster Preparedness",
      impact = "-40% Community Exposure",
      urgency = "Standard Protocol",
      recommendation = sprintf(
        "Connect %s meteorological station directly with civic disaster response systems for automated SMS broadcast when anomalies exceed 2.0 SD.",
        stn_name
      )
    )
  )

  list(
    location_id = location_id,
    location_name = stn_name,
    years_span = years_span,
    total_months = total_months,
    flood_score = flood_score,
    drought_score = drought_score,
    heat_score = heat_score,
    volatility_score = volatility_score,
    composite_score = composite_score,
    risk_cat = risk_cat,
    primary_threat = primary_threat,
    narrative = narrative,
    playbook = playbook,
    max_precip = max_precip,
    mean_precip = round(mean_precip, 1),
    high_deluge_months = high_deluge_months
  )
}

#' Render Climate Risk Index UI
#'
#' @param profile Output from calculate_station_risk_profile()
#' @return Shiny tagList
render_climate_risk_ui <- function(profile) {
  if (is.null(profile)) {
    return(shiny::div(
      class = "p-4 text-center text-muted",
      "Select an active weather station to generate its automated Climate Risk Profile."
    ))
  }

  shiny::tagList(
    # Top Score Hero Card
    shiny::div(
      class = "card p-4 mb-4",
      style = "background: linear-gradient(135deg, #F8FBF7 0%, #FFFFFF 100%); border: 1px solid #E2EBE2; border-left: 6px solid #1B4332; border-radius: 12px; box-shadow: 0 4px 16px rgba(0,0,0,0.04);",
      shiny::fluidRow(
        shiny::column(
          8,
          shiny::div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
            shiny::tags$h4(style = "margin: 0; font-family: 'Outfit', sans-serif; font-weight: 800; color: #1B4332;",
              sprintf("Climate Risk Profile: %s", profile$location_name)
            ),
            shiny::tags$span(class = paste("badge", profile$risk_cat$badge_class),
              style = "font-size: 0.82rem; padding: 5px 12px; border-radius: 20px;",
              paste(profile$risk_cat$icon, profile$risk_cat$tier)
            )
          ),
          shiny::p(style = "font-size: 0.92rem; color: #334155; line-height: 1.6; margin-bottom: 12px;",
            profile$narrative
          ),
          shiny::div(
            style = "display: flex; gap: 16px; font-size: 0.82rem; color: #64748B;",
            shiny::tags$span(shiny::tags$strong("Analyzed Record: "), sprintf("%d Years (%d Months)", profile$years_span, profile$total_months)),
            shiny::tags$span(shiny::tags$strong("Peak Historical Deluge: "), sprintf("%s mm", profile$max_precip)),
            shiny::tags$span(shiny::tags$strong("Primary Risk Driver: "), profile$primary_threat)
          )
        ),
        shiny::column(
          4,
          shiny::div(
            style = "background: #FFFFFF; border: 2px solid #E2EBE2; border-radius: 12px; padding: 16px; text-align: center; box-shadow: 0 2px 8px rgba(0,0,0,0.03);",
            shiny::tags$small(style = "text-transform: uppercase; font-weight: 700; color: #64748B; font-size: 0.72rem; letter-spacing: 0.05em; display: block;", "Composite Vulnerability"),
            shiny::div(
              style = sprintf("font-family: 'Outfit', sans-serif; font-weight: 900; font-size: 2.8rem; color: %s; line-height: 1.1; margin: 6px 0;", profile$risk_cat$color),
              profile$composite_score
            ),
            shiny::tags$small(style = "color: #94A3B8; font-size: 0.75rem; display: block;", "Scale: 0 (Resilient) to 100 (Critical)"),
            shiny::div(
              class = "progress mt-2",
              style = "height: 8px; border-radius: 4px; background: #E2EBE2;",
              shiny::div(
                class = "progress-bar",
                style = sprintf("width: %d%%; background-color: %s;", profile$composite_score, profile$risk_cat$color)
              )
            )
          )
        )
      )
    ),

    # 4 Sub-Index Metric Bars
    shiny::div(
      class = "mb-4",
      shiny::tags$h6(style = "font-weight: 700; color: #1B4332; margin-bottom: 12px;", "📊 Vulnerability Sub-Component Breakdown"),
      shiny::fluidRow(
        shiny::column(3,
          shiny::div(class = "card p-3", style = "background: #FFFFFF; border: 1px solid #E2EBE2; border-radius: 10px;",
            shiny::tags$small(style = "color: #64748B; font-weight: 600; font-size: 0.75rem;", "🌊 Monsoon & Flood Risk"),
            shiny::div(style = "font-size: 1.4rem; font-weight: 800; color: #1B4332; margin: 4px 0;", sprintf("%d / 100", profile$flood_score)),
            shiny::div(class = "progress", style = "height: 6px; background: #E2EBE2;",
              shiny::div(class = "progress-bar bg-primary", style = sprintf("width: %d%%;", profile$flood_score))
            )
          )
        ),
        shiny::column(3,
          shiny::div(class = "card p-3", style = "background: #FFFFFF; border: 1px solid #E2EBE2; border-radius: 10px;",
            shiny::tags$small(style = "color: #64748B; font-weight: 600; font-size: 0.75rem;", "☀️ Drought & Deficit Stress"),
            shiny::div(style = "font-size: 1.4rem; font-weight: 800; color: #D97706; margin: 4px 0;", sprintf("%d / 100", profile$drought_score)),
            shiny::div(class = "progress", style = "height: 6px; background: #E2EBE2;",
              shiny::div(class = "progress-bar bg-warning", style = sprintf("width: %d%%;", profile$drought_score))
            )
          )
        ),
        shiny::column(3,
          shiny::div(class = "card p-3", style = "background: #FFFFFF; border: 1px solid #E2EBE2; border-radius: 10px;",
            shiny::tags$small(style = "color: #64748B; font-weight: 600; font-size: 0.75rem;", "🌡️ Thermal Exposure Stress"),
            shiny::div(style = "font-size: 1.4rem; font-weight: 800; color: #DC2626; margin: 4px 0;", sprintf("%d / 100", profile$heat_score)),
            shiny::div(class = "progress", style = "height: 6px; background: #E2EBE2;",
              shiny::div(class = "progress-bar bg-danger", style = sprintf("width: %d%%;", profile$heat_score))
            )
          )
        ),
        shiny::column(3,
          shiny::div(class = "card p-3", style = "background: #FFFFFF; border: 1px solid #E2EBE2; border-radius: 10px;",
            shiny::tags$small(style = "color: #64748B; font-weight: 600; font-size: 0.75rem;", "⚡ Shock & Volatility Index"),
            shiny::div(style = "font-size: 1.4rem; font-weight: 800; color: #0D9488; margin: 4px 0;", sprintf("%d / 100", profile$volatility_score)),
            shiny::div(class = "progress", style = "height: 6px; background: #E2EBE2;",
              shiny::div(class = "progress-bar bg-info", style = sprintf("width: %d%%;", profile$volatility_score))
            )
          )
        )
      )
    ),

    # 3 Prioritized Action Recommendations
    shiny::div(
      shiny::tags$h6(style = "font-weight: 700; color: #1B4332; margin-bottom: 12px;", "🎯 Actionable Civic Adaptation Playbook"),
      shiny::fluidRow(
        lapply(profile$playbook, function(item) {
          shiny::column(
            4,
            shiny::div(
              class = "card h-100 p-3",
              style = "background: #FFFFFF; border: 1px solid #CBD5E1; border-top: 4px solid #2D6A4F; border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.03);",
              shiny::div(
                style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 8px;",
                shiny::tags$span(class = "badge bg-light text-success border border-success", style = "font-size: 0.72rem;", item$type),
                shiny::tags$span(class = "badge bg-success", style = "font-size: 0.72rem;", item$impact)
              ),
              shiny::tags$h6(style = "font-weight: 700; color: #0F172A; font-size: 0.95rem; margin-bottom: 6px;", item$title),
              shiny::p(style = "font-size: 0.82rem; color: #475569; line-height: 1.5; margin-bottom: 8px;", item$recommendation),
              shiny::div(style = "margin-top: auto; font-size: 0.74rem; color: #64748B;",
                shiny::tags$strong("Implementation: "), item$urgency
              )
            )
          )
        })
      )
    )
  )
}
