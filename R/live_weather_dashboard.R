# R/live_weather_dashboard.R - UI Builders for Live Weather and Official Alerts
# Strictly separates authoritative official alerts from experimental condition indicators.
# Prominently labels model-estimated numerical weather prediction data.
# Distinguishes data statuses (Updated, Stale, Unavailable, Coverage Not Supported).

#' Render Live Weather Status Pill
#'
#' @param status Character ("Updated", "Stale", "Unavailable", etc.)
#' @param fetch_time Character timestamp
#' @return Shiny HTML span
render_weather_status_pill <- function(status, fetch_time = NULL) {
  if (is.null(status)) status <- "Unavailable"
  
  cfg <- switch(status,
    "Updated" = list(class = "bg-success text-white", text = "Live Updated"),
    "Stale" = list(class = "bg-warning text-dark", text = "Cached / Stale"),
    "Cached alert data is stale." = list(class = "bg-warning text-dark", text = "Alert Cache Stale"),
    "No active alerts returned by the provider." = list(class = "bg-info text-dark", text = "No Active Alerts"),
    "No active alerts (Future alerts scheduled)" = list(class = "bg-primary text-white", text = "Future Alerts Scheduled"),
    "Alert coverage not supported" = list(class = "bg-secondary text-white", text = "Alerts Not Configured"),
    "Alert service unavailable" = list(class = "bg-danger text-white", text = "Alert Service Unavailable"),
    list(class = "bg-danger text-white", text = "Unavailable")
  )
  
  shiny::tags$span(
    class = paste("badge", cfg$class),
    style = "padding: 6px 12px; font-weight: 600; font-size: 0.8rem; border-radius: 20px;",
    cfg$text
  )
}

#' Render Live Current Weather Cards UI
#'
#' @param weather_data List returned by fetch_live_weather()
#' @return Shiny HTML tag
render_current_weather_cards_ui <- function(weather_data) {
  if (is.null(weather_data) || weather_data$status == "Unavailable") {
    return(
      shiny::div(
        class = "alert alert-warning border-warning",
        style = "border-radius: 10px;",
        shiny::tags$strong("Live Weather Data Unavailable: "),
        if (!is.null(weather_data$message)) weather_data$message else "Could not connect to live weather provider."
      )
    )
  }
  
  curr <- weather_data$current
  
  shiny::div(
    # Stale notice if applicable
    if (weather_data$status == "Stale" && !is.null(weather_data$stale_message)) {
      shiny::div(
        class = "alert alert-warning py-2 px-3 mb-3",
        style = "font-size: 0.84rem; border-left: 4px solid #F59E0B;",
        shiny::HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" style="margin-right:6px; vertical-align: -2px;"><circle cx="12" cy="12" r="10"/><line x1="12" y1="8" x2="12" y2="12"/><line x1="12" y1="16" x2="12.01" y2="16"/></svg>'),
        shiny::tags$strong("CACHED / STALE RESPONSE: "),
        weather_data$stale_message
      )
    },
    
    # Model estimate notice banner
    shiny::div(
      style = "background: #F0FDF4; border: 1px solid #BBF7D0; border-radius: 8px; padding: 10px 14px; margin-bottom: 16px; font-size: 0.82rem; color: #166534; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px;",
      shiny::div(
        shiny::tags$strong("MODEL-ESTIMATED WEATHER: "),
        "Values represent numerical weather prediction (NWP) model outputs from Open-Meteo (ECMWF/GFS), not physical ground sensor measurements."
      ),
      shiny::tags$span(class = "badge bg-success bg-opacity-75", paste("Valid Time:", curr$observation_time))
    ),
    
    # Metric cards grid
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::div(
          class = "card shadow-sm p-3 mb-3 bg-white rounded-3 border-0",
          style = "border-top: 4px solid #0D9488 !important;",
          shiny::div(style = "font-size: 0.78rem; text-transform: uppercase; font-weight: 700; color: #64748B;", "Temperature"),
          shiny::div(style = "font-size: 2rem; font-weight: 800; color: #0F172A;", sprintf("%.1f°C", curr$temperature)),
          shiny::div(style = "font-size: 0.8rem; color: #0D9488; font-weight: 500;", sprintf("Feels like: %.1f°C", curr$apparent_temperature))
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "card shadow-sm p-3 mb-3 bg-white rounded-3 border-0",
          style = "border-top: 4px solid #0EA5E9 !important;",
          shiny::div(style = "font-size: 0.78rem; text-transform: uppercase; font-weight: 700; color: #64748B;", "Condition & Humidity"),
          shiny::div(style = "font-size: 1.3rem; font-weight: 700; color: #0F172A; margin: 4px 0;", curr$condition),
          shiny::div(style = "font-size: 0.8rem; color: #64748B;", sprintf("Relative Humidity: %.0f%%", curr$relative_humidity))
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "card shadow-sm p-3 mb-3 bg-white rounded-3 border-0",
          style = "border-top: 4px solid #3B82F6 !important;",
          shiny::div(style = "font-size: 0.78rem; text-transform: uppercase; font-weight: 700; color: #64748B;", "Precipitation"),
          shiny::div(style = "font-size: 2rem; font-weight: 800; color: #0F172A;", sprintf("%.1f mm", curr$precipitation)),
          shiny::div(style = "font-size: 0.8rem; color: #64748B;", curr$precipitation_period)
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "card shadow-sm p-3 mb-3 bg-white rounded-3 border-0",
          style = "border-top: 4px solid #F59E0B !important;",
          shiny::div(style = "font-size: 0.78rem; text-transform: uppercase; font-weight: 700; color: #64748B;", "Wind Speed & Gusts"),
          shiny::div(style = "font-size: 2rem; font-weight: 800; color: #0F172A;", sprintf("%.1f km/h", curr$wind_speed)),
          shiny::div(style = "font-size: 0.8rem; color: #F59E0B; font-weight: 500;", sprintf("Peak Gusts: %.1f km/h", curr$wind_gust))
        )
      )
    )
  )
}

#' Render 7-Day Forecast Cards UI
#'
#' @param daily_df Data frame of daily forecasts
#' @return Shiny HTML tag
render_daily_forecast_cards_ui <- function(daily_df) {
  if (is.list(daily_df) && !is.data.frame(daily_df) && !is.null(daily_df$daily)) {
    daily_df <- daily_df$daily
  }
  if (is.null(daily_df) || !is.data.frame(daily_df) || nrow(daily_df) == 0) return(NULL)
  
  cards <- lapply(seq_len(nrow(daily_df)), function(i) {
    row <- daily_df[i, ]
    shiny::div(
      class = "col",
      shiny::div(
        class = "card shadow-sm h-100 p-2 text-center bg-white border",
        style = "border-radius: 10px; font-size: 0.8rem;",
        shiny::tags$strong(style = "font-size: 0.85rem; color: #0F172A; display: block; margin-bottom: 2px;", row$day_name),
        shiny::tags$span(style = "color: #0D9488; font-weight: 600; display: block; font-size: 0.78rem; min-height: 28px;", row$condition),
        shiny::div(
          style = "margin: 6px 0;",
          shiny::tags$span(style = "color: #EF4444; font-weight: 700;", sprintf("%.0f°", row$temp_max)),
          shiny::tags$span(style = "color: #94A3B8; margin: 0 3px;", "/"),
          shiny::tags$span(style = "color: #3B82F6; font-weight: 600;", sprintf("%.0f°", row$temp_min))
        ),
        shiny::div(
          style = "font-size: 0.74rem; color: #475569;",
          shiny::tags$div(sprintf("Rain: %.1f mm", row$precip_sum)),
          if (!is.null(row$precip_probability) && length(row$precip_probability) == 1 && !is.na(row$precip_probability)) {
            shiny::tags$div(
              style = "color: #2563EB; cursor: help;",
              title = "Precipitation probability indicates chance of rain volume > 0.1mm, NOT flood probability.",
              sprintf("Prob: %.0f%%", row$precip_probability)
            )
          },
          shiny::tags$div(style = "color: #64748B;", sprintf("Gusts: %.0f km/h", row$wind_gust))
        )
      )
    )
  })
  
  shiny::div(
    class = "row row-cols-2 row-cols-md-4 row-cols-lg-7 g-2 mb-3",
    cards
  )
}

#' Render 48-Hour Hourly Trajectory Chart
#'
#' @param hrly_df Data frame of hourly forecasts from fetch_live_weather()
#' @return Plotly widget
render_hourly_trajectory_chart <- function(hrly_df) {
  if (is.null(hrly_df) || nrow(hrly_df) == 0) return(NULL)
  
  p <- ggplot(hrly_df, aes(x = time)) +
    geom_line(aes(y = temperature, color = "Temperature (°C)"), size = 1.0) +
    scale_color_manual(values = c("Temperature (°C)" = "#DC2626")) +
    labs(x = "Time", y = "Temperature (°C)", title = "48-Hour Temperature Trajectory") +
    theme_minimal(base_family = "Plus Jakarta Sans")
  
  ggplotly(p) %>%
    layout(
      font = list(family = "Plus Jakarta Sans, sans-serif", size = 11, color = "#172B25"),
      plot_bgcolor = "rgba(255, 255, 255, 0.7)",
      paper_bgcolor = "transparent",
      margin = list(t = 30, b = 30, l = 40, r = 20)
    )
}

#' Render Official Weather Alerts Cards UI
#'
#' @param alerts_data List returned by fetch_official_weather_alerts()
#' @return Shiny HTML tag
render_official_alerts_ui <- function(alerts_data) {
  if (is.null(alerts_data)) return(NULL)
  
  # Case 1: Alert coverage not supported / key not configured
  if (alerts_data$status == "Alert coverage not supported") {
    return(
      shiny::div(
        class = "card p-3 bg-light border-0 rounded-3 mb-3",
        shiny::div(
          style = "display: flex; align-items: flex-start; gap: 12px;",
          shiny::HTML('<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#64748B" stroke-width="2"><circle cx="12" cy="12" r="10"/><line x1="12" y1="8" x2="12" y2="12"/><line x1="12" y1="16" x2="12.01" y2="16"/></svg>'),
          shiny::div(
            shiny::tags$strong(style = "color: #0F172A; font-size: 0.95rem;", "Official Alerts Unavailable for this Location"),
            shiny::p(style = "margin: 4px 0; font-size: 0.82rem; color: #475569;", alerts_data$message),
            shiny::p(style = "margin: 6px 0 0 0; font-size: 0.76rem; color: #94A3B8; font-style: italic;", alerts_data$disclaimer)
          )
        )
      )
    )
  }
  
  # Case 2: Service unavailable
  if (alerts_data$status == "Alert service unavailable") {
    return(
      shiny::div(
        class = "alert alert-warning py-3 px-3 rounded-3 mb-3",
        style = "font-size: 0.85rem; border-left: 4px solid #F59E0B;",
        shiny::tags$strong("Alert Service Unavailable: "),
        if (!is.null(alerts_data$message)) alerts_data$message else "Could not reach the authoritative alert provider.",
        shiny::div(style = "margin-top: 6px; font-size: 0.76rem; color: #78350F;", alerts_data$disclaimer)
      )
    )
  }
  
  active_list <- alerts_data$alerts_active
  future_list <- alerts_data$alerts_future
  expired_list <- alerts_data$alerts_expired
  
  # Helper to render individual alert card
  build_alert_card <- function(al, card_border_color = "#EF4444", badge_bg = "bg-danger", label = "ACTIVE ALERT") {
    shiny::div(
      class = "card shadow-sm mb-3",
      style = sprintf("border-left: 5px solid %s !important; border-radius: 8px;", card_border_color),
      shiny::div(
        class = "card-header bg-white py-2 px-3",
        style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 6px;",
        shiny::div(
          shiny::tags$strong(style = "font-size: 1rem; color: #0F172A;", al$event_name),
          shiny::tags$span(style = "font-size: 0.78rem; color: #64748B; margin-left: 8px;", paste("Issuing Authority:", al$issuing_authority))
        ),
        shiny::div(
          shiny::tags$span(class = paste("badge", badge_bg), al$severity),
          if (!is.null(al$urgency) && al$urgency != "Unknown") shiny::tags$span(class = "badge bg-secondary ms-1", al$urgency)
        )
      ),
      shiny::div(
        class = "card-body py-2 px-3",
        style = "font-size: 0.84rem;",
        shiny::div(
          style = "color: #475569; margin-bottom: 6px;",
          shiny::tags$strong("Effective: "), al$start_time, " | ",
          shiny::tags$strong("Expires: "), al$expiry_time, " | ",
          shiny::tags$strong("Affected Area: "), al$affected_area
        ),
        if (nchar(al$description) > 0) shiny::p(style = "color: #1E293B; margin-bottom: 6px;", al$description),
        if (nchar(al$instructions) > 0) {
          shiny::div(
            style = "background: #FEF2F2; padding: 6px 10px; border-radius: 6px; color: #991B1B; margin-bottom: 6px;",
            shiny::tags$strong("Recommended Actions: "), al$instructions
          )
        },
        shiny::div(
          style = "font-size: 0.74rem; color: #94A3B8; display: flex; justify-content: space-between;",
          shiny::tags$span(paste("Provider Feed:", al$provider)),
          if (!is.null(al$source_link)) shiny::tags$a(href = al$source_link, target = "_blank", "Official Source Bulletin ↗")
        )
      )
    )
  }
  
  shiny::div(
    # Stale notice if cached data was served
    if (alerts_data$status == "Cached alert data is stale." && !is.null(alerts_data$message)) {
      shiny::div(
        class = "alert alert-warning py-2 px-3 mb-3",
        style = "font-size: 0.84rem;",
        shiny::tags$strong("CACHED / STALE ALERTS: "), alerts_data$message
      )
    },
    
    # Active alerts
    if (length(active_list) > 0) {
      shiny::div(
        shiny::tags$h6(style = "font-weight: 700; color: #DC2626; margin-bottom: 10px;", sprintf("🚨 %d Active Official Alert(s)", length(active_list))),
        lapply(active_list, function(al) build_alert_card(al, card_border_color = "#EF4444", badge_bg = "bg-danger", label = "ACTIVE"))
      )
    } else {
      shiny::div(
        class = "card p-3 bg-light border-0 rounded-3 mb-3",
        shiny::div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="#0D9488" stroke-width="2"><path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"/><polyline points="22 4 12 14.01 9 11.01"/></svg>'),
          shiny::div(
            shiny::tags$strong(style = "color: #0F172A;", "No Active Official Alerts Returned by Provider"),
            shiny::p(style = "margin: 0; font-size: 0.8rem; color: #64748B;", sprintf("Checked authoritative feed (%s) at %s.", alerts_data$provider, alerts_data$fetch_time))
          )
        )
      )
    },
    
    # Future alerts
    if (length(future_list) > 0) {
      shiny::div(
        shiny::tags$h6(style = "font-weight: 700; color: #D97706; margin-top: 14px; margin-bottom: 8px;", sprintf("⏳ %d Future Scheduled Alert(s)", length(future_list))),
        lapply(future_list, function(al) build_alert_card(al, card_border_color = "#F59E0B", badge_bg = "bg-warning text-dark", label = "FUTURE"))
      )
    },
    
    # Expired alerts (collapsible)
    if (length(expired_list) > 0) {
      shiny::tags$details(
        style = "margin-top: 10px;",
        shiny::tags$summary(style = "cursor: pointer; font-size: 0.82rem; color: #64748B;", sprintf("📁 View %d Expired Alert(s)", length(expired_list))),
        shiny::div(
          style = "margin-top: 8px; opacity: 0.75;",
          lapply(expired_list, function(al) build_alert_card(al, card_border_color = "#94A3B8", badge_bg = "bg-secondary", label = "EXPIRED"))
        )
      )
    },
    
    # Safety Disclaimer (Mandatory)
    shiny::div(
      style = "font-size: 0.76rem; color: #94A3B8; font-style: italic; margin-top: 10px;",
      alerts_data$disclaimer
    )
  )
}

#' Render Experimental Condition Indicators UI
#'
#' @param indicators List returned by evaluate_experimental_indicators()
#' @return Shiny HTML tag
render_experimental_indicators_ui <- function(indicators) {
  if (is.null(indicators) || length(indicators) == 0) {
    return(
      shiny::div(
        class = "alert alert-light border py-2 px-3 mb-3",
        style = "font-size: 0.82rem; color: #64748B;",
        "No experimental condition thresholds triggered for the selected daily forecast (daily max temp < threshold, 24h rain < threshold, gusts < threshold)."
      )
    )
  }
  
  cards <- lapply(indicators, function(ind) {
    shiny::div(
      class = "card p-3 mb-2 bg-white border-warning rounded-3 shadow-sm",
      style = "border-left: 4px solid #F59E0B !important;",
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
        shiny::tags$strong(style = "color: #B45309; font-size: 0.92rem;", ind$name),
        shiny::tags$span(class = "badge bg-warning text-dark", sprintf("%.1f %s", ind$value, ind$unit))
      ),
      shiny::div(style = "font-size: 0.8rem; color: #475569; margin-bottom: 4px;", sprintf("Forecast Period: %s | Threshold: %s", ind$period, ind$threshold)),
      shiny::p(style = "margin: 0; font-size: 0.82rem; color: #1E293B; font-weight: 500;", ind$why),
      shiny::p(style = "margin: 4px 0 0 0; font-size: 0.74rem; color: #64748B; font-style: italic;", ind$disclaimer)
    )
  })
  
  shiny::div(cards)
}
