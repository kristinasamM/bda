# R/dashboard_modules.R - Modular Dashboard Components for ClimateWatch

#' Create Mode Banner HTML
#'
#' @param is_demo Logical indicating whether demo data is active
#' @return Shiny HTML tag
render_mode_banner <- function(is_demo = TRUE) {
  if (is_demo) {
    shiny::div(
      class = "demo-banner-premium",
      shiny::div(
        class = "banner-content-left",
        shiny::tags$span(class = "status-dot-pulse amber"),
        shiny::tags$strong("DEMO MODE: "),
        shiny::tags$span(" Displaying offline synthetic demonstration data with realistic Indian climatology & injected test anomalies (2001–2024).")
      ),
      shiny::tags$span(class = "badge-pill-amber", "SYNTHETIC DEMO DATA")
    )
  } else {
    shiny::div(
      class = "real-banner-premium",
      shiny::div(
        class = "banner-content-left",
        shiny::tags$span(class = "status-dot-pulse green"),
        shiny::tags$strong("LIVE PIPELINE: "),
        shiny::tags$span(" Displaying real historical daily gridded estimates retrieved from the NASA POWER API.")
      ),
      shiny::tags$span(class = "badge-pill-green", "NASA POWER OBSERVATIONS")
    )
  }
}

#' Create Premium Metric KPI Card HTML
#'
#' @param title Card title
#' @param value Primary value string
#' @param subtitle Secondary subtitle
#' @param accent Accent color theme: "teal", "blue", "red", "amber"
#' @param icon_svg Raw SVG icon string (optional)
#' @return Shiny HTML div
create_kpi_card <- function(title, value, subtitle = NULL, accent = "teal", icon_svg = NULL) {
  # Default icons based on accent
  default_svg <- switch(accent,
    "teal" = '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 22s-8-4.5-8-11.8A8 8 0 0 1 12 2a8 8 0 0 1 8 8.2c0 7.3-8 11.8-8 11.8z"/><circle cx="12" cy="10" r="3"/></svg>',
    "blue" = '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="4" width="18" height="18" rx="2" ry="2"/><line x1="16" y1="2" x2="16" y2="6"/><line x1="8" y1="2" x2="8" y2="6"/><line x1="3" y1="10" x2="21" y2="10"/></svg>',
    "red" = '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14 14.76V3.5a2.5 2.5 0 0 0-5 0v11.26a4.5 4.5 0 1 0 5 0z"/></svg>',
    "amber" = '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M20 16.2A4.5 4.5 0 0 0 17.5 8h-1.8A7 7 0 1 0 4 14.9"/><path d="M16 14v6"/><path d="M8 14v6"/><path d="M12 16v6"/></svg>',
    '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/></svg>'
  )
  
  icon_html <- if (!is.null(icon_svg)) icon_svg else default_svg

  shiny::div(
    class = paste("kpi-card-premium", accent),
    shiny::div(class = paste("kpi-top-bar", accent)),
    shiny::div(
      class = "kpi-body-flex",
      shiny::div(
        class = "kpi-text-col",
        shiny::div(class = "kpi-title-text", title),
        shiny::div(class = "kpi-number-text", value),
        if (!is.null(subtitle)) shiny::div(class = "kpi-subtitle-text", subtitle)
      ),
      shiny::div(
        class = paste("kpi-icon-container", accent),
        shiny::HTML(icon_html)
      )
    )
  )
}

#' Render Explainable Anomaly Card HTML
#'
#' @param card_data List returned by generate_anomaly_card_data()
#' @return Shiny HTML div
render_anomaly_card_ui <- function(card_data) {
  if (is.null(card_data)) return(NULL)

  sev_class <- tolower(card_data$severity)
  card_classes <- paste("anomaly-explanation-card-premium", sev_class)

  shiny::div(
    class = card_classes,
    shiny::div(
      class = "anomaly-card-header",
      shiny::div(
        class = "anomaly-card-title-group",
        shiny::tags$span(class = "loc-pin-icon", shiny::HTML('<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 10c0 7-9 13-9 13s-9-6-9-13a9 9 0 0 1 18 0z"/><circle cx="12" cy="10" r="3"/></svg>')),
        shiny::span(class = "loc-name-bold", card_data$location),
        shiny::span(class = "date-chip", card_data$month_year)
      ),
      shiny::tags$span(class = paste("badge-pill", card_data$badge_class), card_data$anomaly_type)
    ),
    shiny::div(
      class = "anomaly-narrative-bubble",
      shiny::HTML(paste0('<span class="quote-mark">“</span>', card_data$explanation, '<span class="quote-mark">”</span>'))
    ),
    shiny::div(
      class = "anomaly-metrics-grid",
      shiny::div(
        class = "metric-chip",
        shiny::div(class = "chip-label", "Observed"),
        shiny::div(class = "chip-val highlight", card_data$observed_value)
      ),
      shiny::div(
        class = "metric-chip",
        shiny::div(class = "chip-label", "Baseline Ref"),
        shiny::div(class = "chip-val", card_data$baseline_reference)
      ),
      shiny::div(
        class = "metric-chip",
        shiny::div(class = "chip-label", "Deviation"),
        shiny::div(class = "chip-val", card_data$deviation)
      ),
      shiny::div(
        class = "metric-chip",
        shiny::div(class = "chip-label", "Standardized Score"),
        shiny::div(class = "chip-val", card_data$standardized_score)
      ),
      shiny::div(
        class = "metric-chip",
        shiny::div(class = "chip-label", "Coverage"),
        shiny::div(class = "chip-val", card_data$coverage)
      )
    )
  )
}
