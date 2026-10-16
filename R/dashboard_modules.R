#' Create Active Dataset Top Banner HTML
#'
#' Displayed persistently across historical analysis pages to indicate the current dataset.
#'
#' @param active_ds Active dataset list or NULL
#' @return Shiny HTML tag
render_active_dataset_banner <- function(active_ds = NULL) {
  if (is.null(active_ds)) {
    shiny::div(
      class = "active-dataset-banner empty",
      style = "background: linear-gradient(90deg, #FFFBEB 0%, #FEF3C7 50%, #FDE68A 100%); border: 1px solid #FCD34D; border-left: 5px solid #F59E0B; border-radius: 10px; padding: 10px 18px; margin-bottom: 18px; display: flex; justify-content: space-between; align-items: center; box-shadow: 0 1px 3px rgba(0,0,0,0.04); flex-wrap: wrap; gap: 8px;",
      shiny::div(
        style = "display: flex; align-items: center; gap: 10px;",
        shiny::tags$span(style = "color: #D97706; font-size: 1.2rem;", shiny::HTML('⚠️')),
        shiny::tags$div(
          shiny::tags$strong(style = "color: #92400E;", "No Active Historical Dataset: "),
          shiny::tags$span(style = "color: #78350F; font-size: 0.88rem;", "Historical trends, baseline anomalies, and forecasts require an activated weather dataset.")
        )
      ),
      shiny::actionButton("nav_to_upload_btn", "Upload Historical Data", class = "btn btn-sm btn-primary", style = "font-weight: 600; font-size: 0.82rem; padding: 4px 12px;")
    )
  } else {
    meta <- active_ds$meta
    ds_name <- if (!is.null(meta$dataset_name)) meta$dataset_name else active_ds$dataset_id
    ds_id <- active_ds$dataset_id
    date_str <- if (!is.null(meta$date_range)) paste(meta$date_range[1], "to", meta$date_range[2]) else "N/A"
    loc_count <- if (!is.null(meta$locations_count)) meta$locations_count else nrow(active_ds$locations_df)
    rec_count <- if (!is.null(meta$cleaned_record_count)) format(meta$cleaned_record_count, big.mark = ",") else "N/A"

    shiny::div(
      class = "active-dataset-banner active",
      style = "background: linear-gradient(90deg, #F0FDF4 0%, #DCFCE7 50%, #BBF7D0 100%); border: 1px solid #86EFAC; border-left: 5px solid #16A34A; border-radius: 10px; padding: 10px 18px; margin-bottom: 18px; display: flex; justify-content: space-between; align-items: center; box-shadow: 0 1px 3px rgba(0,0,0,0.04); flex-wrap: wrap; gap: 8px;",
      shiny::div(
        style = "display: flex; align-items: center; gap: 10px;",
        shiny::tags$span(style = "color: #15803D; font-size: 1.2rem;", shiny::HTML('🌿')),
        shiny::tags$div(
          shiny::tags$strong(style = "color: #14532D;", paste0("Active Dataset: ", ds_name)),
          shiny::tags$span(style = "font-size: 0.82rem; color: #166534; margin-left: 8px;",
            sprintf("(%s records | %d location%s | %s)", rec_count, loc_count, if (loc_count != 1) "s" else "", date_str)
          )
        )
      ),
      shiny::div(
        style = "display: flex; align-items: center; gap: 8px;",
        shiny::tags$span(class = "badge bg-success", style = "font-size: 0.72rem; letter-spacing: 0.04em;", "ACTIVE PIPELINE"),
        shiny::actionButton("nav_manage_datasets", "Manage / Switch", class = "btn btn-sm btn-outline-success", style = "font-size: 0.78rem; padding: 3px 10px;")
      )
    )
  }
}

#' Create Empty State Placeholder Card HTML
#'
#' @param feature_name Name of the feature (e.g. "Climate Overview", "Anomaly Explorer")
#' @param message Specific guidance message
#' @return Shiny HTML div
render_historical_empty_state <- function(feature_name, message = "Please upload an actual historical weather dataset to calculate series and view analyses.") {
  shiny::div(
    class = "card p-5 text-center empty-state-card",
    style = "background: #FFFFFF; border: 1px dashed #CBD5E1; border-radius: 14px; margin: 20px 0; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.03);",
    shiny::div(
      style = "max-width: 540px; margin: 0 auto;",
      shiny::tags$div(
        style = "width: 64px; height: 64px; margin: 0 auto 16px; border-radius: 50%; background: #F0FDF4; border: 2px solid #86EFAC; display: flex; align-items: center; justify-content: center;",
        shiny::HTML('<svg width="32" height="32" viewBox="0 0 24 24" fill="none" stroke="#16A34A" stroke-width="2"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="17 8 12 3 7 8"/><line x1="12" y1="3" x2="12" y2="15"/></svg>')
      ),
      shiny::tags$h4(style = "font-family: 'Outfit', sans-serif; font-weight: 700; color: #0F172A; margin-bottom: 8px;", paste("Upload Data to View", feature_name)),
      shiny::p(style = "color: #64748B; font-size: 0.92rem; line-height: 1.5; margin-bottom: 20px;", message),
      shiny::div(
        style = "display: flex; gap: 10px; justify-content: center; flex-wrap: wrap;",
        shiny::actionButton("empty_state_goto_upload", "Go to Data Upload", class = "btn btn-primary px-4", style = "font-weight: 600;"),
        shiny::actionButton("empty_state_try_sample", "Try Sample Weather Dataset", class = "btn btn-outline-success px-3")
      )
    )
  )
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
