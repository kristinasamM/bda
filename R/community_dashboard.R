# R/community_dashboard.R - UI Builders and Render Helpers for Community Impact Simulator
# Matches ClimateWatch design system (Plus Jakarta Sans, curated bslib tokens, custom CSS)

#' Render Concern Level Badge HTML
#'
#' @param level Character concern level string
#' @return Shiny HTML span
render_concern_badge <- function(level) {
  badge_cfg <- switch(level,
    "High" = list(class = "badge bg-danger", icon = '<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"/><line x1="12" y1="9" x2="12" y2="13"/><line x1="12" y1="17" x2="12.01" y2="17"/></svg>', label = "High Concern"),
    "Moderate" = list(class = "badge bg-warning text-dark", icon = '<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"><circle cx="12" cy="12" r="10"/><line x1="12" y1="8" x2="12" y2="12"/><line x1="12" y1="16" x2="12.01" y2="16"/></svg>', label = "Moderate Concern"),
    "Low" = list(class = "badge bg-success", icon = '<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"/><polyline points="22 4 12 14.01 9 11.01"/></svg>', label = "Low Concern"),
    "No trigger from selected monthly outlook" = list(class = "badge bg-secondary", icon = '<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><line x1="4.93" y1="4.93" x2="19.07" y2="19.07"/></svg>', label = "No Climate Trigger"),
    list(class = "badge bg-light text-secondary border", icon = '<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><path d="M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"/><line x1="12" y1="17" x2="12.01" y2="17"/></svg>', label = "Insufficient Information")
  )
  
  shiny::tags$span(
    class = paste("badge-pill", badge_cfg$class),
    style = "display: inline-flex; align-items: center; gap: 5px; font-weight: 700; padding: 5px 11px; font-size: 0.8rem; border-radius: 20px;",
    shiny::HTML(badge_cfg$icon),
    shiny::tags$span(badge_cfg$label)
  )
}

#' Render Single Explainable Impact Card UI
#'
#' @param card List containing card assessment fields
#' @param card_id Unique string ID for HTML collapse toggles
#' @return Shiny HTML div card
render_impact_card_ui <- function(card, card_id) {
  if (is.null(card)) return(NULL)
  
  collapse_id <- paste0("collapse_", card_id)
  
  # Border color accent based on concern level
  accent_border <- switch(card$concern_level,
    "High" = "border-danger",
    "Moderate" = "border-warning",
    "Low" = "border-success",
    "No trigger from selected monthly outlook" = "border-secondary",
    "border-light"
  )
  
  shiny::div(
    class = paste("card shadow-sm mb-3", accent_border),
    style = "border-width: 2px; border-radius: 12px; overflow: hidden; background: #FFFFFF;",
    shiny::div(
      class = "card-header bg-white py-3",
      style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #F1F5F9;",
      shiny::tags$h6(
        style = "margin: 0; font-family: 'Outfit', sans-serif; font-weight: 700; color: #0F172A; font-size: 1.02rem;",
        card$title
      ),
      render_concern_badge(card$concern_level)
    ),
    shiny::div(
      class = "card-body py-3",
      # Climate signal & Vulnerability summary row
      shiny::div(
        style = "background: #F8FAFC; border-radius: 8px; padding: 10px 14px; margin-bottom: 12px; font-size: 0.83rem;",
        shiny::div(
          style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
          shiny::tags$strong(style = "color: #475569;", "Climate Outlook Signal:"),
          shiny::tags$span(
            class = if (card$active_signal) "text-primary fw-bold" else "text-muted",
            card$climate_signal
          )
        ),
        shiny::div(
          style = "display: flex; justify-content: space-between;",
          shiny::tags$strong(style = "color: #475569;", "Relevant Local Inputs:"),
          shiny::tags$span(
            style = "color: #1E293B;",
            paste(sapply(names(card$inputs), function(k) sprintf("%s: %s", tools::toTitleCase(gsub("_", " ", k)), card$inputs[[k]])), collapse = " | ")
          )
        )
      ),
      
      # Plain-language narrative explanation
      shiny::p(
        style = "font-size: 0.88rem; line-height: 1.5; color: #334155; margin-bottom: 12px;",
        card$explanation
      ),
      
      # Missing inputs alert (if any)
      if (length(card$missing_inputs) > 0) {
        shiny::div(
          class = "alert alert-light border py-2 px-3 mb-3",
          style = "font-size: 0.8rem; color: #64748B; border-radius: 8px; background: #FFFBEB;",
          shiny::tags$strong(style = "color: #B45309;", "Unspecified Parameters: "),
          paste(card$missing_inputs, collapse = ", "),
          " (defaulted to Unknown rather than assuming low vulnerability)."
        )
      },
      
      # Practical Preparedness Suggestions
      shiny::tags$div(
        style = "margin-top: 8px;",
        shiny::tags$strong(style = "font-size: 0.82rem; text-transform: uppercase; letter-spacing: 0.04em; color: #0D9488;", "Practical Community Suggestions:"),
        shiny::tags$ul(
          style = "font-size: 0.83rem; color: #475569; padding-left: 20px; margin-top: 6px; margin-bottom: 12px;",
          lapply(card$suggestions, function(s) shiny::tags$li(s))
        )
      ),
      
      # Expandable "How this was calculated" section
      shiny::tags$div(
        style = "border-top: 1px dashed #E2E8F0; padding-top: 8px; margin-top: 8px;",
        shiny::tags$button(
          class = "btn btn-sm btn-link p-0 text-decoration-none text-muted",
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", collapse_id),
          `aria-expanded` = "false",
          style = "font-size: 0.78rem; display: flex; align-items: center; gap: 4px;",
          shiny::HTML('<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="6 9 12 15 18 9"/></svg>'),
          "How this was calculated (demonstration rules)"
        ),
        shiny::div(
          class = "collapse mt-2",
          id = collapse_id,
          shiny::div(
            class = "card card-body bg-light py-2 px-3",
            style = "font-size: 0.77rem; color: #64748B; font-family: monospace; border-radius: 6px;",
            shiny::tags$strong(style = "font-family: sans-serif; color: #334155; margin-bottom: 2px;", "Applied Evaluation Logic:"),
            card$calculation_rule,
            shiny::tags$div(style = "margin-top: 4px; font-family: sans-serif;", "Evaluated in strict precedence: Unavailable -> Inactive Signal -> High -> Moderate -> Low -> Insufficient Information.")
          )
        )
      )
    )
  )
}

#' Render What-If Comparison Summary Cards UI
#'
#' @param comparison List returned from compare_whatif_scenarios()
#' @return Shiny HTML div
render_whatif_comparison_ui <- function(comparison) {
  if (is.null(comparison)) return(NULL)
  
  # Summary badge of changed inputs
  change_pills <- if (comparison$total_changes > 0) {
    lapply(comparison$changed_inputs, function(ci) {
      shiny::tags$span(
        class = "badge bg-primary bg-opacity-10 text-primary border border-primary border-opacity-25 me-1 mb-1",
        style = "font-size: 0.76rem; font-weight: 600; padding: 4px 8px;",
        sprintf("%s: %s → %s", ci$name, ci$from, ci$to)
      )
    })
  } else {
    shiny::tags$span(class = "badge bg-secondary bg-opacity-10 text-muted", "No inputs modified yet (What-If matches Current profile)")
  }
  
  shiny::div(
    shiny::div(
      class = "mb-3 p-3 bg-white rounded-3 border",
      shiny::tags$strong(style = "font-size: 0.85rem; color: #0F172A; display: block; margin-bottom: 6px;", "Modified Assumptions:"),
      shiny::div(style = "display: flex; flex-wrap: wrap; gap: 4px;", change_pills)
    ),
    
    # 4 Comparison rows
    shiny::div(
      class = "list-group",
      lapply(comparison$comparisons, function(cmp) {
        row_bg <- if (cmp$is_changed) "#F0FDF4" else "#FFFFFF"
        border_col <- if (cmp$is_changed) "#86EFAC" else "#E2E8F0"
        
        shiny::div(
          class = "list-group-item p-3 mb-2 rounded border",
          style = sprintf("background: %s; border-color: %s !important;", row_bg, border_col),
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
            shiny::tags$h6(style = "margin: 0; font-weight: 700; color: #0F172A;", cmp$title),
            shiny::div(
              style = "display: flex; align-items: center; gap: 8px;",
              shiny::tags$span(style = "font-size: 0.8rem; color: #64748B;", "Current:"),
              render_concern_badge(cmp$current_level),
              shiny::HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="#64748B" stroke-width="2"><polyline points="9 18 15 12 9 6"/></svg>'),
              shiny::tags$span(style = "font-size: 0.8rem; color: #64748B;", "What-If:"),
              render_concern_badge(cmp$whatif_level)
            )
          ),
          shiny::p(style = "margin: 0; font-size: 0.82rem; color: #334155;", cmp$explanation)
        )
      })
    )
  )
}
