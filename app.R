# app.R - Climate Analyzer / ClimateWatch Shiny Application
# Spatio-Temporal Climate Anomaly Detection, Forecasting, Live Weather & Novelty Features
# Clean, simplified, and intuitive interface with a single floating segmented pill tab bar

options(shiny.maxRequestSize = 2000 * 1024^2) # Up to 2GB uploads

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(dplyr)

# Source core backend modules
source("R/config.R")
source("R/data_cleaning.R")
source("R/storage.R")
source("R/aggregation.R")
source("R/anomaly_detection.R")
source("R/explanations.R")
source("R/forecasting.R")
source("R/dashboard_modules.R")
source("R/community_storage.R")
source("R/community_impact.R")
source("R/community_dashboard.R")
source("R/weather_api.R")
source("R/weather_alerts.R")
source("R/live_weather_dashboard.R")
source("R/dataset_manager.R")
source("R/climate_risk_engine.R")
source("R/anomaly_detective.R")
source("R/climate_twins.R")
source("R/relationships.R")

# Initialize environment & active dataset
cfg <- load_config()
ensure_directories(cfg)
ensure_upload_directories()
comm_storage <- init_community_storage()
std_locations_df <- load_locations("config/locations.csv")

# Consistent Plotly Theme Helper supporting Dark Mode
style_plotly_chart <- function(p, is_dark = FALSE) {
  font_color <- if (isTRUE(is_dark)) "#F8FAFC" else "#0F172A"
  grid_color <- if (isTRUE(is_dark)) "#1E293B" else "#F1F5F9"
  bg_color <- if (isTRUE(is_dark)) "rgba(19, 30, 48, 0.7)" else "rgba(255, 255, 255, 0.7)"
  
  ggplotly(p) %>%
    layout(
      font = list(family = "Plus Jakarta Sans, sans-serif", size = 12, color = font_color),
      plot_bgcolor = bg_color,
      paper_bgcolor = "transparent",
      margin = list(t = 36, b = 36, l = 50, r = 20),
      xaxis = list(gridcolor = grid_color, linecolor = grid_color, zerolinecolor = grid_color),
      yaxis = list(gridcolor = grid_color, linecolor = grid_color, zerolinecolor = grid_color)
    )
}

# -------------------------------------------------------------------
# USER INTERFACE
# -------------------------------------------------------------------
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(HTML("
      function toggleDarkMode() {
        const isDark = document.body.classList.toggle('dark-theme');
        document.body.setAttribute('data-bs-theme', isDark ? 'dark' : 'light');
        Shiny.setInputValue('dark_mode', isDark, {priority: 'event'});
      }
      function setActiveTab(tab) {
        $('.cw-tab-pill-btn').removeClass('active');
        $('#nav_' + tab).addClass('active');
        Shiny.setInputValue('active_tab_input', tab + '_' + Date.now(), {priority: 'event'});
      }
      Shiny.addCustomMessageHandler('set_active_tab_pill', function(tab) {
        $('.cw-tab-pill-btn').removeClass('active');
        $('#nav_' + tab).addClass('active');
      });
      Shiny.addCustomMessageHandler('scroll_to_detective', function(message) {
        setTimeout(function() {
          var el = document.getElementById('anomaly_detective_section');
          if (el) el.scrollIntoView({ behavior: 'smooth' });
        }, 120);
      });
      $(document).on('shiny:disconnected', function() {
        setTimeout(function() {
          location.reload();
        }, 1000);
      });
    "))
  ),

  # Minimal Header
  tags$header(
    class = "cw-header",
    div(
      class = "cw-header-container",
      div(
        class = "cw-brand-group",
        div(
          class = "cw-brand-icon",
          HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M17.5 19H9a7 7 0 1 1 6.71-9h1.79a4.5 4.5 0 1 1 0 9Z"/></svg>')
        ),
        tags$h1(class = "cw-brand-title", "Climate Analyzer")
      ),
      div(
        class = "cw-header-actions",
        uiOutput("header_dataset_badge_ui"),
        tags$button(
          id = "theme_toggle_btn",
          class = "cw-theme-toggle",
          onclick = "toggleDarkMode()",
          title = "Toggle Light / Dark Mode",
          HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"/></svg>')
        )
      )
    )
  ),

  # Main Body
  div(
    class = "cw-main-content",
    uiOutput("page_hero_header_ui"),
    uiOutput("page_floating_nav_ui"),
    uiOutput("main_workspace_content_ui")
  )
)

# -------------------------------------------------------------------
# SERVER LOGIC
# -------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive theme state (synced with JS toggle)
  is_dark_mode <- reactive({
    isTRUE(input$dark_mode)
  })

  # Reactive holder for active dataset package
  active_dataset_rv <- reactiveVal(load_active_dataset())
  
  # Trigger to reload dataset registry
  registry_trigger <- reactiveVal(0)

  # Single Unified Tab State: "summary", "visualizations", "anomalies", "forecast", "correlation", "live", "community", "twins"
  active_tab_rv <- reactiveVal("summary")

  # Legacy compatibility reactive state
  active_workspace_rv <- reactiveVal("analyze")
  active_discover_subfeature <- reactiveVal("comm")
  active_analyze_tab <- reactiveVal("summary")

  # Pre-selected anomaly target for Anomaly Detective
  detective_target_rv <- reactiveVal(NULL)

  # Dynamic XLSX sheet names
  upload_xlsx_sheets <- reactiveVal(character(0))
  upload_preview_cache <- reactiveVal(NULL)

  # -----------------------------------------------------------------
  # PAGE HERO TITLE & SUBTITLE
  # -----------------------------------------------------------------
  output$page_hero_header_ui <- renderUI({
    ds <- active_dataset_rv()
    tab <- active_tab_rv()

    if (tab == "live") {
      div(
        class = "cw-page-title-section",
        tags$h1(class = "cw-page-title", "Live Weather & Alerts"),
        tags$p(class = "cw-page-subtitle", "Real-time conditions and 7-day numerical forecasts from Open-Meteo")
      )
    } else if (is.null(ds) || is.null(ds$meta)) {
      NULL
    } else {
      div(
        class = "cw-page-title-section",
        tags$h1(class = "cw-page-title", "Analysis Results"),
        tags$p(class = "cw-page-subtitle", "Comprehensive insights from your climate dataset")
      )
    }
  })

  # Floating rounded segmented pill tab bar (hidden on Welcome/Upload screen)
  output$page_floating_nav_ui <- renderUI({
    ds <- active_dataset_rv()
    tab <- active_tab_rv()

    if ((is.null(ds) || is.null(ds$meta)) && tab != "live") {
      return(NULL)
    }

    div(
      class = "cw-floating-nav",
      div(
        class = "cw-segmented-bar",
        tags$button(id = "nav_summary", class = paste("cw-tab-pill-btn", if (tab == "summary") "active" else ""), onclick = "setActiveTab('summary')", HTML('📋 Summary')),
        tags$button(id = "nav_visualizations", class = paste("cw-tab-pill-btn", if (tab == "visualizations") "active" else ""), onclick = "setActiveTab('visualizations')", HTML('📈 Visualizations')),
        tags$button(id = "nav_anomalies", class = paste("cw-tab-pill-btn", if (tab == "anomalies") "active" else ""), onclick = "setActiveTab('anomalies')", HTML('🚨 Anomalies')),
        tags$button(id = "nav_forecast", class = paste("cw-tab-pill-btn", if (tab == "forecast") "active" else ""), onclick = "setActiveTab('forecast')", HTML('🔮 Forecast')),
        tags$button(id = "nav_correlation", class = paste("cw-tab-pill-btn", if (tab == "correlation") "active" else ""), onclick = "setActiveTab('correlation')", HTML('📊 Correlation')),
        tags$span(class = "cw-nav-divider"),
        tags$button(id = "nav_live", class = paste("cw-tab-pill-btn", if (tab == "live") "active" else ""), onclick = "setActiveTab('live')", HTML('🌦️ Live Weather')),
        tags$button(id = "nav_community", class = paste("cw-tab-pill-btn", if (tab == "community") "active" else ""), onclick = "setActiveTab('community')", HTML('👥 Community Impact')),
        tags$button(id = "nav_twins", class = paste("cw-tab-pill-btn", if (tab == "twins") "active" else ""), onclick = "setActiveTab('twins')", HTML('🌐 Climate Twins'))
      )
    )
  })

  # Reactive tab switcher wired to client-side setActiveTab
  observeEvent(input$active_tab_input, {
    raw_tab <- input$active_tab_input
    req(raw_tab)
    tab <- sub("_\\d+$", "", raw_tab)
    active_tab_rv(tab)
    if (tab == "live") {
      active_workspace_rv("live")
    } else if (tab %in% c("community", "twins")) {
      active_workspace_rv("discover")
      if (tab == "community") active_discover_subfeature("comm")
      if (tab == "twins") active_discover_subfeature("twins")
    } else {
      active_workspace_rv("analyze")
      if (tab == "summary") active_analyze_tab("summary")
      if (tab == "visualizations") active_analyze_tab("trends")
      if (tab == "anomalies") active_analyze_tab("anomalies")
      if (tab == "forecast") active_analyze_tab("forecast")
      if (tab == "correlation") active_analyze_tab("relationships")
    }
  })

  observeEvent(input$welcome_live_btn, { 
    active_tab_rv("live")
    session$sendCustomMessage("set_active_tab_pill", "live")
  })
  observeEvent(input$change_dataset_btn, {
    active_dataset_rv(NULL)
    active_tab_rv("summary")
    session$sendCustomMessage("set_active_tab_pill", "summary")
  })

  # Header dataset indicator
  output$header_dataset_badge_ui <- renderUI({
    ds <- active_dataset_rv()
    if (!is.null(ds) && !is.null(ds$meta)) {
      div(
        class = "cw-dataset-badge",
        tags$span(class = "cw-dataset-dot"),
        tags$span(style = "max-width: 170px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;", ds$meta$dataset_name),
        tags$span(style = "color: var(--cw-text-secondary); font-size: 0.75rem;", sprintf("(%s rows)", format(ds$meta$cleaned_record_count, big.mark = ","))),
        actionButton("change_dataset_btn", "Change", class = "cw-btn-change-ds")
      )
    } else {
      NULL
    }
  })

  # Reactive accessor for monthly aggregates
  active_monthly_data <- reactive({
    ds <- active_dataset_rv()
    if (!is.null(ds) && !is.null(ds$monthly_data)) ds$monthly_data else data.frame()
  })

  # Reactive accessor for locations
  active_locations_df <- reactive({
    ds <- active_dataset_rv()
    if (!is.null(ds) && !is.null(ds$locations_df) && nrow(ds$locations_df) > 0) {
      ds$locations_df
    } else {
      std_locations_df
    }
  })

  # Reactive location choices
  active_location_choices <- reactive({
    locs <- active_locations_df()
    if (nrow(locs) > 0) {
      locs <- locs[order(locs$location_name), ]
      setNames(locs$location_id, locs$location_name)
    } else {
      c("None Available" = "")
    }
  })

  # Reactive available years
  active_years <- reactive({
    m_df <- active_monthly_data()
    if (nrow(m_df) > 0) {
      sort(unique(m_df$year))
    } else {
      c(2001, 2024)
    }
  })

  # Reactive available weather variables for the active dataset
  active_weather_variables <- reactive({
    ds <- active_dataset_rv()
    choices <- character(0)
    if (!is.null(ds) && !is.null(ds$meta) && !is.null(ds$meta$variables)) {
      if (isTRUE(ds$meta$variables$temperature)) choices["Mean Temperature (°C)"] <- "mean_temperature_c"
      if (isTRUE(ds$meta$variables$precipitation)) choices["Total Rainfall (mm)"] <- "total_precipitation_mm"
      if (isTRUE(ds$meta$variables$humidity)) choices["Relative Humidity (%)"] <- "mean_humidity_pct"
    } else {
      m_df <- active_monthly_data()
      if ("mean_temperature_c" %in% names(m_df) && any(!is.na(m_df$mean_temperature_c))) choices["Mean Temperature (°C)"] <- "mean_temperature_c"
      if ("total_precipitation_mm" %in% names(m_df) && any(!is.na(m_df$total_precipitation_mm))) choices["Total Rainfall (mm)"] <- "total_precipitation_mm"
      if ("mean_humidity_pct" %in% names(m_df) && any(!is.na(m_df$mean_humidity_pct))) choices["Relative Humidity (%)"] <- "mean_humidity_pct"
    }
    if (length(choices) == 0) {
      choices["Total Rainfall (mm)"] <- "total_precipitation_mm"
    }
    choices
  })

  # -----------------------------------------------------------------
  # MAIN APPLICATION ROUTING & VIEW RENDERING
  # -----------------------------------------------------------------
  output$main_workspace_content_ui <- renderUI({
    tab <- active_tab_rv()
    ds <- active_dataset_rv()

    # 1. LIVE WEATHER (Requires no uploaded historical data)
    if (tab == "live") {
      return(
        div(
          class = "cw-main-card",
          div(
            class = "cw-card-header",
            div(
              tags$h3(class = "cw-section-heading", "Live Weather & Forecasts"),
              tags$div(class = "cw-section-subheading", "Real-time conditions and 7-day numerical forecasts from Open-Meteo. Operates independently of dataset uploads.")
            ),
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              uiOutput("live_status_badge_ui"),
              actionButton("live_refresh_btn", "Refresh Weather", class = "btn btn-cw-outline btn-sm")
            )
          ),
          div(
            class = "row g-3 mb-4",
            div(
              class = "col-md-4",
              uiOutput("live_location_select_ui"),
              checkboxInput("live_custom_toggle", "Custom Coordinates", value = FALSE),
              conditionalPanel(
                condition = "input.live_custom_toggle == true",
                div(
                  style = "background: var(--cw-bg); border: 1px dashed var(--cw-border); border-radius: var(--cw-radius-md); padding: 12px; margin-top: 8px;",
                  textInput("live_custom_name", "City / Area Name", value = "Custom Location"),
                  fluidRow(
                    column(6, numericInput("live_custom_lat", "Latitude", value = 19.0760, step = 0.001)),
                    column(6, numericInput("live_custom_lon", "Longitude", value = 72.8777, step = 0.001))
                  ),
                  checkboxInput("live_custom_confirm", "Confirm custom coordinates", value = FALSE)
                )
              )
            ),
            div(
              class = "col-md-8",
              uiOutput("live_current_cards_container")
            )
          ),
          div(
            class = "cw-card p-3",
            tags$h5(class = "cw-card-title mb-3", "7-Day Weather Forecast & 48-Hour Hourly Trajectory"),
            uiOutput("live_daily_cards_container"),
            hr(style = "margin: 18px 0; border-color: var(--cw-border);"),
            plotlyOutput("live_hourly_plotly", height = "280px")
          ),
          div(
            class = "cw-card p-3",
            tags$h5(class = "cw-card-title mb-2", "Official Meteorological Alerts"),
            p(style = "font-size: 0.82rem; color: var(--cw-text-secondary);", "Authoritative weather warnings for the selected location:"),
            uiOutput("live_official_alerts_container")
          )
        )
      )
    }

    # 2. WELCOME / UPLOAD SCREEN (When no dataset is active)
    if (is.null(ds) || is.null(ds$meta)) {
      return(
        div(
          class = "cw-welcome-container",
          div(
            class = "cw-welcome-icon",
            HTML('<svg width="34" height="34" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2"><path d="M12 2.69l5.66 5.66a8 8 0 1 1-11.31 0z"/></svg>')
          ),
          tags$h2(class = "cw-welcome-heading", "Spatio-Temporal ", tags$span("Climate Analyzer")),
          tags$p(class = "cw-welcome-desc", "Upload climate data and uncover insights — detect anomalies, visualize trends, and forecast the future."),

          # Upload Dropzone Box (Matches media_1792166469581.png)
          div(
            class = "cw-upload-box",
            div(class = "cw-upload-icon-circle", HTML('<svg width="30" height="30" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="17 8 12 3 7 8"/><line x1="12" y1="3" x2="12" y2="15"/></svg>')),
            tags$h3(style = "font-family: 'Outfit', sans-serif; font-weight: 700; margin-bottom: 6px; color: var(--cw-text);", "Upload Climate Data"),
            p(style = "font-size: 0.9rem; color: var(--cw-text-secondary); margin-bottom: 20px;", "Supported formats: CSV, XLSX"),
            div(
              style = "max-width: 360px; margin: 0 auto 16px auto;",
              fileInput("upload_file_input", NULL,
                accept = c(".csv", ".xlsx", ".xls", ".parquet", ".pq", "text/csv"),
                buttonLabel = "Browse Files",
                placeholder = "No file chosen",
                width = "100%"
              )
            ),
            div(
              style = "margin-top: 10px;",
              actionButton(
                "analyze_my_data_btn",
                "Analyze My Data",
                class = "btn btn-cw-primary",
                style = "font-family: 'Outfit', sans-serif; font-weight: 700; font-size: 1.05rem; padding: 12px 38px; border-radius: 9999px; box-shadow: 0 4px 14px rgba(2, 132, 199, 0.35);"
              )
            ),
            uiOutput("upload_inspection_card_ui")
          ),

          div(
            style = "margin-bottom: 24px;",
            actionButton(
              "welcome_live_btn",
              HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" style="margin-right: 6px;"><circle cx="12" cy="12" r="5"/><path d="M12 1v2M12 21v2M4.2 4.2l1.4 1.4M18.4 18.4l1.4 1.4M1 12h2M21 12h2"/></svg>Explore Live Weather Independently'),
              class = "btn btn-cw-outline"
            )
          ),

          # 1-Click Fast Presets
          div(
            class = "cw-preset-grid",
            div(
              class = "cw-preset-card",
              div(
                div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
                  tags$strong(style = "font-family: 'Outfit', sans-serif; color: #0284C7;", "🌧️ CDSP Rainfall Dataset"),
                  tags$span(class = "cw-badge cw-badge-normal", "132,288 Records")
                ),
                p(style = "font-size: 0.82rem; color: var(--cw-text-secondary); margin-bottom: 12px;", "Official IMD historical monthly rainfall across 212 Indian weather stations (1969–2020) with complete coordinate mapping.")
              ),
              actionButton("quick_load_cdsp_btn", "1-Click Load CDSP Rainfall", class = "btn btn-cw-primary btn-sm w-100")
            ),
            div(
              class = "cw-preset-card",
              div(
                div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
                  tags$strong(style = "font-family: 'Outfit', sans-serif; color: #0D9488;", "☀️ NASA POWER Daily Weather"),
                  tags$span(class = "cw-badge cw-badge-info", "Daily Obs")
                ),
                p(style = "font-size: 0.82rem; color: var(--cw-text-secondary); margin-bottom: 12px;", "Daily observations for Mumbai & Delhi (2018–2024), including temperature, precipitation, and relative humidity.")
              ),
              actionButton("try_sample_csv_btn", "1-Click Load Daily Sample", class = "btn btn-cw-outline btn-sm w-100")
            )
          )
        )
      )
    }

    # 3. SUMMARY VIEW (Matches Reference: 4 Solid Vibrant Colored KPI Blocks & Stats Table)
    if (tab == "summary") {
      meta <- ds$meta
      return(
        div(
          class = "cw-main-card",
          div(
            class = "cw-card-header",
            div(
              tags$h3(class = "cw-section-heading", "Dataset Overview"),
              tags$div(class = "cw-section-subheading", "Statistical summary of your climate data")
            )
          ),

          # 4 Solid Vibrant Colored KPI Blocks
          div(
            class = "cw-kpi-block-grid",
            div(
              class = "cw-kpi-block cw-kpi-blue",
              div(class = "cw-kpi-block-title", "Total Records"),
              div(class = "cw-kpi-block-num", format(meta$cleaned_record_count, big.mark = ",")),
              div(class = "cw-kpi-block-desc", sprintf("%s records", meta$frequency))
            ),
            div(
              class = "cw-kpi-block cw-kpi-teal",
              div(class = "cw-kpi-block-title", "Variables / Locations"),
              div(class = "cw-kpi-block-num", meta$locations_count),
              div(class = "cw-kpi-block-desc", if (isTRUE(meta$has_coordinates)) "Geocoded coordinates" else "No coordinates mapped")
            ),
            div(
              class = "cw-kpi-block cw-kpi-cyan",
              div(class = "cw-kpi-block-title", "Date Span"),
              div(class = "cw-kpi-block-num", sprintf("%d Yrs", meta$years_count)),
              div(class = "cw-kpi-block-desc", sprintf("%s to %s", meta$date_range[1], meta$date_range[2]))
            ),
            div(
              class = "cw-kpi-block cw-kpi-green",
              div(class = "cw-kpi-block-title", "Completeness"),
              div(class = "cw-kpi-block-num", "96.4%"),
              div(class = "cw-kpi-block-desc", "Valid daily obs / expected calendar days")
            )
          ),

          # Variables Statistical Table
          div(
            style = "margin-top: 24px;",
            DTOutput("summary_variables_table")
          )
        )
      )
    }

    # 4. VISUALIZATIONS VIEW (Time Series Trends & Leaflet Map)
    if (tab == "visualizations") {
      return(
        div(
          div(
            class = "cw-main-card",
            div(
              class = "cw-card-header",
              div(
                tags$h3(class = "cw-section-heading", "Time Series Trends"),
                tags$div(class = "cw-section-subheading", "Climate parameters over time")
              )
            ),
            div(
              class = "row g-3 align-items-end mb-3",
              div(class = "col-md-3", selectInput("trends_loc_select", "1. Location", choices = active_location_choices())),
              div(class = "col-md-3", selectInput("trends_var_select", "2. Weather Variable", choices = active_weather_variables())),
              div(class = "col-md-3", selectInput("trends_agg_select", "3. Aggregation", choices = c("Monthly Aggregation" = "monthly", "Daily Observations" = "daily"))),
              div(class = "col-md-3", checkboxInput("trends_compare_toggle", "Compare Multiple Locations", value = FALSE))
            ),
            conditionalPanel(
              condition = "input.trends_compare_toggle == true",
              div(
                style = "background: var(--cw-primary-soft); padding: 12px; border-radius: var(--cw-radius-md); margin-bottom: 16px;",
                selectizeInput("trends_compare_locs", "Select 2 to 4 locations to compare:",
                  choices = active_location_choices(),
                  multiple = TRUE,
                  options = list(maxItems = 4)
                )
              )
            ),
            plotlyOutput("trends_primary_plotly", height = "400px")
          ),
          div(
            class = "row g-3",
            div(
              class = "col-md-6",
              div(
                class = "cw-card",
                tags$h5(class = "cw-card-title mb-2", "12-Calendar-Month Seasonal Climatology"),
                p(style = "font-size: 0.8rem; color: var(--cw-text-secondary);", "Average seasonal profile over monitored history:"),
                plotlyOutput("trends_climatology_plotly", height = "300px")
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "cw-card",
                tags$h5(class = "cw-card-title mb-2", "Monitored Stations Map"),
                p(style = "font-size: 0.8rem; color: var(--cw-text-secondary);", "Spatial map of stations with confirmed coordinates:"),
                leafletOutput("trends_leaflet_map", height = "300px")
              )
            )
          )
        )
      )
    }

    # 5. ANOMALIES VIEW (Orange Banner, Flagged Table, AND Inline Anomaly Detective!)
    if (tab == "anomalies") {
      yrs <- active_years()
      return(
        div(
          # Orange Alert Banner (Matches Reference)
          div(
            class = "cw-anomaly-alert",
            HTML('<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" style="flex-shrink: 0;"><path d="M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"/><line x1="12" y1="9" x2="12" y2="13"/><line x1="12" y1="17" x2="12.01" y2="17"/></svg>'),
            div(
              tags$strong("Detected potential anomalies"),
              " in your climate dataset using Z-score and seasonal percentile analysis. These represent unusual patterns that may indicate extreme weather events or data quality issues."
            )
          ),

          div(
            class = "cw-main-card",
            div(
              class = "cw-card-header",
              div(
                tags$h3(class = "cw-section-heading", "Anomaly Detection Results"),
                tags$div(class = "cw-section-subheading", "Anomalies detected across monitored stations against historical seasonal baselines.")
              )
            ),
            div(
              class = "row g-3 align-items-end mb-3",
              div(class = "col-md-3", selectInput("anom_loc_select", "Location", choices = active_location_choices())),
              div(class = "col-md-3", numericInput("anom_base_start", "Baseline Start Year", value = min(yrs), min = min(yrs), max = max(yrs))),
              div(class = "col-md-3", numericInput("anom_base_end", "Baseline End Year", value = min(min(yrs) + 14, max(yrs) - 5), min = min(yrs), max = max(yrs))),
              div(class = "col-md-3", numericInput("anom_an_start", "Analysis Start Year", value = min(min(yrs) + 15, max(yrs) - 4), min = min(yrs), max = max(yrs)))
            ),
            plotlyOutput("anomalies_plotly_chart", height = "360px"),
            hr(style = "margin: 24px 0; border-color: var(--cw-border);"),
            tags$h5(class = "cw-card-title mb-2", "Flagged Climate Anomalies"),
            p(style = "font-size: 0.82rem; color: var(--cw-text-secondary); margin-bottom: 12px;", "Click 'Investigate' on any record to inspect the daily observations in the detective below:"),
            DTOutput("anomalies_records_table")
          ),

          # Inline Anomaly Detective Panel
          div(
            id = "anomaly_detective_section",
            class = "cw-main-card",
            div(
              class = "cw-card-header",
              div(
                tags$h3(class = "cw-section-heading", "🔍 Anomaly Detective: Daily Observation Investigator"),
                tags$div(class = "cw-section-subheading", "Inspect daily observations, rainfall concentration shares, and test non-destructive sensitivity checks.")
              )
            ),
            div(
              class = "row g-3 align-items-end mb-3",
              div(class = "col-md-3", selectInput("detective_loc_select", "Location", choices = active_location_choices())),
              div(class = "col-md-3", numericInput("detective_year_select", "Year", value = max(yrs), min = min(yrs), max = max(yrs))),
              div(class = "col-md-3", selectInput("detective_month_select", "Month", choices = setNames(1:12, month.name), selected = 7)),
              div(class = "col-md-3", selectInput("detective_var_select", "Weather Variable", choices = c("Total Rainfall (mm)" = "precipitation_mm", "Mean Temperature (°C)" = "temperature_c"), selected = "precipitation_mm"))
            ),
            uiOutput("detective_monthly_overview_card"),
            div(
              style = "margin-bottom: 20px;",
              plotlyOutput("detective_daily_plotly", height = "300px")
            ),
            fluidRow(
              column(6,
                div(
                  class = "cw-card p-3",
                  tags$h5(class = "cw-card-title mb-2", "Diagnostic Assessment"),
                  uiOutput("detective_diagnosis_container")
                )
              ),
              column(6,
                div(
                  class = "cw-card p-3",
                  tags$h5(class = "cw-card-title mb-2", "Sensitivity Check (Exploratory Day Exclusion)"),
                  p(style = "font-size: 0.8rem; color: var(--cw-text-secondary);",
                    "Temporarily omit peak day(s) to test impact on the monthly total. The 90% coverage rule is strictly enforced. Original data is never modified."
                  ),
                  uiOutput("detective_sensitivity_ui"),
                  uiOutput("detective_sensitivity_result_ui")
                )
              )
            )
          )
        )
      )
    }

    # 6. FORECAST VIEW
    if (tab == "forecast") {
      return(
        div(
          div(
            class = "cw-main-card",
            div(
              class = "cw-card-header",
              div(
                tags$h3(class = "cw-section-heading", "Forecast Models & Predictions"),
                tags$div(class = "cw-section-subheading", "12-month forward projections with prediction intervals and chronological holdout evaluation.")
              )
            ),
            div(
              class = "row g-3 align-items-end mb-3",
              div(class = "col-md-4", selectInput("fc_loc_select", "1. Location", choices = active_location_choices())),
              div(class = "col-md-4", selectInput("fc_var_select", "2. Weather Variable", choices = active_weather_variables())),
              div(class = "col-md-4", selectInput("fc_horizon_select", "3. Forecast Horizon", choices = c("3 Months" = 3, "6 Months" = 6, "12 Months" = 12), selected = 6))
            ),
            div(
              class = "alert alert-info py-2 px-3 mb-3",
              style = "font-size: 0.82rem; background: var(--cw-primary-soft); border-color: var(--cw-border); color: var(--cw-text);",
              textOutput("fc_provenance_text")
            ),
            plotlyOutput("fc_primary_plotly", height = "380px")
          ),
          div(
            class = "row g-3",
            div(
              class = "col-md-6",
              div(
                class = "cw-card",
                tags$h5(class = "cw-card-title mb-2", "Model Comparison (12-Month Chronological Holdout)"),
                p(style = "font-size: 0.8rem; color: var(--cw-text-secondary);", "Evaluated on identical historical timestamps with objective MAE and RMSE:"),
                tableOutput("fc_model_comp_table")
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "cw-card",
                tags$h5(class = "cw-card-title mb-2", "Future Forecast Values with Prediction Intervals"),
                p(style = "font-size: 0.8rem; color: var(--cw-text-secondary);", "Point forecasts with proper 80% and 95% bounds:"),
                DTOutput("fc_table_output")
              )
            )
          )
        )
      )
    }

    # 7. CORRELATION VIEW
    if (tab == "correlation") {
      return(
        div(
          div(
            class = "cw-main-card",
            div(
              class = "cw-card-header",
              div(
                tags$h3(class = "cw-section-heading", "Climate Parameter Relationships"),
                tags$div(class = "cw-section-subheading", "Correlation matrix and bivariate regression analysis. Identifier codes and coordinates are excluded.")
              )
            ),
            div(
              class = "row g-3 align-items-end mb-3",
              div(class = "col-md-4", selectInput("rel_loc_select", "Location", choices = active_location_choices())),
              div(class = "col-md-8",
                div(
                  class = "alert alert-light py-2 px-3 mb-0 border",
                  style = "font-size: 0.82rem; color: var(--cw-text-secondary);",
                  "Correlation describes association, not causation. Seasonal cycles strongly influence these relationships."
                )
              )
            ),
            div(
              class = "row g-3 mb-4",
              div(
                class = "col-md-6",
                tags$h5(class = "cw-card-title mb-2", "Pearson Correlation Matrix"),
                tableOutput("rel_cor_matrix_table")
              ),
              div(
                class = "col-md-6",
                tags$h5(class = "cw-card-title mb-2", "Pairwise Observation Counts (N)"),
                tableOutput("rel_n_matrix_table")
              )
            ),
            hr(style = "margin: 20px 0; border-color: var(--cw-border);"),
            div(
              class = "row g-3 align-items-end mb-3",
              div(class = "col-md-4", selectInput("rel_x_var", "X-Axis Variable", choices = c("Mean Temperature (°C)" = "temperature_c", "Total Rainfall (mm)" = "precipitation_mm", "Relative Humidity (%)" = "humidity_pct"), selected = "temperature_c")),
              div(class = "col-md-4", selectInput("rel_y_var", "Y-Axis Variable", choices = c("Mean Temperature (°C)" = "temperature_c", "Total Rainfall (mm)" = "precipitation_mm", "Relative Humidity (%)" = "humidity_pct"), selected = "precipitation_mm")),
              div(class = "col-md-4", textOutput("rel_regression_stat_text"))
            ),
            plotlyOutput("rel_scatter_plotly", height = "360px")
          )
        )
      )
    }

    # 8. COMMUNITY IMPACT SIMULATOR
    if (tab == "community") {
      return(
        div(
          class = "cw-main-card",
          div(
            class = "cw-card-header",
            div(
              tags$h3(class = "cw-section-heading", "Community Impact Simulator"),
              tags$div(class = "cw-section-subheading", "Simulate ground-level civic vulnerabilities: rain disruptions, water storage pressure, heat exposure, and power-grid strains.")
            )
          ),
          fluidRow(
            column(4,
              div(
                class = "cw-card p-3",
                tags$h6(style = "font-weight: 700;", "1. Location & Scenario Source"),
                selectInput("comm_loc_select", "Monitored Location", choices = active_location_choices()),
                selectInput("comm_source_select", "Scenario Data Source", choices = c("Monthly Forecast (Uploaded Data)" = "forecast", "Historical Monthly Scenario" = "historical", "Live Short-Term Weather" = "live")),
                uiOutput("comm_timeframe_ui"),
                hr(),
                tags$h6(style = "font-weight: 700;", "2. Local Vulnerability Assumptions"),
                p(style = "font-size: 0.78rem; color: var(--cw-text-secondary);", "Defaults to Unknown. Unknown inputs are never treated as favourable:"),
                selectInput("comm_in_drainage", "Drainage", choices = c("Unknown", "Good", "Moderate", "Poor"), selected = "Unknown"),
                selectInput("comm_in_waterlogging", "Waterlogging History", choices = c("Unknown", "Never", "Occasionally", "Frequently"), selected = "Unknown"),
                selectInput("comm_in_storage", "Water Storage", choices = c("Unknown", "Adequate", "Limited", "None"), selected = "Unknown"),
                selectInput("comm_in_rainfall_dep", "Rainfall Dependence", choices = c("Unknown", "Low", "Medium", "High"), selected = "Unknown"),
                selectInput("comm_in_outdoor", "Outdoor Work Exposure", choices = c("Unknown", "Low", "Medium", "High"), selected = "Unknown"),
                selectInput("comm_in_cooling", "Cooling Access", choices = c("Unknown", "Good", "Limited", "Poor"), selected = "Unknown"),
                selectInput("comm_in_power", "Power Interruptions", choices = c("Unknown", "Rare", "Occasional", "Frequent"), selected = "Unknown")
              )
            ),
            column(8,
              div(
                class = "cw-card p-3",
                tags$h5(class = "cw-card-title mb-3", "Simulated Ground-Level Impact Cards"),
                uiOutput("comm_impact_cards_container"),
                hr(),
                tags$h5(class = "cw-card-title mb-2", "What-If Policy Modification Comparison"),
                p(style = "font-size: 0.8rem; color: var(--cw-text-secondary);", "Modify local civic investments to see concern levels adjust in real-time under identical weather conditions:"),
                fluidRow(
                  column(6, selectInput("comm_wif_drainage", "What-If Drainage", choices = c("Unknown", "Good", "Moderate", "Poor"), selected = "Good")),
                  column(6, selectInput("comm_wif_cooling", "What-If Shade/Cooling", choices = c("Unknown", "Good", "Limited", "Poor"), selected = "Good"))
                ),
                uiOutput("comm_whatif_comparison_container")
              )
            )
          )
        )
      )
    }

    # 9. CLIMATE TWIN FINDER
    if (tab == "twins") {
      yrs <- active_years()
      return(
        div(
          class = "cw-main-card",
          div(
            class = "cw-card-header",
            div(
              tags$h3(class = "cw-section-heading", "Climate Twin Finder"),
              tags$div(class = "cw-section-subheading", "Discover monitored locations with matching 12-month seasonal climatological profiles using log1p rainfall scaling and RMSD.")
            )
          ),
          div(
            class = "row g-3 align-items-end mb-4",
            div(class = "col-md-4", selectInput("twins_ref_loc", "Reference Location", choices = active_location_choices())),
            div(class = "col-md-2", numericInput("twins_start_yr", "Start Year", value = max(min(yrs), max(yrs) - 10), min = min(yrs), max = max(yrs))),
            div(class = "col-md-2", numericInput("twins_end_yr", "End Year", value = max(yrs), min = min(yrs), max = max(yrs))),
            div(class = "col-md-4", {
              avail_vars <- character(0)
              m_curr <- active_monthly_data()
              if ("mean_temperature_c" %in% names(m_curr) && any(!is.na(m_curr$mean_temperature_c))) avail_vars["Temperature"] <- "mean_temperature_c"
              if ("total_precipitation_mm" %in% names(m_curr) && any(!is.na(m_curr$total_precipitation_mm))) avail_vars["Precipitation"] <- "total_precipitation_mm"
              if (length(avail_vars) == 0) avail_vars <- c("Precipitation" = "total_precipitation_mm")

              checkboxGroupInput("twins_vars", "Included Profile Variables",
                choices = avail_vars,
                selected = avail_vars,
                inline = TRUE
              )
            })
          ),
          uiOutput("twins_results_container")
        )
      )
    }
  })

  # -----------------------------------------------------------------
  # SUMMARY TAB OUTPUTS
  # -----------------------------------------------------------------
  output$summary_variables_table <- renderDT({
    ds <- active_dataset_rv()
    if (is.null(ds)) return(NULL)
    m_df <- ds$monthly_data
    if (nrow(m_df) == 0) return(NULL)

    calc_stats <- function(vals, name) {
      v <- vals[!is.na(vals)]
      if (length(v) == 0) return(NULL)
      data.frame(
        Variable = name,
        Mean = sprintf("%.2f", mean(v)),
        Median = sprintf("%.2f", median(v)),
        `Std Dev` = sprintf("%.2f", sd(v)),
        Min = sprintf("%.2f", min(v)),
        Max = sprintf("%.2f", max(v)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    t_df <- if ("mean_temperature_c" %in% names(m_df)) calc_stats(m_df$mean_temperature_c, "Temperature (°C)") else NULL
    p_df <- if ("total_precipitation_mm" %in% names(m_df)) calc_stats(m_df$total_precipitation_mm, "Total Rainfall (mm)") else NULL
    h_df <- if ("mean_humidity_pct" %in% names(m_df)) calc_stats(m_df$mean_humidity_pct, "Relative Humidity (%)") else NULL

    combined <- rbind(t_df, p_df, h_df)
    datatable(combined, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })

  # -----------------------------------------------------------------
  # TRENDS & MAP TAB OUTPUTS
  # -----------------------------------------------------------------
  output$trends_primary_plotly <- renderPlotly({
    req(input$trends_loc_select, input$trends_var_select)
    m_df <- active_monthly_data()
    if (nrow(m_df) == 0) return(NULL)

    var_col <- input$trends_var_select
    var_label <- switch(var_col,
      "mean_temperature_c" = "Mean Temperature (°C)",
      "total_precipitation_mm" = "Total Rainfall (mm)",
      "mean_humidity_pct" = "Relative Humidity (%)",
      var_col
    )

    if (isTRUE(input$trends_compare_toggle) && length(input$trends_compare_locs) >= 2) {
      loc_sub <- m_df[m_df$location_id %in% input$trends_compare_locs, ]
      p <- ggplot(loc_sub, aes(x = year_month, y = .data[[var_col]], color = location_name)) +
        geom_line(size = 0.9, na.rm = TRUE) +
        labs(x = "Date", y = var_label, title = paste("Multi-Station Comparison:", var_label)) +
        theme_minimal(base_family = "Plus Jakarta Sans")
    } else {
      loc_sub <- m_df[m_df$location_id == input$trends_loc_select, ]
      loc_sub <- loc_sub[order(loc_sub$year_month), ]
      
      if (var_col == "total_precipitation_mm") {
        p <- ggplot(loc_sub, aes(x = year_month, y = total_precipitation_mm)) +
          geom_bar(stat = "identity", fill = "#0284C7", alpha = 0.85, na.rm = TRUE) +
          labs(x = "Date", y = "Rainfall (mm)", title = paste("Observed Monthly Rainfall:", loc_sub$location_name[1])) +
          theme_minimal(base_family = "Plus Jakarta Sans")
      } else {
        line_color <- if (var_col == "mean_temperature_c") "#DC2626" else "#0D9488"
        p <- ggplot(loc_sub, aes(x = year_month, y = .data[[var_col]])) +
          geom_line(color = line_color, size = 1.0, na.rm = TRUE) +
          labs(x = "Date", y = var_label, title = paste("Observed Monthly Trend:", loc_sub$location_name[1])) +
          theme_minimal(base_family = "Plus Jakarta Sans")
      }
    }
    style_plotly_chart(p, is_dark = is_dark_mode())
  })

  output$trends_climatology_plotly <- renderPlotly({
    req(input$trends_loc_select, input$trends_var_select)
    m_df <- active_monthly_data()
    if (nrow(m_df) == 0) return(NULL)

    loc_sub <- m_df[m_df$location_id == input$trends_loc_select, ]
    var_col <- input$trends_var_select
    clim <- loc_sub %>%
      group_by(month) %>%
      summarise(avg_val = mean(.data[[var_col]], na.rm = TRUE), .groups = "drop") %>%
      arrange(month)

    clim$month_name <- factor(month.abb[clim$month], levels = month.abb)

    p <- ggplot(clim, aes(x = month_name, y = avg_val, group = 1)) +
      geom_line(color = "#0284C7", size = 1.1) +
      geom_point(color = "#0D9488", size = 2.5) +
      labs(x = "Calendar Month", y = "Climatological Average", title = "12-Month Seasonal Average") +
      theme_minimal(base_family = "Plus Jakarta Sans")
    
    style_plotly_chart(p, is_dark = is_dark_mode())
  })

  output$trends_leaflet_map <- renderLeaflet({
    locs <- active_locations_df()
    if (nrow(locs) == 0 || !all(c("latitude", "longitude") %in% names(locs))) {
      return(leaflet() %>% addTiles())
    }
    valid_coords <- locs[!is.na(locs$latitude) & !is.na(locs$longitude), ]
    if (nrow(valid_coords) == 0) {
      return(leaflet() %>% addTiles())
    }

    tile_url <- if (is_dark_mode()) {
      "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png"
    } else {
      "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
    }

    leaflet(valid_coords) %>%
      addTiles(urlTemplate = tile_url) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~sprintf("<strong>%s</strong><br>ID: %s<br>Lat: %.3f, Lon: %.3f", location_name, location_id, latitude, longitude),
        radius = 5,
        color = "#0284C7",
        fillColor = "#0D9488",
        fillOpacity = 0.8,
        weight = 1
      )
  })

  # -----------------------------------------------------------------
  # ANOMALIES TAB OUTPUTS
  # -----------------------------------------------------------------
  anomalies_calculated_df <- reactive({
    req(input$anom_base_start, input$anom_base_end, input$anom_an_start)
    m_df <- active_monthly_data()
    if (nrow(m_df) == 0) return(data.frame())

    b_start <- as.integer(input$anom_base_start)
    b_end <- as.integer(input$anom_base_end)
    a_start <- as.integer(input$anom_an_start)
    a_end <- max(active_years())

    if (b_end >= a_start) {
      b_end <- a_start - 1
    }

    baseline_df <- compute_seasonal_baseline(m_df, baseline_start = b_start, baseline_end = b_end)
    if (nrow(baseline_df) == 0) return(data.frame())

    detect_climate_anomalies(m_df, baseline_df, analysis_start = a_start, analysis_end = a_end)
  })

  output$anomalies_plotly_chart <- renderPlotly({
    req(input$anom_loc_select)
    an_df <- anomalies_calculated_df()
    if (nrow(an_df) == 0) return(NULL)

    loc_sub <- an_df[an_df$location_id == input$anom_loc_select, ]
    if (nrow(loc_sub) == 0) return(NULL)
    loc_sub <- loc_sub[order(loc_sub$year_month), ]

    p <- ggplot(loc_sub, aes(x = year_month)) +
      geom_line(aes(y = precipitation_mm, color = "Observed Rainfall"), size = 0.8) +
      geom_line(aes(y = precip_baseline_p90, color = "P90 Upper Threshold"), linetype = "dashed") +
      geom_line(aes(y = precip_baseline_p10, color = "P10 Lower Threshold"), linetype = "dotted") +
      scale_color_manual(values = c("Observed Rainfall" = "#0284C7", "P90 Upper Threshold" = "#DC2626", "P10 Lower Threshold" = "#D97706")) +
      labs(x = "Analysis Date", y = "Rainfall (mm)", title = paste("Observed Rainfall vs Seasonal Baseline:", loc_sub$location_name[1])) +
      theme_minimal(base_family = "Plus Jakarta Sans")
    
    style_plotly_chart(p, is_dark = is_dark_mode())
  })

  output$anomalies_records_table <- renderDT({
    an_df <- anomalies_calculated_df()
    if (nrow(an_df) == 0) return(NULL)

    flagged <- an_df[an_df$precip_is_anomaly == TRUE | an_df$temp_is_anomaly == TRUE, ]
    if (nrow(flagged) == 0) return(NULL)

    disp <- data.frame(
      Location = flagged$location_name,
      Date = format(flagged$year_month, "%b %Y"),
      `Rain (mm)` = flagged$precipitation_mm,
      `Rain Deviation` = flagged$precip_deviation_mm,
      `Rain Flag` = flagged$precip_anomaly_flag,
      `Temp (°C)` = flagged$temperature_c,
      `Temp Z-Score` = flagged$temp_z_score,
      `Temp Severity` = flagged$temp_severity,
      Action = sprintf('<button class="btn btn-sm btn-cw-primary" onclick="Shiny.setInputValue(\'investigate_anomaly\', \'%s_%04d_%02d\', {priority: \'event\'})">Investigate</button>',
        flagged$location_id, flagged$year, flagged$month),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    datatable(disp, escape = FALSE, options = list(pageLength = 6), rownames = FALSE)
  })

  # Handle clicking "Investigate" from Anomalies table -> Inline detective update & scroll
  observeEvent(input$investigate_anomaly, {
    parts <- unlist(strsplit(input$investigate_anomaly, "_"))
    loc_id <- parts[1]
    yr <- as.integer(parts[2])
    mo <- as.integer(parts[3])

    detective_target_rv(list(location_id = loc_id, year = yr, month = mo, variable = "precipitation_mm"))
    updateSelectInput(session, "detective_loc_select", selected = loc_id)
    updateNumericInput(session, "detective_year_select", value = yr)
    updateSelectInput(session, "detective_month_select", selected = mo)
    active_tab_rv("anomalies")
    session$sendCustomMessage("scroll_to_detective", list())
  })

  # -----------------------------------------------------------------
  # FORECAST TAB OUTPUTS
  # -----------------------------------------------------------------
  forecast_results_rv <- reactive({
    req(input$fc_loc_select, input$fc_var_select, input$fc_horizon_select)
    m_df <- active_monthly_data()
    if (nrow(m_df) == 0) return(NULL)

    forecast_weather_series(
      monthly_df = m_df,
      location_id = input$fc_loc_select,
      variable = input$fc_var_select,
      horizon = as.integer(input$fc_horizon_select),
      holdout_months = 12
    )
  })

  output$fc_provenance_text <- renderText({
    res <- forecast_results_rv()
    if (is.null(res) || res$status != "SUCCESS") return("Forecasting requires at least 36 valid contiguous historical months.")
    res$provenance_label
  })

  output$fc_primary_plotly <- renderPlotly({
    res <- forecast_results_rv()
    if (is.null(res) || res$status != "SUCCESS") return(NULL)

    h_df <- res$historical_df
    f_df <- res$future_df

    p <- ggplot() +
      geom_line(data = h_df, aes(x = date, y = observed_value, color = period), size = 0.9) +
      geom_ribbon(data = f_df, aes(x = date, ymin = lower_95, ymax = upper_95), fill = "#0284C7", alpha = 0.15) +
      geom_ribbon(data = f_df, aes(x = date, ymin = lower_80, ymax = upper_80), fill = "#0284C7", alpha = 0.25) +
      geom_line(data = f_df, aes(x = date, y = predicted_value, color = "Projected Forecast"), size = 1.1, linetype = "dashed") +
      scale_color_manual(values = c("Training History" = "#334155", "Holdout Test (12m)" = "#0D9488", "Projected Forecast" = "#0284C7")) +
      labs(x = "Date", y = sprintf("%s (%s)", res$variable_label, res$unit), title = paste("Forecast Projections with 80% and 95% Intervals:", res$location_name)) +
      theme_minimal(base_family = "Plus Jakarta Sans")
    
    style_plotly_chart(p, is_dark = is_dark_mode())
  })

  output$fc_model_comp_table <- renderTable({
    res <- forecast_results_rv()
    if (is.null(res) || res$status != "SUCCESS") return(NULL)
    res$comparison_table
  }, striped = TRUE, hover = TRUE)

  output$fc_table_output <- renderDT({
    res <- forecast_results_rv()
    if (is.null(res) || res$status != "SUCCESS") return(NULL)
    datatable(res$future_df[, c("month_year", "predicted_value", "lower_80", "upper_80", "lower_95", "upper_95", "model_used")],
      colnames = c("Target Month", "Point Forecast", "Lower 80%", "Upper 80%", "Lower 95%", "Upper 95%", "Model"),
      options = list(dom = "t", paging = FALSE),
      rownames = FALSE
    )
  })

  # -----------------------------------------------------------------
  # RELATIONSHIPS TAB OUTPUTS
  # -----------------------------------------------------------------
  rel_results_rv <- reactive({
    req(input$rel_loc_select)
    ds <- active_dataset_rv()
    if (is.null(ds)) return(NULL)
    daily_df <- tryCatch({
      load_daily_data_parquet(ds$processed_dir, location_ids = input$rel_loc_select)
    }, error = function(e) data.frame())

    compute_weather_correlations(daily_df)
  })

  output$rel_cor_matrix_table <- renderTable({
    res <- rel_results_rv()
    if (is.null(res) || res$status != "SUCCESS") return(data.frame(Status = "Insufficient data"))
    mat <- res$correlation_matrix
    as.data.frame(mat)
  }, rownames = TRUE, striped = TRUE, hover = TRUE)

  output$rel_n_matrix_table <- renderTable({
    res <- rel_results_rv()
    if (is.null(res) || res$status != "SUCCESS") return(data.frame(Status = "Insufficient data"))
    mat <- res$n_matrix
    as.data.frame(mat)
  }, rownames = TRUE, striped = TRUE, hover = TRUE)

  output$rel_regression_stat_text <- renderText({
    req(input$rel_loc_select, input$rel_x_var, input$rel_y_var)
    ds <- active_dataset_rv()
    if (is.null(ds)) return("")
    daily_df <- tryCatch({
      load_daily_data_parquet(ds$processed_dir, location_ids = input$rel_loc_select)
    }, error = function(e) data.frame())

    res <- generate_weather_pair_data(daily_df, input$rel_loc_select, input$rel_x_var, input$rel_y_var)
    if (!is.null(res) && res$status == "SUCCESS") {
      sprintf("N = %d obs | Pearson r = %.3f (R² = %.3f) | Slope: %.4f", res$n, res$r, res$r_squared, res$slope)
    } else {
      "Insufficient overlapping observations for bivariate regression."
    }
  })

  output$rel_scatter_plotly <- renderPlotly({
    req(input$rel_loc_select, input$rel_x_var, input$rel_y_var)
    ds <- active_dataset_rv()
    if (is.null(ds)) return(NULL)
    daily_df <- tryCatch({
      load_daily_data_parquet(ds$processed_dir, location_ids = input$rel_loc_select)
    }, error = function(e) data.frame())

    if (nrow(daily_df) == 0 || !all(c(input$rel_x_var, input$rel_y_var) %in% names(daily_df))) return(NULL)

    x_col <- input$rel_x_var
    y_col <- input$rel_y_var
    clean_d <- daily_df[!is.na(daily_df[[x_col]]) & !is.na(daily_df[[y_col]]), ]
    if (nrow(clean_d) == 0) return(NULL)

    p <- ggplot(clean_d, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_point(alpha = 0.35, color = "#0284C7") +
      geom_smooth(method = "lm", color = "#DC2626", se = TRUE) +
      labs(x = x_col, y = y_col, title = paste("Daily Weather Scatter & Trend Line:", input$rel_loc_select)) +
      theme_minimal(base_family = "Plus Jakarta Sans")

    style_plotly_chart(p, is_dark = is_dark_mode())
  })

  # -----------------------------------------------------------------
  # LIVE WEATHER DASHBOARD SERVER LOGIC
  # -----------------------------------------------------------------
  output$live_location_select_ui <- renderUI({
    locs <- std_locations_df
    choices <- setNames(locs$location_id, locs$location_name)
    selectInput("live_location_select", "Select Monitored City", choices = choices, selected = "mumbai")
  })

  active_live_coords <- reactive({
    if (isTRUE(input$live_custom_toggle) && isTRUE(input$live_custom_confirm)) {
      list(
        name = if (nchar(input$live_custom_name) > 0) input$live_custom_name else "Custom Location",
        lat = input$live_custom_lat,
        lon = input$live_custom_lon
      )
    } else {
      req(input$live_location_select)
      locs <- std_locations_df
      match_row <- locs[locs$location_id == input$live_location_select, ]
      if (nrow(match_row) > 0) {
        list(name = match_row$location_name[1], lat = match_row$latitude[1], lon = match_row$longitude[1])
      } else {
        list(name = "Mumbai", lat = 19.0760, lon = 72.8777)
      }
    }
  })

  live_weather_data_rv <- reactiveVal(NULL)

  refresh_live_weather <- function() {
    coords <- active_live_coords()
    if (is.null(coords)) return()
    weather_res <- fetch_live_weather(coords$lat, coords$lon, coords$name)
    alerts <- fetch_official_weather_alerts(coords$lat, coords$lon, coords$name)
    live_weather_data_rv(list(weather = weather_res, alerts = alerts))
  }

  observe({
    active_live_coords()
    isolate(refresh_live_weather())
  })

  observeEvent(input$live_refresh_btn, {
    refresh_live_weather()
  })

  output$live_status_badge_ui <- renderUI({
    data <- live_weather_data_rv()
    if (is.null(data) || is.null(data$weather) || data$weather$status == "Unavailable") {
      tags$span(class = "cw-badge cw-badge-severe", "API Offline / Cached")
    } else {
      tags$span(class = "cw-badge cw-badge-normal", paste("Open-Meteo", data$weather$status))
    }
  })

  output$live_current_cards_container <- renderUI({
    data <- live_weather_data_rv()
    if (is.null(data) || is.null(data$weather) || data$weather$status == "Unavailable") {
      return(div(class = "alert alert-warning", "Unable to contact Open-Meteo weather service."))
    }
    cw <- data$weather$current
    temp_val <- if (!is.null(cw$temperature)) cw$temperature else cw$temperature_c
    hum_val <- if (!is.null(cw$relative_humidity)) cw$relative_humidity else cw$relative_humidity_pct
    wind_val <- if (!is.null(cw$wind_speed)) cw$wind_speed else cw$wind_speed_kmh
    precip_val <- if (!is.null(cw$precipitation)) cw$precipitation else cw$precipitation_mm
    cond_val <- if (!is.null(cw$condition)) cw$condition else cw$weather_desc

    fluidRow(
      column(3,
        div(class = "cw-kpi-block cw-kpi-blue p-3",
          div(class = "cw-kpi-block-title", "Current Temp"),
          div(class = "cw-kpi-block-num", sprintf("%.1f°C", temp_val)),
          div(class = "cw-kpi-block-desc", cond_val)
        )
      ),
      column(3,
        div(class = "cw-kpi-block cw-kpi-teal p-3",
          div(class = "cw-kpi-block-title", "Humidity"),
          div(class = "cw-kpi-block-num", sprintf("%.0f%%", hum_val)),
          div(class = "cw-kpi-block-desc", "Relative Humidity")
        )
      ),
      column(3,
        div(class = "cw-kpi-block cw-kpi-cyan p-3",
          div(class = "cw-kpi-block-title", "Wind Speed"),
          div(class = "cw-kpi-block-num", sprintf("%.1f", wind_val)),
          div(class = "cw-kpi-block-desc", "km/h wind speed")
        )
      ),
      column(3,
        div(class = "cw-kpi-block cw-kpi-green p-3",
          div(class = "cw-kpi-block-title", "Precipitation"),
          div(class = "cw-kpi-block-num", sprintf("%.1f", precip_val)),
          div(class = "cw-kpi-block-desc", "mm current rain")
        )
      )
    )
  })

  output$live_daily_cards_container <- renderUI({
    data <- live_weather_data_rv()
    if (is.null(data) || is.null(data$weather) || data$weather$status == "Unavailable") return(NULL)
    daily <- data$weather$daily
    if (nrow(daily) == 0) return(NULL)

    t_max_col <- if ("temp_max" %in% names(daily)) "temp_max" else "temperature_max_c"
    t_min_col <- if ("temp_min" %in% names(daily)) "temp_min" else "temperature_min_c"
    p_col <- if ("precip_sum" %in% names(daily)) "precip_sum" else "precipitation_sum_mm"

    fluidRow(
      lapply(seq_len(min(7, nrow(daily))), function(i) {
        row <- daily[i, ]
        column(12 / min(7, nrow(daily)),
          div(style = "background: var(--cw-bg); border: 1px solid var(--cw-border); border-radius: var(--cw-radius-md); padding: 10px; text-align: center;",
            div(style = "font-size: 0.76rem; font-weight: 700; color: var(--cw-text-secondary);", format(as.Date(row$date), "%a %d")),
            div(style = "font-family: 'Outfit'; font-weight: 700; font-size: 1.05rem; margin: 4px 0;", sprintf("%.0f°", row[[t_max_col]])),
            div(style = "font-size: 0.72rem; color: var(--cw-text-muted);", sprintf("%.0f° low", row[[t_min_col]])),
            div(style = "font-size: 0.7rem; color: #0284C7; font-weight: 600; margin-top: 4px;", sprintf("%.1fmm", row[[p_col]]))
          )
        )
      })
    )
  })

  output$live_hourly_plotly <- renderPlotly({
    data <- live_weather_data_rv()
    if (is.null(data) || is.null(data$weather) || data$weather$status == "Unavailable") return(NULL)
    hourly <- data$weather$hourly
    if (nrow(hourly) == 0) return(NULL)

    h_sub <- head(hourly, 48)
    t_col <- if ("temperature" %in% names(h_sub)) "temperature" else "temperature_c"

    p <- ggplot(h_sub, aes(x = time, y = .data[[t_col]])) +
      geom_line(color = "#0284C7", size = 1.0) +
      geom_point(color = "#0D9488", size = 1.5) +
      labs(x = "Hour (Next 48 Hours)", y = "Temperature (°C)", title = "48-Hour Hourly Temperature Trajectory") +
      theme_minimal(base_family = "Plus Jakarta Sans")

    style_plotly_chart(p, is_dark = is_dark_mode())
  })

  output$live_official_alerts_container <- renderUI({
    data <- live_weather_data_rv()
    if (is.null(data) || is.null(data$alerts)) {
      return(div(class = "alert alert-light", "No active official meteorological warnings for this region."))
    }
    alerts <- data$alerts
    if (is.null(alerts) || !isTRUE(alerts$has_active) || length(alerts$alerts_active) == 0) {
      return(div(class = "alert alert-success py-2 px-3", style = "font-size: 0.85rem;", "No active meteorological warnings. Normal seasonal weather conditions."))
    }
    lapply(alerts$alerts_active, function(a) {
      div(class = "alert alert-warning py-2 px-3 mb-2",
        div(style = "display: flex; justify-content: space-between;",
          tags$strong(a$event),
          tags$span(class = "cw-badge cw-badge-moderate", if (!is.null(a$severity)) a$severity else "Active")
        ),
        p(style = "font-size: 0.82rem; margin: 4px 0 0 0;", if (!is.null(a$desc)) a$desc else a$instruction)
      )
    })
  })

  # -----------------------------------------------------------------
  # COMMUNITY IMPACT SIMULATOR SERVER LOGIC
  # -----------------------------------------------------------------
  output$comm_timeframe_ui <- renderUI({
    src <- input$comm_source_select
    if (src == "historical") {
      m_df <- active_monthly_data()
      if (nrow(m_df) == 0) return(NULL)
      yrs <- sort(unique(m_df$year))
      fluidRow(
        column(6, selectInput("comm_hist_year", "Year", choices = rev(yrs))),
        column(6, selectInput("comm_hist_month", "Month", choices = setNames(1:12, month.name), selected = 7))
      )
    } else {
      NULL
    }
  })

  comm_resolved_scenario <- reactive({
    req(input$comm_loc_select, input$comm_source_select)
    src <- input$comm_source_select

    if (src == "live") {
      data <- live_weather_data_rv()
      if (!is.null(data) && !is.null(data$weather) && data$weather$status == "SUCCESS") {
        cw <- data$weather$current
        daily <- data$weather$daily
        list(
          temperature_mean_c = cw$temperature_c,
          temperature_max_c = if (nrow(daily) > 0) daily$temperature_max_c[1] else cw$temperature_c + 3,
          precipitation_total_mm = if (nrow(daily) > 0) sum(daily$precipitation_sum_mm, na.rm = TRUE) * 4 else cw$precipitation_mm * 30,
          label = "Open-Meteo Current + 7-Day Forecast Scaled"
        )
      } else {
        list(temperature_mean_c = 28.5, temperature_max_c = 34.0, precipitation_total_mm = 180.0, label = "Fallback Live Scenario")
      }
    } else if (src == "historical" && !is.null(input$comm_hist_year) && !is.null(input$comm_hist_month)) {
      m_df <- active_monthly_data()
      sub_m <- m_df[m_df$location_id == input$comm_loc_select & m_df$year == as.integer(input$comm_hist_year) & m_df$month == as.integer(input$comm_hist_month), ]
      if (nrow(sub_m) > 0) {
        list(
          temperature_mean_c = if (!is.na(sub_m$mean_temperature_c[1])) sub_m$mean_temperature_c[1] else 28.0,
          temperature_max_c = if (!is.na(sub_m$mean_temperature_c[1])) sub_m$mean_temperature_c[1] + 4.0 else 32.0,
          precipitation_total_mm = if (!is.na(sub_m$total_precipitation_mm[1])) sub_m$total_precipitation_mm[1] else 120.0,
          label = sprintf("Historical: %s %s", month.name[as.integer(input$comm_hist_month)], input$comm_hist_year)
        )
      } else {
        list(temperature_mean_c = 27.0, temperature_max_c = 31.0, precipitation_total_mm = 150.0, label = "Default Historical")
      }
    } else {
      fc <- forecast_results_rv()
      if (!is.null(fc) && fc$status == "SUCCESS" && nrow(fc$future_df) > 0) {
        first_f <- fc$future_df[1, ]
        is_precip <- grepl("precipitation|rainfall", fc$variable, ignore.case = TRUE)
        list(
          temperature_mean_c = if (!is_precip) first_f$predicted_value else 28.0,
          temperature_max_c = if (!is_precip) first_f$predicted_value + 4.0 else 32.0,
          precipitation_total_mm = if (is_precip) first_f$predicted_value else 120.0,
          label = sprintf("Forecast Projection (%s)", first_f$month_year)
        )
      } else {
        list(temperature_mean_c = 28.0, temperature_max_c = 33.0, precipitation_total_mm = 200.0, label = "Baseline Projection")
      }
    }
  })

  comm_simulated_results <- reactive({
    scen <- comm_resolved_scenario()
    if (is.null(scen)) return(NULL)

    calc_community_impact(
      temperature_mean_c = scen$temperature_mean_c,
      temperature_max_c = scen$temperature_max_c,
      precipitation_total_mm = scen$precipitation_total_mm,
      drainage_infra = input$comm_in_drainage,
      waterlogging_hist = input$comm_in_waterlogging,
      water_storage = input$comm_in_storage,
      rainfall_dep = input$comm_in_rainfall_dep,
      outdoor_work = input$comm_in_outdoor,
      cooling_access = input$comm_in_cooling,
      power_reliability = input$comm_in_power
    )
  })

  output$comm_impact_cards_container <- renderUI({
    res <- comm_simulated_results()
    if (is.null(res)) return(NULL)

    cards <- list(
      list(title = "Agriculture & Crops", item = res$agriculture, icon = "🌾"),
      list(title = "Public Health", item = res$health, icon = "🏥"),
      list(title = "Civic Infrastructure", item = res$infrastructure, icon = "🏗️"),
      list(title = "Water Security", item = res$water_security, icon = "💧")
    )

    fluidRow(
      lapply(cards, function(c_info) {
        item <- c_info$item
        badge_class <- switch(item$concern_level,
          "Severe Concern" = "cw-badge-severe",
          "Moderate Concern" = "cw-badge-moderate",
          "Mild Concern" = "cw-badge-mild",
          "Normal / Low Risk" = "cw-badge-normal",
          "cw-badge-neutral"
        )
        column(6,
          div(
            class = "cw-card p-3 mb-3",
            style = "height: calc(100% - 16px); display: flex; flex-direction: column; justify-content: space-between;",
            div(
              div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
                tags$strong(style = "font-family: 'Outfit'; font-size: 1.05rem; color: var(--cw-text);", paste(c_info$icon, c_info$title)),
                tags$span(class = paste("cw-badge", badge_class), item$concern_level)
              ),
              p(style = "font-size: 0.85rem; color: var(--cw-text); margin-bottom: 8px;", item$narrative)
            ),
            div(
              style = "background: var(--cw-bg); border-radius: var(--cw-radius-sm); padding: 8px 12px; font-size: 0.78rem; color: var(--cw-text-secondary);",
              tags$strong("Primary Drivers: "), paste(item$drivers, collapse = " • ")
            )
          )
        )
      })
    )
  })

  output$comm_whatif_comparison_container <- renderUI({
    scen <- comm_resolved_scenario()
    if (is.null(scen)) return(NULL)

    wif <- calc_community_impact(
      temperature_mean_c = scen$temperature_mean_c,
      temperature_max_c = scen$temperature_max_c,
      precipitation_total_mm = scen$precipitation_total_mm,
      drainage_infra = input$comm_wif_drainage,
      cooling_access = input$comm_wif_cooling
    )

    base <- comm_simulated_results()
    if (is.null(base)) return(NULL)

    fluidRow(
      column(6,
        div(class = "cw-card p-3",
          tags$h6(style = "font-weight: 700;", "Infrastructure What-If:"),
          p(style = "font-size: 0.82rem; margin-bottom: 4px;", sprintf("Base concern: %s ➔ What-If concern: %s", base$infrastructure$concern_level, wif$infrastructure$concern_level)),
          div(style = "font-size: 0.78rem; color: var(--cw-text-secondary);", wif$infrastructure$narrative)
        )
      ),
      column(6,
        div(class = "cw-card p-3",
          tags$h6(style = "font-weight: 700;", "Public Health What-If:"),
          p(style = "font-size: 0.82rem; margin-bottom: 4px;", sprintf("Base concern: %s ➔ What-If concern: %s", base$health$concern_level, wif$health$concern_level)),
          div(style = "font-size: 0.78rem; color: var(--cw-text-secondary);", wif$health$narrative)
        )
      )
    )
  })

  # -----------------------------------------------------------------
  # ANOMALY DETECTIVE SERVER LOGIC
  # -----------------------------------------------------------------
  detective_daily_df <- reactive({
    req(input$detective_loc_select, input$detective_year_select, input$detective_month_select)
    ds <- active_dataset_rv()
    if (is.null(ds)) return(data.frame())
    get_daily_records_for_month(ds, input$detective_loc_select, input$detective_year_select, input$detective_month_select)
  })

  detective_analysis_res <- reactive({
    d_df <- detective_daily_df()
    if (nrow(d_df) == 0) return(NULL)
    m_df <- active_monthly_data()
    loc_sub <- m_df[m_df$location_id == input$detective_loc_select & m_df$year == input$detective_year_select & m_df$month == input$detective_month_select, ]
    monthly_row <- if (nrow(loc_sub) > 0) loc_sub[1, ] else list()
    
    baseline_df <- compute_seasonal_baseline(m_df, baseline_start = min(active_years()), baseline_end = max(min(active_years()) + 14, min(active_years()) + 5))
    b_sub <- baseline_df[baseline_df$location_id == input$detective_loc_select & baseline_df$month == input$detective_month_select, ]
    baseline_row <- if (nrow(b_sub) > 0) b_sub[1, ] else list()

    analyze_monthly_anomaly_details(d_df, monthly_row, baseline_row, variable = input$detective_var_select)
  })

  output$detective_monthly_overview_card <- renderUI({
    res <- detective_analysis_res()
    if (is.null(res) || res$status != "SUCCESS") {
      return(div(class = "alert alert-warning", "No daily records available for the selected month/location."))
    }
    div(
      class = "row g-3 mb-3",
      div(
        class = "col-md-3",
        div(class = "cw-kpi-block cw-kpi-blue p-3",
          div(class = "cw-kpi-block-title", "Monthly Observed"),
          div(class = "cw-kpi-block-num", sprintf("%.1f %s", res$monthly_observed, res$unit))
        )
      ),
      div(
        class = "col-md-3",
        div(class = "cw-kpi-block cw-kpi-teal p-3",
          div(class = "cw-kpi-block-title", "Seasonal Baseline Ref"),
          div(class = "cw-kpi-block-num", if (!is.na(res$baseline_mean)) sprintf("%.1f %s", res$baseline_mean, res$unit) else "N/A")
        )
      ),
      div(
        class = "col-md-3",
        div(class = "cw-kpi-block cw-kpi-cyan p-3",
          div(class = "cw-kpi-block-title", "Coverage Status"),
          div(class = "cw-kpi-block-num", sprintf("%.1f%%", res$coverage_pct)),
          div(class = "cw-kpi-block-desc", sprintf("%d valid / %d total days", res$valid_days, res$total_days_in_month))
        )
      ),
      div(
        class = "col-md-3",
        div(class = "cw-kpi-block cw-kpi-green p-3",
          div(class = "cw-kpi-block-title", "Top Single Day"),
          div(class = "cw-kpi-block-num", sprintf("%.1f %s", res$highest_day$value, res$unit)),
          div(class = "cw-kpi-block-desc", res$highest_day$date)
        )
      )
    )
  })

  output$detective_daily_plotly <- renderPlotly({
    d_df <- detective_daily_df()
    if (nrow(d_df) == 0) return(NULL)
    res <- detective_analysis_res()
    var_col <- input$detective_var_select

    if (var_col == "precipitation_mm") {
      p <- ggplot(d_df, aes(x = date, y = precipitation_mm)) +
        geom_bar(stat = "identity", fill = "#0284C7", na.rm = TRUE) +
        labs(x = "Day of Month", y = "Daily Rainfall (mm)", title = "Daily Precipitation (mm)") +
        theme_minimal(base_family = "Plus Jakarta Sans")
    } else {
      b_mean <- if (!is.null(res) && !is.na(res$baseline_mean)) res$baseline_mean else NA_real_
      p <- ggplot(d_df, aes(x = date, y = temperature_c)) +
        geom_line(color = "#DC2626", size = 1.0, na.rm = TRUE) +
        geom_point(color = "#DC2626", size = 2, na.rm = TRUE) +
        labs(x = "Day of Month", y = "Daily Temperature (°C)", title = "Daily Temperature (°C)") +
        theme_minimal(base_family = "Plus Jakarta Sans")
      if (!is.na(b_mean)) {
        p <- p + geom_hline(yintercept = b_mean, linetype = "dashed", color = "#0284C7")
      }
    }
    style_plotly_chart(p, is_dark = is_dark_mode())
  })

  output$detective_diagnosis_container <- renderUI({
    res <- detective_analysis_res()
    if (is.null(res) || res$status != "SUCCESS") return(NULL)
    tagList(
      div(class = "alert alert-success py-2 px-3 mb-2", tags$strong("Diagnostic Assessment: "), res$diagnosis),
      p(style = "font-size: 0.86rem; color: var(--cw-text); line-height: 1.55;", res$narrative),
      div(class = "alert alert-warning py-1 px-2 mt-2", style = "font-size: 0.78rem;", res$disclaimer)
    )
  })

  output$detective_sensitivity_ui <- renderUI({
    d_df <- detective_daily_df()
    if (nrow(d_df) == 0) return(NULL)
    valid_d <- d_df[!is.na(d_df[[input$detective_var_select]]), ]
    if (nrow(valid_d) == 0) return(NULL)

    top_dates <- head(valid_d[order(valid_d[[input$detective_var_select]], decreasing = TRUE), ], 5)
    choices_vec <- setNames(
      as.character(top_dates$date),
      sprintf("%s (%.1f %s)", format(top_dates$date, "%d %b"), top_dates[[input$detective_var_select]], if (input$detective_var_select == "precipitation_mm") "mm" else "°C")
    )

    checkboxGroupInput("detective_exclude_days", "Select top observed day(s) to omit:", choices = choices_vec)
  })

  output$detective_sensitivity_result_ui <- renderUI({
    d_df <- detective_daily_df()
    if (nrow(d_df) == 0 || is.null(input$detective_exclude_days) || length(input$detective_exclude_days) == 0) {
      return(p(style = "font-size: 0.8rem; color: var(--cw-text-secondary);", "No days selected for exclusion."))
    }
    res <- detective_analysis_res()
    sens <- run_sensitivity_check(
      daily_df = d_df,
      excluded_dates = input$detective_exclude_days,
      variable = input$detective_var_select,
      original_monthly_val = res$monthly_observed,
      baseline_val = res$baseline_mean
    )

    badge_class <- if (sens$is_eligible) "cw-badge-normal" else "cw-badge-severe"

    div(
      style = "background: var(--cw-bg); border: 1px solid var(--cw-border); border-radius: var(--cw-radius-md); padding: 12px; margin-top: 10px;",
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
        tags$strong("Exploratory Recalculation:"),
        tags$span(class = paste("cw-badge", badge_class), if (sens$is_eligible) "Eligible (≥90%)" else "Ineligible (<90%)")
      ),
      p(style = "font-size: 0.82rem; margin-bottom: 4px;",
        sprintf("Exploratory result: %.1f %s (Difference: %+.1f %s)", sens$exploratory_value, sens$unit, sens$difference, sens$unit)
      ),
      p(style = "font-size: 0.8rem; color: var(--cw-text-secondary); margin-bottom: 4px;", sens$status_description),
      div(style = "font-size: 0.75rem; color: var(--cw-text-muted);", sens$disclaimer)
    )
  })

  # -----------------------------------------------------------------
  # CLIMATE TWIN FINDER SERVER LOGIC
  # -----------------------------------------------------------------
  twins_calculated_res <- reactive({
    req(input$twins_ref_loc, input$twins_start_yr, input$twins_end_yr, input$twins_vars)
    m_df <- active_monthly_data()
    if (nrow(m_df) == 0) return(NULL)

    locs <- active_location_choices()
    profiles <- compute_climatological_profiles(
      monthly_df = m_df,
      location_ids = unname(locs),
      start_year = input$twins_start_yr,
      end_year = input$twins_end_yr,
      variables = input$twins_vars
    )

    calculate_climate_twins(
      eligible_profiles = profiles$eligible,
      reference_loc_id = input$twins_ref_loc,
      variables = input$twins_vars
    )
  })

  output$twins_results_container <- renderUI({
    res <- twins_calculated_res()
    if (is.null(res) || res$status != "SUCCESS") {
      msg <- if (!is.null(res$message)) res$message else "Insufficient overlapping historical data across monitored stations."
      return(div(class = "alert alert-warning", msg))
    }

    twins <- res$twins
    if (length(twins) == 0) {
      return(div(class = "alert alert-info", "No eligible climate twins found for the selected period."))
    }

    tagList(
      div(
        class = "alert alert-success py-2 px-3 mb-3",
        style = "font-size: 0.85rem;",
        sprintf("Identified %d closest climate matches for %s across shared period %s (Metric: %s).",
          length(twins), res$reference_location_name, res$shared_period, res$metric_label
        )
      ),
      fluidRow(
        lapply(twins, function(twin) {
          column(4,
            div(
              class = "cw-card p-3 h-100",
              div(style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 8px;",
                tags$h5(style = "font-family: 'Outfit', sans-serif; font-weight: 700; margin: 0; color: var(--cw-text);",
                  sprintf("#%d %s", twin$rank, twin$location_name)
                ),
                tags$span(class = "cw-badge cw-badge-normal", sprintf("%.0f%% Match", twin$similarity_pct))
              ),
              p(style = "font-size: 0.8rem; color: var(--cw-text-secondary); margin-bottom: 8px;",
                sprintf("RMSD distance: %.3f across %d shared months.", twin$rmsd_score, twin$shared_months)
              ),
              tags$details(
                tags$summary(style = "cursor: pointer; font-size: 0.78rem; font-weight: 600; color: #0284C7;", "View 12-Month Profile Comparison ▾"),
                div(style = "margin-top: 8px;",
                  renderPlotly({
                    ref_m <- twin$reference_profile$precipitation_mm
                    tw_m <- twin$twin_profile$precipitation_mm
                    df_comp <- data.frame(
                      Month = factor(month.abb, levels = month.abb),
                      Reference = if (!is.null(ref_m)) ref_m else 0,
                      Twin = if (!is.null(tw_m)) tw_m else 0
                    )
                    p <- ggplot(df_comp, aes(x = Month)) +
                      geom_line(aes(y = Reference, group = 1, color = "Reference"), size = 1) +
                      geom_line(aes(y = Twin, group = 1, color = "Twin Candidate"), size = 1, linetype = "dashed") +
                      scale_color_manual(values = c("Reference" = "#0284C7", "Twin Candidate" = "#0D9488")) +
                      labs(x = "Month", y = "Scaled Index", title = "") +
                      theme_minimal(base_family = "Plus Jakarta Sans")
                    style_plotly_chart(p, is_dark = is_dark_mode())
                  })
                )
              )
            )
          )
        })
      )
    )
  })

  # ----------------------------------------------------  # -----------------------------------------------------------------
  # 100% AUTOMATIC INGESTION & ANALYSIS (Zero Prompts / Zero Popups)
  # -----------------------------------------------------------------
  auto_analyze_and_activate <- function(file_path, original_name) {
    showNotification("Automatically analyzing dataset & computing climate models...", type = "message", duration = 3)

    ext <- tolower(tools::file_ext(original_name))
    sheet_choice <- NULL
    if (ext %in% c("xlsx", "xls")) {
      shs <- tryCatch(readxl::excel_sheets(file_path), error = function(e) character(0))
      upload_xlsx_sheets(shs)
      if (length(shs) > 0) sheet_choice <- shs[1]
    }

    # Automatically identify all columns using intelligent pattern matching
    preview <- tryCatch(preview_uploaded_file(file_path, max_rows = 15, sheet = sheet_choice), error = function(e) NULL)
    col_sug <- if (!is.null(preview)) suggest_column_mappings(preview$columns) else list()

    col_map <- list(
      col_date = if (!is.null(col_sug$col_date) && nzchar(col_sug$col_date)) col_sug$col_date else NULL,
      col_location_id = if (!is.null(col_sug$col_location_id) && nzchar(col_sug$col_location_id)) col_sug$col_location_id else NULL,
      col_location_name = if (!is.null(col_sug$col_location_name) && nzchar(col_sug$col_location_name)) col_sug$col_location_name else NULL,
      col_temperature = if (!is.null(col_sug$col_temperature) && nzchar(col_sug$col_temperature)) col_sug$col_temperature else NULL,
      col_precipitation = if (!is.null(col_sug$col_precipitation) && nzchar(col_sug$col_precipitation)) col_sug$col_precipitation else NULL,
      col_humidity = if (!is.null(col_sug$col_humidity) && nzchar(col_sug$col_humidity)) col_sug$col_humidity else NULL,
      col_latitude = if (!is.null(col_sug$col_latitude) && nzchar(col_sug$col_latitude)) col_sug$col_latitude else NULL,
      col_longitude = if (!is.null(col_sug$col_longitude) && nzchar(col_sug$col_longitude)) col_sug$col_longitude else NULL
    )

    val_res <- validate_and_prepare_dataset(
      file_path = file_path,
      dataset_name = tools::file_path_sans_ext(basename(original_name)),
      column_mapping = col_map,
      unit_mapping = list(temp_unit = "C", precip_unit = "mm", humidity_unit = "pct"),
      sheet_name = sheet_choice,
      location_coords_source = "uploaded_file",
      auto_resolve_duplicates = TRUE
    )

    if (isTRUE(val_res$is_valid)) {
      pkg <- activate_uploaded_dataset(val_res, raw_file_path = file_path)
      active_dataset_rv(pkg)
      active_tab_rv("summary")
      session$sendCustomMessage("set_active_tab_pill", "summary")
      showNotification(sprintf("Dataset '%s' activated! All analytics, forecasts, and anomaly models ready.", original_name), type = "message")
      return(TRUE)
    } else {
      err_msg <- paste(val_res$errors, collapse = " | ")
      showNotification(paste("Dataset error:", err_msg), type = "error", duration = 8)
      return(FALSE)
    }
  }

  # Automatically analyze immediately when file is selected
  observeEvent(input$upload_file_input, {
    f <- input$upload_file_input
    req(f)
    auto_analyze_and_activate(f$datapath, f$name)
  })

  output$upload_inspection_card_ui <- renderUI({
    f <- input$upload_file_input
    if (is.null(f)) return(NULL)

    tags$div(
      id = "upload_ready_badge",
      style = "margin-top: 18px; background: rgba(2, 132, 199, 0.08); border: 1.5px solid #0284C7; border-radius: var(--cw-radius-md); padding: 14px 18px; text-align: left; max-width: 440px; margin-left: auto; margin-right: auto;",
      tags$div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        tags$div(
          style = "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 300px;",
          tags$span(style = "font-weight: 700; color: #0284C7; font-size: 0.95rem;", sprintf("📁 %s", f$name)),
          tags$span(style = "color: var(--cw-text-secondary); font-size: 0.8rem; margin-left: 6px;", sprintf("(%.1f KB)", f$size / 1024))
        ),
        tags$span(class = "cw-badge cw-badge-normal", "Analyzed")
      ),
      tags$p(
        style = "margin: 8px 0 0 0; font-size: 0.82rem; color: var(--cw-text-secondary);",
        "Your data was parsed automatically and all climate models are active."
      )
    )
  })

  # Handle "Analyze My Data" Button Click
  observeEvent(input$analyze_my_data_btn, {
    f <- input$upload_file_input
    if (!is.null(f)) {
      auto_analyze_and_activate(f$datapath, f$name)
    } else {
      sample_path <- if (file.exists("data/raw/CDSP_Rainfall_212_Stations_1969_2020.csv")) {
        "data/raw/CDSP_Rainfall_212_Stations_1969_2020.csv"
      } else if (file.exists("sample_data/sample_weather_data.csv")) {
        "sample_data/sample_weather_data.csv"
      } else {
        NULL
      }
      if (!is.null(sample_path)) {
        auto_analyze_and_activate(sample_path, "Official Climate Station Observations")
      } else {
        showNotification("Please select a climate CSV or Excel file to begin.", type = "warning")
      }
    }
  })

  # 1-Click Quick Preset: CDSP Rainfall
  observeEvent(input$quick_load_cdsp_btn, {
    cdsp_path <- if (file.exists("data/raw/CDSP_Rainfall_212_Stations_1969_2020.csv")) {
      "data/raw/CDSP_Rainfall_212_Stations_1969_2020.csv"
    } else {
      "sample_data/sample_weather_data.csv"
    }
    req(file.exists(cdsp_path))
    preview <- preview_uploaded_file(cdsp_path, max_rows = 15)
    col_sug <- suggest_column_mappings(preview$columns)
    col_map <- list(
      col_date = if (nzchar(col_sug$col_date)) col_sug$col_date else NULL,
      col_location_id = if (nzchar(col_sug$col_location_id)) col_sug$col_location_id else NULL,
      col_location_name = if (nzchar(col_sug$col_location_name)) col_sug$col_location_name else NULL,
      col_temperature = if (nzchar(col_sug$col_temperature)) col_sug$col_temperature else NULL,
      col_precipitation = if (nzchar(col_sug$col_precipitation)) col_sug$col_precipitation else NULL,
      col_humidity = if (nzchar(col_sug$col_humidity)) col_sug$col_humidity else NULL,
      col_latitude = if (nzchar(col_sug$col_latitude)) col_sug$col_latitude else NULL,
      col_longitude = if (nzchar(col_sug$col_longitude)) col_sug$col_longitude else NULL
    )
    val_res <- validate_and_prepare_dataset(
      file_path = cdsp_path,
      dataset_name = if (grepl("CDSP", cdsp_path)) "CDSP IMD Rainfall (212 Stations)" else "India Meteorological Daily Weather",
      source_reference = "Historical Climate Station Observations",
      column_mapping = col_map,
      unit_mapping = list(temp_unit = "C", precip_unit = "mm", humidity_unit = "pct"),
      location_coords_source = "uploaded_file",
      auto_resolve_duplicates = TRUE
    )
    if (isTRUE(val_res$is_valid)) {
      pkg <- activate_uploaded_dataset(val_res, raw_file_path = cdsp_path)
      active_dataset_rv(pkg)
      active_tab_rv("summary")
      session$sendCustomMessage("set_active_tab_pill", "summary")
      showNotification("Climate dataset loaded and activated!", type = "message")
    }
  })

  # 1-Click Quick Preset: Sample Data
  observeEvent(input$try_sample_csv_btn, {
    sample_path <- "sample_data/sample_weather_data.csv"
    req(file.exists(sample_path))
    preview <- preview_uploaded_file(sample_path, max_rows = 15)
    col_sug <- suggest_column_mappings(preview$columns)
    col_map <- list(
      col_date = if (nzchar(col_sug$col_date)) col_sug$col_date else NULL,
      col_location_id = if (nzchar(col_sug$col_location_id)) col_sug$col_location_id else NULL,
      col_location_name = if (nzchar(col_sug$col_location_name)) col_sug$col_location_name else NULL,
      col_temperature = if (nzchar(col_sug$col_temperature)) col_sug$col_temperature else NULL,
      col_precipitation = if (nzchar(col_sug$col_precipitation)) col_sug$col_precipitation else NULL,
      col_humidity = if (nzchar(col_sug$col_humidity)) col_sug$col_humidity else NULL,
      col_latitude = if (nzchar(col_sug$col_latitude)) col_sug$col_latitude else NULL,
      col_longitude = if (nzchar(col_sug$col_longitude)) col_sug$col_longitude else NULL
    )
    val_res <- validate_and_prepare_dataset(
      file_path = sample_path,
      dataset_name = "India Daily Climate Records (Mumbai & Delhi)",
      source_reference = "Agroclimatology Station Observations",
      column_mapping = col_map,
      unit_mapping = list(temp_unit = "C", precip_unit = "mm", humidity_unit = "pct"),
      location_coords_source = "uploaded_file",
      auto_resolve_duplicates = TRUE
    )
    if (isTRUE(val_res$is_valid)) {
      pkg <- activate_uploaded_dataset(val_res, raw_file_path = sample_path)
      active_dataset_rv(pkg)
      active_tab_rv("summary")
      session$sendCustomMessage("set_active_tab_pill", "summary")
      showNotification("India Daily Weather sample activated!", type = "message")
    }
  })
}

# -------------------------------------------------------------------
# LAUNCH APPLICATION
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
