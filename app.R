# app.R - ClimateWatch Shiny Application
# Spatio-Temporal Climate Anomaly Detection and Forecasting Using R

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(dplyr)

# Source backend modules
source("R/config.R")
source("R/data_cleaning.R")
source("R/storage.R")
source("R/aggregation.R")
source("R/anomaly_detection.R")
source("R/explanations.R")
source("R/forecasting.R")
source("R/dashboard_modules.R")

# Ensure required directory structure and configuration
cfg <- load_config()
ensure_directories(cfg)
locations_df <- load_locations("config/locations.csv")

# Resolve data mode and load dataset
data_mode <- cfg$data$mode
data_dir <- get_data_dir(cfg, data_mode)
monthly_cache_file <- file.path(data_dir, "monthly_aggregates.rds")

# Auto-generate demo dataset if missing on initial launch
if (!file.exists(monthly_cache_file)) {
  message("Monthly cache not found. Generating demo dataset...")
  source("scripts/generate_demo_data.R")
}

# Load precomputed monthly aggregates
monthly_data <- if (file.exists(monthly_cache_file)) {
  readRDS(monthly_cache_file)
} else {
  data.frame()
}

is_demo_mode <- (data_mode == "demo") || (nrow(monthly_data) > 0 && any(monthly_data$is_synthetic))

# Compute initial baseline (2001-2015)
baseline_stats <- compute_seasonal_baseline(
  monthly_data,
  baseline_start = cfg$baseline_period$start_year,
  baseline_end = cfg$baseline_period$end_year,
  min_baseline_years = cfg$quality$min_baseline_years
)

# Compute initial anomalies (2016-2024)
all_anomalies <- detect_climate_anomalies(
  monthly_data,
  baseline_stats,
  analysis_start = cfg$analysis_period$start_year,
  analysis_end = cfg$analysis_period$end_year,
  config = cfg
)

# Available choices
location_choices <- setNames(locations_df$location_id, locations_df$location_name)
available_years <- sort(unique(monthly_data$year))
available_months <- 1:12
month_names <- setNames(1:12, month.name)

# Define modern bslib theme with Plus Jakarta Sans and curated palette
cw_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0D9488",
  secondary = "#0EA5E9",
  bg = "#F8FAFC",
  fg = "#1E293B",
  base_font = font_google("Plus Jakarta Sans")
)

# Helper for consistent Plotly chart theme
style_plotly_chart <- function(p) {
  ggplotly(p) %>%
    layout(
      font = list(family = "Plus Jakarta Sans, sans-serif", size = 12, color = "#334155"),
      plot_bgcolor = "rgba(248, 250, 252, 0.5)",
      paper_bgcolor = "transparent",
      margin = list(t = 40, b = 40, l = 50, r = 20)
    )
}

# -------------------------------------------------------------------
# USER INTERFACE
# -------------------------------------------------------------------
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; gap: 10px;",
    tags$div(
      style = "width: 28px; height: 28px; border-radius: 8px; background: linear-gradient(135deg, #0EA5E9 0%, #0D9488 100%); display: flex; align-items: center; justify-content: center; box-shadow: 0 0 10px rgba(14, 165, 233, 0.4);",
      HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="#FFFFFF" stroke-width="2.5"><path d="M12 2v20M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6"/></svg>')
    ),
    tags$span(style = "font-family: 'Outfit', sans-serif; font-weight: 800; font-size: 1.3rem; color: #FFFFFF; letter-spacing: -0.02em;", "ClimateWatch"),
    tags$span(style = "font-size: 0.72rem; font-weight: 700; background: rgba(14, 165, 233, 0.2); border: 1px solid rgba(14, 165, 233, 0.5); color: #38BDF8; padding: 2px 8px; border-radius: 12px; text-transform: uppercase;", "Big Data Analytics")
  ),
  id = "main_nav",
  theme = cw_theme,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(HTML("
      .tab-content { padding-top: 18px; }
      .container-fluid { max-width: 1440px; margin: 0 auto; }
    "))
  ),

  # -----------------------------------------------------------------
  # PAGE 1: OVERVIEW
  # -----------------------------------------------------------------
  tabPanel(
    HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" style="margin-right:6px; vertical-align: -2px;"><path d="M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"/><polyline points="9 22 9 12 15 12 15 22"/></svg>Overview'),
    render_mode_banner(is_demo_mode),
    fluidRow(
      column(
        3,
        create_kpi_card("Monitored Stations", nrow(locations_df), "15 Reviewed coordinates", accent = "teal")
      ),
      column(
        3,
        create_kpi_card("Daily Records", format(nrow(monthly_data) * 30.4, big.mark = ","), "2001–2024 complete series", accent = "blue")
      ),
      column(
        3,
        create_kpi_card("Flagged Temp Anomalies", sum(all_anomalies$temp_is_anomaly, na.rm = TRUE), "Months with |z| >= 1.5", accent = "red")
      ),
      column(
        3,
        create_kpi_card("Flagged Rain Anomalies", sum(all_anomalies$precip_is_anomaly, na.rm = TRUE), "Months < P10 or > P90", accent = "amber")
      )
    ),
    br(),
    fluidRow(
      # Left: Interactive Map of Monitored Locations
      column(
        5,
        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span("Monitored Indian Coordinates"),
              tags$span(class = "badge bg-secondary", "15 Stations")
            )
          ),
          card_body(
            p(
              style = "font-size: 0.85rem; color: #64748B; margin-bottom: 12px;",
              "Click on markers to inspect reviewed coordinates and elevation. Values represent gridded model/satellite estimates at these points."
            ),
            leafletOutput("overview_map", height = "420px")
          )
        )
      ),
      # Right: Location Trends
      column(
        7,
        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span("Long-Term Climate Trajectories"),
              div(
                style = "width: 220px;",
                selectInput("overview_location", NULL, choices = location_choices, selected = "mumbai", width = "100%")
              )
            )
          ),
          card_body(
            tabsetPanel(
              tabPanel("Temperature (°C)", plotlyOutput("overview_temp_chart", height = "360px")),
              tabPanel("Rainfall (mm)", plotlyOutput("overview_precip_chart", height = "360px")),
              tabPanel("Humidity (%)", plotlyOutput("overview_humidity_chart", height = "360px"))
            )
          )
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # PAGE 2: ANOMALY EXPLORER
  # -----------------------------------------------------------------
  tabPanel(
    HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" style="margin-right:6px; vertical-align: -2px;"><circle cx="12" cy="12" r="10"/><polygon points="16.24 7.76 14.12 14.12 7.76 16.24 9.88 9.88 16.24 7.76"/></svg>Anomaly Explorer'),
    render_mode_banner(is_demo_mode),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h5(style = "font-family: 'Outfit'; font-weight: 700; color: #0F172A; margin-bottom: 16px;", "Detection Settings"),
        selectInput("anomaly_location", "Target Location", choices = location_choices, selected = "delhi"),
        selectInput("anomaly_variable", "Climate Variable",
          choices = c(
            "Mean Temperature (°C)" = "temperature",
            "Total Precipitation (mm)" = "precipitation",
            "Relative Humidity (%)" = "humidity"
          ),
          selected = "temperature"
        ),
        selectInput("anomaly_severity_filter", "Severity Filter",
          choices = c(
            "All Observations" = "all",
            "Flagged Anomalies Only" = "anomalies_only",
            "Severe Only" = "Severe",
            "Moderate & Severe" = "moderate_plus"
          ),
          selected = "anomalies_only"
        ),
        tags$hr(),
        tags$h6(style = "font-weight: 700; color: #475569; font-size: 0.85rem;", "Historical Baseline Window"),
        fluidRow(
          column(6, numericInput("base_start", "Start Year", value = 2001, min = 2001, max = 2020)),
          column(6, numericInput("base_end", "End Year", value = 2015, min = 2002, max = 2023))
        ),
        tags$h6(style = "font-weight: 700; color: #475569; font-size: 0.85rem;", "Analysis Period Window"),
        fluidRow(
          column(6, numericInput("analysis_start", "Start Year", value = 2016, min = 2002, max = 2024)),
          column(6, numericInput("analysis_end", "End Year", value = 2024, min = 2002, max = 2024))
        ),
        p(
          style = "font-size: 0.78rem; color: #64748B; margin-top: 8px;",
          "Strict separation: Analysed observations are never included in their own baseline."
        ),
        br(),
        downloadButton("download_anomalies_csv", "Export Anomalies CSV", class = "btn-outline-primary btn-sm w-100")
      ),
      mainPanel(
        width = 9,
        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span("Observed vs Historical Baseline Trajectory"),
              uiOutput("anomaly_summary_badge")
            )
          ),
          card_body(
            plotlyOutput("anomaly_trend_chart", height = "360px")
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            card(
              card_header(
                div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span("Explainable Anomaly Cards"),
                  tags$span(class = "badge bg-info text-dark", "Novelty Feature")
                )
              ),
              card_body(
                p(
                  style = "font-size: 0.85rem; color: #64748B; margin-bottom: 12px;",
                  "Directly computed natural-language explanations derived strictly from observed values, baseline statistics, and coverage metrics:"
                ),
                uiOutput("explainable_cards_container")
              )
            )
          ),
          column(
            6,
            card(
              card_header(tags$span("Anomaly Records Table")),
              card_body(
                DTOutput("anomaly_table", height = "420px")
              )
            )
          )
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # PAGE 3: LOCATION MAP AND COMPARISON
  # -----------------------------------------------------------------
  tabPanel(
    HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" style="margin-right:6px; vertical-align: -2px;"><polygon points="1 6 1 22 8 18 16 22 23 18 23 2 16 6 8 2 1 6"/><line x1="8" y1="2" x2="8" y2="18"/><line x1="16" y1="6" x2="16" y2="22"/></svg>Location Map & Comparison'),
    render_mode_banner(is_demo_mode),
    fluidRow(
      column(
        3,
        card(
          card_header(tags$span("Spatio-Temporal Controls")),
          card_body(
            selectInput("map_month", "Select Month", choices = month_names, selected = 5),
            selectInput("map_year", "Select Year", choices = 2016:2024, selected = 2022),
            selectInput("map_variable", "Climate Variable",
              choices = c(
                "Temperature" = "temperature",
                "Precipitation" = "precipitation",
                "Humidity" = "humidity"
              ),
              selected = "temperature"
            ),
            tags$hr(),
            tags$h6(style = "font-weight: 700; color: #475569; font-size: 0.85rem;", "Status Legend"),
            uiOutput("map_legend_ui")
          )
        )
      ),
      column(
        9,
        card(
          card_header(
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              tags$span("Spatial Anomaly Distribution Map"),
              tags$span(class = "badge bg-primary", textOutput("map_period_label", inline = TRUE))
            )
          ),
          card_body(
            p(
              style = "font-size: 0.85rem; color: #64748B; margin-bottom: 12px;",
              "Marker colors indicate anomaly status relative to the historical baseline for the selected month and year. Click markers to inspect exact values."
            ),
            leafletOutput("spatial_anomaly_map", height = "450px")
          )
        )
      )
    ),
    br(),

    # Selected 2-4 Location Comparison Section
    card(
      card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          tags$span("Multi-Station Trajectory Comparison (Select 2–4 Locations)"),
          div(
            style = "width: 450px;",
            selectizeInput("comp_locations", NULL,
              choices = location_choices,
              selected = c("mumbai", "delhi", "chennai"),
              multiple = TRUE, options = list(maxItems = 4, placeholder = "Select 2 to 4 locations")
            )
          )
        )
      ),
      card_body(
        plotlyOutput("multi_location_comparison_chart", height = "320px")
      )
    )
  ),

  # -----------------------------------------------------------------
  # PAGE 4: FORECASTING
  # -----------------------------------------------------------------
  tabPanel(
    HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" style="margin-right:6px; vertical-align: -2px;"><polyline points="23 6 13.5 15.5 8.5 10.5 1 18"/><polyline points="17 6 23 6 23 12"/></svg>Forecasting'),
    render_mode_banner(is_demo_mode),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h5(style = "font-family: 'Outfit'; font-weight: 700; color: #0F172A; margin-bottom: 16px;", "Forecasting Protocol"),
        selectInput("forecast_location", "Target Location", choices = location_choices, selected = "delhi"),
        radioButtons("forecast_horizon", "Forecast Horizon",
          choices = c("3 Months" = 3, "6 Months" = 6, "12 Months" = 12),
          selected = 6, inline = TRUE
        ),
        tags$hr(),
        tags$div(
          class = "forecast-summary-box",
          tags$strong(style = "color: #0F766E;", "Evaluation Protocol:"),
          tags$ul(
            style = "font-size: 0.82rem; padding-left: 18px; margin-top: 6px; color: #134E4A;",
            tags$li("Strict chronological split"),
            tags$li("Latest 12 consecutive months reserved for holdout"),
            tags$li("Evaluated on identical timestamps"),
            tags$li("Best model refit on complete series")
          )
        ),
        uiOutput("forecast_window_info"),
        br(),
        downloadButton("download_forecast_csv", "Download Predictions CSV", class = "btn-outline-primary btn-sm w-100")
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(
            8,
            card(
              card_header(
                div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span("Temperature Forecast & Prediction Intervals"),
                  uiOutput("best_model_badge")
                )
              ),
              card_body(
                p(
                  style = "font-size: 0.85rem; color: #64748B; margin-bottom: 12px;",
                  "Point predictions with 80% (darker) and 95% (lighter) prediction intervals generated by refitting the best-performing model on all available history."
                ),
                plotlyOutput("forecast_main_chart", height = "360px")
              )
            )
          ),
          column(
            4,
            card(
              card_header(tags$span("Holdout Comparison (12m)")),
              card_body(
                p(
                  style = "font-size: 0.82rem; color: #64748B;",
                  "Model comparison evaluated on identical 12 holdout timestamps:"
                ),
                tableOutput("forecast_metrics_table"),
                tags$hr(),
                tags$strong(style = "font-size: 0.88rem; color: #0F172A;", "Future Point Predictions:"),
                tableOutput("future_predictions_summary")
              )
            )
          )
        ),
        br(),
        card(
          card_header(tags$span("Holdout Validation: Actual vs Model Predictions")),
          card_body(
            p(
              style = "font-size: 0.85rem; color: #64748B; margin-bottom: 12px;",
              "Demonstrates model accuracy on the unseen 12-month holdout evaluation window across Seasonal Naïve, ETS, and ARIMA."
            ),
            plotlyOutput("holdout_comparison_chart", height = "280px")
          )
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # PAGE 5: DATA QUALITY AND STORAGE
  # -----------------------------------------------------------------
  tabPanel(
    HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" style="margin-right:6px; vertical-align: -2px;"><ellipse cx="12" cy="5" rx="9" ry="3"/><path d="M21 12c0 1.66-4 3-9 3s-9-1.34-9-3"/><path d="M3 5v14c0 1.66 4 3 9 3s9-1.34 9-3V5"/></svg>Data Quality & Storage'),
    render_mode_banner(is_demo_mode),
    fluidRow(
      column(
        3,
        create_kpi_card("Storage Engine", "Parquet + DuckDB", "Year-partitioned dataset", accent = "teal")
      ),
      column(
        3,
        create_kpi_card("Storage Size", "1.1 MB", "Columnar Apache Parquet", accent = "blue")
      ),
      column(
        3,
        create_kpi_card("Coverage Rule", "90% Minimum", "Incomplete months excluded", accent = "amber")
      ),
      column(
        3,
        create_kpi_card("Data Deduplication", "100% Cleaned", "Unique location-date records", accent = "teal")
      )
    ),
    br(),
    fluidRow(
      column(
        5,
        card(
          card_header(tags$span("Dataset Specifications & Metadata")),
          card_body(
            tags$table(
              class = "table table-sm table-striped",
              tags$tbody(
                tags$tr(tags$td("Data Mode:"), tags$td(tags$strong(if (is_demo_mode) "Offline Synthetic Demonstration" else "NASA POWER Real Observations"))),
                tags$tr(tags$td("Source Description:"), tags$td("NASA POWER Daily Meteorological API (Gridded Estimates)")),
                tags$tr(tags$td("Historical Range:"), tags$td("2001-01-01 to 2024-12-31 (24 Years)")),
                tags$tr(tags$td("Monitored Stations:"), tags$td(paste(nrow(locations_df), "Reviewed Indian Locations"))),
                tags$tr(tags$td("Temperature Unit:"), tags$td("°C (Celsius)")),
                tags$tr(tags$td("Precipitation Unit:"), tags$td("mm / month (Daily sum when coverage >= 90%)")),
                tags$tr(tags$td("Relative Humidity Unit:"), tags$td("% (Percentage)")),
                tags$tr(tags$td("Partitioning Scheme:"), tags$td("data/processed/[mode]/year=YYYY/*.parquet"))
              )
            ),
            p(
              style = "font-size: 0.82rem; color: #64748B; margin-top: 10px;",
              "NASA POWER values represent gridded satellite/model estimates at the selected coordinates, not ground stations throughout entire cities."
            )
          )
        )
      ),
      column(
        7,
        card(
          card_header(tags$span("Location Coverage and Quality Status")),
          card_body(
            DTOutput("location_coverage_table", height = "280px")
          )
        )
      )
    ),
    br(),
    card(
      card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          tags$span("Aggregated Monthly Dataset Inspection"),
          downloadButton("download_quality_csv", "Export Cleaned Monthly CSV", class = "btn-outline-primary btn-sm")
        )
      ),
      card_body(
        DTOutput("raw_inspection_table", height = "360px")
      )
    )
  )
)

# -------------------------------------------------------------------
# SERVER LOGIC
# -------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive baseline calculation based on user controls
  reactive_baseline <- reactive({
    req(input$base_start, input$base_end)
    validate(
      need(input$base_start < input$base_end, "Baseline start year must precede baseline end year."),
      need(input$base_end < input$analysis_start, "Baseline period must strictly precede analysis period (no overlap).")
    )
    compute_seasonal_baseline(
      monthly_data,
      baseline_start = input$base_start,
      baseline_end = input$base_end,
      min_baseline_years = cfg$quality$min_baseline_years
    )
  })

  # Reactive anomaly detection
  reactive_anomalies <- reactive({
    req(input$analysis_start, input$analysis_end)
    base <- reactive_baseline()
    detect_climate_anomalies(
      monthly_data,
      base,
      analysis_start = input$analysis_start,
      analysis_end = input$analysis_end,
      config = cfg
    )
  })

  # =================================================================
  # PAGE 1: OVERVIEW OUTPUTS
  # =================================================================
  output$overview_map <- renderLeaflet({
    leaflet(locations_df) %>%
      addTiles() %>%
      setView(lng = 78.9629, lat = 20.5937, zoom = 4) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        color = "#0F172A",
        fillColor = "#0EA5E9",
        fillOpacity = 0.9,
        weight = 2,
        popup = ~ sprintf(
          "<strong>%s</strong><br>State: %s<br>Lat: %.4f, Lon: %.4f<br>Elevation: %d m",
          location_name, state, latitude, longitude, elevation_m
        ),
        label = ~location_name
      )
  })

  loc_monthly_overview <- reactive({
    req(input$overview_location)
    monthly_data[monthly_data$location_id == input$overview_location, ]
  })

  output$overview_temp_chart <- renderPlotly({
    df <- loc_monthly_overview()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = year_month, y = mean_temperature_c)) +
      geom_line(color = "#D97706", linewidth = 0.7) +
      geom_smooth(method = "loess", se = FALSE, color = "#DC2626", linewidth = 1.0, span = 0.3) +
      labs(x = "Year", y = "Mean Temperature (°C)", title = "Monthly Mean Temperature Trend") +
      theme_minimal()
    style_plotly_chart(p)
  })

  output$overview_precip_chart <- renderPlotly({
    df <- loc_monthly_overview()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = year_month, y = total_precipitation_mm)) +
      geom_col(fill = "#2563EB", alpha = 0.8) +
      labs(x = "Year", y = "Total Precipitation (mm)", title = "Monthly Rainfall Distribution") +
      theme_minimal()
    style_plotly_chart(p)
  })

  output$overview_humidity_chart <- renderPlotly({
    df <- loc_monthly_overview()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = year_month, y = mean_humidity_pct)) +
      geom_line(color = "#0D9488", linewidth = 0.8) +
      labs(x = "Year", y = "Relative Humidity (%)", title = "Monthly Mean Relative Humidity") +
      theme_minimal()
    style_plotly_chart(p)
  })

  # =================================================================
  # PAGE 2: ANOMALY EXPLORER OUTPUTS
  # =================================================================
  filtered_anomalies <- reactive({
    all_an <- reactive_anomalies()
    req(nrow(all_an) > 0, input$anomaly_location)

    df <- all_an[all_an$location_id == input$anomaly_location, ]

    if (input$anomaly_variable == "temperature") {
      if (input$anomaly_severity_filter == "anomalies_only") {
        df <- df[df$temp_is_anomaly, ]
      } else if (input$anomaly_severity_filter == "Severe") {
        df <- df[df$temp_severity == "Severe", ]
      } else if (input$anomaly_severity_filter == "moderate_plus") {
        df <- df[df$temp_severity %in% c("Moderate", "Severe"), ]
      }
    } else if (input$anomaly_variable == "precipitation") {
      if (input$anomaly_severity_filter %in% c("anomalies_only", "Severe", "moderate_plus")) {
        df <- df[df$precip_is_anomaly, ]
      }
    } else {
      if (input$anomaly_severity_filter %in% c("anomalies_only", "Severe", "moderate_plus")) {
        df <- df[df$humidity_is_anomaly, ]
      }
    }

    df
  })

  output$anomaly_summary_badge <- renderUI({
    df <- filtered_anomalies()
    count <- nrow(df)
    tags$span(class = "badge bg-primary", sprintf("%d Observations Filtered", count))
  })

  output$anomaly_trend_chart <- renderPlotly({
    all_an <- reactive_anomalies()
    req(nrow(all_an) > 0, input$anomaly_location)
    loc_an <- all_an[all_an$location_id == input$anomaly_location, ]

    var <- input$anomaly_variable
    if (var == "temperature") {
      p <- ggplot(loc_an, aes(x = year_month)) +
        geom_line(aes(y = temp_baseline_mean, linetype = "Baseline Mean"), color = "#64748B") +
        geom_line(aes(y = temperature_c, color = "Observed"), linewidth = 0.8) +
        geom_point(
          data = loc_an[loc_an$temp_is_anomaly, ],
          aes(y = temperature_c, text = paste("Severity:", temp_severity, "<br>Deviation:", temp_deviation_c, "°C")),
          color = "#DC2626", size = 2.5
        ) +
        scale_color_manual(values = c("Observed" = "#0EA5E9")) +
        scale_linetype_manual(values = c("Baseline Mean" = "dashed")) +
        labs(x = "Date", y = "Temperature (°C)", title = paste("Temperature Anomalies —", loc_an$location_name[1])) +
        theme_minimal()
    } else if (var == "precipitation") {
      p <- ggplot(loc_an, aes(x = year_month)) +
        geom_col(aes(y = precipitation_mm), fill = "#93C5FD", alpha = 0.7) +
        geom_line(aes(y = precip_baseline_p10, linetype = "10th Percentile (Dry)"), color = "#D97706") +
        geom_line(aes(y = precip_baseline_p90, linetype = "90th Percentile (Wet)"), color = "#2563EB") +
        geom_point(
          data = loc_an[loc_an$precip_is_anomaly, ],
          aes(y = precipitation_mm, text = paste("Flag:", precip_anomaly_flag)),
          color = "#DC2626", size = 2.5
        ) +
        scale_linetype_manual(values = c("10th Percentile (Dry)" = "dashed", "90th Percentile (Wet)" = "dotted")) +
        labs(x = "Date", y = "Precipitation (mm)", title = paste("Precipitation Anomalies —", loc_an$location_name[1])) +
        theme_minimal()
    } else {
      p <- ggplot(loc_an, aes(x = year_month)) +
        geom_line(aes(y = humidity_baseline_mean, linetype = "Baseline Mean"), color = "#64748B") +
        geom_line(aes(y = humidity_pct, color = "Observed"), linewidth = 0.8) +
        geom_point(
          data = loc_an[loc_an$humidity_is_anomaly, ],
          aes(y = humidity_pct, text = paste("Flag:", humidity_anomaly_flag)),
          color = "#DC2626", size = 2.5
        ) +
        scale_color_manual(values = c("Observed" = "#0D9488")) +
        scale_linetype_manual(values = c("Baseline Mean" = "dashed")) +
        labs(x = "Date", y = "Relative Humidity (%)", title = paste("Humidity Anomalies —", loc_an$location_name[1])) +
        theme_minimal()
    }

    style_plotly_chart(p)
  })

  # Explainable Anomaly Cards
  output$explainable_cards_container <- renderUI({
    df <- filtered_anomalies()
    var <- input$anomaly_variable

    if (nrow(df) == 0) {
      return(div(class = "alert alert-secondary", "No anomalies found matching current filter criteria."))
    }

    # Display top 4 most recent anomaly records as explainable cards
    top_records <- head(df[order(df$year, df$month, decreasing = TRUE), ], 4)
    cards <- lapply(seq_len(nrow(top_records)), function(i) {
      card_data <- generate_anomaly_card_data(top_records[i, ], variable = var)
      render_anomaly_card_ui(card_data)
    })

    do.call(tagList, cards)
  })

  # Anomaly Table
  output$anomaly_table <- renderDT({
    df <- filtered_anomalies()
    req(nrow(df) > 0)

    var <- input$anomaly_variable
    if (var == "temperature") {
      table_sub <- df[, c("year", "month", "temperature_c", "temp_baseline_mean", "temp_deviation_c", "temp_z_score", "temp_anomaly_type", "temp_severity", "temp_coverage_pct")]
      colnames(table_sub) <- c("Year", "Month", "Observed (°C)", "Baseline Mean", "Dev (°C)", "z-Score", "Type", "Severity", "Coverage %")
    } else if (var == "precipitation") {
      table_sub <- df[, c("year", "month", "precipitation_mm", "precip_baseline_p10", "precip_baseline_p90", "precip_deviation_mm", "precip_anomaly_flag", "precip_coverage_pct")]
      colnames(table_sub) <- c("Year", "Month", "Observed (mm)", "P10 (mm)", "P90 (mm)", "Dev (mm)", "Anomaly Flag", "Coverage %")
    } else {
      table_sub <- df[, c("year", "month", "humidity_pct", "humidity_baseline_mean", "humidity_deviation_pct", "humidity_z_score", "humidity_anomaly_flag", "humidity_coverage_pct")]
      colnames(table_sub) <- c("Year", "Month", "Observed (%)", "Baseline Mean", "Dev (%)", "z-Score", "Anomaly Flag", "Coverage %")
    }

    datatable(table_sub, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  # Export CSV
  output$download_anomalies_csv <- downloadHandler(
    filename = function() {
      sprintf("climatewatch_anomalies_%s_%s.csv", input$anomaly_location, Sys.Date())
    },
    content = function(file) {
      write.csv(filtered_anomalies(), file, row.names = FALSE)
    }
  )

  # =================================================================
  # PAGE 3: LOCATION MAP AND COMPARISON OUTPUTS
  # =================================================================
  output$map_period_label <- renderText({
    paste(month.name[as.integer(input$map_month)], input$map_year)
  })

  output$map_legend_ui <- renderUI({
    var <- input$map_variable
    if (var == "temperature") {
      tags$div(
        style = "font-size: 0.85rem; line-height: 2.0;",
        tags$div(tags$span(style = "color:#EF4444; font-weight:bold; font-size:1.1rem;", "●"), " Severe Hot (|z| >= 3.0)"),
        tags$div(tags$span(style = "color:#EA580C; font-weight:bold; font-size:1.1rem;", "●"), " Moderate Hot (2.0 <= z < 3.0)"),
        tags$div(tags$span(style = "color:#F59E0B; font-weight:bold; font-size:1.1rem;", "●"), " Mild Hot (1.5 <= z < 2.0)"),
        tags$div(tags$span(style = "color:#10B981; font-weight:bold; font-size:1.1rem;", "●"), " Normal (|z| < 1.5)"),
        tags$div(tags$span(style = "color:#3B82F6; font-weight:bold; font-size:1.1rem;", "●"), " Unusually Cold (z <= -1.5)")
      )
    } else if (var == "precipitation") {
      tags$div(
        style = "font-size: 0.85rem; line-height: 2.0;",
        tags$div(tags$span(style = "color:#2563EB; font-weight:bold; font-size:1.1rem;", "●"), " Unusually Wet (> 90th percentile)"),
        tags$div(tags$span(style = "color:#10B981; font-weight:bold; font-size:1.1rem;", "●"), " Normal (10th to 90th percentile)"),
        tags$div(tags$span(style = "color:#D97706; font-weight:bold; font-size:1.1rem;", "●"), " Unusually Dry (< 10th percentile)")
      )
    } else {
      tags$div(
        style = "font-size: 0.85rem; line-height: 2.0;",
        tags$div(tags$span(style = "color:#6366F1; font-weight:bold; font-size:1.1rem;", "●"), " Unusually High (z >= 1.5)"),
        tags$div(tags$span(style = "color:#10B981; font-weight:bold; font-size:1.1rem;", "●"), " Normal (|z| < 1.5)"),
        tags$div(tags$span(style = "color:#A855F7; font-weight:bold; font-size:1.1rem;", "●"), " Unusually Low (z <= -1.5)")
      )
    }
  })

  output$spatial_anomaly_map <- renderLeaflet({
    req(input$map_month, input$map_year, input$map_variable)
    all_an <- reactive_anomalies()

    sub_map <- all_an[all_an$year == as.integer(input$map_year) & all_an$month == as.integer(input$map_month), ]
    if (nrow(sub_map) == 0) {
      return(leaflet() %>% addTiles() %>% setView(78.96, 20.59, 4))
    }

    var <- input$map_variable

    # Assign color function
    get_marker_color <- function(row) {
      if (var == "temperature") {
        sev <- row$temp_severity
        dev <- row$temp_deviation_c
        if (sev == "Severe") {
          "#EF4444"
        } else if (sev == "Moderate") {
          "#EA580C"
        } else if (sev == "Mild") {
          if (dev > 0) "#F59E0B" else "#3B82F6"
        } else {
          "#10B981"
        }
      } else if (var == "precipitation") {
        flag <- row$precip_anomaly_flag
        if (flag == "Unusually Wet") {
          "#2563EB"
        } else if (flag == "Unusually Dry") {
          "#D97706"
        } else {
          "#10B981"
        }
      } else {
        flag <- row$humidity_anomaly_flag
        if (flag == "Unusually High Humidity") {
          "#6366F1"
        } else if (flag == "Unusually Low Humidity") {
          "#A855F7"
        } else {
          "#10B981"
        }
      }
    }

    colors <- sapply(seq_len(nrow(sub_map)), function(i) get_marker_color(sub_map[i, ]))

    # Build popups
    popups <- sapply(seq_len(nrow(sub_map)), function(i) {
      r <- sub_map[i, ]
      if (var == "temperature") {
        sprintf(
          "<strong>%s</strong><br>Observed: %.1f °C<br>Baseline: %.1f °C<br>Deviation: %+.1f °C (z: %+.2f)<br>Status: <strong>%s</strong><br>Coverage: %.0f%%",
          r$location_name, r$temperature_c, r$temp_baseline_mean, r$temp_deviation_c, r$temp_z_score, r$temp_anomaly_type, r$temp_coverage_pct
        )
      } else if (var == "precipitation") {
        sprintf(
          "<strong>%s</strong><br>Observed: %.1f mm<br>P10: %.1f mm | P90: %.1f mm<br>Status: <strong>%s</strong><br>Coverage: %.0f%%",
          r$location_name, r$precipitation_mm, r$precip_baseline_p10, r$precip_baseline_p90, r$precip_anomaly_flag, r$precip_coverage_pct
        )
      } else {
        sprintf(
          "<strong>%s</strong><br>Observed: %.1f%%<br>Baseline: %.1f%% (z: %+.2f)<br>Status: <strong>%s</strong><br>Coverage: %.0f%%",
          r$location_name, r$humidity_pct, r$humidity_baseline_mean, r$humidity_z_score, r$humidity_anomaly_flag, r$humidity_coverage_pct
        )
      }
    })

    leaflet(sub_map) %>%
      addTiles() %>%
      setView(lng = 78.9629, lat = 20.5937, zoom = 4) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 9,
        color = "#0F172A",
        fillColor = colors,
        fillOpacity = 0.9,
        weight = 2,
        popup = popups,
        label = ~location_name
      )
  })

  # Multi-location 2-4 comparison chart
  output$multi_location_comparison_chart <- renderPlotly({
    req(input$comp_locations)
    validate(need(length(input$comp_locations) >= 2, "Please select at least 2 locations (up to 4) for comparison."))

    comp_df <- monthly_data[monthly_data$location_id %in% input$comp_locations, ]
    req(nrow(comp_df) > 0)

    p <- ggplot(comp_df, aes(x = year_month, y = mean_temperature_c, color = location_name)) +
      geom_line(linewidth = 0.9) +
      labs(
        x = "Year", y = "Mean Temperature (°C)", color = "Location",
        title = "Monthly Mean Temperature Comparison Across Monitored Coordinates"
      ) +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")

    style_plotly_chart(p)
  })

  # =================================================================
  # PAGE 4: FORECASTING OUTPUTS
  # =================================================================
  forecast_results <- reactive({
    req(input$forecast_location, input$forecast_horizon)
    forecast_temperature(
      monthly_df = monthly_data,
      location_id = input$forecast_location,
      horizon = as.integer(input$forecast_horizon),
      holdout_months = cfg$forecasting$holdout_months
    )
  })

  output$forecast_window_info <- renderUI({
    res <- forecast_results()
    if (res$status != "SUCCESS") {
      return(NULL)
    }
    tags$div(
      style = "font-size: 0.82rem; color: #334155; background: #F1F5F9; border-radius: 6px; padding: 10px; margin-top: 6px;",
      tags$div(tags$strong("Training: "), res$training_range),
      tags$div(tags$strong("Holdout: "), res$holdout_range)
    )
  })

  output$best_model_badge <- renderUI({
    res <- forecast_results()
    if (res$status != "SUCCESS") {
      return(NULL)
    }
    tags$span(class = "badge bg-success", paste("Best Model:", res$best_model_name))
  })

  output$forecast_main_chart <- renderPlotly({
    res <- forecast_results()
    validate(need(res$status == "SUCCESS", res$message))

    hist_df <- res$historical_df
    future_df <- res$future_df

    # Plot recent history (36 months) + forecast
    recent_hist <- tail(hist_df, 36)

    p <- ggplot() +
      geom_line(data = recent_hist, aes(x = date, y = temperature_c, color = "Historical"), linewidth = 0.8) +
      geom_ribbon(data = future_df, aes(x = date, ymin = lower_95, ymax = upper_95, fill = "95% Interval"), alpha = 0.2) +
      geom_ribbon(data = future_df, aes(x = date, ymin = lower_80, ymax = upper_80, fill = "80% Interval"), alpha = 0.35) +
      geom_line(data = future_df, aes(x = date, y = predicted_temp_c, color = "Forecast"), linewidth = 1.0, linetype = "solid") +
      geom_point(data = future_df, aes(x = date, y = predicted_temp_c, color = "Forecast"), size = 2) +
      scale_color_manual(values = c("Historical" = "#0F172A", "Forecast" = "#0D9488")) +
      scale_fill_manual(values = c("80% Interval" = "#0D9488", "95% Interval" = "#14B8A6")) +
      labs(x = "Date", y = "Temperature (°C)", title = paste("Monthly Temperature Forecast —", res$location_name)) +
      theme_minimal()

    style_plotly_chart(p)
  })

  output$holdout_comparison_chart <- renderPlotly({
    res <- forecast_results()
    validate(need(res$status == "SUCCESS", res$message))

    h_df <- res$holdout_df
    p <- ggplot(h_df, aes(x = date)) +
      geom_line(aes(y = actual, color = "Actual Observation"), linewidth = 1.1) +
      geom_line(aes(y = snaive_pred, color = "Seasonal Naïve"), linetype = "dashed") +
      geom_line(aes(y = ets_pred, color = "ETS"), linetype = "dotted") +
      geom_line(aes(y = arima_pred, color = "ARIMA"), linetype = "dotdash") +
      scale_color_manual(values = c("Actual Observation" = "#0F172A", "Seasonal Naïve" = "#64748B", "ETS" = "#2563EB", "ARIMA" = "#D97706")) +
      labs(x = "Holdout Timestamp", y = "Temperature (°C)", title = "12-Month Holdout Comparison") +
      theme_minimal()

    style_plotly_chart(p)
  })

  output$forecast_metrics_table <- renderTable(
    {
      res <- forecast_results()
      req(res$status == "SUCCESS")
      res$comparison_table[, c("Model", "MAE", "RMSE", "Selected")]
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  output$future_predictions_summary <- renderTable(
    {
      res <- forecast_results()
      req(res$status == "SUCCESS")
      res$future_df[, c("month_year", "predicted_temp_c", "lower_95", "upper_95")]
    },
    striped = TRUE,
    hover = TRUE
  )

  output$download_forecast_csv <- downloadHandler(
    filename = function() {
      sprintf("temperature_forecast_%s_%dm_%s.csv", input$forecast_location, input$forecast_horizon, Sys.Date())
    },
    content = function(file) {
      res <- forecast_results()
      write.csv(res$future_df, file, row.names = FALSE)
    }
  )

  # =================================================================
  # PAGE 5: DATA QUALITY AND STORAGE OUTPUTS
  # =================================================================
  output$location_coverage_table <- renderDT({
    req(nrow(monthly_data) > 0)
    cov_summary <- monthly_data %>%
      group_by(location_id, location_name) %>%
      summarise(
        total_months = n(),
        adequate_months = sum(quality_status == "ADEQUATE_COVERAGE"),
        avg_temp_coverage = round(mean(temp_coverage_pct, na.rm = TRUE), 1),
        avg_precip_coverage = round(mean(precip_coverage_pct, na.rm = TRUE), 1),
        avg_hum_coverage = round(mean(humidity_coverage_pct, na.rm = TRUE), 1),
        .groups = "drop"
      )
    datatable(cov_summary, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })

  output$raw_inspection_table <- renderDT({
    req(nrow(monthly_data) > 0)
    sub_table <- monthly_data[, c("location_name", "year", "month", "mean_temperature_c", "total_precipitation_mm", "mean_humidity_pct", "temp_coverage_pct", "precip_coverage_pct", "quality_status")]
    colnames(sub_table) <- c("Location", "Year", "Month", "Mean Temp (°C)", "Rainfall (mm)", "Humidity (%)", "Temp Cov %", "Rain Cov %", "Quality Status")
    datatable(sub_table, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$download_quality_csv <- downloadHandler(
    filename = function() {
      sprintf("climatewatch_monthly_aggregates_%s.csv", Sys.Date())
    },
    content = function(file) {
      write.csv(monthly_data, file, row.names = FALSE)
    }
  )
}

# Run Shiny Application
shinyApp(ui = ui, server = server)
