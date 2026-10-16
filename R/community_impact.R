# R/community_impact.R - Core Impact Assessment Logic and Rule Evaluation Engine
# Deterministic, rule-based scenario simulator for community vulnerability and climate signals.
# Strictly implements project-demonstrated evaluation rules, precedence, and what-if comparisons.

# -------------------------------------------------------------------
# 1. CLIMATE SIGNAL EXTRACTION
# -------------------------------------------------------------------

#' Extract Climate Signals from Monthly Outlook and Baseline
#'
#' Evaluates whether the selected monthly outlook (forecast or historical)
#' crosses defined seasonal thresholds:
#' - Hot: mean temp >= baseline mean + (z_mild * sd) [z >= 1.5]
#' - Dry: rainfall <= seasonal P10 threshold
#' - Wet: rainfall >= seasonal P90 threshold
#' - High Humidity: mean humidity >= baseline mean + (1.5 * sd), when valid baseline exists.
#'
#' @param outlook_row Named list or 1-row data.frame with observed/predicted values
#' @param baseline_row Named list or 1-row data.frame with baseline statistics
#' @param config Application settings list
#' @return List of climate signals with values, thresholds, and statuses
extract_monthly_climate_signals <- function(outlook_row, baseline_row, config = NULL) {
  z_mild <- if (!is.null(config$anomaly$z_mild)) config$anomaly$z_mild else 1.5
  
  # Temperature signal
  temp_val <- if (!is.null(outlook_row$temperature)) as.numeric(outlook_row$temperature) else NA_real_
  b_temp_mean <- if (!is.null(baseline_row$temp_baseline_mean)) as.numeric(baseline_row$temp_baseline_mean) else NA_real_
  b_temp_sd <- if (!is.null(baseline_row$temp_baseline_sd)) as.numeric(baseline_row$temp_baseline_sd) else NA_real_
  
  if (is.na(temp_val) || is.na(b_temp_mean) || is.na(b_temp_sd) || b_temp_sd <= 0) {
    temp_signal <- "Unavailable"
    temp_thresh <- NA_real_
    temp_z <- NA_real_
  } else {
    temp_thresh <- round(b_temp_mean + (z_mild * b_temp_sd), 2)
    temp_z <- round((temp_val - b_temp_mean) / b_temp_sd, 2)
    temp_signal <- if (temp_val >= temp_thresh) "Hot" else "Normal"
  }
  
  # Rainfall signal
  precip_val <- if (!is.null(outlook_row$precipitation)) as.numeric(outlook_row$precipitation) else NA_real_
  b_p10 <- if (!is.null(baseline_row$precip_baseline_p10)) as.numeric(baseline_row$precip_baseline_p10) else NA_real_
  b_p90 <- if (!is.null(baseline_row$precip_baseline_p90)) as.numeric(baseline_row$precip_baseline_p90) else NA_real_
  b_precip_mean <- if (!is.null(baseline_row$precip_baseline_mean)) as.numeric(baseline_row$precip_baseline_mean) else NA_real_
  
  if (is.na(precip_val) || is.na(b_p10) || is.na(b_p90)) {
    precip_signal <- "Unavailable"
  } else {
    if (precip_val >= b_p90) {
      precip_signal <- "Wet"
    } else if (precip_val <= b_p10) {
      precip_signal <- "Dry"
    } else {
      precip_signal <- "Normal"
    }
  }
  
  # Humidity signal (supporting context)
  hum_val <- if (!is.null(outlook_row$humidity)) as.numeric(outlook_row$humidity) else NA_real_
  b_hum_mean <- if (!is.null(baseline_row$humidity_baseline_mean)) as.numeric(baseline_row$humidity_baseline_mean) else NA_real_
  b_hum_sd <- if (!is.null(baseline_row$humidity_baseline_sd)) as.numeric(baseline_row$humidity_baseline_sd) else NA_real_
  
  if (is.na(hum_val) || is.na(b_hum_mean) || is.na(b_hum_sd) || b_hum_sd <= 0) {
    hum_signal <- "Unavailable"
    hum_thresh <- NA_real_
    hum_z <- NA_real_
  } else {
    hum_thresh <- round(b_hum_mean + (1.5 * b_hum_sd), 1)
    hum_z <- round((hum_val - b_hum_mean) / b_hum_sd, 2)
    hum_signal <- if (hum_val >= hum_thresh) "High Humidity" else "Normal"
  }
  
  list(
    timescale = "monthly",
    temperature = list(
      value = temp_val,
      baseline_mean = b_temp_mean,
      baseline_sd = b_temp_sd,
      threshold = temp_thresh,
      z_score = temp_z,
      signal = temp_signal,
      unit = "°C"
    ),
    precipitation = list(
      value = precip_val,
      baseline_mean = b_precip_mean,
      threshold_p10 = b_p10,
      threshold_p90 = b_p90,
      signal = precip_signal,
      unit = "mm"
    ),
    humidity = list(
      value = hum_val,
      baseline_mean = b_hum_mean,
      baseline_sd = b_hum_sd,
      threshold = hum_thresh,
      z_score = hum_z,
      signal = hum_signal,
      unit = "%"
    )
  )
}

#' Extract Climate Signals from Short-Term Live Weather Day
#'
#' @param daily_row List with daily weather fields (temp_max, precip_sum, wind_gust, humidity)
#' @param baseline_month_row Baseline row for current month (optional supporting context)
#' @param config Application settings
#' @return List of live short-term signals with defined daily thresholds
extract_live_climate_signals <- function(daily_row, baseline_month_row = NULL, config = NULL) {
  # Configured daily thresholds
  hot_thresh <- if (!is.null(config$community$live_hot_threshold_c)) config$community$live_hot_threshold_c else 40.0
  wet_thresh <- if (!is.null(config$community$live_wet_threshold_mm)) config$community$live_wet_threshold_mm else 35.0
  wind_thresh <- if (!is.null(config$community$live_wind_threshold_kmh)) config$community$live_wind_threshold_kmh else 50.0
  
  t_max <- if (!is.null(daily_row$temp_max)) as.numeric(daily_row$temp_max) else NA_real_
  p_sum <- if (!is.null(daily_row$precip_sum)) as.numeric(daily_row$precip_sum) else NA_real_
  w_gust <- if (!is.null(daily_row$wind_gust)) as.numeric(daily_row$wind_gust) else NA_real_
  h_val <- if (!is.null(daily_row$humidity)) as.numeric(daily_row$humidity) else NA_real_
  
  temp_sig <- if (is.na(t_max)) "Unavailable" else if (t_max >= hot_thresh) "Hot" else "Normal"
  precip_sig <- if (is.na(p_sum)) "Unavailable" else if (p_sum >= wet_thresh) "Wet" else "Normal"
  wind_sig <- if (is.na(w_gust)) "Unavailable" else if (w_gust >= wind_thresh) "Strong Wind" else "Normal"
  hum_sig <- if (is.na(h_val)) "Unavailable" else if (h_val >= 80.0) "High Humidity" else "Normal"
  
  list(
    timescale = "live_daily",
    temperature = list(
      value = t_max,
      threshold = hot_thresh,
      signal = temp_sig,
      unit = "°C",
      metric_label = "Daily Maximum Temperature"
    ),
    precipitation = list(
      value = p_sum,
      threshold = wet_thresh,
      signal = precip_sig,
      unit = "mm",
      metric_label = "Daily 24h Precipitation"
    ),
    wind = list(
      value = w_gust,
      threshold = wind_thresh,
      signal = wind_sig,
      unit = "km/h",
      metric_label = "Peak Wind Gust"
    ),
    humidity = list(
      value = h_val,
      threshold = 80.0,
      signal = hum_sig,
      unit = "%",
      metric_label = "Relative Humidity"
    )
  )
}

# -------------------------------------------------------------------
# 2. INDIVIDUAL IMPACT EVALUATION FUNCTIONS
# -------------------------------------------------------------------

#' Assess Rain-Related Disruption
#'
#' Demonstration Rules:
#' - Wet signal + poor drainage OR frequent waterlogging -> High
#' - Wet signal + moderate drainage OR occasional waterlogging -> Moderate
#' - Wet signal + good drainage AND no previous waterlogging -> Low
#' - Wet signal with insufficient relevant inputs -> Insufficient information
#'
#' @param precip_signal Character ("Wet", "Normal", "Dry", "Unavailable")
#' @param drainage Character ("Good", "Moderate", "Poor", "Unknown")
#' @param waterlogging Character ("Never", "Occasionally", "Frequently", "Unknown")
#' @return List with card evaluation details
assess_rain_disruption <- function(precip_signal, drainage, waterlogging) {
  drainage <- if (is.null(drainage)) "Unknown" else drainage
  waterlogging <- if (is.null(waterlogging)) "Unknown" else waterlogging
  
  missing_inputs <- c()
  if (drainage == "Unknown") missing_inputs <- c(missing_inputs, "Drainage condition")
  if (waterlogging == "Unknown") missing_inputs <- c(missing_inputs, "Previous waterlogging history")
  
  # Rule precedence step 1: Climate signal availability
  if (precip_signal == "Unavailable") {
    return(list(
      title = "Rain-Related Disruption",
      concern_level = "Insufficient information",
      climate_signal = "Unavailable",
      active_signal = FALSE,
      inputs = list(drainage = drainage, waterlogging = waterlogging),
      explanation = "Rainfall outlook or seasonal baseline data is unavailable for this period. A scenario assessment cannot be computed.",
      suggestions = c("Review seasonal station coverage", "Check local municipal drainage advisories directly"),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule Precedence 1: Signal is Unavailable -> Insufficient information"
    ))
  }
  
  # Rule precedence step 2: Inactive climate signal
  if (precip_signal != "Wet") {
    return(list(
      title = "Rain-Related Disruption",
      concern_level = "No trigger from selected monthly outlook",
      climate_signal = paste0("Normal/Dry (", precip_signal, ")"),
      active_signal = FALSE,
      inputs = list(drainage = drainage, waterlogging = waterlogging),
      explanation = "The selected outlook does not project an unusually wet rainfall signal above the seasonal P90 threshold. Note: The absence of a monthly wet trigger does not rule out brief intense downpours or localized street ponding.",
      suggestions = c(
        "Maintain routine cleaning of private gutters and surface drains",
        "Observe standard municipal monsoon advisories during rain events"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule Precedence 2: Precip signal is not 'Wet' -> No trigger from selected monthly outlook"
    ))
  }
  
  # Rule precedence step 3: Active signal -> Evaluate High first
  # Wet + Poor drainage OR Frequent waterlogging
  if (drainage == "Poor" || waterlogging == "Frequently") {
    reasons <- c()
    if (drainage == "Poor") reasons <- c(reasons, "poor drainage")
    if (waterlogging == "Frequently") reasons <- c(reasons, "frequently recurring waterlogging history")
    
    return(list(
      title = "Rain-Related Disruption",
      concern_level = "High",
      climate_signal = "Wet",
      active_signal = TRUE,
      inputs = list(drainage = drainage, waterlogging = waterlogging),
      explanation = sprintf(
        "A wet climate signal combined with %s indicates elevated risk of surface accumulation, localized travel bottlenecks, and prolonged waterlogging.",
        paste(reasons, collapse = " and ")
      ),
      suggestions = c(
        "Clear silt and blockages from immediate premises and boundary drains",
        "Elevate sensitive domestic inventory, electrical connections, and motors in ground-level areas",
        "Identify alternate elevated pedestrian and vehicular transit routes in advance",
        "Keep emergency rain gear, gumboots, and battery-powered flashlights readily accessible"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 1 (High): Wet signal + (Poor drainage OR Frequent waterlogging)"
    ))
  }
  
  # Rule precedence step 4: Evaluate Moderate
  # Wet + Moderate drainage OR Occasional waterlogging
  if (drainage == "Moderate" || waterlogging == "Occasionally") {
    reasons <- c()
    if (drainage == "Moderate") reasons <- c(reasons, "moderate drainage capacity")
    if (waterlogging == "Occasionally") reasons <- c(reasons, "occasional waterlogging history")
    
    return(list(
      title = "Rain-Related Disruption",
      concern_level = "Moderate",
      climate_signal = "Wet",
      active_signal = TRUE,
      inputs = list(drainage = drainage, waterlogging = waterlogging),
      explanation = sprintf(
        "A wet climate signal with %s suggests potential for temporary slow drainage and localized sidewalk water pooling during peak rainfall spells.",
        paste(reasons, collapse = " and ")
      ),
      suggestions = c(
        "Inspect local storm gullies and report surface clogging before heavy rainfall",
        "Plan commute schedules with extra transit buffer during peak downpours",
        "Ensure basement and underground sump runoff grates are clear of debris"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 2 (Moderate): Wet signal + (Moderate drainage OR Occasional waterlogging)"
    ))
  }
  
  # Rule precedence step 5: Evaluate Low
  # Wet + Good drainage AND Never waterlogged
  if (drainage == "Good" && waterlogging == "Never") {
    return(list(
      title = "Rain-Related Disruption",
      concern_level = "Low",
      climate_signal = "Wet",
      active_signal = TRUE,
      inputs = list(drainage = drainage, waterlogging = waterlogging),
      explanation = "Although an unusually wet climate signal is projected, good local drainage and no history of previous waterlogging indicate substantial local resilience to surface runoff.",
      suggestions = c(
        "Maintain routine monitoring of property perimeter drainage",
        "Ensure rainwater harvesting silt traps are cleaned regularly"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 3 (Low): Wet signal + (Good drainage AND Never waterlogged)"
    ))
  }
  
  # Rule precedence step 6: Insufficient information
  list(
    title = "Rain-Related Disruption",
    concern_level = "Insufficient information",
    climate_signal = "Wet",
    active_signal = TRUE,
    inputs = list(drainage = drainage, waterlogging = waterlogging),
    explanation = "An unusually wet climate signal is active, but local drainage and previous waterlogging are Unknown. Cannot determine vulnerability concern level without key neighborhood characteristics.",
    suggestions = c(
      "Update profile with known drainage or waterlogging observations",
      "Consult local community members or municipal ward maps to identify historical flood spots"
    ),
    missing_inputs = missing_inputs,
    calculation_rule = "Rule 4: Wet signal with unresolved Unknown inputs -> Insufficient information"
  )
}

#' Assess Water-Supply Pressure
#'
#' Demonstration Rules:
#' - Dry signal + no water storage OR high rainfall dependence -> High
#' - Dry signal + limited storage OR medium rainfall dependence -> Moderate
#' - Dry signal + adequate storage AND low rainfall dependence -> Low
#' - Dry signal with insufficient relevant inputs -> Insufficient information
#'
#' @param precip_signal Character ("Dry", "Normal", "Wet", "Unavailable")
#' @param water_storage Character ("Adequate", "Limited", "None", "Unknown")
#' @param rainfall_dependence Character ("Low", "Medium", "High", "Unknown")
#' @param is_live_short_term Logical indicating whether evaluation is on daily timescale
#' @return List with card evaluation details
assess_water_supply_pressure <- function(precip_signal, water_storage, rainfall_dependence, is_live_short_term = FALSE) {
  water_storage <- if (is.null(water_storage)) "Unknown" else water_storage
  rainfall_dependence <- if (is.null(rainfall_dependence)) "Unknown" else rainfall_dependence
  
  missing_inputs <- c()
  if (water_storage == "Unknown") missing_inputs <- c(missing_inputs, "Local water storage capacity")
  if (rainfall_dependence == "Unknown") missing_inputs <- c(missing_inputs, "Rainfall dependence for supply")
  
  # Timescale restriction: Disable on live short-term daily weather
  if (is_live_short_term) {
    return(list(
      title = "Water-Supply Pressure",
      concern_level = "Insufficient information",
      climate_signal = "Timescale Not Applicable",
      active_signal = FALSE,
      inputs = list(water_storage = water_storage, rainfall_dependence = rainfall_dependence),
      explanation = "Water-supply pressure reflects multi-week reservoir, aquifer, and municipal supply dynamics. Short-term daily or 48-hour weather forecasts do not provide sufficient timescale evidence to evaluate long-term municipal water depletion.",
      suggestions = c(
        "Switch to the Monthly Forecast or Historical Monthly assessment source to evaluate seasonal water stress",
        "Monitor local municipal water utility announcements for supply rationing notices"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Timescale Check: Live daily forecasts cannot establish multi-week water-supply pressure"
    ))
  }
  
  # Rule precedence step 1: Signal availability
  if (precip_signal == "Unavailable") {
    return(list(
      title = "Water-Supply Pressure",
      concern_level = "Insufficient information",
      climate_signal = "Unavailable",
      active_signal = FALSE,
      inputs = list(water_storage = water_storage, rainfall_dependence = rainfall_dependence),
      explanation = "Rainfall outlook or baseline percentile thresholds are unavailable for this period.",
      suggestions = c("Review historical station data coverage"),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule Precedence 1: Signal is Unavailable -> Insufficient information"
    ))
  }
  
  # Rule precedence step 2: Inactive climate signal
  if (precip_signal != "Dry") {
    return(list(
      title = "Water-Supply Pressure",
      concern_level = "No trigger from selected monthly outlook",
      climate_signal = paste0("Normal/Wet (", precip_signal, ")"),
      active_signal = FALSE,
      inputs = list(water_storage = water_storage, rainfall_dependence = rainfall_dependence),
      explanation = "The selected outlook does not project an unusually dry monthly signal below the seasonal P10 threshold. Note: Ongoing routine water conservation remains recommended regardless of monthly precipitation.",
      suggestions = c(
        "Continue standard water-efficiency practices",
        "Inspect domestic plumbing and overhead tanks for leaks"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule Precedence 2: Precip signal is not 'Dry' -> No trigger from selected monthly outlook"
    ))
  }
  
  # Rule precedence step 3: Active signal -> Evaluate High first
  # Dry + No water storage OR High rainfall dependence
  if (water_storage == "None" || rainfall_dependence == "High") {
    reasons <- c()
    if (water_storage == "None") reasons <- c(reasons, "absence of buffer water storage")
    if (rainfall_dependence == "High") reasons <- c(reasons, "high dependence on immediate rainfall / shallow wells")
    
    return(list(
      title = "Water-Supply Pressure",
      concern_level = "High",
      climate_signal = "Dry",
      active_signal = TRUE,
      inputs = list(water_storage = water_storage, rainfall_dependence = rainfall_dependence),
      explanation = sprintf(
        "An unusually dry monthly outlook combined with %s indicates acute susceptibility to local supply shortages and depleted groundwater replenishment.",
        paste(reasons, collapse = " and ")
      ),
      suggestions = c(
        "Prioritize water allocation strictly for drinking, cooking, and essential sanitation",
        "Install or expand domestic buffer rainwater and municipal storage tanks",
        "Explore community bulk tanker agreements or shared borewell contingency plans",
        "Repair all dripping taps, flush valves, and overhead tank overflows immediately"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 1 (High): Dry signal + (No storage OR High rainfall dependence)"
    ))
  }
  
  # Rule precedence step 4: Evaluate Moderate
  # Dry + Limited storage OR Medium rainfall dependence
  if (water_storage == "Limited" || rainfall_dependence == "Medium") {
    reasons <- c()
    if (water_storage == "Limited") reasons <- c(reasons, "limited buffer storage capacity")
    if (rainfall_dependence == "Medium") reasons <- c(reasons, "moderate dependence on rainfall replenishment")
    
    return(list(
      title = "Water-Supply Pressure",
      concern_level = "Moderate",
      climate_signal = "Dry",
      active_signal = TRUE,
      inputs = list(water_storage = water_storage, rainfall_dependence = rainfall_dependence),
      explanation = sprintf(
        "An unusually dry monthly outlook with %s suggests potential for local water pressure drops or intermittent supply rationing.",
        paste(reasons, collapse = " and ")
      ),
      suggestions = c(
        "Implement rotational storage tank filling schedules",
        "Stagger non-essential laundry, vehicle washing, and garden irrigation",
        "Audit domestic daily consumption per capita"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 2 (Moderate): Dry signal + (Limited storage OR Medium rainfall dependence)"
    ))
  }
  
  # Rule precedence step 5: Evaluate Low
  # Dry + Adequate storage AND Low rainfall dependence
  if (water_storage == "Adequate" && rainfall_dependence == "Low") {
    return(list(
      title = "Water-Supply Pressure",
      concern_level = "Low",
      climate_signal = "Dry",
      active_signal = TRUE,
      inputs = list(water_storage = water_storage, rainfall_dependence = rainfall_dependence),
      explanation = "Although an unusually dry monthly outlook is projected, adequate local storage infrastructure and low dependence on direct local rainfall provide high buffer capacity against supply interruptions.",
      suggestions = c(
        "Maintain periodic water quality testing for stored tank water",
        "Preserve water conservation protocols during prolonged dry intervals"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 3 (Low): Dry signal + (Adequate storage AND Low rainfall dependence)"
    ))
  }
  
  # Rule precedence step 6: Insufficient information
  list(
    title = "Water-Supply Pressure",
    concern_level = "Insufficient information",
    climate_signal = "Dry",
    active_signal = TRUE,
    inputs = list(water_storage = water_storage, rainfall_dependence = rainfall_dependence),
    explanation = "An unusually dry climate signal is active, but local storage capacity and rainfall dependence are Unknown. Cannot determine vulnerability concern level without these infrastructure inputs.",
    suggestions = c(
      "Update profile with estimated domestic storage days or primary water source type"
    ),
    missing_inputs = missing_inputs,
    calculation_rule = "Rule 4: Dry signal with unresolved Unknown inputs -> Insufficient information"
  )
}

#' Assess Outdoor Heat Exposure
#'
#' Demonstration Rules:
#' - Hot signal + high outdoor exposure AND limited/poor shade or cooling -> High
#' - Hot signal + high outdoor exposure OR limited/poor shade or cooling -> Moderate
#' - Hot signal + low/medium outdoor exposure AND good shade/cooling -> Low
#' - Hot signal with insufficient relevant inputs -> Insufficient information
#'
#' @param temp_signal Character ("Hot", "Normal", "Cold", "Unavailable")
#' @param outdoor_work Character ("Low", "Medium", "High", "Unknown")
#' @param shade_cooling Character ("Good", "Limited", "Poor", "Unknown")
#' @return List with card evaluation details
assess_outdoor_heat_exposure <- function(temp_signal, outdoor_work, shade_cooling) {
  outdoor_work <- if (is.null(outdoor_work)) "Unknown" else outdoor_work
  shade_cooling <- if (is.null(shade_cooling)) "Unknown" else shade_cooling
  
  missing_inputs <- c()
  if (outdoor_work == "Unknown") missing_inputs <- c(missing_inputs, "Outdoor work exposure")
  if (shade_cooling == "Unknown") missing_inputs <- c(missing_inputs, "Access to shade or cooling")
  
  # Rule precedence step 1: Signal availability
  if (temp_signal == "Unavailable") {
    return(list(
      title = "Outdoor Heat Exposure",
      concern_level = "Insufficient information",
      climate_signal = "Unavailable",
      active_signal = FALSE,
      inputs = list(outdoor_work = outdoor_work, shade_cooling = shade_cooling),
      explanation = "Temperature outlook or baseline standard deviation is unavailable for this period.",
      suggestions = c("Review historical temperature data coverage"),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule Precedence 1: Signal is Unavailable -> Insufficient information"
    ))
  }
  
  # Rule precedence step 2: Inactive climate signal
  if (temp_signal != "Hot") {
    return(list(
      title = "Outdoor Heat Exposure",
      concern_level = "No trigger from selected monthly outlook",
      climate_signal = paste0("Normal (", temp_signal, ")"),
      active_signal = FALSE,
      inputs = list(outdoor_work = outdoor_work, shade_cooling = shade_cooling),
      explanation = "The selected outlook does not cross the positive hot-anomaly threshold (+1.5 Z-score above baseline mean). Note: This reflects monthly average conditions and does not rule out brief hot afternoons or warm days.",
      suggestions = c(
        "Maintain adequate hydration during outdoor activities",
        "Wear sun-protective hats and lightweight clothing when working outside"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule Precedence 2: Temp signal is not 'Hot' -> No trigger from selected monthly outlook"
    ))
  }
  
  # Rule precedence step 3: Active signal -> Evaluate High first
  # Hot + High outdoor work AND Limited/Poor shade/cooling
  if (outdoor_work == "High" && shade_cooling %in% c("Limited", "Poor")) {
    return(list(
      title = "Outdoor Heat Exposure",
      concern_level = "High",
      climate_signal = "Hot",
      active_signal = TRUE,
      inputs = list(outdoor_work = outdoor_work, shade_cooling = shade_cooling),
      explanation = "An unusually hot climate signal combined with high outdoor physical exposure and limited or poor shade/cooling indicates severe thermal strain and dehydration vulnerability.",
      suggestions = c(
        "Reschedule heavy manual outdoor work to early mornings (before 10:00 AM) or late afternoons (after 4:30 PM)",
        "Set up mandatory shaded rest stations equipped with drinking water and Oral Rehydration Salts (ORS)",
        "Implement a buddy system to identify early symptoms of heat exhaustion (dizziness, nausea, cramping)",
        "Provide wide-brimmed head coverings, loose breathable cotton clothing, and frequent rest pauses"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 1 (High): Hot signal + (High outdoor work AND Limited/Poor shade)"
    ))
  }
  
  # Rule precedence step 4: Evaluate Moderate
  # Hot + High outdoor work OR Limited/Poor shade/cooling
  if (outdoor_work == "High" || shade_cooling %in% c("Limited", "Poor")) {
    reasons <- c()
    if (outdoor_work == "High") reasons <- c(reasons, "high outdoor exposure")
    if (shade_cooling %in% c("Limited", "Poor")) reasons <- c(reasons, "constrained access to shade and cool rest areas")
    
    return(list(
      title = "Outdoor Heat Exposure",
      concern_level = "Moderate",
      climate_signal = "Hot",
      active_signal = TRUE,
      inputs = list(outdoor_work = outdoor_work, shade_cooling = shade_cooling),
      explanation = sprintf(
        "An unusually hot climate signal with %s suggests heightened thermal discomfort and risk of fatigue during peak midday hours.",
        paste(reasons, collapse = " and ")
      ),
      suggestions = c(
        "Establish shaded rest breaks every 45–60 minutes for outdoor personnel",
        "Encourage regular water and electrolyte intake rather than waiting for thirst",
        "Utilize damp towels, misting fans, or shaded transit zones where feasible"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 2 (Moderate): Hot signal + (High outdoor work OR Limited/Poor shade)"
    ))
  }
  
  # Rule precedence step 5: Evaluate Low
  # Hot + Low/Medium outdoor work AND Good shade/cooling
  if (outdoor_work %in% c("Low", "Medium") && shade_cooling == "Good") {
    return(list(
      title = "Outdoor Heat Exposure",
      concern_level = "Low",
      climate_signal = "Hot",
      active_signal = TRUE,
      inputs = list(outdoor_work = outdoor_work, shade_cooling = shade_cooling),
      explanation = "Although an unusually hot climate signal is projected, moderate-to-low outdoor exposure and good access to shaded or cooled spaces effectively cushion against thermal strain.",
      suggestions = c(
        "Maintain standard daytime hydration protocols",
        "Ventilate indoor spaces during cooler night and early morning hours"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 3 (Low): Hot signal + (Low/Medium outdoor work AND Good shade)"
    ))
  }
  
  # Rule precedence step 6: Insufficient information
  list(
    title = "Outdoor Heat Exposure",
    concern_level = "Insufficient information",
    climate_signal = "Hot",
    active_signal = TRUE,
    inputs = list(outdoor_work = outdoor_work, shade_cooling = shade_cooling),
    explanation = "An unusually hot climate signal is active, but outdoor work exposure and shade access are Unknown. Cannot determine vulnerability concern level without these personal and occupational parameters.",
    suggestions = c(
      "Update profile with estimated hours spent outdoors or availability of shaded facilities"
    ),
    missing_inputs = missing_inputs,
    calculation_rule = "Rule 4: Hot signal with unresolved Unknown inputs -> Insufficient information"
  )
}

#' Assess Cooling-Access Pressure
#'
#' Demonstration Rules:
#' - Hot signal + frequent power interruptions AND limited/poor shade or cooling -> High
#' - Hot signal + occasional/frequent power interruptions OR limited/poor shade or cooling -> Moderate
#' - Hot signal + rare interruptions AND good shade/cooling -> Low
#' - Hot signal with insufficient relevant inputs -> Insufficient information
#'
#' @param temp_signal Character ("Hot", "Normal", "Cold", "Unavailable")
#' @param power_interruptions Character ("Rare", "Occasional", "Frequent", "Unknown")
#' @param shade_cooling Character ("Good", "Limited", "Poor", "Unknown")
#' @return List with card evaluation details
assess_cooling_access_pressure <- function(temp_signal, power_interruptions, shade_cooling) {
  power_interruptions <- if (is.null(power_interruptions)) "Unknown" else power_interruptions
  shade_cooling <- if (is.null(shade_cooling)) "Unknown" else shade_cooling
  
  missing_inputs <- c()
  if (power_interruptions == "Unknown") missing_inputs <- c(missing_inputs, "Power interruption frequency")
  if (shade_cooling == "Unknown") missing_inputs <- c(missing_inputs, "Access to shade or cooling")
  
  # Rule precedence step 1: Signal availability
  if (temp_signal == "Unavailable") {
    return(list(
      title = "Cooling-Access Pressure",
      concern_level = "Insufficient information",
      climate_signal = "Unavailable",
      active_signal = FALSE,
      inputs = list(power_interruptions = power_interruptions, shade_cooling = shade_cooling),
      explanation = "Temperature outlook or baseline statistics are unavailable for this period.",
      suggestions = c("Review historical station data coverage"),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule Precedence 1: Signal is Unavailable -> Insufficient information"
    ))
  }
  
  # Rule precedence step 2: Inactive climate signal
  if (temp_signal != "Hot") {
    return(list(
      title = "Cooling-Access Pressure",
      concern_level = "No trigger from selected monthly outlook",
      climate_signal = paste0("Normal (", temp_signal, ")"),
      active_signal = FALSE,
      inputs = list(power_interruptions = power_interruptions, shade_cooling = shade_cooling),
      explanation = "The selected outlook does not project an unusually hot monthly mean anomaly. Standard electrical grid cooling and domestic ventilation requirements are anticipated.",
      suggestions = c(
        "Conduct routine maintenance on fans, air-coolers, or air conditioners",
        "Ensure window mesh and blinds are in good working order"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule Precedence 2: Temp signal is not 'Hot' -> No trigger from selected monthly outlook"
    ))
  }
  
  # Rule precedence step 3: Active signal -> Evaluate High first
  # Hot + Frequent power interruptions AND Limited/Poor shade/cooling
  if (power_interruptions == "Frequent" && shade_cooling %in% c("Limited", "Poor")) {
    return(list(
      title = "Cooling-Access Pressure",
      concern_level = "High",
      climate_signal = "Hot",
      active_signal = TRUE,
      inputs = list(power_interruptions = power_interruptions, shade_cooling = shade_cooling),
      explanation = "An unusually hot climate signal combined with frequent electrical outages and poor access to passive shade or cooling creates acute indoor thermal trap risks and sleep disruption.",
      suggestions = c(
        "Service inverter batteries, backup generators, or portable rechargeable fans prior to peak hot months",
        "Create natural cross-ventilation corridors by opening shaded opposing windows at dawn and dusk",
        "Apply solar-reflective coatings, bamboo chick-blinds, or wet curtains to sun-exposed exterior walls",
        "Identify accessible community centers, libraries, or cooled public buildings for relief during peak outages"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 1 (High): Hot signal + (Frequent power cuts AND Limited/Poor shade)"
    ))
  }
  
  # Rule precedence step 4: Evaluate Moderate
  # Hot + Occasional/Frequent power cuts OR Limited/Poor shade/cooling
  if (power_interruptions %in% c("Occasional", "Frequent") || shade_cooling %in% c("Limited", "Poor")) {
    reasons <- c()
    if (power_interruptions %in% c("Occasional", "Frequent")) reasons <- c(reasons, "unreliable electrical power supply")
    if (shade_cooling %in% c("Limited", "Poor")) reasons <- c(reasons, "limited passive cooling and shade infrastructure")
    
    return(list(
      title = "Cooling-Access Pressure",
      concern_level = "Moderate",
      climate_signal = "Hot",
      active_signal = TRUE,
      inputs = list(power_interruptions = power_interruptions, shade_cooling = shade_cooling),
      explanation = sprintf(
        "An unusually hot climate signal with %s suggests episodic indoor stuffiness and cooling constraints during midday peak electrical demand periods.",
        paste(reasons, collapse = " and ")
      ),
      suggestions = c(
        "Minimize daytime operation of heat-emitting appliances (microwaves, ovens, high-wattage lamps)",
        "Ensure domestic ceiling fans and evaporative air-coolers are descaled and functioning efficiently",
        "Maintain solar shading on west-facing and south-facing windows during afternoon hours"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 2 (Moderate): Hot signal + (Occasional/Frequent power cuts OR Limited/Poor shade)"
    ))
  }
  
  # Rule precedence step 5: Evaluate Low
  # Hot + Rare power cuts AND Good shade/cooling
  if (power_interruptions == "Rare" && shade_cooling == "Good") {
    return(list(
      title = "Cooling-Access Pressure",
      concern_level = "Low",
      climate_signal = "Hot",
      active_signal = TRUE,
      inputs = list(power_interruptions = power_interruptions, shade_cooling = shade_cooling),
      explanation = "Although an unusually hot climate signal is projected, reliable electrical grid power and good domestic shade/cooling access provide robust defense against thermal discomfort.",
      suggestions = c(
        "Set thermostat temperatures to an energy-efficient 24–26°C to prevent excessive grid load",
        "Keep air filters clean to optimize cooling airflow efficiency"
      ),
      missing_inputs = missing_inputs,
      calculation_rule = "Rule 3 (Low): Hot signal + (Rare power cuts AND Good shade)"
    ))
  }
  
  # Rule precedence step 6: Insufficient information
  list(
    title = "Cooling-Access Pressure",
    concern_level = "Insufficient information",
    climate_signal = "Hot",
    active_signal = TRUE,
    inputs = list(power_interruptions = power_interruptions, shade_cooling = shade_cooling),
    explanation = "An unusually hot climate signal is active, but power interruption frequency and cooling access are Unknown. Cannot determine vulnerability concern level without these infrastructure indicators.",
    suggestions = c(
      "Update profile with estimated weekly power outages or cooling availability"
    ),
    missing_inputs = missing_inputs,
    calculation_rule = "Rule 4: Hot signal with unresolved Unknown inputs -> Insufficient information"
  )
}

# -------------------------------------------------------------------
# 3. COMPREHENSIVE ASSESSMENT AND WHAT-IF COMPARISON
# -------------------------------------------------------------------

#' Evaluate All Four Impact Cards
#'
#' @param climate_signals List returned by extract_monthly_climate_signals() or extract_live_climate_signals()
#' @param profile List or data frame row of vulnerability inputs
#' @return List containing all 4 impact card assessments
assess_all_community_impacts <- function(climate_signals, profile) {
  is_live <- isTRUE(climate_signals$timescale == "live_daily")
  
  t_sig <- climate_signals$temperature$signal
  p_sig <- climate_signals$precipitation$signal
  
  card_rain <- assess_rain_disruption(
    precip_signal = p_sig,
    drainage = profile$drainage,
    waterlogging = profile$waterlogging
  )
  
  card_water <- assess_water_supply_pressure(
    precip_signal = p_sig,
    water_storage = profile$water_storage,
    rainfall_dependence = profile$rainfall_dependence,
    is_live_short_term = is_live
  )
  
  card_heat <- assess_outdoor_heat_exposure(
    temp_signal = t_sig,
    outdoor_work = profile$outdoor_work,
    shade_cooling = profile$shade_cooling
  )
  
  card_cooling <- assess_cooling_access_pressure(
    temp_signal = t_sig,
    power_interruptions = profile$power_interruptions,
    shade_cooling = profile$shade_cooling
  )
  
  list(
    rain_disruption = card_rain,
    water_supply = card_water,
    outdoor_heat = card_heat,
    cooling_access = card_cooling,
    climate_signals = climate_signals,
    profile = profile
  )
}

#' Compare Current Scenario vs What-If Scenario
#'
#' Evaluates what-if modifications against the baseline scenario,
#' identifying which vulnerability inputs changed and producing plain-language
#' explanations for why each impact concern level changed or remained unchanged.
#'
#' @param current_assessment List from assess_all_community_impacts()
#' @param whatif_assessment List from assess_all_community_impacts() with what-if profile
#' @return List of comparison results per impact card
compare_whatif_scenarios <- function(current_assessment, whatif_assessment) {
  cur_prof <- current_assessment$profile
  wif_prof <- whatif_assessment$profile
  
  # Identify changed inputs
  v_fields <- c(
    drainage = "Drainage",
    waterlogging = "Previous waterlogging",
    water_storage = "Water storage",
    rainfall_dependence = "Rainfall dependence",
    outdoor_work = "Outdoor work exposure",
    shade_cooling = "Access to shade/cooling",
    power_interruptions = "Power interruptions"
  )
  
  changed_inputs <- list()
  for (f in names(v_fields)) {
    c_val <- if (!is.null(cur_prof[[f]])) cur_prof[[f]] else "Unknown"
    w_val <- if (!is.null(wif_prof[[f]])) wif_prof[[f]] else "Unknown"
    if (c_val != w_val) {
      changed_inputs[[f]] <- list(
        name = v_fields[[f]],
        from = c_val,
        to = w_val
      )
    }
  }
  
  cards <- c("rain_disruption", "water_supply", "outdoor_heat", "cooling_access")
  card_names <- c(
    rain_disruption = "Rain-related disruption",
    water_supply = "Water-supply pressure",
    outdoor_heat = "Outdoor heat exposure",
    cooling_access = "Cooling-access pressure"
  )
  
  card_comparisons <- list()
  
  for (c_key in cards) {
    cur_c <- current_assessment[[c_key]]
    wif_c <- whatif_assessment[[c_key]]
    
    cur_level <- cur_c$concern_level
    wif_level <- wif_c$concern_level
    is_changed <- (cur_level != wif_level)
    
    # Generate transparent explanation for why it changed or stayed same
    explanation <- ""
    
    if (c_key == "rain_disruption") {
      d_diff <- cur_prof$drainage != wif_prof$drainage
      w_diff <- cur_prof$waterlogging != wif_prof$waterlogging
      
      if (!cur_c$active_signal) {
        explanation <- "No change: The climate signal does not trigger rain disruption under either scenario."
      } else if (is_changed) {
        diff_desc <- c()
        if (d_diff) diff_desc <- c(diff_desc, sprintf("drainage changed from %s to %s", cur_prof$drainage, wif_prof$drainage))
        if (w_diff) diff_desc <- c(diff_desc, sprintf("previous waterlogging changed from %s to %s", cur_prof$waterlogging, wif_prof$waterlogging))
        explanation <- sprintf(
          "Rain-related disruption changed from %s to %s because %s.",
          cur_level, wif_level, paste(diff_desc, collapse = ", and ")
        )
      } else {
        if (wif_level == "High" && (wif_prof$drainage == "Poor" || wif_prof$waterlogging == "Frequently")) {
          persistence_factor <- if (wif_prof$waterlogging == "Frequently") "previous waterlogging remains Frequent" else "drainage remains Poor"
          explanation <- sprintf("Concern level remained High because %s, maintaining elevated runoff vulnerability.", persistence_factor)
        } else if (d_diff || w_diff) {
          explanation <- sprintf("Concern level remained %s despite changes because the updated conditions still fall under the %s threshold rule.", wif_level, tolower(wif_level))
        } else {
          explanation <- "No change: Relevant vulnerability assumptions for rain disruption (drainage and waterlogging) were not modified."
        }
      }
      
    } else if (c_key == "water_supply") {
      s_diff <- cur_prof$water_storage != wif_prof$water_storage
      r_diff <- cur_prof$rainfall_dependence != wif_prof$rainfall_dependence
      
      if (!cur_c$active_signal) {
        explanation <- "No change: The climate signal does not trigger water-supply pressure under either scenario."
      } else if (is_changed) {
        diff_desc <- c()
        if (s_diff) diff_desc <- c(diff_desc, sprintf("water storage changed from %s to %s", cur_prof$water_storage, wif_prof$water_storage))
        if (r_diff) diff_desc <- c(diff_desc, sprintf("rainfall dependence changed from %s to %s", cur_prof$rainfall_dependence, wif_prof$rainfall_dependence))
        explanation <- sprintf(
          "Water-supply pressure changed from %s to %s because %s.",
          cur_level, wif_level, paste(diff_desc, collapse = ", and ")
        )
      } else {
        if (wif_level == "High" && (wif_prof$water_storage == "None" || wif_prof$rainfall_dependence == "High")) {
          persistence_factor <- if (wif_prof$water_storage == "None") "no water storage is available" else "rainfall dependence remains High"
          explanation <- sprintf("Concern level remained High because %s, sustaining critical vulnerability.", persistence_factor)
        } else if (s_diff || r_diff) {
          explanation <- sprintf("Concern level remained %s because the modified inputs still satisfy the %s criteria.", wif_level, tolower(wif_level))
        } else {
          explanation <- "No change: Water storage and rainfall dependence inputs were not modified."
        }
      }
      
    } else if (c_key == "outdoor_heat") {
      o_diff <- cur_prof$outdoor_work != wif_prof$outdoor_work
      sh_diff <- cur_prof$shade_cooling != wif_prof$shade_cooling
      
      if (!cur_c$active_signal) {
        explanation <- "No change: The climate signal does not trigger heat exposure under either scenario."
      } else if (is_changed) {
        diff_desc <- c()
        if (o_diff) diff_desc <- c(diff_desc, sprintf("outdoor work exposure changed from %s to %s", cur_prof$outdoor_work, wif_prof$outdoor_work))
        if (sh_diff) diff_desc <- c(diff_desc, sprintf("shade access changed from %s to %s", cur_prof$shade_cooling, wif_prof$shade_cooling))
        explanation <- sprintf(
          "Outdoor heat exposure changed from %s to %s because %s.",
          cur_level, wif_level, paste(diff_desc, collapse = ", and ")
        )
      } else {
        if (wif_level == "High" && wif_prof$outdoor_work == "High" && wif_prof$shade_cooling %in% c("Limited", "Poor")) {
          explanation <- "Concern level remained High because both outdoor work exposure remains High and shade access remains Limited/Poor."
        } else if (o_diff || sh_diff) {
          explanation <- sprintf("Concern level remained %s because the modified inputs still align with the %s assessment rule.", wif_level, tolower(wif_level))
        } else {
          explanation <- "No change: Outdoor work exposure and shade access inputs were not modified."
        }
      }
      
    } else if (c_key == "cooling_access") {
      p_diff <- cur_prof$power_interruptions != wif_prof$power_interruptions
      sh_diff <- cur_prof$shade_cooling != wif_prof$shade_cooling
      
      if (!cur_c$active_signal) {
        explanation <- "No change: The climate signal does not trigger cooling pressure under either scenario."
      } else if (is_changed) {
        diff_desc <- c()
        if (p_diff) diff_desc <- c(diff_desc, sprintf("power interruptions changed from %s to %s", cur_prof$power_interruptions, wif_prof$power_interruptions))
        if (sh_diff) diff_desc <- c(diff_desc, sprintf("shade access changed from %s to %s", cur_prof$shade_cooling, wif_prof$shade_cooling))
        explanation <- sprintf(
          "Cooling-access pressure changed from %s to %s because %s.",
          cur_level, wif_level, paste(diff_desc, collapse = ", and ")
        )
      } else {
        if (wif_level == "High" && wif_prof$power_interruptions == "Frequent" && wif_prof$shade_cooling %in% c("Limited", "Poor")) {
          explanation <- "Concern level remained High because frequent power cuts and poor shade access continue concurrently."
        } else if (p_diff || sh_diff) {
          explanation <- sprintf("Concern level remained %s because adjusted inputs still meet the %s condition criteria.", wif_level, tolower(wif_level))
        } else {
          explanation <- "No change: Power interruptions and shade access inputs were not modified."
        }
      }
    }
    
    card_comparisons[[c_key]] <- list(
      title = card_names[[c_key]],
      current_level = cur_level,
      whatif_level = wif_level,
      is_changed = is_changed,
      explanation = explanation
    )
  }
  
  list(
    changed_inputs = changed_inputs,
    comparisons = card_comparisons,
    total_changes = length(changed_inputs)
  )
}
