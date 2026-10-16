# ClimateWatch: Run Instructions

## Quick Start (Run Immediately)
1. **Launch Application Server**:
   ```powershell
   Rscript scripts/run_app.R 3838
   ```
2. **Open Dashboard**:
   Navigate in your browser to:
   `http://127.0.0.1:3838`

3. **1-Click Load & Launch**:
   - On the **Data Upload** tab, click **`🌧️ 1-Click Load & Launch All Features`** to immediately activate the **CDSP Rainfall Dataset** (132,288 records across 212 Indian IMD weather stations).
   - Or click **`📂 1-Click Load Sample`** for NASA POWER Mumbai & Delhi observations.
   - Or drag-and-drop any custom weather `.csv` or `.parquet` file up to 2GB.

---

## Complete Features Overview
The application delivers 5 core features and 2 novelty features:

1. **Data Upload Hub**:
   - Single-step drag & drop for CSV/Parquet datasets up to 2GB.
   - Auto-inspection of columns, dates, and Indian weather stations.
   - 1-Click fast presets for CDSP Rainfall (132k rows) and NASA POWER samples.
   - Persistent active dataset status card with one-click page navigation.

2. **Climate Overview**:
   - Interactive Leaflet map of India displaying all 212 weather stations with exact coordinates.
   - KPI metric cards (Total Stations, Time Horizon, Eligible Months, Flagged Anomalies).
   - Monthly time-series trend analysis with multi-station comparative overlay.

3. **Climate Anomaly Explorer**:
   - Rigorous historical baselines (mean ± 1.5/2.0 SD) and non-overlapping analysis windows.
   - Flagged extreme weather anomaly markers with interactive Plotly visualizer.
   - Plain-language explainable cards explaining each anomaly in conversational terms.
   - Comprehensive anomaly records data table with CSV export.

4. **Time-Series Forecasting**:
   - Multi-model forecasting engine (Seasonal Naive, Exponential Smoothing ETS, and ARIMA).
   - Holdout train/test validation with objective accuracy metrics (RMSE, MAE, MAPE).
   - Future 12-month projections with 80% and 95% confidence intervals.

5. **Live Weather & Alerts**:
   - Real-time Open-Meteo weather API integration (no API key required).
   - Current conditions, 7-day daily forecast, and 48-hour hourly trajectory.
   - Extreme weather operational indicators comparing live conditions against baselines.

6. **Novelty Feature 1: Community Climate Impact Simulator**:
   - Translates meteorological anomalies into ground-level socio-economic impacts:
     - Rain Disruption & Urban Waterlogging
     - Agricultural Water Supply & Drought Pressure
     - Outdoor Worker Heat Exposure
     - Grid Strain & Cooling Access
   - Interactive **What-If Policy Sliders**: Test civic interventions (e.g. improving storm drainage, heat relief shelters, crop insurance) to see concern levels drop in real-time.
   - Unverified Community Observations log for citizen reporting.

7. **Novelty Feature 2: Automated Climate Risk Index & Civic Advisory Engine**:
   - Generates a **0–100 Composite Climate Vulnerability Score** for any chosen weather station.
   - Evaluates Monsoon Flood Risk, Drought Deficit Stress, Thermal Strain, and Shock Volatility.
   - AI-style Natural Language Executive Briefing for civic planners.
   - 3 prioritized municipal/community adaptation interventions with estimated risk reduction percentages.

---

## Run Test Suite
To verify backend analytical integrity, DuckDB aggregation, and model calculations:
```powershell
Rscript tests/testthat.R
```
All 4 test suites (Analytics, Community Live, Upload Pipeline, and Climate Risk) pass with 100% green tests.


