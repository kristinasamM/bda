# ClimateWatch: Spatio-Temporal Climate Anomaly Detection & Forecasting Platform

A full-stack R Shiny analytical dashboard for meteorological data analysis, spatio-temporal anomaly detection, automated multi-model time-series forecasting, live Open-Meteo API weather, and community climate impact simulation.

---

## 🚀 How Anyone Can Run This Application

### Method 1: Instant 1-Line Launch via R (No git clone needed!)
If you or your friend has **R** or **RStudio** installed, simply open R and run this **single command**:

```r
# Install shiny if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")

# Launch directly from GitHub!
shiny::runGitHub("bda", "kristinasamM")
```
*Shiny will automatically download the repository and launch the app in your browser!*

---

### Method 2: Clone or Download ZIP

1. **Clone the repository**:
   ```bash
   git clone https://github.com/kristinasamM/bda.git
   cd bda
   ```
   *(Or click **Code** > **Download ZIP** on GitHub and extract it).*

2. **Install required packages**:
   Open R or RStudio and run:
   ```r
   source("scripts/setup.R")
   ```

3. **Launch the app**:
   ```r
   source("scripts/run_app.R")
   ```
   Or from terminal / PowerShell:
   ```bash
   Rscript scripts/run_app.R 3838
   ```
   Then open your browser at `http://127.0.0.1:3838/`.

---

## 🌐 How to Share with Others on the Same Wi-Fi

The application server binds to `0.0.0.0`, allowing anyone connected to the same Wi-Fi network (home, university, or mobile hotspot) to view and use the app in real time from their phone or laptop.

Simply share your machine's local IP:
```
http://<your-local-ip>:3838/
```
*(Example: `http://192.168.1.101:3838/`)*

---

## ☁️ How to Publish as a Public Website (Shinyapps.io)

To provide a permanent public link (e.g., `https://kristinasamm.shinyapps.io/climate-analyzer`) accessible to anyone on any device without R:

1. Sign up for a free account at [shinyapps.io](https://www.shinyapps.io/).
2. Run in R:
   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name='<ACCOUNT>', token='<TOKEN>', secret='<SECRET>')
   rsconnect::deployApp()
   ```

---

## 🌟 Key Capabilities

- **Zero-Configuration Automated Ingestion**:
  - Drag-and-drop CSV or Excel datasets up to 2GB.
  - Automatic column heuristic detection, coordinate resolution for 212 IMD Indian meteorological stations, and multi-observation / sub-daily consolidation.
- **Statistical Summary & Visualizations**:
  - Dynamic KPI metric cards, data distribution summaries, and interactive multi-station time-series trend analysis.
- **Explainable Anomaly Detective**:
  - Strict seasonal baseline calculations with contextual narrative explanations.
- **Multi-Model Forecasting**:
  - Compares **Seasonal Naïve**, **Exponential Smoothing (ETS)**, and **ARIMA** models evaluated on a 12-month holdout set, with 80% and 95% prediction intervals.
- **Live Weather & Operational Alerts**:
  - Real-time weather and 7-day trajectories powered by Open-Meteo.
- **Community Impact Simulator**:
  - Socio-economic impact modeling with what-if scenario levers.
- **Climate Twin Finder**:
  - Spatial similarity matching identifying stations with matching climatic profiles.
- **Automated Test Suite**:
  - 224 unit tests covering validation, anomaly detection, DuckDB aggregation, and forecasting.
