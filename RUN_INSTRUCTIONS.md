# ClimateWatch: Run Instructions

## Prerequisites
- **R (>= 4.2.0)** installed.
- (Optional) **RStudio Desktop** for interactive running.

---

## 1. Install Dependencies
Run in PowerShell / Terminal:
```powershell
Rscript scripts/setup.R
```
Or inside R console:
```r
source("scripts/setup.R")
```

---

## 2. Generate Offline Synthetic Demo Data
Generates 2001–2024 daily meteorological series across 15 Indian locations with documented test anomalies and stores partitioned Parquet:
```powershell
Rscript scripts/generate_demo_data.R
```
Or inside R console:
```r
source("scripts/generate_demo_data.R")
```

---

## 3. Download Real Historical Data (NASA POWER API)
Retrieves real daily gridded estimates (T2M, PRECTOTCORR, RH2M) from NASA POWER:
```powershell
Rscript scripts/download_data.R
```
Or inside R console:
```r
source("scripts/download_data.R")
```

---

## 4. Prepare / Rebuild Cleaned Data and Aggregates
Re-runs data cleaning and DuckDB monthly aggregation:
```powershell
# For demo mode:
Rscript scripts/prepare_data.R demo

# For real mode:
Rscript scripts/prepare_data.R real
```

---

## 5. Launch the Application
Start the ClimateWatch Shiny dashboard on port 3838:
```powershell
Rscript scripts/run_app.R
```
Or inside R / RStudio:
```r
shiny::runApp(".", port = 3838)
```
Open your browser at:
`http://127.0.0.1:3838`

---

## 6. Switch Between Real and Demo Modes
Open `config/settings.yml` and toggle `data.mode`:
```yaml
data:
  mode: "demo"   # Set to "demo" for offline synthetic data
  # mode: "real" # Set to "real" for downloaded NASA POWER data
```

---

## 7. Run Test Suite
Execute the full testthat analytical validation suite:
```powershell
Rscript tests/testthat.R
```
