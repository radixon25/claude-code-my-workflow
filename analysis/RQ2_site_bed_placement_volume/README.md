# RQ2: Do More Open Beds Lead to More Placements at a Shelter?

## Research Question
When a shelter has more open beds than usual, does it place more people that day?

## Method
- **Model:** Poisson fixed-effects regression (`fixest::fepois`)
- **Unit:** Site-day panel (one row per shelter per day)
- **Outcome:** `n_placed` — daily placement count at each shelter
- **Exposure:** `beds_lag1` — yesterday's Shift 2 available beds (36h lag)
- **Fixed effects:** Shelter + day-of-week (absorbs time-invariant site differences)
- **Controls:** Weather (temperature, cold day), demand (TBH/TBG closures)
- **Clustering:** Standard errors by date (system-wide daily shocks)
- **Lag structure:** Primary = lag-1; sensitivity = lag-0, lag-2, 2-day avg, distributed

## Script
`scripts/R/site_placement_analysis.R`

## Key Figures
| File | Description |
|------|-------------|
| `01_site_day_heatmap.png` | Placement counts by site and day |
| `02_beds_vs_placements_scatter.png` | Raw scatter with LOESS |
| `03_panel_diagnostics.png` | Panel balance, zeros, variance |
| `04_weather_timeseries.png` | Weather controls over time |
| `05_primary_forest.png` | Primary model forest plot |
| `06_lag_comparison.png` | Lag structure sensitivity |
| `07_sensitivity_forest.png` | All sensitivity variants |
| `08_population_subgroup.png` | Population-stratified models |

## Key Tables
| File | Description |
|------|-------------|
| `primary_coefficients.csv` | All primary model coefficients |
| `sensitivity_coefficients.csv` | All sensitivity variants |
| `site_summary.csv` | Per-site descriptive statistics |
| `site_day_panel.csv` | Full analysis panel |
