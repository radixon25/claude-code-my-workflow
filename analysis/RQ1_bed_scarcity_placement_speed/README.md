# RQ1: Does Bed Scarcity Slow Placements?

## Research Question
Does bed scarcity slow placements and shift cases toward non-placement exits (cancellation, non-contact, refusal)?

## Method
- **Model:** Cause-specific Cox proportional hazards regression
- **Unit:** Case-day panel (one row per case per day open)
- **Population:** TBC cases, ASH-placeable populations (Men, Women, Youth)
- **Exposure:** `pop_available_s2` — same-day, population-matched Shift 2 beds
- **Competing risks:** 5 cause-specific models (PLACED, CANCELLED, NON_CONTACT, REFUSED, OTHER)
- **Stratification:** population + age bin (separate baseline hazards)
- **Clustering:** date x population (within-population daily correlation)

## Script
`scripts/R/bed_scarcity_throughput.R`

## Key Figures
| File | Description |
|------|-------------|
| `01_exit_distribution.png` | Exit outcome distribution |
| `02_kaplan_meier.png` | KM survival curves by population |
| `03_age_distribution.png` | Case age distribution |
| `04_bed_timeseries.png` | Bed availability over time |
| `05_km_by_bed_tertile.png` | KM by bed availability tertile |
| `06_hazard_ratios_forest.png` | Primary forest plot |
| `07_sensitivity_comparison.png` | Sensitivity analysis |
| `08_schoenfeld_residuals.png` | PH assumption diagnostics |
| `09_cumulative_incidence.png` | Competing risk CIF |
| `10_population_subgroup.png` | Population-stratified results |

## Key Tables
| File | Description |
|------|-------------|
| `primary_coefficients.csv` | All primary model coefficients |
| `exit_distribution.csv` | Exit outcome counts |
| `sensitivity_results.csv` | All sensitivity variants |
| `case_day_panel.csv` | Full analysis panel |
