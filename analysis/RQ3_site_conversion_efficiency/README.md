# RQ3: Which Shelters Convert Open Beds to Placements Most Efficiently?

## Research Question
Which shelters convert open beds to placements most readily, and how does conversion efficiency vary across demographics?

## Method
- **Model:** Site-specific Poisson regressions (one per shelter)
- **Extension of:** RQ2 (same site-day panel, Step 8 of the script)
- **Outcome:** `n_placed` — daily placement count at each shelter
- **Exposure:** `beds_lag1` — yesterday's available beds
- **Controls:** Weather (temperature, cold day)
- **Ranking:** Shelters ranked by site-specific beds-to-placements coefficient
- **Reliability filter:** Excludes sites with abs(coef) > 5, SE > 5, or mean beds < 0.5

## Script
`scripts/R/site_placement_analysis.R` (Step 8: Site Conversion Extension)

## Key Figures
| File | Description |
|------|-------------|
| `09_site_conversion_rates.png` | Bar chart ranking shelters by conversion coefficient with 95% CI |
| `10_efficiency_scatter.png` | Scatter: mean beds vs conversion coefficient (diminishing returns?) |
| `11_conversion_dashboard_heatmap.png` | Dashboard heatmap grouped by demographic served |

## Key Tables
| File | Description |
|------|-------------|
| `site_conversion_rates.csv` | Per-shelter conversion coefficient, SE, p-value, volume |

## Note on Runs
The `runs/` folder is a symlink to `../RQ2_site_bed_placement_volume/runs/` since RQ3 outputs are produced by the same script as RQ2.
