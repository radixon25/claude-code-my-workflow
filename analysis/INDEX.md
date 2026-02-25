# ASH Constraint Analysis — Research Questions

This directory contains all analysis outputs organized by research question.
Each RQ folder has its own README with methodology, key findings, and output inventory.

| RQ | Folder | Question | Method | Script |
|----|--------|----------|--------|--------|
| 1 | `RQ1_bed_scarcity_placement_speed/` | Does bed scarcity slow placements and shift cases toward non-placement exits? | Cause-specific Cox regression (case-day panel) | `scripts/R/bed_scarcity_throughput.R` |
| 2 | `RQ2_site_bed_placement_volume/` | When a shelter has more open beds than usual, does it place more people? | Poisson fixed-effects regression (site-day panel) | `scripts/R/site_placement_analysis.R` |
| 3 | `RQ3_site_conversion_efficiency/` | Which shelters convert open beds to placements most efficiently? | Site-specific Poisson regressions (extension of RQ2) | `scripts/R/site_placement_analysis.R` (Step 8) |

## Folder Layout

Each RQ folder contains:
- `README.md` — Methodology, key findings, output inventory
- `figures/` — Latest PNG plots (auto-copied on each run)
- `tables/` — Latest CSV coefficient tables (auto-copied on each run)
- `data/` — Intermediate datasets (panels, crosswalks)
- `runs/` — Timestamped run archives (full history)

## How to Reproduce

```bash
# RQ1: Bed scarcity & placement speed
Rscript scripts/R/bed_scarcity_throughput.R

# RQ2 + RQ3: Site-level analysis + conversion efficiency
Rscript scripts/R/site_placement_analysis.R
```

Both scripts auto-copy their latest outputs to the appropriate `figures/`, `tables/`, and `data/` folders.
