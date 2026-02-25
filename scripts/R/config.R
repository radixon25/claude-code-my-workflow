# =============================================================================
# config.R — Single source of truth for reporting cycle configuration
# =============================================================================
# Update this file each reporting cycle. All other scripts read from here.
#
# USAGE:
#   source(here("R", "config.R"))   # from build_data.R and render_*.R scripts
#
# WHAT TO UPDATE EACH CYCLE:
#   1. REPORT_YEAR           — if the year changes
#   2. REPORT_PERIOD_*       — bump the period number for the current cycle
#   3. RAW_CSV_PATH          — point to the new Salesforce export
# =============================================================================

library(here)

# --- Project root (auto-detected from .git / .Rproj / .here) ----------------
# All paths below are built from here(). Scripts can be run from any directory.
PROJECT_ROOT <- here::here()


# --- Reporting year (shared across all period types) -------------------------
REPORT_YEAR <- 2026


# --- Period values (numeric) -------------------------------------------------
# Monthly:    1-12 (month number)
# Quarterly:  1-4  (quarter number)
# Weekly:     1-53 (ISO week number)
# Yearly:     NULL (uses REPORT_YEAR only)
REPORT_PERIOD_MONTHLY   <- 2
REPORT_PERIOD_QUARTERLY <- 1
REPORT_PERIOD_WEEKLY    <- 4


# --- File paths --------------------------------------------------------------
# Raw Salesforce flex question export (absolute — stays outside the project)
RAW_CSV_PATH <- "C:/Users/sagea/Desktop/A Safe Haven/Data/Raw Base Exports/SR_FQ_Y2026.csv"

# Mapping files (inside the project)
COL_MAP_PATH <- here("data", "mapping", "fq_col_map.csv")
SLA_MAP_PATH <- here("data", "mapping", "SLA_Compliance_Map.xlsx")

# Output path for the pre-computed report object
OUTPUT_RDS   <- here("data", "processed", "reports.rds")

# --- Additional data paths (analysis scripts) ---------------------------------
# Pre-pivoted parquet (one row per WO, all case types, includes shelter_placed)
PARQUET_PATH     <- "C:/Users/sagea/Desktop/A Safe Haven/monthlyreport/data/processed/wide.parquet"

# Weather data (fetched from Open-Meteo, cached locally)
WEATHER_CSV_PATH <- "C:/Users/sagea/Desktop/A Safe Haven/monthlyreport/data/inputs/weather_daily.csv"
