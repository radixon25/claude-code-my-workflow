# =============================================================================
# shelter_beds.R — Shelter Bed Availability Data Reshaping Pipeline
# =============================================================================
# Reads raw bed availability data (3 shift-level counts per site per day),
# cleans it, computes site-day summaries and system-level daily aggregates.
#
# INPUT:
#   scripts/R/Data/shelter_beds/bed_availability_2-23-26.csv
#   ~23K rows: 60+ Chicago shelter sites, 3 shifts/day, Apr 2025–Feb 2026
#
# OUTPUTS (to data/processed/):
#   shelter_beds_site_day.csv          — one row per site-day
#   shelter_beds_system_daily.csv      — system-level daily aggregates
#   shelter_beds_system_daily_by_pop.csv — by population group
#
# DESIGN NOTES:
#   - Shift 2 (9am count) is the most authoritative measure — completed
#     when consistent staffing is present at all locations
#   - All sites treated equally (ASH and non-ASH use same process;
#     all beds in same queue)
#   - Empty shift counts → NA (not 0); "no answer" in notes also → NA
#   - Analysis window: Jan 1 – Feb 23, 2026
#
# USAGE:
#   Rscript scripts/R/shelter_beds.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(stringr)
})

# ====== PROJECT ROOT ======
# Walk up from this script to find .git (same pattern as staffing_levels.R)
find_project_root <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    if (nchar(dir) > 0) {
      d <- dir
      while (d != dirname(d)) {
        if (file.exists(file.path(d, ".git"))) return(d)
        d <- dirname(d)
      }
    }
  }
  d <- getwd()
  while (d != dirname(d)) {
    if (file.exists(file.path(d, ".git"))) return(d)
    d <- dirname(d)
  }
  stop("Could not find project root (.git directory)")
}

project_root <- find_project_root()
here <- function(...) file.path(project_root, ...)

# Output directory
out_dir <- here("data", "processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Analysis window
ANALYSIS_START <- as.Date("2026-01-01")
ANALYSIS_END   <- as.Date("2026-02-23")

cat("\n==========================================================\n")
cat("  Shelter Bed Availability — Data Reshaping Pipeline\n")
cat("  Window:", format(ANALYSIS_START), "to", format(ANALYSIS_END), "\n")
cat("==========================================================\n\n")


# =============================================================================
# STEP 1 — Read & Clean Raw CSV
# =============================================================================
cat("--- Step 1: Read & Clean Raw CSV ---\n\n")

bed_path <- here("scripts", "R", "Data", "shelter_beds",
                 "bed_availability_2-23-26.csv")

if (!file.exists(bed_path)) {
  stop("Bed availability file not found: ", bed_path)
}

raw <- read_csv(bed_path, show_col_types = FALSE)
cat("  Raw rows:", nrow(raw), "| Columns:", ncol(raw), "\n")

# --- 1a: Parse dates ---------------------------------------------------------
raw <- raw %>%
  mutate(
    date = mdy(Date)
  ) %>%
  filter(!is.na(date))

cat("  Dates parsed:", sum(!is.na(raw$date)), "/", nrow(raw), "valid\n")
cat("  Date range:", format(min(raw$date)), "to", format(max(raw$date)), "\n")

# --- 1b: Parse Capacity (handle commas like "1,300") -------------------------
raw <- raw %>%
  mutate(
    capacity = as.numeric(gsub(",", "", Capacity))
  )

# --- 1c: Parse shift counts (empty → NA, not 0) ------------------------------
# Shift count columns may contain empty strings, NA, or non-numeric text
parse_shift_count <- function(x) {
  # Convert to character, trim whitespace
  x_char <- str_trim(as.character(x))
  # Empty strings and explicit NA → NA
  x_char[x_char == "" | is.na(x_char)] <- NA
  # Try to parse as numeric; non-numeric → NA with warning suppressed
  suppressWarnings(as.numeric(x_char))
}

raw <- raw %>%
  mutate(
    shift1_available = parse_shift_count(`Shift 1 Count`),
    shift2_available = parse_shift_count(`Shift 2 Count`),
    shift3_available = parse_shift_count(`Shift 3 Count`),
    shift_latest     = parse_shift_count(`Shift Latest Count`)
  )

# --- 1d: Flag "no answer" / missing in shift notes ---------------------------
# If shift notes contain "no answer" or similar, the count is unreliable
flag_no_answer <- function(notes) {
  str_detect(tolower(as.character(notes)), "no answer|did not answer|n/a|na")
}

raw <- raw %>%
  mutate(
    s1_no_answer = flag_no_answer(`Shift 1 Notes`),
    s2_no_answer = flag_no_answer(`Shift 2 Notes`),
    s3_no_answer = flag_no_answer(`Shift 3 Notes`),
    # If "no answer" flagged, set count to NA (unreliable)
    shift1_available = ifelse(s1_no_answer & !is.na(s1_no_answer),
                              NA_real_, shift1_available),
    shift2_available = ifelse(s2_no_answer & !is.na(s2_no_answer),
                              NA_real_, shift2_available),
    shift3_available = ifelse(s3_no_answer & !is.na(s3_no_answer),
                              NA_real_, shift3_available)
  )

# --- 1e: Filter to analysis window -------------------------------------------
beds <- raw %>%
  filter(date >= ANALYSIS_START, date <= ANALYSIS_END)

cat("  Analysis window rows:", nrow(beds), "\n")
cat("  Unique sites:", n_distinct(beds$`Site Name`), "\n")
cat("  Unique dates:", n_distinct(beds$date), "\n")

# --- 1f: Classify shelter types -----------------------------------------------
cat("  Shelter types:\n")
print(table(beds$`Shelter Type`), row.names = FALSE)
cat("\n")

# --- 1g: Data quality summary -------------------------------------------------
cat("  Shift count completeness (within analysis window):\n")
cat("    Shift 1:", sum(!is.na(beds$shift1_available)), "/", nrow(beds),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(beds$shift1_available))))
cat("    Shift 2:", sum(!is.na(beds$shift2_available)), "/", nrow(beds),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(beds$shift2_available))))
cat("    Shift 3:", sum(!is.na(beds$shift3_available)), "/", nrow(beds),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(beds$shift3_available))))
cat("    Latest: ", sum(!is.na(beds$shift_latest)), "/", nrow(beds),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(beds$shift_latest))))
cat("\n")


# =============================================================================
# STEP 2 — Site-Day Level Summary
# =============================================================================
cat("--- Step 2: Site-Day Level Summary ---\n\n")

site_day <- beds %>%
  mutate(
    site_id   = `Site ID`,
    site_name = `Site Name`,
    agency    = Agency,
    population = Population,
    shelter_type = `Shelter Type`,
    ward      = Ward,
    geography = Geography
  ) %>%
  group_by(site_id, site_name, agency, population, shelter_type, ward,
           geography, date, capacity) %>%
  summarise(
    shift1_available = first(shift1_available),
    shift2_available = first(shift2_available),
    shift3_available = first(shift3_available),
    shift_latest     = first(shift_latest),
    .groups = "drop"
  ) %>%
  mutate(
    # Utilization (Shift 2 is primary)
    utilization_s2 = ifelse(
      !is.na(shift2_available) & capacity > 0,
      round(1 - (shift2_available / capacity), 4),
      NA_real_
    ),
    # Within-day shift deltas
    delta_12 = shift1_available - shift2_available,  # overnight change
    delta_23 = shift2_available - shift3_available,  # daytime placement proxy
    # Reporting completeness (0-3 shifts reported)
    reporting_completeness = (!is.na(shift1_available)) +
                             (!is.na(shift2_available)) +
                             (!is.na(shift3_available)),
    # Temporal features
    day_of_week = wday(date, label = TRUE),
    weekend     = as.integer(wday(date) %in% c(1, 7))
  )

cat("  Site-day rows:", nrow(site_day), "\n")
cat("  Avg utilization (Shift 2):", round(mean(site_day$utilization_s2,
                                                na.rm = TRUE) * 100, 1), "%\n")
cat("  Avg delta_23 (Shift 2→3):", round(mean(site_day$delta_23,
                                                na.rm = TRUE), 1), "beds\n")
cat("\n")


# =============================================================================
# STEP 3 — System-Level Daily Aggregates
# =============================================================================
cat("--- Step 3: System-Level Daily Aggregates ---\n\n")

system_daily <- site_day %>%
  group_by(date) %>%
  summarise(
    # System capacity (sum of all site capacities)
    system_capacity = sum(capacity, na.rm = TRUE),
    # Shift 2 — primary, most authoritative (9am with consistent staffing)
    system_available_s2 = sum(shift2_available, na.rm = TRUE),
    sites_reporting_s2  = sum(!is.na(shift2_available)),
    # Shift 1 and 3 — for sensitivity
    system_available_s1 = sum(shift1_available, na.rm = TRUE),
    sites_reporting_s1  = sum(!is.na(shift1_available)),
    system_available_s3 = sum(shift3_available, na.rm = TRUE),
    sites_reporting_s3  = sum(!is.na(shift3_available)),
    # Shift Latest
    system_latest = sum(shift_latest, na.rm = TRUE),
    sites_reporting_latest = sum(!is.na(shift_latest)),
    # Site counts
    total_sites = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # Utilization
    system_utilization_s2 = round(1 - (system_available_s2 / system_capacity), 4),
    system_utilization_s1 = round(1 - (system_available_s1 / system_capacity), 4),
    system_utilization_s3 = round(1 - (system_available_s3 / system_capacity), 4),
    # Reporting completeness
    pct_sites_reporting_s2 = round(sites_reporting_s2 / total_sites, 4),
    pct_sites_reporting_s1 = round(sites_reporting_s1 / total_sites, 4),
    # Within-day system-level deltas
    system_delta_12 = system_available_s1 - system_available_s2,
    system_delta_23 = system_available_s2 - system_available_s3,
    # Temporal
    day_of_week = wday(date, label = TRUE),
    weekend     = as.integer(wday(date) %in% c(1, 7)),
    week_number = isoweek(date)
  ) %>%
  arrange(date)

cat("  System-level rows:", nrow(system_daily), "\n")
cat("  Date range:", format(min(system_daily$date)), "to",
    format(max(system_daily$date)), "\n")

cat("\n  System-level summary (Shift 2 — primary):\n")
cat("    Mean available beds:", round(mean(system_daily$system_available_s2), 0), "\n")
cat("    Mean utilization:   ", round(mean(system_daily$system_utilization_s2) * 100, 1), "%\n")
cat("    Mean sites reporting:", round(mean(system_daily$sites_reporting_s2), 0),
    "/", round(mean(system_daily$total_sites), 0), "\n")
cat("    Mean delta_23:      ", round(mean(system_daily$system_delta_23,
                                            na.rm = TRUE), 1), "beds\n")
cat("\n")


# =============================================================================
# STEP 4 — Population-Specific Daily Aggregates
# =============================================================================
cat("--- Step 4: Population-Specific Daily Aggregates ---\n\n")

pop_daily <- site_day %>%
  group_by(date, population) %>%
  summarise(
    pop_capacity     = sum(capacity, na.rm = TRUE),
    pop_available_s2 = sum(shift2_available, na.rm = TRUE),
    sites_reporting  = sum(!is.na(shift2_available)),
    pop_available_s1 = sum(shift1_available, na.rm = TRUE),
    pop_available_s3 = sum(shift3_available, na.rm = TRUE),
    total_sites      = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pop_utilization_s2 = round(1 - (pop_available_s2 / pop_capacity), 4),
    pct_reporting      = round(sites_reporting / total_sites, 4),
    day_of_week        = wday(date, label = TRUE),
    weekend            = as.integer(wday(date) %in% c(1, 7))
  ) %>%
  arrange(date, population)

cat("  Population groups:\n")
print(table(pop_daily$population))
cat("\n")

cat("  Population-level summary (Shift 2, mean across days):\n")
pop_daily %>%
  group_by(population) %>%
  summarise(
    days             = n(),
    mean_capacity    = round(mean(pop_capacity), 0),
    mean_available   = round(mean(pop_available_s2), 0),
    mean_utilization = round(mean(pop_utilization_s2, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("\n")


# =============================================================================
# STEP 5 — Write Output Files
# =============================================================================
cat("--- Step 5: Write Output Files ---\n\n")

site_day_path <- file.path(out_dir, "shelter_beds_site_day.csv")
system_path   <- file.path(out_dir, "shelter_beds_system_daily.csv")
pop_path      <- file.path(out_dir, "shelter_beds_system_daily_by_pop.csv")

write_csv(site_day, site_day_path)
cat("  Written:", site_day_path, "\n")
cat("    Rows:", nrow(site_day), "| Cols:", ncol(site_day), "\n")

write_csv(system_daily, system_path)
cat("  Written:", system_path, "\n")
cat("    Rows:", nrow(system_daily), "| Cols:", ncol(system_daily), "\n")

write_csv(pop_daily, pop_path)
cat("  Written:", pop_path, "\n")
cat("    Rows:", nrow(pop_daily), "| Cols:", ncol(pop_daily), "\n")

cat("\n==========================================================\n")
cat("  Shelter bed data reshaping complete.\n")
cat("  Primary measure: Shift 2 (9am, most authoritative)\n")
cat("  All sites included equally (same operational process)\n")
cat("==========================================================\n")
