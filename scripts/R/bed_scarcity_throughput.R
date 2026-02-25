# =============================================================================
# bed_scarcity_throughput.R — Bed Scarcity & Placement Throughput Study
# =============================================================================
# Cause-specific Cox regression on a case-day panel linking population-matched
# bed availability to placement hazard and competing exit risks.
#
# RESEARCH QUESTION:
#   Does bed scarcity slow placements and shift cases toward non-placement
#   exits (cancellation, non-contact, refusal)?
#
# SCOPE:
#   - TBC cases only, ASH-placeable populations (Men, Women, Youth)
#   - Excludes: Families (not placed), DV (no identifier), Low Barrier, Unknown
#   - Time window: Jan 10, 2026 – current export date
#
# UNIT OF ANALYSIS:
#   Case-day panel (one row per case per day open)
#
# PRIMARY EXPOSURE:
#   pop_available_s2 — same-day, population-matched beds at 9am (Shift 2)
#
# MODELS:
#   5 cause-specific Cox models (PLACED, CANCELLED, NON_CONTACT, REFUSED, OTHER)
#   + sensitivity variants (staffing, utilization, lag-1, spline, restricted)
#
# DESIGN DECISIONS (agreed 2026-02-23):
#   - strata(population) + strata(age_bin) — separate baseline hazards
#   - cluster(day_date × population) — within-pop correlation
#   - 4 age bins: 0-20h, 20h-7d, 7-30d, 30d+
#   - Staffing excluded from primary (reverse causation); disp_24h in sensitivity
#   - Raw beds primary; utilization sensitivity
#   - Same-day exposure primary; lag-1 sensitivity
#   - REFUSED flagged as preliminary (n=62)
#
# OUTPUTS:
#   analysis/RQ1_bed_scarcity_placement_speed/runs/YYYY-MM-DD_HHMMSS/
#     data/       — intermediate datasets (panel, joined)
#     figures/    — 9+ diagnostic and interpretive plots
#     tables/     — coefficient tables, exit distributions, sensitivity
#     run_log.txt — console output + run metadata
#
# USAGE:
#   Rscript scripts/R/bed_scarcity_throughput.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(survival)
  library(splines)
})

# Conditional loads — these may not be installed on all machines
has_survminer  <- requireNamespace("survminer", quietly = TRUE)
has_tidycmprsk <- requireNamespace("tidycmprsk", quietly = TRUE)
has_cmprsk     <- requireNamespace("cmprsk", quietly = TRUE)

if (has_survminer)  library(survminer)
if (has_tidycmprsk) library(tidycmprsk)


# =============================================================================
# STEP 0 — Project Root, Configuration, Output Directory
# =============================================================================

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

# --- Worktree fallback root (for finding shared data across worktrees) -------
# If running from a git worktree (.claude/worktrees/X), the main project root
# is 3 levels up. Try multiple levels to find data/processed/.
find_main_root <- function(start) {
  d <- start
  for (i in 1:5) {
    d <- normalizePath(file.path(d, ".."), winslash = "/")
    if (file.exists(file.path(d, "data", "processed"))) return(d)
  }
  start
}
main_root <- find_main_root(project_root)

# --- Configuration -----------------------------------------------------------
ANALYSIS_START <- as.Date("2026-01-10")
EXPORT_DATE    <- Sys.Date()  # rolling — changes with each run
RUN_TIMESTAMP  <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

# ASH-placeable populations
ASH_POPULATIONS <- c("Men", "Women", "Youth")

# Age bin boundaries (in days)
AGE_BIN_BREAKS <- c(0, 20/24, 7, 30, Inf)
AGE_BIN_LABELS <- c("bin1_0_20h", "bin2_20h_7d", "bin3_7_30d", "bin4_30d_plus")

# --- Timestamped output directory --------------------------------------------
run_dir <- here("analysis", "RQ1_bed_scarcity_placement_speed", "runs", RUN_TIMESTAMP)
dir.create(file.path(run_dir, "data"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "tables"),  recursive = TRUE, showWarnings = FALSE)

# --- Logging -----------------------------------------------------------------
log_file <- file.path(run_dir, "run_log.txt")
log_con  <- file(log_file, open = "wt")

# Tee output to both console and log file
log_msg <- function(...) {
  msg <- paste0(...)
  cat(msg)
  writeLines(msg, log_con)
  flush(log_con)
}

# --- Theme -------------------------------------------------------------------
theme_report <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom"
  )

pop_colors <- c(Men = "#2c7bb6", Women = "#d7191c", Youth = "#fdae61")

# --- Run header --------------------------------------------------------------
log_msg("\n==========================================================\n")
log_msg("  Bed Scarcity & Placement Throughput Study\n")
log_msg("  Cox Cause-Specific Competing Risks Analysis\n")
log_msg("  Run: ", RUN_TIMESTAMP, "\n")
log_msg("  Output: ", run_dir, "\n")
log_msg("==========================================================\n\n")

log_msg("  R version:     ", R.version.string, "\n")
log_msg("  survival:      ", as.character(packageVersion("survival")), "\n")
log_msg("  survminer:     ", if (has_survminer) as.character(packageVersion("survminer")) else "NOT INSTALLED", "\n")
log_msg("  tidycmprsk:    ", if (has_tidycmprsk) as.character(packageVersion("tidycmprsk")) else "NOT INSTALLED", "\n")
log_msg("  Analysis start:", format(ANALYSIS_START), "\n")
log_msg("  Export date:   ", format(EXPORT_DATE), "\n")
log_msg("\n")


# =============================================================================
# STEP 1 — Data Preparation
# =============================================================================
log_msg("--- Step 1: Data Preparation ---\n\n")

# --- 1.0: Read data source ----------------------------------------------------
# Primary: parquet (pre-pivoted, one row per WO, all case types)
# Fallback: raw CSV (requires pivot)
PARQUET_PATH <- "C:/Users/sagea/Desktop/A Safe Haven/monthlyreport/data/processed/wide.parquet"

if (file.exists(PARQUET_PATH) && requireNamespace("arrow", quietly = TRUE)) {
  log_msg("  Reading parquet: ", PARQUET_PATH, "\n")
  all_parquet <- arrow::read_parquet(PARQUET_PATH)
  log_msg("  Total rows (all case types): ", nrow(all_parquet), "\n")

  # Filter to TBC and map columns to match the original CSV-derived structure
  # Parquet columns: wo_number, wo_tc, status, created_date, end_date, age,
  #   client_gender, youth_client, hh_makeup, activity_outcome
  # Need: wo_number, status, created, end_date, age_raw, gender, youth, household, outcome
  cases <- all_parquet %>%
    dplyr::filter(wo_tc == "TBC") %>%
    dplyr::transmute(
      wo_number = wo_number,
      status    = status,
      created   = created_date,     # string: "1/1/2026"
      end_date  = end_date,         # string: "1/1/2026 7:28"
      age_raw   = age,
      gender    = client_gender,
      youth     = youth_client,
      household = hh_makeup,
      outcome   = activity_outcome
    )

  log_msg("  Unique TBC cases: ", nrow(cases), "\n")
  log_msg("  (Data source: parquet)\n")

} else {
  # Fallback: raw CSV with pivot
  sf_path <- "C:/Users/sagea/Desktop/A Safe Haven/Data/Raw Base Exports/SR_FQ_Y2026.csv"
  log_msg("  Parquet not available, reading CSV: ", sf_path, "\n")
  sf_raw <- read_csv(sf_path, show_col_types = FALSE, locale = locale(encoding = "latin1"))

  log_msg("  Raw rows: ", nrow(sf_raw), "\n")

  tbc_raw <- sf_raw %>%
    filter(`Work Order: Type Short Code` == "TBC")

  log_msg("  TBC rows (all questions): ", nrow(tbc_raw), "\n")

  flex_questions <- c(
    "Is the client male or female?",
    "Are you a youth? (18-24 years of age)",
    "What is the makeup of the household seeking shelter?",
    "Activity Outcome"
  )

  flex_wide <- tbc_raw %>%
    filter(Question %in% flex_questions) %>%
    select(`Work Order Number`, Question, Answer) %>%
    distinct(`Work Order Number`, Question, .keep_all = TRUE) %>%
    pivot_wider(names_from = Question, values_from = Answer) %>%
    rename(
      wo_number = `Work Order Number`,
      gender    = `Is the client male or female?`,
      youth     = `Are you a youth? (18-24 years of age)`,
      household = `What is the makeup of the household seeking shelter?`,
      outcome   = `Activity Outcome`
    )

  case_meta <- tbc_raw %>%
    distinct(`Work Order Number`, .keep_all = TRUE) %>%
    select(
      wo_number = `Work Order Number`,
      status    = Status,
      created   = `Created Date`,
      end_date  = `End Date`,
      age_raw   = Age
    )

  cases <- case_meta %>%
    left_join(flex_wide, by = "wo_number")

  log_msg("  Unique TBC cases: ", nrow(cases), "\n")
  log_msg("  (Data source: CSV with pivot)\n")
}

# --- 1.1b: Map to population ------------------------------------------------
# Gender cleaning: parquet has raw Salesforce values with variants
# ("Male", "male", "MALE", "Male | Male", etc). Normalize first.
cases <- cases %>%
  mutate(
    gender_clean = case_when(
      grepl("\\bMale\\b", gender, ignore.case = TRUE) &
        !grepl("\\bFemale\\b", gender, ignore.case = TRUE)   ~ "Male",
      grepl("\\bFemale\\b", gender, ignore.case = TRUE) &
        !grepl("\\bMale\\b", gender, ignore.case = TRUE)     ~ "Female",
      TRUE ~ "Unknown"
    ),
    population = case_when(
      # Anyone with children → Families (will be excluded)
      grepl("Child", household, ignore.case = TRUE) ~ "Families",
      # Youth (18-24) regardless of gender
      youth == "Yes"                                 ~ "Youth",
      # Gender-based (using cleaned gender)
      gender_clean == "Male"                         ~ "Men",
      gender_clean == "Female"                       ~ "Women",
      # Catch-all
      TRUE                                           ~ "Unknown"
    ),
    ash_places = as.integer(population %in% ASH_POPULATIONS)
  )

# QA table: population distribution
log_msg("  Population distribution (all TBC):\n")
pop_dist <- cases %>% count(population, ash_places) %>% arrange(desc(n))
for (i in seq_len(nrow(pop_dist))) {
  log_msg(sprintf("    %-12s ASH=%d  n=%d\n",
                   pop_dist$population[i], pop_dist$ash_places[i], pop_dist$n[i]))
}
log_msg(sprintf("    TOTAL: %d | ASH-placeable: %d | Excluded: %d\n\n",
                 nrow(cases), sum(cases$ash_places), sum(!cases$ash_places)))

# Filter to ASH-placeable
cases <- cases %>% filter(ash_places == 1)

# --- 1.2: Parse dates and recode exit types ----------------------------------
cases <- cases %>%
  mutate(
    created_date = mdy(created),
    closed_dt    = mdy_hm(end_date),
    closed_date  = as.Date(closed_dt),
    # Censor date for open cases
    censor_date  = if_else(is.na(closed_date), EXPORT_DATE, closed_date)
  ) %>%
  filter(!is.na(created_date))

# Apply analysis window
cases <- cases %>%
  filter(created_date >= ANALYSIS_START)

log_msg("  Cases in analysis window (>= ", format(ANALYSIS_START), "): ", nrow(cases), "\n")

# Recode exit types
cases <- cases %>%
  mutate(
    exit_type = case_when(
      outcome == "Client(s) Placed in Shelter"                ~ "PLACED",
      outcome == "Request Cancelled"                          ~ "CANCELLED",
      outcome %in% c("Gone on Arrival",
                      "Client Left SR Location",
                      "Unable to Reach Client")               ~ "NON_CONTACT",
      outcome == "Refused Service"                            ~ "REFUSED",
      !is.na(outcome)                                         ~ "OTHER",
      TRUE                                                    ~ NA_character_
    ),
    is_closed = !is.na(exit_type)
  )

# QA table: exit type distribution
log_msg("\n  Exit type distribution:\n")
exit_dist <- cases %>%
  mutate(exit_display = if_else(is.na(exit_type), "OPEN (censored)", exit_type)) %>%
  count(exit_display) %>%
  arrange(desc(n))
for (i in seq_len(nrow(exit_dist))) {
  log_msg(sprintf("    %-20s n=%d\n", exit_dist$exit_display[i], exit_dist$n[i]))
}

# Cross-tab: population × exit type
log_msg("\n  Population × Exit Type:\n")
pop_exit <- cases %>%
  mutate(exit_display = if_else(is.na(exit_type), "OPEN", exit_type)) %>%
  count(population, exit_display) %>%
  pivot_wider(names_from = exit_display, values_from = n, values_fill = 0)
print(as.data.frame(pop_exit), row.names = FALSE)
log_msg("\n")

# --- 1.3: Build Case-Day Panel -----------------------------------------------
log_msg("  Building case-day panel...\n")

# For each case, generate one row per day from created_date to censor_date
panel <- cases %>%
  select(wo_number, population, created_date, censor_date, exit_type, is_closed) %>%
  mutate(
    # Duration in days
    duration = as.numeric(censor_date - created_date),
    # Cases closed same day: use 0.5 day to avoid zero-length intervals
    duration = pmax(duration, 0.5)
  ) %>%
  rowwise() %>%
  mutate(
    day_seq = list(seq(0, floor(duration)))
  ) %>%
  ungroup() %>%
  unnest(day_seq) %>%
  mutate(
    day_date   = created_date + day_seq,
    tstart     = day_seq,
    tstop      = if_else(day_seq == floor(as.numeric(censor_date - created_date)),
                          as.numeric(censor_date - created_date),
                          day_seq + 1),
    # Ensure tstop > tstart
    tstop      = pmax(tstop, tstart + 0.01),
    age_days   = as.numeric(day_date - created_date),
    # Event indicators: only on the last row (closure day) for closed cases
    is_last_day = (day_date == censor_date),
    event_PLACED      = as.integer(is_last_day & is_closed & exit_type == "PLACED"),
    event_CANCELLED   = as.integer(is_last_day & is_closed & exit_type == "CANCELLED"),
    event_NON_CONTACT = as.integer(is_last_day & is_closed & exit_type == "NON_CONTACT"),
    event_REFUSED     = as.integer(is_last_day & is_closed & exit_type == "REFUSED"),
    event_OTHER       = as.integer(is_last_day & is_closed & exit_type == "OTHER"),
    # Day features
    day_of_week = wday(day_date, label = TRUE),
    weekend     = as.integer(wday(day_date) %in% c(1, 7))
  )

# Replace any NAs in event indicators with 0
event_cols <- c("event_PLACED", "event_CANCELLED", "event_NON_CONTACT",
                "event_REFUSED", "event_OTHER")
panel <- panel %>%
  mutate(across(all_of(event_cols), ~replace_na(.x, 0L)))

log_msg(sprintf("  Panel built: %d rows | %d unique cases | %d unique dates\n",
                 nrow(panel), n_distinct(panel$wo_number), n_distinct(panel$day_date)))

# Remove rows beyond analysis window (bed data may not extend past export date)
panel <- panel %>%
  filter(day_date >= ANALYSIS_START, day_date <= EXPORT_DATE)

log_msg(sprintf("  Panel after date filter: %d rows\n", nrow(panel)))
log_msg("\n")


# =============================================================================
# STEP 2 — Attach Exposures
# =============================================================================
log_msg("--- Step 2: Attach Exposures ---\n\n")

# --- 2.1: Population-Matched Beds -------------------------------------------
# Bed data may be in current worktree or main project root
bed_pop_path <- here("data", "processed", "shelter_beds_system_daily_by_pop.csv")
if (!file.exists(bed_pop_path)) {
  # Fallback: look in main project root (if running from a worktree)
  bed_pop_path <- file.path(main_root, "data", "processed",
                             "shelter_beds_system_daily_by_pop.csv")
  if (!file.exists(bed_pop_path)) {
    stop("Bed data not found. Run shelter_beds.R first to generate processed data.\n",
         "  Checked: ", here("data", "processed"), "\n",
         "  Checked: ", file.path(main_root, "data", "processed"), "\n")
  }
  log_msg("  Using bed data from main project root: ", bed_pop_path, "\n")
}
bed_pop <- read_csv(bed_pop_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# Filter to ASH populations and compute lag
bed_pop_ash <- bed_pop %>%
  filter(population %in% ASH_POPULATIONS) %>%
  select(date, population, pop_available_s2, pop_utilization_s2, pop_capacity) %>%
  arrange(population, date) %>%
  group_by(population) %>%
  mutate(
    pop_available_s2_lag1 = lag(pop_available_s2, 1)
  ) %>%
  ungroup()

log_msg("  Bed data loaded: ", nrow(bed_pop_ash), " rows (", n_distinct(bed_pop_ash$date),
        " dates × ", n_distinct(bed_pop_ash$population), " populations)\n")

# Join to panel
panel <- panel %>%
  left_join(bed_pop_ash, by = c("day_date" = "date", "population" = "population"))

# Report join quality
bed_join_rate <- mean(!is.na(panel$pop_available_s2))
log_msg(sprintf("  Bed data join rate: %.1f%% of panel rows have bed data\n",
                 bed_join_rate * 100))

# --- 2.2: Staffing (for sensitivity models) ----------------------------------
staffing_path <- here("scripts", "R", "Data", "staffing_log", "clean",
                      "staffing_daily_levels_2026.csv")
if (!file.exists(staffing_path)) {
  # Fallback: check main project root
  staffing_path <- file.path(main_root, "scripts", "R", "Data", "staffing_log",
                              "clean", "staffing_daily_levels_2026.csv")
}

if (file.exists(staffing_path)) {
  staffing_raw <- read_csv(staffing_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date))

  # Compute daily dispatcher total (sum across shifts)
  daily_disp <- staffing_raw %>%
    filter(role_clean == "Dispatcher") %>%
    group_by(date) %>%
    summarise(disp_daily = sum(scheduled_staff, na.rm = TRUE), .groups = "drop")

  # For 24h window: need shift-level. Compute similarly to staffing_impact.R
  field_roles <- c("Intake Driver", "Dispatcher", "Shift Supervisor")
  shift_order_map <- c(AM = 0L, PM = 1L, OVN = 2L)

  shift_staff <- staffing_raw %>%
    filter(role_clean %in% field_roles) %>%
    group_by(date, shift_code, role_clean) %>%
    summarise(staff = sum(scheduled_staff), .groups = "drop") %>%
    pivot_wider(names_from = role_clean, values_from = staff, values_fill = 0)

  names(shift_staff) <- gsub(" ", "_", tolower(names(shift_staff)))

  shift_staff <- shift_staff %>%
    mutate(
      intake_pairs = floor(intake_driver / 2),
      shift_ord = shift_order_map[shift_code],
      shift_idx = as.integer(date - min(date)) * 3L + shift_ord
    ) %>%
    arrange(shift_idx) %>%
    mutate(
      pairs_lag1 = lag(intake_pairs, 1),
      pairs_lag2 = lag(intake_pairs, 2),
      disp_lag1  = lag(dispatcher, 1),
      disp_lag2  = lag(dispatcher, 2),
      field_lag1 = lag(intake_driver + dispatcher + shift_supervisor, 1),
      field_lag2 = lag(intake_driver + dispatcher + shift_supervisor, 2),
      pairs_24h  = intake_pairs + pairs_lag1 + pairs_lag2,
      disp_24h   = dispatcher + disp_lag1 + disp_lag2,
      field_24h  = (intake_driver + dispatcher + shift_supervisor) + field_lag1 + field_lag2
    )

  # Daily 24h aggregates (take max of shift-level 24h windows within each day)
  daily_staffing_24h <- shift_staff %>%
    group_by(date) %>%
    summarise(
      pairs_24h = max(pairs_24h, na.rm = TRUE),
      disp_24h  = max(disp_24h, na.rm = TRUE),
      field_24h = max(field_24h, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(c(pairs_24h, disp_24h, field_24h),
                  ~if_else(is.infinite(.x), NA_real_, .x)))

  panel <- panel %>%
    left_join(daily_staffing_24h, by = c("day_date" = "date"))

  staff_join_rate <- mean(!is.na(panel$disp_24h))
  log_msg(sprintf("  Staffing data join rate: %.1f%%\n", staff_join_rate * 100))
} else {
  log_msg("  WARNING: Staffing data not found — sensitivity models will be skipped\n")
  panel$pairs_24h <- NA_real_
  panel$disp_24h  <- NA_real_
  panel$field_24h <- NA_real_
}

# --- 2.3: Create clustering variable ----------------------------------------
panel <- panel %>%
  mutate(
    day_pop = interaction(day_date, population, drop = TRUE)
  )

log_msg(sprintf("  Cluster groups (day × population): %d\n", n_distinct(panel$day_pop)))
log_msg("\n")


# =============================================================================
# STEP 3 — Input Diagnostic Plots
# =============================================================================
log_msg("--- Step 3: Input Diagnostic Plots ---\n\n")

fig_dir <- file.path(run_dir, "figures")

# --- Plot 1: Beds Over Time by Population ------------------------------------
p1 <- bed_pop_ash %>%
  ggplot(aes(x = date, y = pop_available_s2, color = population)) +
  geom_line(linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", linewidth = 0.5) +
  scale_color_manual(values = pop_colors) +
  labs(
    title    = "Available Beds Over Time by Population (Shift 2, 9am)",
    subtitle = paste("Analysis window:", format(ANALYSIS_START), "to", format(EXPORT_DATE)),
    x = NULL, y = "Available Beds (Shift 2)", color = "Population"
  ) +
  theme_report

ggsave(file.path(fig_dir, "01_beds_timeseries_by_population.png"),
       p1, width = 10, height = 5, dpi = 150)
log_msg("  Saved: 01_beds_timeseries_by_population.png\n")

# --- Plot 2: Utilization Over Time by Population -----------------------------
p2 <- bed_pop_ash %>%
  ggplot(aes(x = date, y = pop_utilization_s2, color = population)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_color_manual(values = pop_colors) +
  scale_y_continuous(labels = percent_format(), limits = c(0.5, 1.05)) +
  labs(
    title    = "Shelter Utilization Over Time by Population (Shift 2)",
    subtitle = "Red dashed line = 95% utilization threshold",
    x = NULL, y = "Utilization (Occupied / Capacity)", color = "Population"
  ) +
  theme_report

ggsave(file.path(fig_dir, "02_utilization_timeseries_by_population.png"),
       p2, width = 10, height = 5, dpi = 150)
log_msg("  Saved: 02_utilization_timeseries_by_population.png\n")

# --- Plot 3: Staffing Over Time ----------------------------------------------
if (exists("daily_staffing_24h") && nrow(daily_staffing_24h) > 0) {
  staff_long <- daily_staffing_24h %>%
    select(date, pairs_24h, disp_24h, field_24h) %>%
    pivot_longer(-date, names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric,
      levels = c("pairs_24h", "disp_24h", "field_24h"),
      labels = c("Intake Pairs (24h)", "Dispatchers (24h)", "Field Staff (24h)")
    ))

  p3 <- ggplot(staff_long, aes(x = date, y = value, color = metric)) +
    geom_line(linewidth = 0.8) +
    labs(
      title    = "Staffing Levels Over Time (24h Window)",
      subtitle = "Daily max of shift-level 24h windows",
      x = NULL, y = "Staff Count (24h Window)", color = NULL
    ) +
    theme_report

  ggsave(file.path(fig_dir, "03_staffing_timeseries.png"),
         p3, width = 10, height = 5, dpi = 150)
  log_msg("  Saved: 03_staffing_timeseries.png\n")
}

# --- Plot 4: Placement Throughput Over Time -----------------------------------
daily_placements <- panel %>%
  filter(event_PLACED == 1) %>%
  count(day_date, name = "placements")

daily_open <- panel %>%
  count(day_date, name = "open_cases")

daily_throughput <- daily_open %>%
  left_join(daily_placements, by = "day_date") %>%
  mutate(
    placements = replace_na(placements, 0L),
    placement_rate = placements / open_cases
  )

p4 <- ggplot(daily_throughput, aes(x = day_date)) +
  geom_col(aes(y = placements, fill = "Placements"), alpha = 0.7, width = 0.8) +
  geom_line(aes(y = placement_rate * max(placements) / max(placement_rate, na.rm = TRUE),
                color = "Placement Rate"),
            linewidth = 0.8) +
  scale_y_continuous(
    name = "Daily Placements",
    sec.axis = sec_axis(
      ~ . * max(daily_throughput$placement_rate, na.rm = TRUE) /
        max(daily_throughput$placements, na.rm = TRUE),
      name = "Placement Rate (placements/open)",
      labels = percent_format()
    )
  ) +
  scale_fill_manual(values = c("Placements" = "#2c7bb6")) +
  scale_color_manual(values = c("Placement Rate" = "#d7191c")) +
  labs(
    title    = "Daily Placement Throughput",
    subtitle = "Placements per day and placement rate (placements/open cases)",
    x = NULL, fill = NULL, color = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "04_daily_placements.png"),
       p4, width = 10, height = 5, dpi = 150)
log_msg("  Saved: 04_daily_placements.png\n\n")


# =============================================================================
# STEP 4 — Age Stratification
# =============================================================================
log_msg("--- Step 4: Age Stratification ---\n\n")

panel <- panel %>%
  mutate(
    age_bin = cut(age_days,
                  breaks = AGE_BIN_BREAKS,
                  labels = AGE_BIN_LABELS,
                  right  = FALSE,
                  include.lowest = TRUE)
  )

# QA: age bin distribution
log_msg("  Age bin distribution (panel rows):\n")
age_dist <- panel %>% count(age_bin) %>% mutate(pct = round(n / sum(n) * 100, 1))
for (i in seq_len(nrow(age_dist))) {
  log_msg(sprintf("    %-20s %8d rows (%5.1f%%)\n",
                   age_dist$age_bin[i], age_dist$n[i], age_dist$pct[i]))
}

# Sparsity check: events by age_bin × population × exit_type
log_msg("\n  Sparsity check (events by population × age_bin):\n")
sparsity <- panel %>%
  filter(event_PLACED == 1 | event_CANCELLED == 1 | event_NON_CONTACT == 1 |
           event_REFUSED == 1 | event_OTHER == 1) %>%
  mutate(exit = case_when(
    event_PLACED == 1      ~ "PLACED",
    event_CANCELLED == 1   ~ "CANCELLED",
    event_NON_CONTACT == 1 ~ "NON_CONTACT",
    event_REFUSED == 1     ~ "REFUSED",
    event_OTHER == 1       ~ "OTHER"
  )) %>%
  count(population, age_bin, exit) %>%
  pivot_wider(names_from = exit, values_from = n, values_fill = 0)

print(as.data.frame(sparsity), row.names = FALSE)
write_csv(sparsity, file.path(run_dir, "tables", "diagnostic_sparsity.csv"))
log_msg("\n")

# Flag sparse cells
sparse_cells <- sparsity %>%
  pivot_longer(c(PLACED, CANCELLED, NON_CONTACT, REFUSED, OTHER),
               names_to = "exit", values_to = "n") %>%
  filter(n > 0, n < 10)

if (nrow(sparse_cells) > 0) {
  log_msg("  WARNING: Sparse cells (events < 10):\n")
  for (i in seq_len(nrow(sparse_cells))) {
    log_msg(sprintf("    %s × %s × %s: n=%d\n",
                     sparse_cells$population[i], sparse_cells$age_bin[i],
                     sparse_cells$exit[i], sparse_cells$n[i]))
  }
  log_msg("\n")
}


# =============================================================================
# STEP 5 — Primary Cox Models
# =============================================================================
log_msg("--- Step 5: Primary Cox Models ---\n\n")

# Drop rows with missing bed exposure (can't model without exposure)
panel_model <- panel %>%
  filter(!is.na(pop_available_s2), !is.na(age_bin))

log_msg(sprintf("  Modeling dataset: %d rows (%d dropped for missing beds/age_bin)\n",
                 nrow(panel_model), nrow(panel) - nrow(panel_model)))

# --- Helper: Fit and summarize Cox model -------------------------------------
fit_cox <- function(data, event_col, model_label, formula_rhs = NULL) {
  log_msg(sprintf("\n  MODEL: %s (event = %s)\n", model_label, event_col))

  # Default formula
  if (is.null(formula_rhs)) {
    formula_rhs <- "pop_available_s2 + factor(day_of_week) + strata(age_bin) + strata(population)"
  }

  full_formula <- as.formula(paste0(
    "Surv(tstart, tstop, ", event_col, ") ~ ", formula_rhs
  ))

  n_events <- sum(data[[event_col]], na.rm = TRUE)
  log_msg(sprintf("    Events: %d | Rows: %d\n", n_events, nrow(data)))

  if (n_events < 10) {
    log_msg("    SKIPPED — too few events (< 10)\n")
    return(NULL)
  }

  tryCatch({
    fit <- coxph(full_formula, data = data, cluster = day_pop)

    # Extract coefficients
    s <- summary(fit)
    coefs <- as.data.frame(s$coefficients)
    coefs$term <- rownames(coefs)

    # Robust SEs from clustering
    if ("robust se" %in% names(coefs)) {
      coefs$se_robust <- coefs$`robust se`
    } else {
      coefs$se_robust <- coefs$`se(coef)`
    }

    result <- data.frame(
      model     = model_label,
      term      = coefs$term,
      estimate  = coefs$coef,
      se        = coefs$se_robust,
      HR        = exp(coefs$coef),
      HR_lo95   = exp(coefs$coef - 1.96 * coefs$se_robust),
      HR_hi95   = exp(coefs$coef + 1.96 * coefs$se_robust),
      p_value   = coefs$`Pr(>|z|)`,
      n_events  = n_events,
      row.names = NULL,
      stringsAsFactors = FALSE
    )

    # Print key results
    bed_row <- result %>% filter(grepl("pop_available|pop_util", term))
    if (nrow(bed_row) > 0) {
      for (j in seq_len(nrow(bed_row))) {
        log_msg(sprintf("    %s: HR=%.4f [%.4f, %.4f] p=%.2e\n",
                         bed_row$term[j], bed_row$HR[j],
                         bed_row$HR_lo95[j], bed_row$HR_hi95[j],
                         bed_row$p_value[j]))
      }
    }

    log_msg(sprintf("    Concordance: %.3f\n", s$concordance[1]))

    list(fit = fit, coefs = result, n_events = n_events)
  }, error = function(e) {
    log_msg(sprintf("    ERROR: %s\n", e$message))
    NULL
  })
}

# --- Primary Models (5 cause-specific) ----------------------------------------
exit_types <- c("PLACED", "CANCELLED", "NON_CONTACT", "REFUSED", "OTHER")
event_cols <- paste0("event_", exit_types)

primary_results <- list()
for (i in seq_along(exit_types)) {
  result <- fit_cox(
    data        = panel_model,
    event_col   = event_cols[i],
    model_label = paste0("M", i, "_", exit_types[i])
  )
  if (!is.null(result)) {
    primary_results[[exit_types[i]]] <- result
  }
}

# --- Sensitivity Models (PLACED only) ----------------------------------------
log_msg("\n--- Sensitivity Models (PLACED) ---\n")

# M1b: Add staffing
if (!all(is.na(panel_model$disp_24h))) {
  panel_staff <- panel_model %>% filter(!is.na(disp_24h))
  sens_staffing <- fit_cox(
    data = panel_staff,
    event_col = "event_PLACED",
    model_label = "M1b_PLACED_staffing",
    formula_rhs = "pop_available_s2 + disp_24h + factor(day_of_week) + strata(age_bin) + strata(population)"
  )
} else {
  log_msg("  M1b skipped — no staffing data\n")
  sens_staffing <- NULL
}

# M1c: Utilization instead of raw beds
sens_util <- fit_cox(
  data = panel_model %>% filter(!is.na(pop_utilization_s2)),
  event_col = "event_PLACED",
  model_label = "M1c_PLACED_utilization",
  formula_rhs = "pop_utilization_s2 + factor(day_of_week) + strata(age_bin) + strata(population)"
)

# M1d: Lag-1 beds
sens_lag <- fit_cox(
  data = panel_model %>% filter(!is.na(pop_available_s2_lag1)),
  event_col = "event_PLACED",
  model_label = "M1d_PLACED_lag1",
  formula_rhs = "pop_available_s2_lag1 + factor(day_of_week) + strata(age_bin) + strata(population)"
)

# M1e: Cluster by day_date only (not × population)
panel_model_dayonly <- panel_model %>%
  mutate(day_cluster = day_date)
sens_cluster <- tryCatch({
  fit_temp <- coxph(
    Surv(tstart, tstop, event_PLACED) ~
      pop_available_s2 + factor(day_of_week) +
      strata(age_bin) + strata(population),
    data = panel_model_dayonly,
    cluster = day_cluster
  )
  s <- summary(fit_temp)
  coefs <- as.data.frame(s$coefficients)
  bed_coef <- coefs["pop_available_s2", ]
  se_r <- if ("robust se" %in% names(coefs)) bed_coef$`robust se` else bed_coef$`se(coef)`
  log_msg(sprintf("\n  M1e_PLACED_cluster_dayonly: beds HR=%.4f [%.4f, %.4f]\n",
                   exp(bed_coef$coef),
                   exp(bed_coef$coef - 1.96 * se_r),
                   exp(bed_coef$coef + 1.96 * se_r)))
  list(fit = fit_temp, label = "M1e_PLACED_cluster_dayonly")
}, error = function(e) {
  log_msg(sprintf("  M1e ERROR: %s\n", e$message))
  NULL
})

# M1f: First 30 days only
panel_30d <- panel_model %>% filter(age_days < 30)
sens_30d <- fit_cox(
  data = panel_30d,
  event_col = "event_PLACED",
  model_label = "M1f_PLACED_first30d",
  formula_rhs = "pop_available_s2 + factor(day_of_week) + strata(age_bin) + strata(population)"
)

# M1g: Spline for nonlinearity
sens_spline <- tryCatch({
  fit_spline <- coxph(
    Surv(tstart, tstop, event_PLACED) ~
      ns(pop_available_s2, df = 3) + factor(day_of_week) +
      strata(age_bin) + strata(population),
    data = panel_model,
    cluster = day_pop
  )
  log_msg("\n  M1g_PLACED_spline: fitted (see Plot 08 for visualization)\n")
  list(fit = fit_spline, label = "M1g_PLACED_spline")
}, error = function(e) {
  log_msg(sprintf("  M1g ERROR: %s\n", e$message))
  NULL
})

log_msg("\n")


# =============================================================================
# STEP 6 — Output Visualizations
# =============================================================================
log_msg("--- Step 6: Output Visualizations ---\n\n")

# --- Plot 5: Kaplan-Meier Survival Curves (Low vs High Beds) -----------------
# Group cases by bed quintile on their first panel day
case_first_day <- panel_model %>%
  group_by(wo_number) %>%
  slice_min(day_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    bed_quintile = ntile(pop_available_s2, 5),
    bed_group = case_when(
      bed_quintile == 1 ~ "Low Beds (Q1)",
      bed_quintile == 5 ~ "High Beds (Q5)",
      TRUE              ~ "Middle (Q2-Q4)"
    )
  ) %>%
  select(wo_number, bed_group)

# Merge back to full panel for survival calculation
panel_km <- panel_model %>%
  inner_join(case_first_day, by = "wo_number")

# For KM: need one row per case with time + event
km_data <- cases %>%
  filter(wo_number %in% panel_km$wo_number) %>%
  inner_join(case_first_day, by = "wo_number") %>%
  mutate(
    time_days = as.numeric(censor_date - created_date),
    time_days = pmax(time_days, 0.5),
    event_placed = as.integer(exit_type == "PLACED" & !is.na(exit_type))
  ) %>%
  filter(bed_group != "Middle (Q2-Q4)")

tryCatch({
  km_fit <- survfit(Surv(time_days, event_placed) ~ bed_group, data = km_data)

  if (has_survminer) {
    p5 <- ggsurvplot(
      km_fit, data = km_data,
      palette = c("#2c7bb6", "#d7191c"),
      xlab = "Days Since Case Creation",
      ylab = "Proportion Not Yet Placed",
      title = "Placement Survival: Low vs High Bed Availability",
      subtitle = "Cases grouped by bed quintile at creation",
      legend.labs = c("High Beds (Q5)", "Low Beds (Q1)"),
      risk.table = TRUE,
      conf.int = TRUE,
      ggtheme = theme_report
    )
    # ggsurvplot returns a list (not ggplot); use png/print/dev.off pattern
    png(file.path(fig_dir, "05_survival_low_vs_high_beds.png"),
        width = 10, height = 7, units = "in", res = 150)
    print(p5)
    dev.off()
  } else {
    # Fallback without survminer
    png(file.path(fig_dir, "05_survival_low_vs_high_beds.png"),
        width = 10, height = 6, units = "in", res = 150)
    plot(km_fit, col = c("#d7191c", "#2c7bb6"), lwd = 2,
         xlab = "Days Since Creation", ylab = "Proportion Not Yet Placed",
         main = "Placement Survival: Low vs High Bed Availability")
    legend("topright", legend = levels(factor(km_data$bed_group)),
           col = c("#d7191c", "#2c7bb6"), lwd = 2)
    dev.off()
  }
  log_msg("  Saved: 05_survival_low_vs_high_beds.png\n")
}, error = function(e) {
  log_msg(sprintf("  Plot 5 ERROR: %s\n", e$message))
})

# --- Plot 6: Cumulative Incidence (Competing Risks) --------------------------
tryCatch({
  # Build competing risk event variable
  cr_data <- cases %>%
    filter(wo_number %in% panel_km$wo_number) %>%
    inner_join(case_first_day, by = "wo_number") %>%
    mutate(
      time_days = as.numeric(censor_date - created_date),
      time_days = pmax(time_days, 0.5),
      cr_event = case_when(
        is.na(exit_type)        ~ "censored",
        exit_type == "PLACED"   ~ "PLACED",
        exit_type == "CANCELLED"~ "CANCELLED",
        exit_type == "NON_CONTACT" ~ "NON_CONTACT",
        TRUE                    ~ "OTHER_EXIT"
      ),
      cr_event = factor(cr_event, levels = c("censored", "PLACED", "CANCELLED",
                                               "NON_CONTACT", "OTHER_EXIT"))
    ) %>%
    filter(bed_group != "Middle (Q2-Q4)")

  if (has_cmprsk) {
    log_msg("  Plot 6: Using cmprsk for cumulative incidence\n")
    # cmprsk::cuminc expects: ftime (time), fstatus (0=censored, 1+ = event type), group
    cr_status_num <- as.integer(cr_data$cr_event) - 1L  # 0=censored, 1=PLACED, etc.
    cr_group_vec  <- cr_data$bed_group

    ci_fit <- cmprsk::cuminc(cr_data$time_days, cr_status_num, cr_group_vec)
    png(file.path(fig_dir, "06_cumulative_incidence_low_vs_high.png"),
        width = 10, height = 7, units = "in", res = 150)
    plot(ci_fit, main = "Cumulative Incidence: Low vs High Beds",
         xlab = "Days Since Creation", ylab = "Cumulative Probability",
         col = rep(c("#2c7bb6", "#d7191c"), 4),
         lty = rep(1:4, each = 2), lwd = 2)
    legend("topleft",
           legend = c("PLACED (High)", "PLACED (Low)",
                      "CANCELLED (High)", "CANCELLED (Low)",
                      "NON_CONTACT (High)", "NON_CONTACT (Low)"),
           col = rep(c("#2c7bb6", "#d7191c"), 3),
           lty = rep(1:3, each = 2), lwd = 2, cex = 0.8)
    dev.off()
    log_msg("  Saved: 06_cumulative_incidence_low_vs_high.png\n")
  } else {
    log_msg("  Plot 6 SKIPPED: cmprsk not installed\n")
  }
}, error = function(e) {
  log_msg(sprintf("  Plot 6 ERROR: %s\n", e$message))
})

# --- Plot 7: Hazard Ratio Forest Plot -----------------------------------------
tryCatch({
  # Collect bed coefficients from all primary models
  forest_data <- bind_rows(
    lapply(names(primary_results), function(nm) {
      res <- primary_results[[nm]]
      res$coefs %>%
        filter(grepl("pop_available_s2", term)) %>%
        mutate(outcome = nm)
    })
  )

  if (nrow(forest_data) > 0) {
    forest_data$outcome <- factor(forest_data$outcome,
                                   levels = rev(exit_types))

    p7 <- ggplot(forest_data, aes(x = HR, y = outcome)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
      geom_errorbarh(aes(xmin = HR_lo95, xmax = HR_hi95),
                     height = 0.2, linewidth = 0.8) +
      geom_point(aes(color = if_else(p_value < 0.05, "p < 0.05", "p >= 0.05")),
                 size = 3.5) +
      scale_color_manual(values = c("p < 0.05" = "#2c7bb6", "p >= 0.05" = "grey60")) +
      geom_text(aes(label = sprintf("HR=%.3f", HR)),
                hjust = -0.3, vjust = -0.5, size = 3.2) +
      labs(
        title    = "Effect of Bed Availability on Exit Hazards",
        subtitle = "Hazard Ratios for pop_available_s2 across cause-specific models",
        x = "Hazard Ratio (per 1-bed increase)", y = NULL, color = NULL
      ) +
      annotate("text", x = max(forest_data$HR_hi95, na.rm = TRUE),
               y = 0.5, hjust = 1, vjust = -1, size = 2.8, color = "grey40",
               label = "REFUSED: preliminary (n=62)") +
      theme_report

    ggsave(file.path(fig_dir, "07_hazard_ratio_forest.png"),
           p7, width = 9, height = 5, dpi = 150)
    log_msg("  Saved: 07_hazard_ratio_forest.png\n")
  }
}, error = function(e) {
  log_msg(sprintf("  Plot 7 ERROR: %s\n", e$message))
})

# --- Plot 8: Scarcity Threshold / Nonlinearity Curve -------------------------
tryCatch({
  if (!is.null(sens_spline)) {
    bed_grid <- data.frame(
      pop_available_s2 = seq(
        min(panel_model$pop_available_s2, na.rm = TRUE),
        max(panel_model$pop_available_s2, na.rm = TRUE),
        length.out = 100
      )
    )

    # Get spline term predictions
    spline_fit <- sens_spline$fit
    spline_terms <- predict(spline_fit, type = "terms")

    # Extract spline columns
    spline_cols <- grep("ns\\(pop_available_s2", colnames(spline_terms))
    if (length(spline_cols) > 0) {
      spline_effect <- rowSums(spline_terms[, spline_cols, drop = FALSE])

      # Create prediction data
      pred_df <- data.frame(
        beds = panel_model$pop_available_s2,
        log_hr = spline_effect
      ) %>%
        arrange(beds) %>%
        distinct(beds, .keep_all = TRUE)

      p8 <- ggplot(pred_df, aes(x = beds, y = exp(log_hr))) +
        geom_line(color = "#2c7bb6", linewidth = 1.2) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
        geom_rug(sides = "b", alpha = 0.1) +
        labs(
          title    = "Scarcity Threshold: Nonlinear Effect of Beds on Placement Hazard",
          subtitle = "Natural spline (3 df) — values > 1 indicate faster placement",
          x = "Available Beds (Population-Matched, Shift 2)",
          y = "Relative Placement Hazard"
        ) +
        theme_report

      ggsave(file.path(fig_dir, "08_scarcity_threshold_curve.png"),
             p8, width = 9, height = 5, dpi = 150)
      log_msg("  Saved: 08_scarcity_threshold_curve.png\n")
    }
  } else {
    log_msg("  Plot 8 SKIPPED: spline model not fitted\n")
  }
}, error = function(e) {
  log_msg(sprintf("  Plot 8 ERROR: %s\n", e$message))
})

# --- Plot 9: Counterfactual Placement Probability ----------------------------
tryCatch({
  if (!is.null(primary_results[["PLACED"]])) {
    placed_fit <- primary_results[["PLACED"]]$fit

    # Get bed distribution percentiles
    bed_pctiles <- quantile(panel_model$pop_available_s2, c(0.10, 0.50, 0.90),
                            na.rm = TRUE)

    # Get reference levels for other covariates
    ref_dow <- names(which.max(table(panel_model$day_of_week)))

    # Predict baseline survival at reference levels
    # Then adjust for different bed levels
    bed_coef <- placed_fit$coefficients["pop_available_s2"]

    if (!is.na(bed_coef)) {
      # HR for each bed level relative to median
      hr_low  <- exp(bed_coef * (bed_pctiles[1] - bed_pctiles[2]))
      hr_high <- exp(bed_coef * (bed_pctiles[3] - bed_pctiles[2]))

      # Approximate placement probability at Day 1, 3, 7
      # Using the baseline hazard + adjustment
      base_surv <- survfit(placed_fit)
      # Extract survival probabilities at target times
      target_days <- c(1, 3, 7)

      # Get overall baseline survival at target times
      surv_at_t <- approx(base_surv$time, base_surv$surv,
                           xout = target_days, method = "constant")$y
      surv_at_t[is.na(surv_at_t)] <- 1

      # Placement probability = 1 - survival probability
      # Adjust for different bed levels using HR
      counterfactual <- expand.grid(
        day = target_days,
        bed_level = c("Low (P10)", "Median (P50)", "High (P90)")
      )
      counterfactual$beds <- rep(bed_pctiles, each = length(target_days))

      counterfactual <- counterfactual %>%
        mutate(
          hr_adj = exp(bed_coef * (beds - bed_pctiles[2])),
          # S(t|x) ≈ S_0(t)^hr_adj
          surv_adj = surv_at_t[match(day, target_days)]^hr_adj,
          placed_prob = 1 - surv_adj,
          bed_level = factor(bed_level,
                             levels = c("Low (P10)", "Median (P50)", "High (P90)"))
        )

      p9 <- ggplot(counterfactual, aes(x = factor(day), y = placed_prob,
                                        fill = bed_level)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +
        scale_fill_manual(values = c("Low (P10)" = "#d7191c",
                                      "Median (P50)" = "#fdae61",
                                      "High (P90)" = "#2c7bb6")) +
        scale_y_continuous(labels = percent_format()) +
        labs(
          title    = "Counterfactual Placement Probability",
          subtitle = sprintf("Beds at P10=%d, P50=%d, P90=%d | From Cox primary model",
                              bed_pctiles[1], bed_pctiles[2], bed_pctiles[3]),
          x = "Days Since Case Creation", y = "Probability of Placement",
          fill = "Bed Level"
        ) +
        theme_report

      ggsave(file.path(fig_dir, "09_counterfactual_placement_probability.png"),
             p9, width = 9, height = 5, dpi = 150)
      log_msg("  Saved: 09_counterfactual_placement_probability.png\n")
    }
  }
}, error = function(e) {
  log_msg(sprintf("  Plot 9 ERROR: %s\n", e$message))
})

log_msg("\n")


# =============================================================================
# STEP 7 — Model Diagnostics
# =============================================================================
log_msg("--- Step 7: Model Diagnostics ---\n\n")

# --- Proportional Hazards Test (Primary PLACED model) -------------------------
if (!is.null(primary_results[["PLACED"]])) {
  tryCatch({
    ph_test <- cox.zph(primary_results[["PLACED"]]$fit)
    log_msg("  Proportional Hazards Test (Schoenfeld) — PLACED model:\n")
    ph_df <- as.data.frame(ph_test$table)
    ph_df$term <- rownames(ph_df)
    for (i in seq_len(nrow(ph_df))) {
      log_msg(sprintf("    %-30s chisq=%.2f  p=%.4f %s\n",
                       ph_df$term[i], ph_df$chisq[i], ph_df$p[i],
                       if_else(ph_df$p[i] < 0.05, " *VIOLATION*", "")))
    }

    # Save PH diagnostic plot
    tryCatch({
      png(file.path(fig_dir, "10_ph_diagnostic_placed.png"),
          width = 10, height = 8, units = "in", res = 150)
      plot(ph_test)
      dev.off()
      log_msg("\n  Saved: 10_ph_diagnostic_placed.png\n")
    }, error = function(e) {
      log_msg(sprintf("  PH plot error: %s\n", e$message))
    })
  }, error = function(e) {
    log_msg(sprintf("  PH test error: %s\n", e$message))
  })
}

log_msg("\n")


# =============================================================================
# STEP 8 — Reporting Outputs
# =============================================================================
log_msg("--- Step 8: Reporting Outputs ---\n\n")

table_dir <- file.path(run_dir, "tables")

# --- 8.1: Hazard Ratio Table (all models) -----------------------------------
all_coefs <- bind_rows(
  lapply(primary_results, function(r) r$coefs)
)

# Add sensitivity models
sens_models <- list(sens_staffing, sens_util, sens_lag, sens_30d)
for (sm in sens_models) {
  if (!is.null(sm) && !is.null(sm$coefs)) {
    all_coefs <- bind_rows(all_coefs, sm$coefs)
  }
}

write_csv(all_coefs, file.path(table_dir, "hazard_ratio_table.csv"))
log_msg("  Written: tables/hazard_ratio_table.csv (", nrow(all_coefs), " rows)\n")

# --- 8.2: Exit Distribution Summary ------------------------------------------
exit_summary <- cases %>%
  filter(ash_places == 1, created_date >= ANALYSIS_START) %>%
  mutate(exit_display = if_else(is.na(exit_type), "OPEN", exit_type)) %>%
  count(population, exit_display) %>%
  pivot_wider(names_from = exit_display, values_from = n, values_fill = 0)

write_csv(exit_summary, file.path(table_dir, "exit_distribution.csv"))
log_msg("  Written: tables/exit_distribution.csv\n")

# --- 8.3: Sensitivity Comparison Table ----------------------------------------
sens_comparison <- all_coefs %>%
  filter(grepl("pop_available|pop_util", term)) %>%
  select(model, term, HR, HR_lo95, HR_hi95, p_value, n_events) %>%
  arrange(model)

write_csv(sens_comparison, file.path(table_dir, "sensitivity_comparison.csv"))
log_msg("  Written: tables/sensitivity_comparison.csv\n")

# Print sensitivity comparison
log_msg("\n  Sensitivity Comparison (bed coefficient across models):\n")
for (i in seq_len(nrow(sens_comparison))) {
  log_msg(sprintf("    %-30s %s HR=%.4f [%.4f, %.4f] p=%.2e  (n=%d)\n",
                   sens_comparison$model[i], sens_comparison$term[i],
                   sens_comparison$HR[i], sens_comparison$HR_lo95[i],
                   sens_comparison$HR_hi95[i], sens_comparison$p_value[i],
                   sens_comparison$n_events[i]))
}

# --- 8.4: Save intermediate panel (for reproducibility) ----------------------
panel_save <- panel_model %>%
  select(wo_number, day_date, tstart, tstop, age_days, age_bin, population,
         pop_available_s2, pop_utilization_s2, pop_available_s2_lag1,
         disp_24h, day_of_week, weekend,
         event_PLACED, event_CANCELLED, event_NON_CONTACT,
         event_REFUSED, event_OTHER)

write_csv(panel_save, file.path(run_dir, "data", "case_day_panel.csv"))
log_msg(sprintf("  Written: data/case_day_panel.csv (%d rows)\n", nrow(panel_save)))

log_msg("\n")


# =============================================================================
# STEP 9 — Interpretation Framework
# =============================================================================
log_msg("--- Step 9: Operational Interpretation ---\n\n")

if (!is.null(primary_results[["PLACED"]])) {
  placed_coefs <- primary_results[["PLACED"]]$coefs
  bed_row <- placed_coefs %>% filter(term == "pop_available_s2")

  if (nrow(bed_row) > 0) {
    hr_10bed <- bed_row$HR^10
    hr_lo_10 <- bed_row$HR_lo95^10
    hr_hi_10 <- bed_row$HR_hi95^10
    pct_change <- (1 - hr_10bed) * 100

    log_msg("  KEY FINDINGS (PLACED model):\n\n")
    log_msg(sprintf("  1. A 10-bed drop in population-matched beds reduces the daily\n"))
    log_msg(sprintf("     placement hazard by %.1f%% (95%% CI: %.1f%% to %.1f%%)\n\n",
                     abs(pct_change),
                     abs((1 - hr_hi_10) * 100),
                     abs((1 - hr_lo_10) * 100)))

    # Translate to days: if baseline median time-to-placement is T,
    # and HR for 10-bed drop is h, then new median ≈ T / h
    if (!is.null(primary_results[["PLACED"]]$fit)) {
      base_fit <- survfit(primary_results[["PLACED"]]$fit)
      # Extract median — handle different survfit table formats
      surv_table <- base_fit$table
      median_time <- tryCatch({
        if (is.matrix(surv_table)) {
          # Multiple strata: use overall or first row
          med <- surv_table[1, "median"]
          if (is.na(med)) surv_table[1, "*rmean"] else med
        } else if ("median" %in% names(surv_table)) {
          med <- surv_table["median"]
          if (is.na(med)) surv_table["*rmean"] else med
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_)

      if (!is.na(median_time)) {
        adj_time <- median_time / hr_10bed
        extra_days <- adj_time - median_time
        log_msg(sprintf("  2. This translates to approximately %.1f additional days\n", extra_days))
        log_msg(sprintf("     until placement (baseline median: %.1f days)\n\n", median_time))
      }
    }
  }

  # Cancellation effect
  if (!is.null(primary_results[["CANCELLED"]])) {
    canc_coefs <- primary_results[["CANCELLED"]]$coefs
    canc_bed <- canc_coefs %>% filter(term == "pop_available_s2")
    if (nrow(canc_bed) > 0) {
      canc_hr_10 <- canc_bed$HR^10
      canc_pct   <- (canc_hr_10 - 1) * 100
      if (canc_hr_10 > 1) {
        log_msg(sprintf("  3. A 10-bed drop increases the cancellation hazard by %.1f%%\n",
                         abs(canc_pct)))
      } else {
        log_msg(sprintf("  3. A 10-bed drop decreases the cancellation hazard by %.1f%%\n",
                         abs(canc_pct)))
      }
      log_msg(sprintf("     (HR for 10-bed decrease: %.3f, 95%% CI: %.3f to %.3f)\n\n",
                       1/canc_hr_10,
                       1/canc_bed$HR_hi95^10,
                       1/canc_bed$HR_lo95^10))
    }
  }

  # REFUSED caveat
  if (!is.null(primary_results[["REFUSED"]])) {
    log_msg("  NOTE: REFUSED model results are preliminary (n=62 events).\n")
    log_msg("  Interpret with caution; confidence intervals are wide.\n\n")
  }
}


# =============================================================================
# WRAP UP
# =============================================================================
log_msg("==========================================================\n")
log_msg("  Analysis Complete\n")
log_msg("  Run: ", RUN_TIMESTAMP, "\n")
log_msg("  Output: ", run_dir, "\n")
log_msg("==========================================================\n\n")

# Run metadata summary
log_msg("  RUN METADATA:\n")
log_msg("    Export date:        ", format(EXPORT_DATE), "\n")
log_msg("    Analysis start:    ", format(ANALYSIS_START), "\n")
log_msg("    Total TBC cases:   ", nrow(cases), " (ASH-placeable in window)\n")
log_msg("    Panel rows:        ", nrow(panel_model), "\n")
log_msg("    Unique dates:      ", n_distinct(panel_model$day_date), "\n")
log_msg("    Cluster groups:    ", n_distinct(panel_model$day_pop), "\n")
log_msg("    Populations:       ", paste(ASH_POPULATIONS, collapse = ", "), "\n")
log_msg("    Age bins:          ", paste(AGE_BIN_LABELS, collapse = ", "), "\n")
log_msg("    Primary models:    ", length(primary_results), " of 5 converged\n")
log_msg("    Plots generated:   see figures/\n")
log_msg("    Tables generated:  see tables/\n")

# Close log
close(log_con)
cat("\nLog written to:", log_file, "\n")

# =============================================================================
# COPY LATEST — Populate RQ1 root figures/, tables/, data/ for easy access
# =============================================================================
rq1_root <- here("analysis", "RQ1_bed_scarcity_placement_speed")

copy_latest <- function(src_subdir, dst_dir) {
  src <- file.path(run_dir, src_subdir)
  if (!dir.exists(src)) return(invisible(NULL))
  dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
  # Clear old files in destination
  old_files <- list.files(dst_dir, full.names = TRUE)
  if (length(old_files) > 0) file.remove(old_files)
  # Copy new files
  new_files <- list.files(src, full.names = TRUE)
  if (length(new_files) > 0) file.copy(new_files, dst_dir, overwrite = TRUE)
  cat("  Copied", length(new_files), "files from", src_subdir, "to", dst_dir, "\n")
}

cat("\nCopying latest outputs to RQ1 root...\n")
copy_latest("figures", file.path(rq1_root, "figures"))
copy_latest("tables",  file.path(rq1_root, "tables"))
copy_latest("data",    file.path(rq1_root, "data"))
cat("Done. Latest outputs available at:", rq1_root, "\n")
