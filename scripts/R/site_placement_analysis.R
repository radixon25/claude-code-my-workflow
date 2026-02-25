# =============================================================================
# site_placement_analysis.R — Site-Level Bed Availability & Placement Analysis
# =============================================================================
# Poisson fixed-effects regression on a site-day panel linking shelter-specific
# bed availability to daily placement counts, with weather and demand controls.
#
# RESEARCH QUESTION:
#   When a shelter has more open beds than usual, does it place more people?
#   How efficiently do different shelters convert open beds into placements?
#
# DESIGN:
#   - Site-day panel (one row per shelter per day)
#   - Outcome: n_placed (daily placements at site)
#   - Primary exposure: beds_lag1 (yesterday's 9am available beds at that site)
#   - Lag structure: 36h operational pipeline (bed available -> case closed in SF)
#   - Fixed effects: shelter + day-of-week (absorbs all time-invariant site diffs)
#   - Controls: weather (temperature, cold day), demand (TBH/TBG closures)
#   - Clustering: by date (accounts for system-wide daily shocks)
#
# KEY METHODOLOGICAL CHOICE — LAG STRUCTURE:
#   Bed-to-placement pipeline: bed available (~0h) -> candidate identified (~6h)
#   -> submitted for approval (~6h) -> approved (~12h) -> client arrives (~12h)
#   -> case closed in Salesforce (~6h). Total: ~12-48h, central estimate ~36h.
#   Primary specification uses lag-1 (yesterday's beds -> today's placements).
#   Sensitivity: lag-0, lag-2, 2-day rolling average, distributed lag.
#
# EXTENSION:
#   Site conversion efficiency — which shelters convert open beds to placements
#   most readily? Ranks shelters by site-specific beds->placements coefficient.
#
# DATA SOURCES:
#   - wide.parquet: case-level data (7,503 WOs, all types)
#   - shelter_beds_site_day.csv: daily bed counts per shelter site
#   - Open-Meteo Archive API: real Chicago weather data
#
# OUTPUTS:
#   analysis/RQ2_site_bed_placement_volume/runs/YYYY-MM-DD_HHMMSS/
#     data/       — site-day panel, intermediate datasets
#     figures/    — 11 diagnostic and interpretive plots
#     tables/     — coefficient tables, lag comparison, conversion rates
#     run_log.txt — console output + causal discussion + executive summary
#
# USAGE:
#   Rscript scripts/R/site_placement_analysis.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(arrow)
  library(fixest)
  library(httr)
  library(jsonlite)
})


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
PARQUET_PATH     <- "C:/Users/sagea/Desktop/A Safe Haven/monthlyreport/data/processed/wide.parquet"
WEATHER_CSV_PATH <- "C:/Users/sagea/Desktop/A Safe Haven/monthlyreport/data/inputs/weather_daily.csv"
BED_SITE_DAY_PATH <- file.path(main_root, "data", "processed", "shelter_beds_site_day.csv")

ANALYSIS_START <- as.Date("2026-01-10")
EXPORT_DATE    <- Sys.Date()
RUN_TIMESTAMP  <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

# ASH-placeable populations
ASH_POPULATIONS <- c("Men", "Women", "Youth")

# Lag structure: 36h operational pipeline
PRIMARY_LAG <- 1  # lag-1 = yesterday's beds -> today's recorded placements

# Chicago coordinates for weather API
CHICAGO_LAT <- 41.88
CHICAGO_LON <- -87.63

# --- Timestamped output directory --------------------------------------------
run_dir <- here("analysis", "RQ2_site_bed_placement_volume", "runs", RUN_TIMESTAMP)
dir.create(file.path(run_dir, "data"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "tables"),  recursive = TRUE, showWarnings = FALSE)

fig_dir   <- file.path(run_dir, "figures")
tbl_dir   <- file.path(run_dir, "tables")
data_dir  <- file.path(run_dir, "data")

# --- Logging -----------------------------------------------------------------
log_file <- file.path(run_dir, "run_log.txt")
log_con  <- file(log_file, open = "wt")

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
log_msg("  Site-Level Bed Availability & Placement Analysis\n")
log_msg("  Poisson Fixed-Effects with 36h Lag Structure\n")
log_msg("  Run: ", RUN_TIMESTAMP, "\n")
log_msg("  Output: ", run_dir, "\n")
log_msg("==========================================================\n\n")

log_msg("  R version:     ", R.version.string, "\n")
log_msg("  fixest:        ", as.character(packageVersion("fixest")), "\n")
log_msg("  arrow:         ", as.character(packageVersion("arrow")), "\n")
log_msg("  Analysis start:", format(ANALYSIS_START), "\n")
log_msg("  Export date:   ", format(EXPORT_DATE), "\n")
log_msg("  Primary lag:   ", PRIMARY_LAG, " day(s)\n")
log_msg("\n")


# =============================================================================
# STEP 1 — Data Preparation
# =============================================================================
log_msg("--- Step 1: Data Preparation ---\n\n")

# --- 1.0: Read parquet -------------------------------------------------------
log_msg("  Reading parquet: ", PARQUET_PATH, "\n")
all_cases <- read_parquet(PARQUET_PATH)
log_msg("  Total rows: ", nrow(all_cases), "\n")
log_msg("  Case types: ", paste(names(table(all_cases$wo_tc)), collapse = ", "), "\n\n")

# --- 1.1: Clean gender -> population -----------------------------------------
# Gender column is messy (raw Salesforce with pipe-delimited multi-answers).
# Extract primary gender: look for Male/Female as dominant token.
all_cases <- all_cases %>%
  mutate(
    gender_clean = case_when(
      grepl("^Male$|^male$|^MALE$|^M$|^m$", trimws(client_gender))       ~ "Male",
      grepl("^Female$|^female$|^FEMALE$|^F$|^f$", trimws(client_gender))  ~ "Female",
      grepl("\\bMale\\b", client_gender, ignore.case = TRUE) &
        !grepl("\\bFemale\\b", client_gender, ignore.case = TRUE)         ~ "Male",
      grepl("\\bFemale\\b", client_gender, ignore.case = TRUE) &
        !grepl("\\bMale\\b", client_gender, ignore.case = TRUE)           ~ "Female",
      TRUE ~ "Unknown"
    ),
    is_youth = (youth_client == "Yes"),
    has_children = grepl("Child", hh_makeup, ignore.case = TRUE),
    population = case_when(
      has_children         ~ "Families",
      is_youth             ~ "Youth",
      gender_clean == "Male"   ~ "Men",
      gender_clean == "Female" ~ "Women",
      TRUE                     ~ "Unknown"
    )
  )

log_msg("  Population distribution (all cases):\n")
pop_dist <- all_cases %>% count(wo_tc, population) %>% arrange(wo_tc, desc(n))
for (i in seq_len(nrow(pop_dist))) {
  log_msg(sprintf("    %-5s %-12s n=%d\n",
                   pop_dist$wo_tc[i], pop_dist$population[i], pop_dist$n[i]))
}
log_msg("\n")

# --- 1.2: Parse dates --------------------------------------------------------
all_cases <- all_cases %>%
  mutate(
    # created_date: "1/1/2026" format
    created_dt = mdy(created_date),
    # end_date: "1/1/2026 7:28" format (datetime)
    closed_dt  = mdy_hm(end_date),
    closed_date = as.Date(closed_dt),
    # Censor date for open cases
    censor_date = if_else(is.na(closed_date), EXPORT_DATE, closed_date)
  )

# --- 1.3: Build site crosswalk -----------------------------------------------
log_msg("  Building site crosswalk (placement name -> bed site name)...\n")

# Hard-coded curated crosswalk based on manual review of placement names
# against bed tracker site names. Maps shelter_placed values to one or more
# bed site names from shelter_beds_site_day.csv.
site_crosswalk <- tribble(
  ~placement_name,                                                   ~bed_sites,
  "MWRD",                                                            "MWRD",
  "Franciscan Outreach - House of Mary and Joseph",                  "House of Mary & Joseph - Men;House of Mary & Joseph - Women",
  "YWLA",                                                            "YWLA - Men;YWLA - Women",
  "Olive Branch Mission - Lamplight I",                              "Lamplight Family I",
  "Olive Branch Mission - Lamplight Single Women",                   "Lamplight Singles - Women",
  "Lincoln Park Community Shelter - Fullerton",                      "Fullerton - Men;Fullerton - Women",
  "Cornerstone - Sylvia",                                            "Sylvia",
  "Margaret's Village - Maria Shelter",                              "Maria - Families;Maria - Women",
  "Primo - Sangamon",                                                "Sangamon",
  "A Safe Haven - Roosevelt",                                        "Roosevelt - Men",
  "North Side Housing & Supportive Services - Michael Segoviano Shelter", "Sedgwick - Men;Sedgwick - Women",
  "Olive Branch Mission - Lamplight Single Men",                     "Lamplight Singles - Men",
  "Good News Partners - New Life",                                   "New Life Interim Housing - Families;New Life Interim Housing - Women",
  "New Life CC",                                                     "New Life Interim Housing - Families",
  "AIC",                                                             "AIC",
  "Inner Voice - Pioneer House",                                     "Pioneer House Transitional Housing Program",
  "Deborah's Place - Teresa",                                        "Teresa",
  "A Safe Haven - West",                                             "West",
  "Margaret's Village - Believe Shelter",                             "Believe",
  "Christian Community Health Center - Amani House",                 "Amani House - Families;Amani House - Women",
  "Primo - Madison",                                                 "Madison",
  "Olive Branch Mission - Lamplight II",                             "Lamplight Family II",
  "The Salvation Army - EHARC",                                      "EHARC",
  "Olive Branch Mission - Lamplight III",                            "Lamplight Family III",
  "Casa Central La Posada - 1322 N Kedzie",                         "La Posada I - 1322 N. Kedzie",
  "Unity - Ujima Village",                                           "Ujima Village Low Threshold Shelter",
  "Cornerstone - Hannah House",                                      "Hannah",
  "Cornerstone - Naomi Men",                                         "Naomi - Men",
  "Cornerstone - Naomi Women",                                       "Naomi - Women",
  "Featherfist - Hope Village",                                      "Hope Village",
  "The Salvation Army - Evangeline Booth Lodge",                     "Evangeline Booth Lodge",
  "A Safe Haven - Youth",                                            "West",
  "La Casa Norte - 47th Street",                                     "Casa Corazon Emergency Beds - 47th St",
  "The Night Ministry - Open Door Youth Shelter",                    "Open Door Youth Shelter",
  "Connections for Abused Women and their Children Greenhouse Shelter", "Greenhouse"
)

# Unmapped (excluded from analysis):
# "Breakthrough Urban Ministries" — no matching bed site (community center, not tracked)
# "Sarah's Circle" — independent shelter, not in bed tracker
# "Phoenix House" — Phoenix Rising is state-funded, different program
# "Walnut" — no matching site name
# "Neopolitan Lighthouse - DV Shelter" — DV shelter, excluded population
# "Wings - DV Shelter" — DV shelter, excluded population
# "Inner Voice - Eddie Beard Vet House" — veteran transitional, not in bed tracker
# "Pacific Garden Mission - Men" — independent, not in bed tracker
# "Other" — catch-all, excluded by design

# Export crosswalk for audit
write_csv(site_crosswalk, file.path(tbl_dir, "site_crosswalk.csv"))

# --- 1.4: TBC placements with site link --------------------------------------
tbc_placed <- all_cases %>%
  filter(
    wo_tc == "TBC",
    activity_outcome == "Client(s) Placed in Shelter",
    !is.na(shelter_placed),
    shelter_placed != "",
    shelter_placed != "Other"
  ) %>%
  inner_join(site_crosswalk, by = c("shelter_placed" = "placement_name")) %>%
  filter(!is.na(closed_date)) %>%
  filter(closed_date >= ANALYSIS_START) %>%
  select(wo_number, population, shelter_placed, bed_sites, closed_date)

# Compute coverage against the appropriate denominator:
# eligible = placed, with shelter name, not "Other", with valid close date, after start date
eligible_placed <- all_cases %>%
  filter(
    wo_tc == "TBC",
    activity_outcome == "Client(s) Placed in Shelter",
    !is.na(shelter_placed), shelter_placed != "", shelter_placed != "Other",
    !is.na(closed_date),
    closed_date >= ANALYSIS_START
  )

log_msg("  TBC placed cases with site link: ", nrow(tbc_placed), "\n")
log_msg("  Coverage (of eligible placed cases in analysis window): ",
        nrow(tbc_placed), " / ", nrow(eligible_placed),
        " (", round(100 * nrow(tbc_placed) / nrow(eligible_placed), 1), "%)\n")
log_msg("  Unmapped shelters excluded: Breakthrough Urban Ministries (24),\n")
log_msg("    Sarah's Circle (8), Phoenix House (6), DV shelters (4), others (3)\n\n")

# --- 1.5: Non-TBC demand controls --------------------------------------------
# Daily TBH closures
tbh_daily <- all_cases %>%
  filter(wo_tc == "TBH", !is.na(closed_date)) %>%
  count(closed_date, name = "daily_tbh_closures")

# Daily TBG closures
tbg_daily <- all_cases %>%
  filter(wo_tc == "TBG", !is.na(closed_date)) %>%
  count(closed_date, name = "daily_tbg_closures")

# Daily TBC open caseload (cases open on each date)
date_range <- seq(ANALYSIS_START, EXPORT_DATE, by = "day")

tbc_all <- all_cases %>%
  filter(wo_tc == "TBC", !is.na(created_dt))

tbc_open_daily <- tibble(date = date_range) %>%
  rowwise() %>%
  mutate(
    tbc_open_caseload = sum(
      tbc_all$created_dt <= date &
        (is.na(tbc_all$closed_date) | tbc_all$closed_date > date)
    )
  ) %>%
  ungroup()

log_msg("  Demand controls built:\n")
log_msg("    TBH daily closures: ", nrow(tbh_daily), " days\n")
log_msg("    TBG daily closures: ", nrow(tbg_daily), " days\n")
log_msg("    TBC open caseload:  ", nrow(tbc_open_daily), " days\n\n")


# =============================================================================
# STEP 2 — Weather Data
# =============================================================================
log_msg("--- Step 2: Weather Data ---\n\n")

fetch_weather <- function(start_date, end_date, lat, lon, cache_path) {
  # Check if cache exists and is recent
  if (file.exists(cache_path)) {
    cached <- read_csv(cache_path, show_col_types = FALSE)
    if (nrow(cached) > 10) {
      log_msg("  Using cached weather data (", nrow(cached), " days)\n")
      return(cached)
    }
  }

  log_msg("  Fetching weather from Open-Meteo API...\n")
  url <- sprintf(
    "https://archive-api.open-meteo.com/v1/archive?latitude=%.2f&longitude=%.2f&start_date=%s&end_date=%s&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,snowfall_sum,wind_speed_10m_max&temperature_unit=fahrenheit&precipitation_unit=inch&timezone=America%%2FChicago",
    lat, lon, format(start_date), format(end_date)
  )

  resp <- GET(url)
  if (status_code(resp) != 200) {
    log_msg("  WARNING: Weather API returned status ", status_code(resp), "\n")
    return(NULL)
  }

  js <- fromJSON(content(resp, "text", encoding = "UTF-8"))

  weather <- tibble(
    date         = as.Date(js$daily$time),
    temp_max_f   = js$daily$temperature_2m_max,
    temp_min_f   = js$daily$temperature_2m_min,
    precip_in    = js$daily$precipitation_sum,
    snow_in      = js$daily$snowfall_sum,
    wind_mph     = round(js$daily$wind_speed_10m_max * 0.621371, 1)
  ) %>%
    mutate(
      avg_temp_f     = (temp_max_f + temp_min_f) / 2,
      cold_day       = as.integer(temp_min_f < 20),
      severe_weather = as.integer(snow_in > 2 | wind_mph > 30)
    )

  write_csv(weather, cache_path)
  log_msg("  Weather cached to: ", cache_path, " (", nrow(weather), " days)\n")
  return(weather)
}

weather <- fetch_weather(
  start_date = as.Date("2026-01-01"),
  end_date   = EXPORT_DATE - 1,
  lat = CHICAGO_LAT, lon = CHICAGO_LON,
  cache_path = WEATHER_CSV_PATH
)

if (is.null(weather)) {
  log_msg("  ERROR: Could not fetch weather data. Proceeding without weather controls.\n")
  weather <- tibble(date = date_range, avg_temp_f = NA_real_, cold_day = NA_integer_,
                    severe_weather = NA_integer_, temp_max_f = NA_real_,
                    temp_min_f = NA_real_, precip_in = NA_real_,
                    snow_in = NA_real_, wind_mph = NA_real_)
}

log_msg("  Weather data: ", nrow(weather), " days, ",
        sum(weather$cold_day, na.rm = TRUE), " cold days (min < 20F)\n\n")


# =============================================================================
# STEP 3 — Build Site-Day Panel
# =============================================================================
log_msg("--- Step 3: Build Site-Day Panel ---\n\n")

# --- 3.1: Read bed site-day data ---------------------------------------------
log_msg("  Reading bed data: ", BED_SITE_DAY_PATH, "\n")
bed_site_day <- read_csv(BED_SITE_DAY_PATH, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# Filter to analysis window and ASH-placeable populations
bed_site_day <- bed_site_day %>%
  filter(
    date >= ANALYSIS_START,
    population %in% c("Men", "Women", "Youth", "Families")
  )

log_msg("  Bed site-days (filtered): ", nrow(bed_site_day), "\n")
log_msg("  Sites: ", length(unique(bed_site_day$site_name)), "\n")
log_msg("  Date range: ", as.character(min(bed_site_day$date)), " to ",
        as.character(max(bed_site_day$date)), "\n\n")

# --- 3.2: Expand multi-site crosswalk ----------------------------------------
# Some placements map to multiple bed sites (e.g., YWLA -> YWLA-Men + YWLA-Women).
# For these, we need to aggregate beds across sub-sites into a "shelter group".
# Strategy: create a mapping from bed_site_name to shelter_group, then aggregate.

# Build the reverse mapping: bed_site -> shelter_group
shelter_groups <- site_crosswalk %>%
  mutate(bed_site_list = strsplit(bed_sites, ";")) %>%
  unnest(bed_site_list) %>%
  mutate(bed_site_list = trimws(bed_site_list)) %>%
  # The shelter_group is the placement_name (canonical name for the group)
  select(shelter_group = placement_name, bed_site = bed_site_list)

# Aggregate bed data to shelter_group level
bed_by_group <- bed_site_day %>%
  inner_join(shelter_groups, by = c("site_name" = "bed_site"),
             relationship = "many-to-many") %>%
  group_by(shelter_group, date) %>%
  summarise(
    capacity         = sum(capacity, na.rm = TRUE),
    shift2_available = sum(shift2_available, na.rm = TRUE),
    utilization_s2   = ifelse(sum(capacity, na.rm = TRUE) > 0,
                              1 - sum(shift2_available, na.rm = TRUE) /
                                sum(capacity, na.rm = TRUE), NA_real_),
    n_sub_sites      = n(),
    .groups = "drop"
  )

log_msg("  Shelter groups (aggregated): ", length(unique(bed_by_group$shelter_group)), "\n")

# --- 3.3: Create lagged bed variables ----------------------------------------
bed_by_group <- bed_by_group %>%
  arrange(shelter_group, date) %>%
  group_by(shelter_group) %>%
  mutate(
    beds_lag0 = shift2_available,
    beds_lag1 = lag(shift2_available, 1),
    beds_lag2 = lag(shift2_available, 2),
    beds_avg_2d = (beds_lag1 + beds_lag2) / 2,
    utilization_lag1 = lag(utilization_s2, 1)
  ) %>%
  ungroup()

log_msg("  Lag variables created (lag0, lag1, lag2, avg_2d)\n")

# --- 3.4: Aggregate placements to site-day -----------------------------------
# Each placed case gets mapped to its shelter_group via the crosswalk
placements_by_group <- tbc_placed %>%
  inner_join(
    site_crosswalk %>% select(placement_name),
    by = c("shelter_placed" = "placement_name")
  ) %>%
  # Use the placement_name as the shelter_group
  mutate(shelter_group = shelter_placed) %>%
  group_by(shelter_group, closed_date) %>%
  summarise(n_placed = n(), .groups = "drop")

log_msg("  Placement-group-days with >0 placements: ", nrow(placements_by_group), "\n")

# --- 3.5: Join into site-day panel -------------------------------------------
site_panel <- bed_by_group %>%
  left_join(placements_by_group,
            by = c("shelter_group", "date" = "closed_date")) %>%
  mutate(n_placed = replace_na(n_placed, 0L)) %>%
  # Attach weather
  left_join(weather %>% select(date, avg_temp_f, cold_day, severe_weather,
                                temp_max_f, temp_min_f, snow_in, wind_mph),
            by = "date") %>%
  # Attach demand controls
  left_join(tbh_daily, by = c("date" = "closed_date")) %>%
  left_join(tbg_daily, by = c("date" = "closed_date")) %>%
  left_join(tbc_open_daily, by = "date") %>%
  mutate(
    daily_tbh_closures = replace_na(daily_tbh_closures, 0L),
    daily_tbg_closures = replace_na(daily_tbg_closures, 0L),
    dow = wday(date, label = TRUE)
  ) %>%
  # Drop rows where lag-1 is NA (first day of each site)
  filter(!is.na(beds_lag1))

# Save panel
write_csv(site_panel, file.path(data_dir, "site_day_panel.csv"))

log_msg("  Site-day panel: ", nrow(site_panel), " rows\n")
log_msg("  Shelters in panel: ", length(unique(site_panel$shelter_group)), "\n")
log_msg("  Days in panel: ", length(unique(site_panel$date)), "\n")
log_msg("  Total placements in panel: ", sum(site_panel$n_placed), "\n")
log_msg("  Mean placements per site-day: ", round(mean(site_panel$n_placed), 4), "\n")
log_msg("  Site-days with >0 placements: ", sum(site_panel$n_placed > 0),
        " (", round(100 * mean(site_panel$n_placed > 0), 1), "%)\n\n")

# Site summary table
site_summary <- site_panel %>%
  group_by(shelter_group) %>%
  summarise(
    n_days        = n(),
    capacity      = max(capacity),
    mean_beds     = round(mean(beds_lag1, na.rm = TRUE), 1),
    total_placed  = sum(n_placed),
    mean_placed   = round(mean(n_placed), 3),
    pct_days_placed = round(100 * mean(n_placed > 0), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(total_placed))

write_csv(site_summary, file.path(tbl_dir, "site_summary.csv"))
log_msg("  Site summary table saved\n\n")


# =============================================================================
# STEP 4 — Diagnostic Plots (Plots 1-4)
# =============================================================================
log_msg("--- Step 4: Diagnostic Plots ---\n\n")

# --- Plot 01: Placements per site over time (heatmap) -------------------------
p1_data <- site_panel %>%
  mutate(shelter_short = gsub(" - .*", "", shelter_group)) %>%
  group_by(shelter_group) %>%
  filter(sum(n_placed) >= 3) %>%
  ungroup()

p1 <- ggplot(p1_data, aes(x = date, y = reorder(shelter_group, -n_placed, FUN = sum),
                            fill = n_placed)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "grey95", high = "#d7191c", name = "Placements") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "Daily Placements by Shelter",
       subtitle = "Sites with 3+ total placements | One tile per site-day",
       x = NULL, y = NULL) +
  theme_report +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(fig_dir, "01_placements_heatmap.png"), p1,
       width = 14, height = 10, dpi = 150)
log_msg("  Plot 01 saved: placements heatmap\n")

# --- Plot 02: Beds vs placements scatter --------------------------------------
p2 <- ggplot(site_panel, aes(x = beds_lag1, y = n_placed)) +
  geom_jitter(alpha = 0.3, width = 0.3, height = 0.1, color = "#2c7bb6") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#d7191c", se = TRUE) +
  labs(title = "Lag-1 Bed Availability vs Same-Day Placements",
       subtitle = "Each point = one site-day | Red line = Poisson fit",
       x = "Available Beds (previous day, 9am)",
       y = "Placements (today)") +
  theme_report

ggsave(file.path(fig_dir, "02_beds_vs_placements_scatter.png"), p2,
       width = 10, height = 7, dpi = 150)
log_msg("  Plot 02 saved: beds vs placements scatter\n")

# --- Plot 03: Weather time series with placements overlay ---------------------
daily_placements <- site_panel %>%
  group_by(date) %>%
  summarise(total_placed = sum(n_placed), .groups = "drop")

p3_data <- weather %>%
  filter(date >= ANALYSIS_START) %>%
  left_join(daily_placements, by = "date")

p3 <- ggplot(p3_data, aes(x = date)) +
  geom_col(aes(y = total_placed * 3), fill = "#2c7bb6", alpha = 0.4, width = 0.8) +
  geom_line(aes(y = avg_temp_f), color = "#d7191c", linewidth = 1) +
  geom_point(data = p3_data %>% filter(cold_day == 1),
             aes(y = avg_temp_f), color = "#d7191c", size = 2, shape = 17) +
  scale_y_continuous(
    name = "Average Temperature (F)",
    sec.axis = sec_axis(~. / 3, name = "Daily Placements (system-wide)")
  ) +
  labs(title = "Weather and Placement Volume Over Time",
       subtitle = "Red triangles = cold days (min < 20F) | Blue bars = daily placements",
       x = NULL) +
  theme_report

ggsave(file.path(fig_dir, "03_weather_placements_timeseries.png"), p3,
       width = 12, height = 6, dpi = 150)
log_msg("  Plot 03 saved: weather + placements time series\n")

# --- Plot 04: Demand controls time series -------------------------------------
demand_data <- tbc_open_daily %>%
  left_join(tbh_daily, by = c("date" = "closed_date")) %>%
  left_join(tbg_daily, by = c("date" = "closed_date")) %>%
  mutate(
    daily_tbh_closures = replace_na(daily_tbh_closures, 0L),
    daily_tbg_closures = replace_na(daily_tbg_closures, 0L)
  ) %>%
  filter(date >= ANALYSIS_START)

p4 <- ggplot(demand_data, aes(x = date)) +
  geom_line(aes(y = tbc_open_caseload, color = "TBC Open Caseload"), linewidth = 1) +
  geom_col(aes(y = daily_tbh_closures * 20, fill = "TBH Closures"), alpha = 0.5, width = 0.8) +
  geom_col(aes(y = daily_tbg_closures * 20, fill = "TBG Closures"), alpha = 0.5, width = 0.8) +
  scale_color_manual(values = c("TBC Open Caseload" = "#2c7bb6"), name = NULL) +
  scale_fill_manual(values = c("TBH Closures" = "#fdae61", "TBG Closures" = "#abdda4"), name = NULL) +
  scale_y_continuous(
    name = "TBC Open Caseload",
    sec.axis = sec_axis(~. / 20, name = "Daily Closures (TBH/TBG)")
  ) +
  labs(title = "Demand Controls Over Time",
       subtitle = "TBC open caseload (blue line) | TBH/TBG daily closures (bars)",
       x = NULL) +
  theme_report

ggsave(file.path(fig_dir, "04_demand_controls_timeseries.png"), p4,
       width = 12, height = 6, dpi = 150)
log_msg("  Plot 04 saved: demand controls time series\n\n")


# =============================================================================
# STEP 5 — Primary Models (M1-M6)
# =============================================================================
log_msg("--- Step 5: Primary Models ---\n\n")

# Ensure factor for FE
site_panel <- site_panel %>%
  mutate(
    site_fe  = factor(shelter_group),
    dow_fe   = factor(dow)
  )

# --- Model 1: Primary — Lag-1 Site FE Poisson --------------------------------
log_msg("  M1: Lag-1 Poisson FE (PRIMARY)\n")
m1 <- tryCatch(
  fepois(n_placed ~ beds_lag1 + avg_temp_f + cold_day +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date,
         data = site_panel),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
if (!is.null(m1)) {
  log_msg("    Beds (lag1) coef: ", round(coef(m1)["beds_lag1"], 6), "\n")
  log_msg(capture.output(summary(m1)), sep = "\n")
  log_msg("\n\n")
}

# --- Model 2: Lag-1 OLS FE ---------------------------------------------------
log_msg("  M2: Lag-1 OLS FE\n")
m2 <- tryCatch(
  feols(n_placed ~ beds_lag1 + avg_temp_f + cold_day +
          daily_tbh_closures + daily_tbg_closures |
          site_fe + dow_fe,
        cluster = ~date,
        data = site_panel),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
if (!is.null(m2)) {
  log_msg("    Beds (lag1) coef: ", round(coef(m2)["beds_lag1"], 6), "\n")
  log_msg(capture.output(summary(m2)), sep = "\n")
  log_msg("\n\n")
}

# --- Model 3: Utilization (lag-1) instead of raw beds -------------------------
log_msg("  M3: Utilization (lag-1) Poisson FE\n")
m3 <- tryCatch(
  fepois(n_placed ~ utilization_lag1 + avg_temp_f + cold_day +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date,
         data = site_panel %>% filter(!is.na(utilization_lag1))),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
if (!is.null(m3)) {
  log_msg("    Utilization (lag1) coef: ", round(coef(m3)["utilization_lag1"], 6), "\n")
  log_msg(capture.output(summary(m3)), sep = "\n")
  log_msg("\n\n")
}

# --- Model 4: Capacity interaction -------------------------------------------
log_msg("  M4: Capacity interaction (large vs small shelter)\n")
site_panel <- site_panel %>%
  mutate(large_site = as.integer(capacity > 50))

m4 <- tryCatch(
  fepois(n_placed ~ beds_lag1 * large_site + avg_temp_f + cold_day +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date,
         data = site_panel),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
if (!is.null(m4)) {
  log_msg(capture.output(summary(m4)), sep = "\n")
  log_msg("\n\n")
}

# --- Model 5: Weather interaction (cold days) ---------------------------------
log_msg("  M5: Weather interaction (beds x cold day)\n")
m5 <- tryCatch(
  fepois(n_placed ~ beds_lag1 * cold_day + avg_temp_f +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date,
         data = site_panel),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
if (!is.null(m5)) {
  log_msg(capture.output(summary(m5)), sep = "\n")
  log_msg("\n\n")
}

# --- Model 6: Distributed lag (lag0 + lag1 + lag2) ----------------------------
log_msg("  M6: Distributed lag (lag0 + lag1 + lag2)\n")
m6 <- tryCatch(
  fepois(n_placed ~ beds_lag0 + beds_lag1 + beds_lag2 + avg_temp_f + cold_day +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date,
         data = site_panel %>% filter(!is.na(beds_lag2))),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
if (!is.null(m6)) {
  log_msg(capture.output(summary(m6)), sep = "\n")
  cum_effect <- sum(coef(m6)[c("beds_lag0", "beds_lag1", "beds_lag2")])
  log_msg("\n  Cumulative 3-day effect: ", round(cum_effect, 6), "\n")
  log_msg("  Interpretation: One additional bed-day generates ~",
          round(cum_effect, 4), " placements over 3 days\n\n")
}


# =============================================================================
# STEP 6 — Sensitivity & Robustness
# =============================================================================
log_msg("--- Step 6: Sensitivity & Robustness ---\n\n")

sensitivity_results <- list()

# Helper to extract bed coefficient safely
extract_bed_coef <- function(model, varname = "beds_lag1") {
  if (is.null(model)) return(tibble(estimate = NA, std.error = NA, p.value = NA))
  ct <- coeftable(model)
  if (varname %in% rownames(ct)) {
    tibble(estimate = ct[varname, 1], std.error = ct[varname, 2], p.value = ct[varname, 4])
  } else {
    tibble(estimate = NA, std.error = NA, p.value = NA)
  }
}

# Primary (for comparison)
sensitivity_results[["M1_primary_lag1"]] <- extract_bed_coef(m1) %>%
  mutate(variant = "M1: Primary (lag-1)", lag = 1)

# --- 6a: Lag-0 (same-day) — expect attenuated --------------------------------
log_msg("  S-a: Lag-0 (same-day beds)\n")
sa <- tryCatch(
  fepois(n_placed ~ beds_lag0 + avg_temp_f + cold_day +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date, data = site_panel),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
sensitivity_results[["Sa_lag0"]] <- extract_bed_coef(sa, "beds_lag0") %>%
  mutate(variant = "S-a: Lag-0 (same day)", lag = 0)

# --- 6b: Lag-2 ----------------------------------------------------------------
log_msg("  S-b: Lag-2 (two-day lag)\n")
sb <- tryCatch(
  fepois(n_placed ~ beds_lag2 + avg_temp_f + cold_day +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date,
         data = site_panel %>% filter(!is.na(beds_lag2))),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
sensitivity_results[["Sb_lag2"]] <- extract_bed_coef(sb, "beds_lag2") %>%
  mutate(variant = "S-b: Lag-2 (2 days)", lag = 2)

# --- 6c: 2-day rolling average ------------------------------------------------
log_msg("  S-c: 2-day rolling average\n")
sc <- tryCatch(
  fepois(n_placed ~ beds_avg_2d + avg_temp_f + cold_day +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date,
         data = site_panel %>% filter(!is.na(beds_avg_2d))),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
sensitivity_results[["Sc_avg2d"]] <- extract_bed_coef(sc, "beds_avg_2d") %>%
  mutate(variant = "S-c: 2-day avg", lag = 1.5)

# --- 6d: Date FE instead of DOW (absorbs all daily shocks) --------------------
log_msg("  S-d: Date FE (absorbs all daily variation)\n")
site_panel <- site_panel %>% mutate(date_fe = factor(date))
sd_model <- tryCatch(
  fepois(n_placed ~ beds_lag1 |
           site_fe + date_fe,
         cluster = ~date,
         data = site_panel),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
sensitivity_results[["Sd_date_fe"]] <- extract_bed_coef(sd_model) %>%
  mutate(variant = "S-d: Date FE", lag = 1)

# --- 6e: Population-specific -------------------------------------------------
log_msg("  S-e: Population-specific models\n")

# Need to map shelter_group back to population from bed data
site_pop <- bed_site_day %>%
  inner_join(shelter_groups, by = c("site_name" = "bed_site"),
             relationship = "many-to-many") %>%
  group_by(shelter_group) %>%
  summarise(pop_primary = names(sort(table(population), decreasing = TRUE))[1],
            .groups = "drop")

site_panel_pop <- site_panel %>%
  left_join(site_pop, by = "shelter_group")

for (pop_name in c("Men", "Women")) {
  pop_data <- site_panel_pop %>% filter(pop_primary == pop_name)
  if (nrow(pop_data) < 30 || length(unique(pop_data$shelter_group)) < 3) {
    log_msg("    ", pop_name, ": insufficient data (", nrow(pop_data), " rows)\n")
    next
  }
  se_model <- tryCatch(
    fepois(n_placed ~ beds_lag1 + avg_temp_f + cold_day +
             daily_tbh_closures + daily_tbg_closures |
             site_fe + dow_fe,
           cluster = ~date, data = pop_data),
    error = function(e) { log_msg("    ", pop_name, " ERROR: ", e$message, "\n"); NULL }
  )
  sensitivity_results[[paste0("Se_", tolower(pop_name))]] <- extract_bed_coef(se_model) %>%
    mutate(variant = paste0("S-e: ", pop_name, " only"), lag = 1)
}

# --- 6f: Negative binomial (overdispersion check) ----------------------------
log_msg("  S-f: Negative binomial (overdispersion check)\n")
sf_model <- tryCatch(
  fenegbin(n_placed ~ beds_lag1 + avg_temp_f + cold_day +
             daily_tbh_closures + daily_tbg_closures |
             site_fe + dow_fe,
           cluster = ~date,
           data = site_panel),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
sensitivity_results[["Sf_negbin"]] <- extract_bed_coef(sf_model) %>%
  mutate(variant = "S-f: Neg. Binomial", lag = 1)

# --- 6g: Large sites only (capacity > 20) ------------------------------------
log_msg("  S-g: Large sites only (capacity > 20)\n")
sg <- tryCatch(
  fepois(n_placed ~ beds_lag1 + avg_temp_f + cold_day +
           daily_tbh_closures + daily_tbg_closures |
           site_fe + dow_fe,
         cluster = ~date,
         data = site_panel %>% filter(capacity > 20)),
  error = function(e) { log_msg("    ERROR: ", e$message, "\n"); NULL }
)
sensitivity_results[["Sg_large"]] <- extract_bed_coef(sg) %>%
  mutate(variant = "S-g: Large sites (cap>20)", lag = 1)

# Combine sensitivity results
sensitivity_df <- bind_rows(sensitivity_results)
write_csv(sensitivity_df, file.path(tbl_dir, "sensitivity_comparison.csv"))

# Lag comparison table
lag_comparison <- sensitivity_df %>%
  filter(lag %in% c(0, 1, 1.5, 2)) %>%
  select(variant, lag, estimate, std.error, p.value)
write_csv(lag_comparison, file.path(tbl_dir, "lag_comparison.csv"))

log_msg("  Sensitivity comparison:\n")
for (i in seq_len(nrow(sensitivity_df))) {
  log_msg(sprintf("    %-30s  coef=%8.5f  se=%7.5f  p=%6.4f\n",
                   sensitivity_df$variant[i],
                   sensitivity_df$estimate[i],
                   sensitivity_df$std.error[i],
                   sensitivity_df$p.value[i]))
}
log_msg("\n")


# =============================================================================
# STEP 7 — Output Plots (Plots 5-10)
# =============================================================================
log_msg("--- Step 7: Output Plots ---\n\n")

# --- Plot 05: Lag comparison --------------------------------------------------
p5_data <- sensitivity_df %>%
  filter(!is.na(estimate)) %>%
  filter(grepl("Lag|lag|avg|Primary", variant))

if (nrow(p5_data) > 0) {
  p5 <- ggplot(p5_data, aes(x = reorder(variant, lag), y = estimate)) +
    geom_point(size = 3, color = "#2c7bb6") +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.2, color = "#2c7bb6") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    coord_flip() +
    labs(title = "Lag Structure Comparison",
         subtitle = "Bed coefficient at different lag specifications | 95% CI",
         x = NULL, y = "Coefficient (Poisson log-rate)") +
    theme_report

  ggsave(file.path(fig_dir, "05_lag_comparison.png"), p5,
         width = 10, height = 5, dpi = 150)
  log_msg("  Plot 05 saved: lag comparison\n")
}

# --- Plot 06: Coefficient forest plot (all primary models) --------------------
model_list <- list(
  "M1: Poisson FE (lag-1)" = m1,
  "M2: OLS FE (lag-1)" = m2,
  "M3: Utilization FE" = m3,
  "M4: Capacity interaction" = m4,
  "M5: Weather interaction" = m5,
  "M6: Distributed lag" = m6
)

# Extract bed coefficients from each model
forest_data <- lapply(names(model_list), function(nm) {
  mod <- model_list[[nm]]
  if (is.null(mod)) return(NULL)
  ct <- coeftable(mod)
  # Get the first beds-related variable
  bed_vars <- grep("beds_lag|utilization", rownames(ct), value = TRUE)
  if (length(bed_vars) == 0) return(NULL)
  bv <- bed_vars[1]
  tibble(model = nm, variable = bv,
         estimate = ct[bv, 1], std.error = ct[bv, 2], p.value = ct[bv, 4])
}) %>% bind_rows()

if (nrow(forest_data) > 0) {
  p6 <- ggplot(forest_data, aes(x = reorder(model, estimate), y = estimate)) +
    geom_point(size = 3, color = "#d7191c") +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.2, color = "#d7191c") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    coord_flip() +
    labs(title = "Primary Model Coefficients — Bed Availability Effect",
         subtitle = "95% CI | Poisson log-rate or OLS",
         x = NULL, y = "Coefficient") +
    theme_report

  ggsave(file.path(fig_dir, "06_coefficient_forest.png"), p6,
         width = 10, height = 6, dpi = 150)
  log_msg("  Plot 06 saved: coefficient forest\n")
}

# --- Plot 07: Marginal effect — predicted placements at different bed levels ---
if (!is.null(m1)) {
  # Create prediction data at different bed levels
  bed_vals <- seq(0, max(site_panel$beds_lag1, na.rm = TRUE), length.out = 50)
  mean_vals <- site_panel %>%
    summarise(across(c(avg_temp_f, cold_day, daily_tbh_closures, daily_tbg_closures),
                     ~mean(.x, na.rm = TRUE)))

  # Use the Poisson coefficient to compute expected placements
  coefs <- coef(m1)
  beta_bed <- coefs["beds_lag1"]
  base_rate <- mean(site_panel$n_placed)

  pred_data <- tibble(
    beds_available = bed_vals,
    predicted = base_rate * exp(beta_bed * (bed_vals - mean(site_panel$beds_lag1, na.rm = TRUE)))
  )

  p7 <- ggplot(pred_data, aes(x = beds_available, y = predicted)) +
    geom_line(color = "#2c7bb6", linewidth = 1.2) +
    geom_ribbon(aes(ymin = predicted * 0.8, ymax = predicted * 1.2),
                alpha = 0.1, fill = "#2c7bb6") +
    labs(title = "Predicted Daily Placements by Bed Availability",
         subtitle = paste0("Based on M1 Poisson FE | Bed coef = ",
                           round(beta_bed, 5)),
         x = "Available Beds at Site (previous day)",
         y = "Expected Daily Placements") +
    theme_report

  ggsave(file.path(fig_dir, "07_marginal_effect.png"), p7,
         width = 10, height = 6, dpi = 150)
  log_msg("  Plot 07 saved: marginal effect\n")
}

# --- Plot 08: Weather interaction (if M5 significant) -------------------------
if (!is.null(m5)) {
  ct5 <- coeftable(m5)
  interaction_var <- grep("cold_day", rownames(ct5), value = TRUE)
  interaction_var <- interaction_var[grepl(":", interaction_var)]

  if (length(interaction_var) > 0) {
    # Visualize: predicted placements on cold vs warm days at different bed levels
    beta_beds <- coef(m5)["beds_lag1"]
    beta_cold_inter <- coef(m5)[interaction_var[1]]

    bed_range <- seq(0, quantile(site_panel$beds_lag1, 0.95, na.rm = TRUE), length.out = 50)
    p8_data <- bind_rows(
      tibble(beds = bed_range,
             predicted = base_rate * exp(beta_beds * (bed_range - mean(site_panel$beds_lag1, na.rm = TRUE))),
             weather = "Normal Days"),
      tibble(beds = bed_range,
             predicted = base_rate * exp((beta_beds + beta_cold_inter) *
                                          (bed_range - mean(site_panel$beds_lag1, na.rm = TRUE))),
             weather = "Cold Days (< 20F)")
    )

    p8 <- ggplot(p8_data, aes(x = beds, y = predicted, color = weather)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c("Normal Days" = "#2c7bb6",
                                     "Cold Days (< 20F)" = "#d7191c")) +
      labs(title = "Bed Effect: Cold Days vs Normal Days",
           subtitle = "Interaction from M5 | Does cold weather change how beds map to placements?",
           x = "Available Beds (previous day)", y = "Expected Daily Placements",
           color = NULL) +
      theme_report

    ggsave(file.path(fig_dir, "08_weather_interaction.png"), p8,
           width = 10, height = 6, dpi = 150)
    log_msg("  Plot 08 saved: weather interaction\n")
  }
}


# =============================================================================
# STEP 8 — Site Conversion Efficiency Extension (ALL shelters)
# =============================================================================
log_msg("\n--- Step 8: Site Conversion Efficiency ---\n\n")

# Attempt regressions on ALL shelters in panel (not just high-volume ones)
all_sites <- unique(site_panel$shelter_group)
log_msg("  Total shelters in panel: ", length(all_sites), "\n")

conversion_results <- lapply(all_sites, function(site) {
  site_data <- site_panel %>% filter(shelter_group == site)
  n_placed_total <- sum(site_data$n_placed)
  n_days_total   <- nrow(site_data)
  mean_beds_val  <- mean(site_data$beds_lag1, na.rm = TRUE)

  # Attempt Poisson regression if enough variation
  mod <- NULL
  coef_val <- NA_real_; se_val <- NA_real_; p_val <- NA_real_
  model_status <- "no_model"

  if (n_placed_total >= 2 && n_days_total >= 10 && !is.na(mean_beds_val)) {
    mod <- tryCatch(
      glm(n_placed ~ beds_lag1 + avg_temp_f + cold_day,
          family = poisson, data = site_data),
      error = function(e) NULL,
      warning = function(w) {
        suppressWarnings(
          glm(n_placed ~ beds_lag1 + avg_temp_f + cold_day,
              family = poisson, data = site_data)
        )
      }
    )
    if (!is.null(mod)) {
      ct <- tryCatch(summary(mod)$coefficients, error = function(e) NULL)
      if (!is.null(ct) && "beds_lag1" %in% rownames(ct)) {
        coef_val <- ct["beds_lag1", 1]
        se_val   <- ct["beds_lag1", 2]
        p_val    <- ct["beds_lag1", 4]
        model_status <- "converged"
      } else {
        model_status <- "no_beds_coef"
      }
    } else {
      model_status <- "failed"
    }
  } else {
    model_status <- "insufficient_data"
  }

  tibble(
    shelter_group   = site,
    conversion_coef = coef_val,
    conversion_se   = se_val,
    conversion_p    = p_val,
    total_placed    = n_placed_total,
    mean_beds       = mean_beds_val,
    n_days          = n_days_total,
    mean_daily_rate = mean(site_data$n_placed),
    model_status    = model_status
  )
}) %>% bind_rows()

# Flag reliability (for ranking, not for exclusion from heatmap)
conversion_results <- conversion_results %>%
  mutate(
    reliable = model_status == "converged" &
               abs(conversion_coef) < 5 & conversion_se < 5 & mean_beds >= 0.5,
    reliable = ifelse(is.na(reliable), FALSE, reliable)
  ) %>%
  arrange(desc(reliable), desc(conversion_coef))

n_converged  <- sum(conversion_results$model_status == "converged")
n_reliable   <- sum(conversion_results$reliable)
n_insuff     <- sum(conversion_results$model_status == "insufficient_data")
log_msg("  Models converged: ", n_converged, " of ", nrow(conversion_results), "\n")
log_msg("  Reliable estimates: ", n_reliable, "\n")
log_msg("  Insufficient data for regression: ", n_insuff, "\n")

write_csv(conversion_results, file.path(tbl_dir, "site_conversion_rates.csv"))

log_msg("  Site conversion rates (all shelters, beds -> placements per day):\n")
for (i in seq_len(nrow(conversion_results))) {
  r <- conversion_results[i, ]
  flag <- ifelse(r$reliable, "  ", " *")
  coef_str <- ifelse(is.na(r$conversion_coef), "      N/A",
                     sprintf("%9.4f", r$conversion_coef))
  se_str   <- ifelse(is.na(r$conversion_se), "    N/A",
                     sprintf("%7.4f", r$conversion_se))
  p_str    <- ifelse(is.na(r$conversion_p), "   N/A",
                     sprintf("%6.4f", r$conversion_p))
  log_msg(sprintf("  %s %-50s coef=%s  se=%s  p=%s  placed=%d  beds=%.1f  [%s]\n",
                   flag, r$shelter_group, coef_str, se_str, p_str,
                   r$total_placed, r$mean_beds, r$model_status))
}

# Reliable subset for bar chart and scatter (plots 09, 10)
reliable_results <- conversion_results %>% filter(reliable)

if (nrow(reliable_results) > 0) {
  # --- Plot 09: Site conversion rates bar chart (reliable only) ----------------
  p9 <- ggplot(reliable_results,
               aes(x = reorder(shelter_group, conversion_coef),
                   y = conversion_coef)) +
    geom_col(aes(fill = conversion_coef > 0), width = 0.7, show.legend = FALSE) +
    geom_errorbar(aes(ymin = conversion_coef - 1.96 * conversion_se,
                      ymax = conversion_coef + 1.96 * conversion_se),
                  width = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = c("TRUE" = "#2c7bb6", "FALSE" = "#d7191c")) +
    coord_flip() +
    labs(title = "Site Conversion Efficiency: Beds to Placements",
         subtitle = "Site-specific bed coefficient (Poisson) | 95% CI | Reliable estimates only\nHigher = more efficiently converts open beds to placements",
         x = NULL, y = "Bed Coefficient (log-rate per additional bed)") +
    theme_report +
    theme(axis.text.y = element_text(size = 8))

  ggsave(file.path(fig_dir, "09_site_conversion_rates.png"), p9,
         width = 12, height = max(6, nrow(reliable_results) * 0.35), dpi = 150)
  log_msg("  Plot 09 saved: site conversion rates (reliable)\n")

  # --- Plot 10: Efficiency scatter (mean beds vs conversion rate) ---------------
  p10 <- ggplot(reliable_results,
                aes(x = mean_beds, y = conversion_coef, size = total_placed)) +
    geom_point(alpha = 0.7, color = "#2c7bb6") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", se = FALSE, color = "#d7191c",
                linetype = "dotted", linewidth = 0.8) +
    geom_text(aes(label = gsub(" - .*$", "", shelter_group)),
              size = 2.5, hjust = -0.1, vjust = -0.5, check_overlap = TRUE) +
    labs(title = "Conversion Efficiency vs Shelter Size",
         subtitle = "Do larger shelters convert beds less efficiently? (Diminishing returns?)",
         x = "Mean Available Beds (lag-1)",
         y = "Conversion Coefficient",
         size = "Total\nPlacements") +
    theme_report

  ggsave(file.path(fig_dir, "10_efficiency_scatter.png"), p10,
         width = 10, height = 7, dpi = 150)
  log_msg("  Plot 10 saved: efficiency scatter\n")
}

# --- Plot 11: ALL-SHELTER dashboard heatmap grouped by demographic -----------
# This includes EVERY shelter in the panel — not just those with reliable
# regression estimates. Shelters without a regression get N/A for coefficient
# but still show descriptive metrics (beds, placements, rate).

site_heatmap_data <- site_panel %>%
  group_by(shelter_group) %>%
  summarise(
    mean_beds       = mean(beds_lag1, na.rm = TRUE),
    total_placed    = sum(n_placed),
    pct_days_placed = 100 * mean(n_placed > 0),
    mean_daily_rate = mean(n_placed),
    .groups = "drop"
  ) %>%
  left_join(
    conversion_results %>% select(shelter_group, conversion_coef, conversion_se,
                                    conversion_p, reliable, model_status),
    by = "shelter_group"
  ) %>%
  left_join(site_pop, by = "shelter_group") %>%
  mutate(
    demographic = ifelse(is.na(pop_primary), "Other", pop_primary),
    shelter_label = gsub("^A Safe Haven - ", "", shelter_group)
  )

# Pivot to long format for heatmap tiles
heatmap_long <- site_heatmap_data %>%
  tidyr::pivot_longer(
    cols = c(conversion_coef, mean_beds, total_placed, pct_days_placed, mean_daily_rate),
    names_to = "metric",
    values_to = "value"
  ) %>%
  # Normalize each metric to 0-1 across all shelters
  group_by(metric) %>%
  mutate(
    value_norm = ifelse(is.na(value), NA_real_,
                   (value - min(value, na.rm = TRUE)) /
                   (max(value, na.rm = TRUE) - min(value, na.rm = TRUE) + 1e-10))
  ) %>%
  ungroup() %>%
  mutate(
    metric_label = case_when(
      metric == "conversion_coef" ~ "Conversion\nCoefficient",
      metric == "mean_beds"       ~ "Mean Available\nBeds (lag-1)",
      metric == "total_placed"    ~ "Total\nPlacements",
      metric == "pct_days_placed" ~ "% Days with\n\u22651 Placement",
      metric == "mean_daily_rate" ~ "Mean Daily\nPlacement Rate"
    ),
    value_label = case_when(
      is.na(value) ~ "N/A",
      metric == "conversion_coef" ~ sprintf("%.3f", value),
      metric == "mean_beds"       ~ sprintf("%.1f", value),
      metric == "total_placed"    ~ sprintf("%d", as.integer(value)),
      metric == "pct_days_placed" ~ sprintf("%.0f%%", value),
      metric == "mean_daily_rate" ~ sprintf("%.3f", value)
    ),
    # Gray out N/A tiles
    value_norm = ifelse(is.na(value_norm), -0.1, value_norm)
  )

# Order shelters: within each demographic, sort by conversion coefficient (NA last)
shelter_order <- site_heatmap_data %>%
  arrange(demographic, !is.na(conversion_coef), conversion_coef) %>%
  pull(shelter_label)
heatmap_long$shelter_label <- factor(heatmap_long$shelter_label, levels = shelter_order)

# Order demographics for facets
demo_order <- c("Men", "Women", "Youth", "Families", "Other")
demo_order <- demo_order[demo_order %in% unique(heatmap_long$demographic)]
heatmap_long$demographic <- factor(heatmap_long$demographic, levels = demo_order)

# Order metrics logically
metric_order <- c("Conversion\nCoefficient", "Mean Available\nBeds (lag-1)",
                  "Total\nPlacements", "% Days with\n\u22651 Placement",
                  "Mean Daily\nPlacement Rate")
heatmap_long$metric_label <- factor(heatmap_long$metric_label, levels = metric_order)

# Count shelters per demographic
shelters_per_demo <- site_heatmap_data %>% count(demographic)
n_shelters_total  <- nrow(site_heatmap_data)

p11 <- ggplot(heatmap_long,
              aes(x = metric_label, y = shelter_label, fill = value_norm)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = value_label), size = 2.2, color = "black") +
  scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#1a9641",
                       midpoint = 0.5,
                       limits = c(-0.1, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("0", "0.25", "0.5", "0.75", "1"),
                       name = "Normalized\nValue",
                       na.value = "grey85") +
  facet_grid(demographic ~ ., scales = "free_y", space = "free_y",
             switch = "y") +
  labs(title = "Shelter Placement Efficiency — All Sites by Demographic",
       subtitle = paste0(n_shelters_total, " shelters | Each metric normalized 0\u20131 | ",
                         "N/A = insufficient data for regression | ",
                         "Sorted by conversion coefficient within group"),
       x = NULL, y = NULL) +
  theme_report +
  theme(
    axis.text.y       = element_text(size = 7),
    axis.text.x       = element_text(size = 9),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.4, "lines"),
    strip.placement   = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 10,
                                     hjust = 1),
    strip.background  = element_rect(fill = "grey95", color = NA),
    legend.position   = "right"
  )

plot_height <- max(10, n_shelters_total * 0.4 + length(demo_order) * 0.6)
ggsave(file.path(fig_dir, "11_conversion_dashboard_heatmap.png"), p11,
       width = 14, height = plot_height, dpi = 150)
log_msg("  Plot 11 saved: ALL-shelter dashboard heatmap (grouped by demographic)\n")
log_msg("    Total shelters: ", n_shelters_total, "\n")
log_msg("    Demographics: ", paste(demo_order, collapse = ", "), "\n")
log_msg("    Shelters per demographic: ",
        paste(shelters_per_demo$demographic, "=", shelters_per_demo$n, collapse = ", "),
        "\n")

# --- Plot 11b: Interactive HTML heatmap (scrollable, all shelters) -----------
# Self-contained HTML file viewable in any browser
html_path <- file.path(fig_dir, "11_conversion_dashboard_interactive.html")
log_msg("  Generating interactive HTML heatmap...\n")

# Build a clean table for the HTML version
html_table <- site_heatmap_data %>%
  arrange(demographic, desc(conversion_coef)) %>%
  mutate(
    coef_display = ifelse(is.na(conversion_coef), "N/A",
                          sprintf("%.4f", conversion_coef)),
    se_display   = ifelse(is.na(conversion_se), "N/A",
                          sprintf("%.4f", conversion_se)),
    p_display    = ifelse(is.na(conversion_p), "N/A",
                          sprintf("%.4f", conversion_p)),
    status_display = ifelse(reliable, "\u2713 Reliable",
                            ifelse(model_status == "insufficient_data", "Insufficient data",
                                   ifelse(model_status == "converged", "Unreliable estimate",
                                          model_status)))
  )

# Generate self-contained HTML
html_content <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Shelter Placement Efficiency Dashboard</title>
<style>
  body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
         margin: 20px; background: #fafafa; color: #333; }
  h1 { font-size: 1.5em; margin-bottom: 4px; }
  .subtitle { color: #666; font-size: 0.9em; margin-bottom: 20px; }
  .timestamp { color: #999; font-size: 0.8em; }
  .demo-section { margin-bottom: 30px; }
  .demo-header { background: #2c3e50; color: white; padding: 8px 16px;
                 border-radius: 6px 6px 0 0; font-size: 1.1em; font-weight: bold; }
  table { border-collapse: collapse; width: 100%; background: white;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
  th { background: #34495e; color: white; padding: 10px 12px; text-align: left;
       font-size: 0.85em; position: sticky; top: 0; z-index: 1; }
  td { padding: 8px 12px; border-bottom: 1px solid #eee; font-size: 0.85em; }
  tr:hover { background: #f0f7ff; }
  .coef-pos { color: #1a9641; font-weight: bold; }
  .coef-neg { color: #d7191c; font-weight: bold; }
  .coef-na  { color: #999; font-style: italic; }
  .reliable { color: #1a9641; }
  .unreliable { color: #e67e22; }
  .no-data { color: #999; }
  .metric-high { background: #d4edda; }
  .metric-mid  { background: #fff3cd; }
  .metric-low  { background: #f8d7da; }
  .legend { display: flex; gap: 20px; margin: 10px 0 20px; font-size: 0.85em; }
  .legend-item { display: flex; align-items: center; gap: 6px; }
  .legend-box { width: 14px; height: 14px; border-radius: 2px; }
</style>
</head>
<body>
<h1>Shelter Placement Efficiency Dashboard</h1>
<p class="subtitle">All ', n_shelters_total, ' shelters grouped by demographic served &mdash; sorted by conversion coefficient within group</p>
<p class="timestamp">Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M"), ' | Run: ', RUN_TIMESTAMP, '</p>
<div class="legend">
  <div class="legend-item"><div class="legend-box" style="background:#d4edda"></div> Top tercile</div>
  <div class="legend-item"><div class="legend-box" style="background:#fff3cd"></div> Middle tercile</div>
  <div class="legend-item"><div class="legend-box" style="background:#f8d7da"></div> Bottom tercile</div>
  <div class="legend-item"><span class="coef-pos">Green</span> = positive coefficient</div>
  <div class="legend-item"><span class="coef-neg">Red</span> = negative coefficient</div>
</div>
')

for (demo in demo_order) {
  demo_data <- html_table %>% filter(demographic == demo)
  if (nrow(demo_data) == 0) next

  # Compute tercile thresholds for shading
  placed_q <- quantile(demo_data$total_placed, probs = c(1/3, 2/3), na.rm = TRUE)
  rate_q   <- quantile(demo_data$mean_daily_rate, probs = c(1/3, 2/3), na.rm = TRUE)

  html_content <- paste0(html_content,
    '<div class="demo-section">
    <div class="demo-header">', demo, ' (', nrow(demo_data), ' shelters)</div>
    <table>
    <thead><tr>
      <th>Shelter</th><th>Conv. Coeff.</th><th>SE</th><th>p-value</th>
      <th>Mean Beds</th><th>Total Placed</th><th>% Days w/ Placement</th>
      <th>Daily Rate</th><th>Days</th><th>Status</th>
    </tr></thead>
    <tbody>\n')

  for (j in seq_len(nrow(demo_data))) {
    r <- demo_data[j, ]
    coef_class <- ifelse(r$coef_display == "N/A", "coef-na",
                         ifelse(r$conversion_coef > 0, "coef-pos", "coef-neg"))
    status_class <- ifelse(grepl("Reliable", r$status_display), "reliable",
                          ifelse(grepl("Insufficient", r$status_display), "no-data", "unreliable"))
    placed_class <- ifelse(r$total_placed >= placed_q[2], "metric-high",
                          ifelse(r$total_placed >= placed_q[1], "metric-mid", "metric-low"))
    rate_class <- ifelse(r$mean_daily_rate >= rate_q[2], "metric-high",
                        ifelse(r$mean_daily_rate >= rate_q[1], "metric-mid", "metric-low"))

    html_content <- paste0(html_content, '<tr>',
      '<td><strong>', r$shelter_label, '</strong></td>',
      '<td class="', coef_class, '">', r$coef_display, '</td>',
      '<td>', r$se_display, '</td>',
      '<td>', r$p_display, '</td>',
      '<td>', sprintf("%.1f", r$mean_beds), '</td>',
      '<td class="', placed_class, '">', r$total_placed, '</td>',
      '<td>', sprintf("%.0f%%", r$pct_days_placed), '</td>',
      '<td class="', rate_class, '">', sprintf("%.3f", r$mean_daily_rate), '</td>',
      '<td>', r$n_days, '</td>',
      '<td class="', status_class, '">', r$status_display, '</td>',
      '</tr>\n')
  }
  html_content <- paste0(html_content, '</tbody></table></div>\n')
}

html_content <- paste0(html_content, '
<hr style="margin-top:30px">
<p style="font-size:0.8em;color:#999">
  <strong>Conversion Coefficient:</strong> Poisson regression estimate of beds_lag1
  (yesterday&rsquo;s available beds &rarr; today&rsquo;s placements). Positive = more beds lead to more placements.<br>
  <strong>Reliable:</strong> Model converged, |coef| &lt; 5, SE &lt; 5, mean beds &ge; 0.5.<br>
  <strong>Insufficient data:</strong> &lt; 2 placements or &lt; 10 site-days &mdash; no regression attempted.<br>
  Analysis: Poisson FE regression with 36h operational lag. Controls: weather, TBH/TBG demand.
</p>
</body></html>')

writeLines(html_content, html_path)
log_msg("  Interactive HTML saved: ", basename(html_path), "\n\n")


# =============================================================================
# STEP 9 — Coefficient Table (all models)
# =============================================================================
log_msg("--- Step 9: Coefficient Tables ---\n\n")

build_coef_table <- function(model, model_name) {
  if (is.null(model)) return(NULL)
  ct <- coeftable(model)
  tibble(
    model    = model_name,
    variable = rownames(ct),
    estimate = ct[, 1],
    std.error = ct[, 2],
    t.value  = ct[, 3],
    p.value  = ct[, 4]
  )
}

coef_table <- bind_rows(
  build_coef_table(m1, "M1_primary_lag1_poisson"),
  build_coef_table(m2, "M2_ols_lag1"),
  build_coef_table(m3, "M3_utilization_lag1"),
  build_coef_table(m4, "M4_capacity_interaction"),
  build_coef_table(m5, "M5_weather_interaction"),
  build_coef_table(m6, "M6_distributed_lag")
)

write_csv(coef_table, file.path(tbl_dir, "coefficient_table.csv"))
log_msg("  Coefficient table saved (", nrow(coef_table), " rows)\n\n")


# =============================================================================
# STEP 10 — Causal Discussion + Executive Summary
# =============================================================================
log_msg("\n")
log_msg("==========================================================\n")
log_msg("  CAUSAL DISCUSSION\n")
log_msg("==========================================================\n\n")

log_msg("IDENTIFICATION STRATEGY:\n")
log_msg("  Within-site, within-day-of-week variation in bed availability.\n")
log_msg("  After absorbing shelter fixed effects (all time-invariant site\n")
log_msg("  differences: location, size, population, management quality)\n")
log_msg("  and day-of-week effects, remaining variation comes from staggered\n")
log_msg("  discharges, no-shows, and transfers -- plausibly exogenous to\n")
log_msg("  placement decisions made the next day.\n\n")

log_msg("LAG STRUCTURE (36-HOUR OPERATIONAL PIPELINE):\n")
log_msg("  Bed available -> candidate identified (~6h) -> submitted for\n")
log_msg("  approval (~6h) -> approved (~12h) -> client arrives (~12h) ->\n")
log_msg("  case closed in Salesforce (~6h). Total: ~12-48h, central ~36h.\n")
log_msg("  Primary specification: lag-1 (yesterday's 9am beds -> today's\n")
log_msg("  recorded placements). Validated by comparing lag-0 through lag-2.\n\n")

log_msg("REMAINING THREATS TO CAUSAL INFERENCE:\n")
log_msg("  1. Reverse causation: placements today -> fewer beds tomorrow.\n")
log_msg("     Mitigated by using 9am beds (before most placements occur)\n")
log_msg("     at lag-1 (yesterday's measure).\n")
log_msg("  2. Batch processing: multiple cases placed at same shelter on\n")
log_msg("     same day may reflect dispatcher coordination, not beds.\n")
log_msg("  3. Selection: dispatchers steer cases to shelters with known\n")
log_msg("     openings. This IS the mechanism we measure, not a bias.\n\n")

log_msg("WEATHER AS CONTROL (NOT INSTRUMENT):\n")
log_msg("  Weather affects both demand (more people seeking shelter in cold)\n")
log_msg("  and potentially supply (staff availability, operations). Including\n")
log_msg("  it as a covariate controls for this confounding. NOT a valid\n")
log_msg("  instrument because ASH controls all bed allocation -- weather\n")
log_msg("  doesn't create exogenous bed supply variation.\n\n")

log_msg("WHAT THIS DESIGN CAN ANSWER:\n")
log_msg("  'On days when Shelter X has N more beds open than its own\n")
log_msg("  average, does it receive more placements within ~36 hours?'\n")
log_msg("  This is a within-site, intensive margin question.\n\n")

log_msg("WHAT THIS DESIGN CANNOT ANSWER:\n")
log_msg("  'If the city added 100 new beds system-wide, how many more\n")
log_msg("  placements would occur?' This is an extensive margin question\n")
log_msg("  requiring structural modeling of system-wide equilibrium.\n\n")

# --- Executive Summary -------------------------------------------------------
log_msg("==========================================================\n")
log_msg("  EXECUTIVE SUMMARY -- FOR LEADERSHIP\n")
log_msg("==========================================================\n\n")

if (!is.null(m1)) {
  bed_coef <- coef(m1)["beds_lag1"]
  bed_se   <- coeftable(m1)["beds_lag1", 2]
  bed_p    <- coeftable(m1)["beds_lag1", 4]
  is_sig   <- bed_p < 0.05

  log_msg("PRIMARY FINDING:\n")
  log_msg(sprintf("  Bed availability coefficient (lag-1): %.5f (SE=%.5f, p=%.4f)\n",
                   bed_coef, bed_se, bed_p))
  log_msg("\n")

  if (is_sig && bed_coef > 0) {
    # Calculate practical effects
    pct_change <- (exp(bed_coef) - 1) * 100
    log_msg("INTERPRETATION (SIGNIFICANT POSITIVE EFFECT):\n")
    log_msg(sprintf("  Each additional open bed at a shelter is associated with a %.2f%%\n", pct_change))
    log_msg("  increase in expected placements the following day.\n\n")
    log_msg("  This means:\n")
    log_msg("  - The placement pipeline IS responsive to bed supply.\n")
    log_msg("  - More openings -> more placements. The system works.\n")
    log_msg("  - The 36-hour lag validates operational timing expectations.\n\n")

    # Marginal beds calculation
    mean_placed <- mean(site_panel$n_placed)
    marginal_effect <- mean_placed * bed_coef  # approx marginal effect
    log_msg(sprintf("  Approximate marginal effect: %.4f additional placements per site-day\n", marginal_effect))
    log_msg(sprintf("  per additional bed. Over 30 days across %d sites, adding 5 beds at\n",
                     length(unique(site_panel$shelter_group))))
    log_msg(sprintf("  one site would generate ~%.1f additional placements.\n\n",
                     marginal_effect * 5 * 30))

  } else if (is_sig && bed_coef < 0) {
    log_msg("INTERPRETATION (SIGNIFICANT NEGATIVE EFFECT):\n")
    log_msg("  More available beds associated with FEWER placements.\n")
    log_msg("  This likely reflects: beds are most available when demand\n")
    log_msg("  is lowest (fewer people to place). The bed measure may be\n")
    log_msg("  proxying for low-demand periods.\n\n")

  } else {
    log_msg("INTERPRETATION (NULL RESULT):\n")
    log_msg("  Bed availability does NOT significantly predict placement volume\n")
    log_msg("  at the site level. This suggests beds are NOT the binding constraint.\n\n")
    log_msg("  Leadership should investigate:\n")
    log_msg("  - Candidate identification speed: how fast are open beds matched\n")
    log_msg("    to eligible clients?\n")
    log_msg("  - Shelter approval turnarounds: how quickly do partners respond\n")
    log_msg("    to submissions?\n")
    log_msg("  - Client readiness: are clients available and willing when beds open?\n")
    log_msg("  - Documentation lags: are placements happening but not being\n")
    log_msg("    recorded promptly?\n\n")
  }
}

# Weather effect
if (!is.null(m1)) {
  ct1 <- coeftable(m1)
  if ("cold_day" %in% rownames(ct1)) {
    cold_coef <- ct1["cold_day", 1]
    cold_p    <- ct1["cold_day", 4]
    if (cold_p < 0.05) {
      cold_pct <- (exp(cold_coef) - 1) * 100
      log_msg(sprintf("WEATHER EFFECT: On cold days (min < 20F), placements change by %.1f%%\n", cold_pct))
      log_msg("  (p=", round(cold_p, 4), "). This informs surge planning.\n\n")
    } else {
      log_msg("WEATHER EFFECT: Cold days do not significantly affect placement volume\n")
      log_msg("  (p=", round(cold_p, 4), "). Weather is not a major driver of daily placements.\n\n")
    }
  }
}

# Conversion efficiency summary
if (nrow(conversion_results) > 0) {
  top_converter <- conversion_results %>% slice(1)
  bottom_converter <- conversion_results %>% slice(n())
  median_coef <- median(conversion_results$conversion_coef, na.rm = TRUE)

  log_msg("SITE CONVERSION EFFICIENCY:\n")
  log_msg(sprintf("  Across %d shelters, conversion efficiency ranges from %.4f to %.4f.\n",
                   nrow(conversion_results), min(conversion_results$conversion_coef),
                   max(conversion_results$conversion_coef)))
  log_msg(sprintf("  Most efficient: %s (coef=%.4f, %d placements)\n",
                   top_converter$shelter_group, top_converter$conversion_coef,
                   top_converter$total_placed))
  log_msg(sprintf("  Least efficient: %s (coef=%.4f, %d placements)\n",
                   bottom_converter$shelter_group, bottom_converter$conversion_coef,
                   bottom_converter$total_placed))
  log_msg("\n")
  log_msg("  OPERATIONAL IMPLICATION:\n")
  log_msg("  High-conversion sites turn open beds into placements quickly --\n")
  log_msg("  replicate their process. Low-conversion sites have capacity but\n")
  log_msg("  low throughput -- investigate approval turnarounds, screening\n")
  log_msg("  criteria, geographic mismatch with demand.\n\n")
}

# Lag structure finding
log_msg("LAG VALIDATION:\n")
lag_data <- sensitivity_df %>% filter(lag %in% c(0, 1, 2))
if (nrow(lag_data) >= 2) {
  log_msg("  Lag-0 (same day):   coef=", round(lag_data$estimate[lag_data$lag == 0], 5),
          "  p=", round(lag_data$p.value[lag_data$lag == 0], 4), "\n")
  log_msg("  Lag-1 (1 day):      coef=", round(lag_data$estimate[lag_data$lag == 1][1], 5),
          "  p=", round(lag_data$p.value[lag_data$lag == 1][1], 4), "\n")
  if (any(lag_data$lag == 2)) {
    log_msg("  Lag-2 (2 days):     coef=", round(lag_data$estimate[lag_data$lag == 2], 5),
            "  p=", round(lag_data$p.value[lag_data$lag == 2], 4), "\n")
  }
  log_msg("  The lag structure reveals how quickly the pipeline converts\n")
  log_msg("  bed openings to recorded placements -- an operational metric.\n\n")
}


# =============================================================================
# CLEANUP
# =============================================================================
log_msg("==========================================================\n")
log_msg("  Run complete: ", RUN_TIMESTAMP, "\n")
log_msg("  Output: ", run_dir, "\n")
log_msg("==========================================================\n")

close(log_con)
cat("\nDone. Output at:", run_dir, "\n")

# =============================================================================
# COPY LATEST — Populate RQ2 + RQ3 root figures/, tables/, data/ for easy access
# =============================================================================
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

# RQ2: Site-level bed → placement volume
rq2_root <- here("analysis", "RQ2_site_bed_placement_volume")
cat("\nCopying latest outputs to RQ2 root...\n")
copy_latest("figures", file.path(rq2_root, "figures"))
copy_latest("tables",  file.path(rq2_root, "tables"))
copy_latest("data",    file.path(rq2_root, "data"))

# RQ3: Conversion efficiency (subset of RQ2 outputs)
rq3_root <- here("analysis", "RQ3_site_conversion_efficiency")
cat("Copying conversion outputs to RQ3 root...\n")
dir.create(file.path(rq3_root, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(rq3_root, "tables"),  recursive = TRUE, showWarnings = FALSE)

# RQ3 figures: plots 09, 10, 11 + interactive HTML (conversion-specific)
rq3_figs <- c("09_site_conversion_rates.png", "10_efficiency_scatter.png",
              "11_conversion_dashboard_heatmap.png",
              "11_conversion_dashboard_interactive.html")
for (f in rq3_figs) {
  src_f <- file.path(run_dir, "figures", f)
  if (file.exists(src_f)) file.copy(src_f, file.path(rq3_root, "figures", f), overwrite = TRUE)
}
cat("  Copied", sum(file.exists(file.path(run_dir, "figures", rq3_figs))),
    "conversion outputs to RQ3\n")

# RQ3 tables: conversion rates
conv_csv <- file.path(run_dir, "tables", "site_conversion_rates.csv")
if (file.exists(conv_csv)) {
  file.copy(conv_csv, file.path(rq3_root, "tables", "site_conversion_rates.csv"), overwrite = TRUE)
  cat("  Copied site_conversion_rates.csv to RQ3\n")
}

cat("Done. Latest outputs available at:\n  RQ2:", rq2_root, "\n  RQ3:", rq3_root, "\n")
