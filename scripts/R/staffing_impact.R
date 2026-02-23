# =============================================================================
# staffing_impact.R — Staffing Levels → Case Closure Statistical Analysis
# =============================================================================
# Analyzes how field staffing (intake pairs, dispatchers, supervisors) impacts
# case closure volume, with special focus on TBH (Senior Well-Being) and TBG
# (Crisis Referral) cases and their knock-on effect on shelter (TBC) outcomes.
#
# KEY DESIGN: Links each closure to its shift via End Date timestamp, then
# computes a 24-hour staffing window (closure shift + 2 prior shifts) to
# match the SWB/crisis 24-hour closure goal.
#
# SHIFT SCHEDULE:  AM = 7am-3pm | PM = 3pm-11pm | OVN = 11pm-7am
#
# FIELD ROLES ONLY: Intake Drivers (paired), Dispatchers, Shift Supervisors
#   (BH Clinicians and Data Specialists excluded — program-level, not field ops)
#
# CAVEATS:
#   - n=38 days / ~114 shifts is small; results are exploratory
#   - Created Date lacks timestamps — we use closure shift as anchor
#   - Staffing = scheduled minus call-offs, not actual hours worked
#   - Supervisor count is constant (1/shift) — cannot enter regression models
#   - Correlation ≠ causation; batch processing spikes may inflate counts
#   - Intake pairing computed per shift; intra-day reassignment not captured
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

# Project root — walk up from this script's directory to find .git
PROJECT_ROOT <- normalizePath(file.path(dirname(
  if (interactive()) rstudioapi::getSourceEditorContext()$path
  else commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))] |>
    sub("--file=", "", x = _)
), ".."), winslash = "/")
here <- function(...) file.path(PROJECT_ROOT, ...)

fig_dir <- here("analysis", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

theme_report <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom"
  )

# Colors for case types
type_colors <- c(TBC = "#2c7bb6", TBH = "#d7191c", TBG = "#fdae61")

cat("\n==========================================================\n")
cat("  Staffing Impact on Case Closures (Field Ops Focus)\n")
cat("  Analysis Period: Jan 10 – Feb 16, 2026\n")
cat("  Shift Schedule: AM 7a-3p | PM 3p-11p | OVN 11p-7a\n")
cat("==========================================================\n\n")


# =============================================================================
# STEP 1 — Data Preparation
# =============================================================================
cat("--- Step 1: Data Preparation ---\n\n")

# --- 1a: Staffing data (shift-level) -----------------------------------------
staffing_path <- here("data", "inputs", "staffing_log", "clean",
                      "staffing_daily_levels_2026.csv")
staffing_raw <- read_csv(staffing_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# Field roles only
field_roles <- c("Intake Driver", "Dispatcher", "Shift Supervisor")

# Shift-level staffing: one row per (date, shift_code)
shift_staffing <- staffing_raw %>%
  filter(role_clean %in% field_roles) %>%
  group_by(date, shift_code, role_clean) %>%
  summarise(staff = sum(scheduled_staff), .groups = "drop") %>%
  pivot_wider(names_from = role_clean, values_from = staff, values_fill = 0)

names(shift_staffing) <- gsub(" ", "_", tolower(names(shift_staffing)))

# Intake pairing per shift
shift_staffing <- shift_staffing %>%
  mutate(
    intake_field_pairs  = floor(intake_driver / 2),
    intake_benched      = intake_driver %% 2,
    field_staff         = intake_driver + dispatcher + shift_supervisor
  )

# --- 1b: Build shift sequence with chronological ordering --------------------
# Shift order: AM=0, PM=1, OVN=2 within each date
shift_order_map <- c(AM = 0L, PM = 1L, OVN = 2L)

shift_staffing <- shift_staffing %>%
  mutate(
    shift_ord = shift_order_map[shift_code],
    # Numeric index for lag/lead operations
    shift_idx = as.integer(date - min(date)) * 3L + shift_ord
  ) %>%
  arrange(shift_idx)

# 24-hour window: current shift + 2 prior shifts (= 3 consecutive shifts)
shift_staffing <- shift_staffing %>%
  mutate(
    # Lag-1 and lag-2 values for each staffing variable
    pairs_lag1 = lag(intake_field_pairs, 1, default = NA_real_),
    pairs_lag2 = lag(intake_field_pairs, 2, default = NA_real_),
    disp_lag1  = lag(dispatcher, 1, default = NA_real_),
    disp_lag2  = lag(dispatcher, 2, default = NA_real_),
    field_lag1 = lag(field_staff, 1, default = NA_real_),
    field_lag2 = lag(field_staff, 2, default = NA_real_),
    # 24-hour window sums (current + 2 prior)
    pairs_24h  = intake_field_pairs + pairs_lag1 + pairs_lag2,
    disp_24h   = dispatcher + disp_lag1 + disp_lag2,
    field_24h  = field_staff + field_lag1 + field_lag2
  )

cat("  Shift-level staffing:", nrow(shift_staffing), "shift-days loaded\n")

# --- 1c: Daily aggregates (for daily-level models) ---------------------------
daily_staffing <- shift_staffing %>%
  group_by(date) %>%
  summarise(
    intake_field_pairs = sum(intake_field_pairs),
    dispatcher         = sum(dispatcher),
    shift_supervisor   = sum(shift_supervisor),
    field_staff        = sum(field_staff),
    intake_driver      = sum(intake_driver),
    .groups = "drop"
  )

cat("  Daily staffing:", nrow(daily_staffing), "days\n")

# --- 1d: Case closure data ---------------------------------------------------
sf_path <- "C:/Users/sagea/Desktop/A Safe Haven/Data/Raw Base Exports/SR_FQ_Y2026.csv"
sf_raw <- read_csv(sf_path, show_col_types = FALSE)

# Deduplicate to one row per work order, parse closure datetime
cases <- sf_raw %>%
  distinct(`Work Order Number`, .keep_all = TRUE) %>%
  filter(Status == "Closed", !is.na(`End Date`)) %>%
  mutate(
    closure_dt   = mdy_hm(`End Date`),
    closure_date = as.Date(closure_dt),
    closure_hour = hour(closure_dt),
    case_type    = `Work Order: Type Short Code`
  ) %>%
  filter(!is.na(closure_date))

# Map closure to shift + staffing date
# OVN shift spans midnight: closures 11pm-midnight → OVN of that date
#                           closures midnight-7am   → OVN of PREVIOUS date
cases <- cases %>%
  mutate(
    closure_shift = case_when(
      closure_hour >= 7  & closure_hour < 15 ~ "AM",
      closure_hour >= 15 & closure_hour < 23 ~ "PM",
      TRUE                                   ~ "OVN"
    ),
    # For OVN closures between midnight and 7am, the staffing date is yesterday
    staffing_date = case_when(
      closure_shift == "OVN" & closure_hour < 7 ~ closure_date - 1L,
      TRUE                                      ~ closure_date
    )
  )

cat("  Closures data:", nrow(cases), "closed cases\n")
cat("  Case type breakdown:\n")
print(table(cases$case_type))
cat("\n")

# --- 1e: Shift-level closure counts ------------------------------------------
# Count closures per shift by case type
shift_closures <- cases %>%
  filter(case_type %in% c("TBC", "TBH", "TBG")) %>%
  count(staffing_date, closure_shift, case_type) %>%
  pivot_wider(names_from = case_type, values_from = n, values_fill = 0)

shift_closures_total <- cases %>%
  count(staffing_date, closure_shift, name = "closures_total")

shift_closures <- shift_closures_total %>%
  left_join(shift_closures, by = c("staffing_date", "closure_shift")) %>%
  mutate(across(c(TBC, TBH, TBG), ~replace_na(.x, 0)))

# --- 1f: Join shift-level staffing + closures ---------------------------------
sf <- shift_staffing %>%
  left_join(shift_closures,
            by = c("date" = "staffing_date", "shift_code" = "closure_shift")) %>%
  mutate(
    across(c(closures_total, TBC, TBH, TBG), ~replace_na(.x, 0)),
    weekend = as.integer(wday(date) %in% c(1, 7)),
    shift_factor = factor(shift_code, levels = c("AM", "PM", "OVN"))
  )

# Restrict to overlap period where both staffing and closures exist
overlap_dates <- sf %>%
  filter(closures_total > 0) %>%
  pull(date) %>%
  range()

sf <- sf %>%
  filter(date >= overlap_dates[1], date <= overlap_dates[2])

cat("  Shift-level joined:", nrow(sf), "shifts in overlap period\n")
cat("  Overlap:", as.character(overlap_dates[1]), "to",
    as.character(overlap_dates[2]), "\n")

# Drop shifts missing 24h window data (first 2 shifts of the series)
sf_24h <- sf %>% filter(!is.na(pairs_24h))
cat("  Shifts with complete 24h window:", nrow(sf_24h), "\n")

# --- 1g: Daily-level dataset --------------------------------------------------
daily_closures <- cases %>%
  filter(case_type %in% c("TBC", "TBH", "TBG")) %>%
  count(closure_date, case_type) %>%
  pivot_wider(names_from = case_type, values_from = n,
              values_fill = 0, names_prefix = "closures_")

daily_closures_total <- cases %>%
  count(closure_date, name = "closures_total")

daily_closures <- daily_closures_total %>%
  left_join(daily_closures, by = "closure_date") %>%
  mutate(across(starts_with("closures_"), ~replace_na(.x, 0)))

df <- daily_staffing %>%
  inner_join(daily_closures, by = c("date" = "closure_date")) %>%
  mutate(weekend = as.integer(wday(date) %in% c(1, 7)))

cat("  Daily-level joined:", nrow(df), "days\n\n")


# =============================================================================
# STEP 2 — Descriptive Summary
# =============================================================================
cat("--- Step 2: Descriptive Summary ---\n\n")

# Helper for summary stats
desc_table <- function(data, vars, label) {
  data %>%
    summarise(across(all_of(vars),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd   = ~sd(.x, na.rm = TRUE),
                          min  = ~min(.x, na.rm = TRUE),
                          max  = ~max(.x, na.rm = TRUE)),
                     .names = "{.col}__{.fn}")) %>%
    pivot_longer(everything(),
                 names_to = c("variable", "stat"),
                 names_sep = "__") %>%
    pivot_wider(names_from = stat, values_from = value)
}

# Daily-level stats
daily_vars <- c("field_staff", "intake_field_pairs", "dispatcher",
                "shift_supervisor", "closures_total",
                "closures_TBC", "closures_TBH", "closures_TBG")
desc_daily <- desc_table(df, daily_vars, "Daily")

cat("  DAILY Summary (n =", nrow(df), "days):\n")
print(as.data.frame(desc_daily), row.names = FALSE, digits = 2)
cat("\n")

# Shift-level stats
shift_vars <- c("field_staff", "intake_field_pairs", "dispatcher",
                "closures_total", "TBC", "TBH", "TBG")
desc_shift <- desc_table(sf, shift_vars, "Shift")

cat("  SHIFT-LEVEL Summary (n =", nrow(sf), "shifts):\n")
print(as.data.frame(desc_shift), row.names = FALSE, digits = 2)
cat("\n")

# 24h window stats
window_vars <- c("field_24h", "pairs_24h", "disp_24h",
                 "closures_total", "TBH", "TBG")
desc_24h <- desc_table(sf_24h, window_vars, "24h Window")

cat("  24-HOUR WINDOW Summary (n =", nrow(sf_24h), "shifts):\n")
print(as.data.frame(desc_24h), row.names = FALSE, digits = 2)
cat("\n")

# TBH/TBG closure patterns by shift
cat("  TBH + TBG Closures by Shift:\n")
shift_pattern <- sf %>%
  group_by(shift_code) %>%
  summarise(
    n_shifts   = n(),
    TBH_total  = sum(TBH), TBH_mean = mean(TBH),
    TBG_total  = sum(TBG), TBG_mean = mean(TBG),
    TBC_total  = sum(TBC), TBC_mean = mean(TBC),
    .groups = "drop"
  )
print(as.data.frame(shift_pattern), row.names = FALSE, digits = 2)
cat("\n")

# Weekend vs weekday breakdown for TBH/TBG
cat("  TBH + TBG Closures: Weekday vs Weekend:\n")
we_pattern <- sf %>%
  group_by(weekend) %>%
  summarise(
    n_shifts  = n(),
    TBH_total = sum(TBH), TBH_mean = mean(TBH),
    TBG_total = sum(TBG), TBG_mean = mean(TBG),
    .groups = "drop"
  ) %>%
  mutate(day_type = ifelse(weekend == 1, "Weekend", "Weekday"))
print(as.data.frame(we_pattern), row.names = FALSE, digits = 2)
cat("\n")


# =============================================================================
# STEP 3 — Correlation Analysis
# =============================================================================
cat("--- Step 3: Correlation Analysis ---\n\n")

# --- 3a: Daily-level correlations (field roles only) --------------------------
staff_vars <- c("field_staff", "intake_field_pairs", "dispatcher")
closure_vars <- c("closures_total", "closures_TBC", "closures_TBH",
                   "closures_TBG")

cor_daily <- expand.grid(staff_var = staff_vars, closure_var = closure_vars,
                         stringsAsFactors = FALSE)
cor_daily$r <- NA_real_
cor_daily$p_value <- NA_real_

for (i in seq_len(nrow(cor_daily))) {
  x <- df[[cor_daily$staff_var[i]]]
  y <- df[[cor_daily$closure_var[i]]]
  if (length(unique(x)) > 1 && length(unique(y)) > 1) {
    test <- cor.test(x, y, method = "pearson")
    cor_daily$r[i]       <- test$estimate
    cor_daily$p_value[i] <- test$p.value
  }
}

cor_daily$sig <- ifelse(is.na(cor_daily$p_value), "",
                 ifelse(cor_daily$p_value < 0.001, "***",
                 ifelse(cor_daily$p_value < 0.01,  "**",
                 ifelse(cor_daily$p_value < 0.05,  "*",
                 ifelse(cor_daily$p_value < 0.10,  ".",  "")))))

cat("  DAILY Pearson Correlations: Field Staffing × Closures\n")
cor_daily_wide <- cor_daily %>%
  mutate(label = ifelse(is.na(r), "—", sprintf("%.3f%s", r, sig))) %>%
  select(staff_var, closure_var, label) %>%
  pivot_wider(names_from = closure_var, values_from = label)
print(as.data.frame(cor_daily_wide), row.names = FALSE)
cat("  Signif: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")

# --- 3b: Shift-level correlations (24h window) -------------------------------
window_staff <- c("field_24h", "pairs_24h", "disp_24h")
shift_closure <- c("closures_total", "TBC", "TBH", "TBG")

cor_shift <- expand.grid(staff_var = window_staff, closure_var = shift_closure,
                         stringsAsFactors = FALSE)
cor_shift$r <- NA_real_
cor_shift$p_value <- NA_real_

for (i in seq_len(nrow(cor_shift))) {
  x <- sf_24h[[cor_shift$staff_var[i]]]
  y <- sf_24h[[cor_shift$closure_var[i]]]
  if (length(unique(x)) > 1 && length(unique(y)) > 1) {
    test <- cor.test(x, y, method = "pearson")
    cor_shift$r[i]       <- test$estimate
    cor_shift$p_value[i] <- test$p.value
  }
}

cor_shift$sig <- ifelse(is.na(cor_shift$p_value), "",
                 ifelse(cor_shift$p_value < 0.001, "***",
                 ifelse(cor_shift$p_value < 0.01,  "**",
                 ifelse(cor_shift$p_value < 0.05,  "*",
                 ifelse(cor_shift$p_value < 0.10,  ".",  "")))))

cat("  SHIFT-LEVEL Pearson Correlations: 24h Window Staffing × Closures\n")
cor_shift_wide <- cor_shift %>%
  mutate(label = ifelse(is.na(r), "—", sprintf("%.3f%s", r, sig))) %>%
  select(staff_var, closure_var, label) %>%
  pivot_wider(names_from = closure_var, values_from = label)
print(as.data.frame(cor_shift_wide), row.names = FALSE)
cat("  Signif: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")


# =============================================================================
# STEP 4 — Daily Regression Models (Poisson GLM)
# =============================================================================
cat("--- Step 4: Daily Regression Models (Field Roles Only) ---\n\n")

# Helper: print model summary with IRRs
print_model <- function(model, label) {
  cat(sprintf("  MODEL: %s\n", label))
  cat(sprintf("  Formula: %s\n",
              paste(deparse(formula(model)), collapse = " ")))
  coefs <- summary(model)$coefficients
  irr <- exp(coefs[, "Estimate"])
  irr_lo <- exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"])
  irr_hi <- exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"])
  out <- data.frame(
    term     = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std_err  = coefs[, "Std. Error"],
    IRR      = irr,
    IRR_lo95 = irr_lo,
    IRR_hi95 = irr_hi,
    p_value  = coefs[, "Pr(>|z|)"],
    row.names = NULL
  )
  print(out, digits = 4, row.names = FALSE)
  cat(sprintf("  AIC: %.1f | Residual deviance: %.1f on %d df\n",
              AIC(model), model$deviance, model$df.residual))
  cat("\n")
  invisible(out)
}

# Note: shift_supervisor excluded from regression — constant at 3/day (zero variance)
cat("  NOTE: shift_supervisor constant at 3/day (1/shift); excluded from models.\n\n")

# M1: Total closures ~ field roles + weekend
m1 <- glm(closures_total ~ intake_field_pairs + dispatcher + weekend,
           data = df, family = poisson)
m1_out <- print_model(m1, "Daily Total Closures ~ Field Roles + Weekend")

# M2: TBC (Shelter) ~ field roles + weekend
m2 <- glm(closures_TBC ~ intake_field_pairs + dispatcher + weekend,
           data = df, family = poisson)
m2_out <- print_model(m2, "Daily TBC (Shelter) ~ Field Roles + Weekend")

# M3: TBH (Senior Well-Being) ~ field roles + weekend
m3 <- glm(closures_TBH ~ intake_field_pairs + dispatcher + weekend,
           data = df, family = poisson)
m3_out <- print_model(m3, "Daily TBH (Senior) ~ Field Roles + Weekend")

# M4: TBG (Crisis Referral) ~ field roles + weekend
m4 <- glm(closures_TBG ~ intake_field_pairs + dispatcher + weekend,
           data = df, family = poisson)
m4_out <- print_model(m4, "Daily TBG (Crisis) ~ Field Roles + Weekend")

# M5: Knock-on effect — TBC closures ~ field roles + TBH/TBG demand + weekend
m5 <- glm(closures_TBC ~ intake_field_pairs + dispatcher + weekend +
             closures_TBH + closures_TBG,
           data = df, family = poisson)
m5_out <- print_model(m5,
  "Daily TBC ~ Field Roles + TBH/TBG Demand (Knock-on) + Weekend")


# =============================================================================
# STEP 5 — Shift-Level 24h Window Models
# =============================================================================
cat("--- Step 5: Shift-Level 24h Window Models ---\n\n")
cat("  These models link each shift's closures to the staffing available\n")
cat("  in the 24-hour window (current shift + 2 prior shifts).\n\n")

# M6: TBH per shift ~ 24h window + shift type
m6 <- glm(TBH ~ pairs_24h + disp_24h + shift_factor + weekend,
           data = sf_24h, family = poisson)
m6_out <- print_model(m6, "Shift TBH ~ 24h Window Staffing + Shift + Weekend")

# M7: TBG per shift ~ 24h window + shift type
m7 <- glm(TBG ~ pairs_24h + disp_24h + shift_factor + weekend,
           data = sf_24h, family = poisson)
m7_out <- print_model(m7, "Shift TBG ~ 24h Window Staffing + Shift + Weekend")

# M8: TBC per shift ~ 24h window + concurrent TBH/TBG demand + shift type
m8 <- glm(TBC ~ pairs_24h + disp_24h + TBH + TBG + shift_factor + weekend,
           data = sf_24h, family = poisson)
m8_out <- print_model(m8,
  "Shift TBC ~ 24h Staffing + Concurrent TBH/TBG Demand + Shift + Weekend")

# M9: Total per shift ~ 24h window + shift type
m9 <- glm(closures_total ~ pairs_24h + disp_24h + shift_factor + weekend,
           data = sf_24h, family = poisson)
m9_out <- print_model(m9, "Shift Total ~ 24h Window Staffing + Shift + Weekend")

# --- Compare daily vs shift-level model fit ---
cat("  MODEL FIT COMPARISON:\n")
cat(sprintf("    Daily TBH model (M3) AIC:     %8.1f\n", AIC(m3)))
cat(sprintf("    Shift 24h TBH model (M6) AIC: %8.1f\n", AIC(m6)))
cat(sprintf("    Daily TBG model (M4) AIC:     %8.1f\n", AIC(m4)))
cat(sprintf("    Shift 24h TBG model (M7) AIC: %8.1f\n", AIC(m7)))
cat("    (AIC not directly comparable across scales, but shift models\n")
cat("     have 3x the observations and finer temporal resolution.)\n\n")


# =============================================================================
# STEP 6 — Summary Export
# =============================================================================
cat("--- Step 6: Summary Export ---\n\n")

# Role impact from shift-level models (M6, M7, M8)
extract_coefs <- function(model, model_name) {
  coefs <- summary(model)$coefficients
  data.frame(
    model     = model_name,
    term      = rownames(coefs),
    estimate  = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    IRR       = exp(coefs[, "Estimate"]),
    IRR_lo95  = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"]),
    IRR_hi95  = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]),
    p_value   = coefs[, "Pr(>|z|)"],
    row.names = NULL
  )
}

all_coefs <- bind_rows(
  extract_coefs(m1, "M1_daily_total"),
  extract_coefs(m2, "M2_daily_TBC"),
  extract_coefs(m3, "M3_daily_TBH"),
  extract_coefs(m4, "M4_daily_TBG"),
  extract_coefs(m5, "M5_daily_knockon"),
  extract_coefs(m6, "M6_shift_TBH_24h"),
  extract_coefs(m7, "M7_shift_TBG_24h"),
  extract_coefs(m8, "M8_shift_TBC_knockon_24h"),
  extract_coefs(m9, "M9_shift_total_24h")
)

csv_out_path <- here("data", "inputs", "staffing_log", "clean",
                     "staffing_impact_summary_2026.csv")
write_csv(all_coefs, csv_out_path)
cat("  Model coefficients written to:", csv_out_path, "\n\n")


# =============================================================================
# STEP 7 — Visualizations
# =============================================================================
cat("--- Step 7: Generating Visualizations ---\n\n")

# --- Fig 1: Time series — Daily field staff vs TBH + TBG closures ------------
daily_plot <- df %>%
  mutate(TBH_TBG = closures_TBH + closures_TBG)

staff_scalar <- max(daily_plot$TBH_TBG, na.rm = TRUE) /
                max(daily_plot$field_staff, na.rm = TRUE)

p1 <- ggplot(daily_plot, aes(x = date)) +
  geom_line(aes(y = field_staff * staff_scalar, color = "Field Staff"),
            linewidth = 1) +
  geom_col(aes(y = closures_TBH, fill = "TBH (Senior)"),
           alpha = 0.7, width = 0.8) +
  geom_col(aes(y = closures_TBH + closures_TBG, fill = "TBG (Crisis)"),
           alpha = 0.5, width = 0.8) +
  scale_y_continuous(
    name = "Daily TBH + TBG Closures",
    sec.axis = sec_axis(~./staff_scalar, name = "Field Staff Scheduled")
  ) +
  scale_color_manual(values = c("Field Staff" = "#2c7bb6")) +
  scale_fill_manual(values = c("TBH (Senior)" = "#d7191c",
                                "TBG (Crisis)" = "#fdae61")) +
  labs(
    title    = "Daily Field Staffing vs. SWB + Crisis Closures",
    subtitle = "Jan 10 – Feb 16, 2026 | Field staff = drivers + dispatchers + supervisors",
    x = NULL, color = NULL, fill = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "01_timeseries_field_staff_tbh_tbg.png"),
       p1, width = 10, height = 5, dpi = 150)
cat("  Saved: 01_timeseries_field_staff_tbh_tbg.png\n")

# --- Fig 2: TBH + TBG closures by shift heatmap ------------------------------
shift_heat_data <- sf %>%
  group_by(date, shift_code) %>%
  summarise(TBH = sum(TBH), TBG = sum(TBG), .groups = "drop") %>%
  pivot_longer(cols = c(TBH, TBG), names_to = "case_type",
               values_to = "closures") %>%
  mutate(shift_code = factor(shift_code, levels = c("AM", "PM", "OVN")))

p2 <- ggplot(shift_heat_data, aes(x = date, y = shift_code, fill = closures)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~case_type, ncol = 1) +
  scale_fill_gradient(low = "grey95", high = "#d7191c",
                      name = "Closures") +
  labs(
    title    = "TBH + TBG Closures by Shift",
    subtitle = "Which shifts close the most Senior/Crisis cases?",
    x = NULL, y = "Shift"
  ) +
  theme_report +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(fig_dir, "02_shift_heatmap_tbh_tbg.png"),
       p2, width = 10, height = 5, dpi = 150)
cat("  Saved: 02_shift_heatmap_tbh_tbg.png\n")

# --- Fig 3: Correlation heatmap (both daily and 24h window) -------------------
cor_all <- bind_rows(
  cor_daily %>% mutate(level = "Daily"),
  cor_shift %>% mutate(level = "24h Window")
)

cor_all <- cor_all %>%
  mutate(
    label = ifelse(is.na(r), "—", sprintf("%.2f%s", r, sig)),
    staff_label = paste0(level, ": ", staff_var)
  )

# Order: daily first, then shift
level_order <- c(
  paste0("Daily: ", staff_vars),
  paste0("24h Window: ", window_staff)
)
cor_all$staff_label <- factor(cor_all$staff_label,
                               levels = rev(level_order))

# Unified closure var names for display
cor_all <- cor_all %>%
  mutate(closure_label = case_when(
    closure_var == "closures_total" ~ "Total",
    closure_var == "closures_TBC"   ~ "TBC",
    closure_var == "closures_TBH"   ~ "TBH",
    closure_var == "closures_TBG"   ~ "TBG",
    closure_var == "TBC"            ~ "TBC",
    closure_var == "TBH"            ~ "TBH",
    closure_var == "TBG"            ~ "TBG",
    TRUE ~ closure_var
  ))

cor_all$closure_label <- factor(cor_all$closure_label,
                                levels = c("Total", "TBC", "TBH", "TBG"))

p3 <- ggplot(cor_all, aes(x = closure_label, y = staff_label, fill = r)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.2) +
  scale_fill_gradient2(low = "#d7191c", mid = "white", high = "#2c7bb6",
                       midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
  labs(
    title    = "Correlation Heatmap: Staffing × Closures",
    subtitle = "Daily field roles (top) and 24h shift window (bottom)",
    x = NULL, y = NULL, fill = "r"
  ) +
  theme_report +
  theme(axis.text.x = element_text(angle = 0))

ggsave(file.path(fig_dir, "03_correlation_heatmap.png"),
       p3, width = 9, height = 5, dpi = 150)
cat("  Saved: 03_correlation_heatmap.png\n")

# --- Fig 4: Forest plot — 24h window model IRRs (M6 TBH, M7 TBG) ------------
forest_24h <- bind_rows(
  extract_coefs(m6, "TBH (M6)"),
  extract_coefs(m7, "TBG (M7)")
) %>%
  filter(!term %in% c("(Intercept)")) %>%
  mutate(
    term_label = case_when(
      term == "pairs_24h"      ~ "Intake Pairs (24h)",
      term == "disp_24h"       ~ "Dispatchers (24h)",
      term == "shift_factorPM" ~ "PM Shift",
      term == "shift_factorOVN"~ "OVN Shift",
      term == "weekend"        ~ "Weekend",
      TRUE                     ~ term
    ),
    term_label = factor(term_label,
                        levels = rev(unique(term_label)))
  )

p4 <- ggplot(forest_24h, aes(x = IRR, y = term_label, color = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = IRR_lo95, xmax = IRR_hi95),
                 height = 0.2, linewidth = 0.8,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("TBH (M6)" = "#d7191c",
                                 "TBG (M7)" = "#fdae61")) +
  labs(
    title    = "24h Window Staffing Effects on TBH + TBG Closures",
    subtitle = "Incidence Rate Ratios from shift-level Poisson models | 95% CI",
    x = "IRR (Incidence Rate Ratio)", y = NULL, color = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "04_forest_24h_tbh_tbg.png"),
       p4, width = 9, height = 5, dpi = 150)
cat("  Saved: 04_forest_24h_tbh_tbg.png\n")

# --- Fig 5: Knock-on effect — TBH/TBG demand vs TBC closures -----------------
knockon_coefs <- m8_out %>%
  filter(term %in% c("TBH", "TBG", "pairs_24h", "disp_24h")) %>%
  mutate(
    term_label = case_when(
      term == "TBH"        ~ "Concurrent TBH",
      term == "TBG"        ~ "Concurrent TBG",
      term == "pairs_24h"  ~ "Intake Pairs (24h)",
      term == "disp_24h"   ~ "Dispatchers (24h)",
      TRUE ~ term
    ),
    term_label = factor(term_label,
                        levels = rev(c("Intake Pairs (24h)", "Dispatchers (24h)",
                                       "Concurrent TBH", "Concurrent TBG")))
  )

p5 <- ggplot(knockon_coefs, aes(x = IRR, y = term_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = IRR_lo95, xmax = IRR_hi95),
                 height = 0.2, linewidth = 0.8) +
  geom_point(aes(color = ifelse(p_value < 0.05, "sig", "ns")), size = 3) +
  scale_color_manual(values = c("sig" = "#2c7bb6", "ns" = "grey60"),
                     breaks = c("sig", "ns"),
                     labels = c("p < 0.05", "p >= 0.05"),
                     drop = TRUE) +
  labs(
    title    = "Knock-on Effect: What Drives TBC (Shelter) Closures per Shift?",
    subtitle = "Model M8: TBC ~ 24h Staffing + Concurrent TBH/TBG Demand",
    x = "IRR (Incidence Rate Ratio)", y = NULL, color = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "05_knockon_tbc_forest.png"),
       p5, width = 9, height = 4, dpi = 150)
cat("  Saved: 05_knockon_tbc_forest.png\n")

# --- Fig 6: Scatter — 24h intake pairs vs TBH closures per shift -------------
p6 <- ggplot(sf_24h, aes(x = pairs_24h, y = TBH)) +
  geom_jitter(aes(color = shift_factor, shape = factor(weekend)),
              width = 0.2, height = 0.2, size = 2.5, alpha = 0.7) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              se = TRUE, color = "#d7191c", linewidth = 1) +
  scale_color_manual(values = c(AM = "#2c7bb6", PM = "#fdae61",
                                 OVN = "#7b3294")) +
  scale_shape_manual(values = c("0" = 16, "1" = 17),
                     labels = c("Weekday", "Weekend")) +
  labs(
    title    = "24h Intake Pairs vs. TBH Closures per Shift",
    subtitle = "Poisson fit | Each point = one shift",
    x = "Intake Field Pairs (24h Window)", y = "TBH Closures",
    color = "Shift", shape = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "06_scatter_24h_pairs_tbh.png"),
       p6, width = 8, height = 5, dpi = 150)
cat("  Saved: 06_scatter_24h_pairs_tbh.png\n")

cat("\n==========================================================\n")
cat("  Analysis complete.\n")
cat("==========================================================\n")
