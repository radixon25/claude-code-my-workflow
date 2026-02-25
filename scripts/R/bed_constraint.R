# =============================================================================
# bed_constraint.R — Shelter Bed Availability Constraint Analysis (TBC Cases)
# =============================================================================
# Central question: Does system-wide bed scarcity slow down TBC case closures?
#
# CAUSAL STRATEGY:
#   Shift 2 (9am) bed count is the primary measure — completed when consistent
#   staffing is present at all locations, making it the most authoritative
#   snapshot. All sites (including ASH) use the same operational process and
#   all beds are in queue identically.
#
# SIMULTANEITY:
#   ASH dispatchers place individuals into beds → placement activity depletes
#   beds AND beds constrain dispatch speed. Shift 2 count (9am) partially
#   addresses this: it reflects the state before most same-day dispatch
#   activity. Sensitivity analyses use Shift 1/3/Latest to test robustness.
#
# OUTCOMES:
#   Age        — days from creation to closure (Gamma GLM, log link)
#   Compliance — STAR (<3h) vs not (binary logistic)
#
# SCOPE: TBC (Shelter) cases only, Jan 10 – Feb 23, 2026 (45-day overlap
#         with staffing data)
#
# DEPENDS ON:
#   - shelter_beds.R outputs (data/processed/shelter_beds_*.csv)
#   - staffing_levels.R outputs (data/inputs/staffing_log/clean/)
#   - Salesforce export (config.R → RAW_CSV_PATH)
#
# USAGE:
#   Rscript scripts/R/bed_constraint.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(stringr)
})

# ====== PROJECT ROOT ======
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

fig_dir <- here("analysis", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

out_dir <- here("data", "processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Theme (consistent with staffing_compliance.R)
theme_report <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom"
  )

cat("\n==========================================================\n")
cat("  Bed Availability Constraint Analysis — TBC Cases\n")
cat("  Shift 2 (9am) as Primary Measure\n")
cat("  Analysis Window: Jan 10 – Feb 23, 2026 (45 days)\n")
cat("==========================================================\n\n")


# =============================================================================
# STEP 1 — Load Data Sources
# =============================================================================
cat("--- Step 1: Load Data Sources ---\n\n")

# --- 1a: System-level bed availability (from shelter_beds.R) -----------------
system_bed_path <- here("data", "processed", "shelter_beds_system_daily.csv")
if (!file.exists(system_bed_path)) {
  stop("System bed data not found. Run shelter_beds.R first: ", system_bed_path)
}
system_beds <- read_csv(system_bed_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
cat("  System bed data:", nrow(system_beds), "days loaded\n")

# --- 1b: Population-specific bed availability --------------------------------
pop_bed_path <- here("data", "processed", "shelter_beds_system_daily_by_pop.csv")
pop_beds <- NULL
if (file.exists(pop_bed_path)) {
  pop_beds <- read_csv(pop_bed_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date))
  cat("  Population bed data:", nrow(pop_beds), "rows loaded\n")
}

# --- 1c: Shift-level staffing (from staffing_levels.R) -----------------------
staffing_path <- here("scripts", "R", "Data", "staffing_log", "clean",
                      "staffing_daily_levels_2026.csv")
if (!file.exists(staffing_path)) {
  stop("Staffing data not found. Run staffing_levels.R first: ", staffing_path)
}
staffing_raw <- read_csv(staffing_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

field_roles <- c("Intake Driver", "Dispatcher", "Shift Supervisor")

shift_staffing <- staffing_raw %>%
  filter(role_clean %in% field_roles) %>%
  group_by(date, shift_code, role_clean) %>%
  summarise(staff = sum(scheduled_staff), .groups = "drop") %>%
  pivot_wider(names_from = role_clean, values_from = staff, values_fill = 0)

names(shift_staffing) <- gsub(" ", "_", tolower(names(shift_staffing)))

shift_staffing <- shift_staffing %>%
  mutate(
    intake_field_pairs = floor(intake_driver / 2),
    field_staff        = intake_driver + dispatcher + shift_supervisor
  )

# 24h window (closure shift + 2 prior shifts) — same pattern as staffing_compliance.R
shift_order_map <- c(AM = 0L, PM = 1L, OVN = 2L)

shift_staffing <- shift_staffing %>%
  mutate(
    shift_ord = shift_order_map[shift_code],
    shift_idx = as.integer(date - min(date)) * 3L + shift_ord
  ) %>%
  arrange(shift_idx) %>%
  mutate(
    pairs_lag1 = lag(intake_field_pairs, 1),
    pairs_lag2 = lag(intake_field_pairs, 2),
    disp_lag1  = lag(dispatcher, 1),
    disp_lag2  = lag(dispatcher, 2),
    field_lag1 = lag(field_staff, 1),
    field_lag2 = lag(field_staff, 2),
    pairs_24h  = intake_field_pairs + pairs_lag1 + pairs_lag2,
    disp_24h   = dispatcher + disp_lag1 + disp_lag2,
    field_24h  = field_staff + field_lag1 + field_lag2
  )

cat("  Shift staffing:", nrow(shift_staffing), "shift-days loaded\n")

# --- 1d: TBC Cases (Salesforce) -----------------------------------------------
sf_path <- "C:/Users/sagea/Desktop/A Safe Haven/Data/Raw Base Exports/SR_FQ_Y2026.csv"
if (!file.exists(sf_path)) {
  stop("Salesforce export not found: ", sf_path)
}
sf_raw <- read_csv(sf_path, show_col_types = FALSE)

cases <- sf_raw %>%
  distinct(`Work Order Number`, .keep_all = TRUE) %>%
  filter(
    Status == "Closed",
    !is.na(`End Date`),
    `Work Order: Type Short Code` == "TBC"
  ) %>%
  mutate(
    closure_dt   = mdy_hm(`End Date`),
    closure_date = as.Date(closure_dt),
    closure_hour = hour(closure_dt),
    case_type    = `Work Order: Type Short Code`,
    age_days     = as.numeric(Age)
  ) %>%
  filter(!is.na(closure_date), !is.na(age_days))

cat("  TBC closed cases (all time):", nrow(cases), "\n")

# Map closure to shift + staffing date
cases <- cases %>%
  mutate(
    closure_shift = case_when(
      closure_hour >= 7  & closure_hour < 15 ~ "AM",
      closure_hour >= 15 & closure_hour < 23 ~ "PM",
      TRUE                                   ~ "OVN"
    ),
    staffing_date = case_when(
      closure_shift == "OVN" & closure_hour < 7 ~ closure_date - 1L,
      TRUE                                      ~ closure_date
    )
  )

# Compliance tiers
cases <- cases %>%
  mutate(
    compliance = case_when(
      age_days < 3/24  ~ "STAR Compliant",
      age_days < 20/24 ~ "City Compliant",
      TRUE             ~ "Non-Compliant"
    ),
    compliance_ord = factor(compliance,
                            levels = c("Non-Compliant", "City Compliant",
                                       "STAR Compliant"),
                            ordered = TRUE),
    is_star      = as.integer(compliance == "STAR Compliant"),
    is_compliant = as.integer(compliance != "Non-Compliant"),
    age_hours    = age_days * 24
  )


# =============================================================================
# STEP 2 — Build Analysis Dataset
# =============================================================================
cat("\n--- Step 2: Build Analysis Dataset ---\n\n")

# --- 2a: Join system-level Shift 2 bed availability --------------------------
case_df <- cases %>%
  inner_join(
    system_beds %>%
      select(date, system_capacity, system_available_s2, system_utilization_s2,
             system_available_s1, system_available_s3, system_latest,
             system_delta_23, sites_reporting_s2, pct_sites_reporting_s2),
    by = c("closure_date" = "date")
  )

cat("  Cases with bed data:", nrow(case_df), "\n")

# --- 2b: Join shift-level staffing (24h window) ------------------------------
case_df <- case_df %>%
  inner_join(
    shift_staffing %>%
      select(date, shift_code, intake_field_pairs, dispatcher,
             field_staff, pairs_24h, disp_24h, field_24h),
    by = c("staffing_date" = "date", "closure_shift" = "shift_code")
  ) %>%
  filter(!is.na(pairs_24h))  # need complete 24h window

cat("  Cases with staffing linkage:", nrow(case_df), "\n")

# --- 2c: Add controls --------------------------------------------------------
case_df <- case_df %>%
  mutate(
    weekend      = as.integer(wday(closure_date) %in% c(1, 7)),
    day_of_week  = wday(closure_date, label = TRUE),
    shift_factor = factor(closure_shift, levels = c("AM", "PM", "OVN")),
    week_number  = isoweek(closure_date)
  )

# --- 2d: Join population-specific availability (if population can be mapped) --
# TBC cases serve specific populations; try to match
if (!is.null(pop_beds)) {
  # Check what population field we have in the case data
  pop_cols <- intersect(names(sf_raw), c("Population", "population",
                                          "Flex Question: Population"))
  if (length(pop_cols) > 0) {
    cat("  Population field found in case data:", pop_cols[1], "\n")
    # Population matching is a sensitivity analysis — proceed if data allows
  } else {
    cat("  No population field in case data — population matching skipped\n")
  }
}

# --- 2e: Summary of analysis dataset -----------------------------------------
cat("\n  ANALYSIS DATASET SUMMARY:\n")
cat("  Rows:", nrow(case_df), "\n")
cat("  Date range:", format(min(case_df$closure_date)), "to",
    format(max(case_df$closure_date)), "\n")
cat("  Compliance breakdown:\n")
print(table(case_df$compliance))
cat("\n")

# Write analysis dataset
analysis_path <- file.path(out_dir, "tbc_bed_constraint_analysis.csv")
write_csv(case_df %>% select(
  `Work Order Number`, closure_date, closure_shift, closure_hour,
  age_days, age_hours, compliance, is_star, is_compliant,
  system_capacity, system_available_s2, system_utilization_s2,
  system_available_s1, system_available_s3, system_latest, system_delta_23,
  intake_field_pairs, dispatcher, field_staff, pairs_24h, disp_24h, field_24h,
  weekend, week_number
), analysis_path)
cat("  Written:", analysis_path, "\n\n")


# =============================================================================
# STEP 3 — Descriptive Analysis
# =============================================================================
cat("--- Step 3: Descriptive Analysis ---\n\n")

# --- 3a: System availability time series summary -----------------------------
cat("  SYSTEM BED AVAILABILITY (Shift 2, 9am):\n")
system_beds %>%
  filter(date >= as.Date("2026-01-10"), date <= as.Date("2026-02-23")) %>%
  summarise(
    days             = n(),
    mean_available   = round(mean(system_available_s2), 0),
    sd_available     = round(sd(system_available_s2), 0),
    min_available    = min(system_available_s2),
    max_available    = max(system_available_s2),
    mean_capacity    = round(mean(system_capacity), 0),
    mean_utilization = round(mean(system_utilization_s2) * 100, 1)
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("\n")

# --- 3b: Within-day shift deltas ----------------------------------------------
cat("  WITHIN-DAY SHIFT DELTAS (system-level):\n")
system_beds %>%
  filter(date >= as.Date("2026-01-10"), date <= as.Date("2026-02-23")) %>%
  summarise(
    mean_delta_12     = round(mean(system_delta_12, na.rm = TRUE), 1),
    sd_delta_12       = round(sd(system_delta_12, na.rm = TRUE), 1),
    mean_delta_23     = round(mean(system_delta_23, na.rm = TRUE), 1),
    sd_delta_23       = round(sd(system_delta_23, na.rm = TRUE), 1)
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("  (Positive delta_23 = beds depleted between 9am and afternoon)\n\n")

# --- 3c: TBC case age summary -------------------------------------------------
cat("  TBC CASE AGE (hours):\n")
case_df %>%
  summarise(
    n          = n(),
    mean_hrs   = round(mean(age_hours), 2),
    median_hrs = round(median(age_hours), 2),
    sd_hrs     = round(sd(age_hours), 2),
    p25_hrs    = round(quantile(age_hours, 0.25), 2),
    p75_hrs    = round(quantile(age_hours, 0.75), 2),
    min_hrs    = round(min(age_hours), 2),
    max_hrs    = round(max(age_hours), 2)
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("\n")

# --- 3d: Compliance breakdown -------------------------------------------------
cat("  TBC COMPLIANCE BREAKDOWN:\n")
case_df %>%
  count(compliance) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("\n")

# --- 3e: Correlation: bed availability × age ---------------------------------
cat("  CORRELATIONS: Bed Availability × Case Age:\n")
bed_vars <- c("system_available_s2", "system_utilization_s2",
              "system_available_s1", "system_available_s3")
bed_vars_present <- intersect(bed_vars, names(case_df))

for (bv in bed_vars_present) {
  ct <- cor.test(case_df[[bv]], case_df$age_hours, method = "pearson")
  sig <- ifelse(ct$p.value < 0.001, "***",
         ifelse(ct$p.value < 0.01,  "**",
         ifelse(ct$p.value < 0.05,  "*",
         ifelse(ct$p.value < 0.10,  ".", ""))))
  cat(sprintf("    %s × age_hours: r = %.3f %s (p = %.4f)\n",
              bv, ct$estimate, sig, ct$p.value))
}
cat("    Signif: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")


# =============================================================================
# STEP 4 — Statistical Models
# =============================================================================
cat("--- Step 4: Statistical Models ---\n\n")

# Helper: print Gamma GLM results (same pattern as staffing_compliance.R)
print_gamma <- function(model, label) {
  cat(sprintf("  MODEL: %s\n", label))
  cat(sprintf("  Formula: %s\n",
              paste(deparse(formula(model)), collapse = " ")))
  coefs <- summary(model)$coefficients
  mult <- exp(coefs[, "Estimate"])
  mult_lo <- exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"])
  mult_hi <- exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"])
  out <- data.frame(
    term      = rownames(coefs),
    estimate  = coefs[, "Estimate"],
    std_err   = coefs[, "Std. Error"],
    exp_beta  = mult,
    exp_lo95  = mult_lo,
    exp_hi95  = mult_hi,
    p_value   = coefs[, "Pr(>|t|)"],
    row.names = NULL
  )
  print(out, digits = 4, row.names = FALSE)
  cat(sprintf("  AIC: %.1f | Deviance: %.2f on %d df\n",
              AIC(model), model$deviance, model$df.residual))
  cat("\n")
  invisible(out)
}

# Helper: print binary logistic results
print_logistic <- function(model, label) {
  cat(sprintf("  MODEL: %s\n", label))
  cat(sprintf("  Formula: %s\n",
              paste(deparse(formula(model)), collapse = " ")))
  coefs <- summary(model)$coefficients
  or <- exp(coefs[, "Estimate"])
  or_lo <- exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"])
  or_hi <- exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"])
  out <- data.frame(
    term     = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std_err  = coefs[, "Std. Error"],
    OR       = or,
    OR_lo95  = or_lo,
    OR_hi95  = or_hi,
    p_value  = coefs[, "Pr(>|z|)"],
    row.names = NULL
  )
  print(out, digits = 4, row.names = FALSE)
  cat(sprintf("  AIC: %.1f\n", AIC(model)))
  cat("\n")
  invisible(out)
}

# Helper: extract coefficients for export (same pattern)
extract_coefs <- function(model, model_name, type = "gamma") {
  coefs <- summary(model)$coefficients
  if (type == "gamma") {
    p_col <- "Pr(>|t|)"
  } else {
    p_col <- "Pr(>|z|)"
  }
  data.frame(
    model     = model_name,
    term      = rownames(coefs),
    estimate  = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    effect    = exp(coefs[, "Estimate"]),
    lo95      = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"]),
    hi95      = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]),
    p_value   = coefs[, p_col],
    row.names = NULL
  )
}

# Add small constant to zero-age cases to avoid Gamma issues
model_df <- case_df %>%
  mutate(age_hours = pmax(age_hours, 0.01))


# --- Layer 1: Authoritative Availability (Shift 2, Primary) ------------------
cat("  ===== LAYER 1: Shift 2 (9am) — Primary Models =====\n\n")

# M1: TBC Age ~ system_available_s2 + staffing + shift + weekend
m1 <- glm(age_hours ~ system_available_s2 + pairs_24h + disp_24h +
             shift_factor + weekend,
           data = model_df, family = Gamma(link = "log"))
m1_out <- print_gamma(m1,
  "M1: TBC Age ~ System Beds (Shift 2) + Staffing + Shift + Weekend")

# M2: P(STAR Compliant) ~ system_available_s2 + staffing + shift + weekend
m2 <- glm(is_star ~ system_available_s2 + pairs_24h + disp_24h +
             shift_factor + weekend,
           data = model_df, family = binomial)
m2_out <- print_logistic(m2,
  "M2: P(STAR) ~ System Beds (Shift 2) + Staffing + Shift + Weekend")


# --- Layer 2: Sensitivity — Alternative Shift Measures -----------------------
cat("  ===== LAYER 2: Sensitivity — Alternative Shift Measures =====\n\n")

# M3: Population-matched availability (if population mapping available)
# Placeholder — implement when population matching is confirmed
cat("  M3 (Population-matched): Deferred — population field mapping TBD\n\n")

# M4: Shift 1 (pre-dawn count)
m4 <- glm(age_hours ~ system_available_s1 + pairs_24h + disp_24h +
             shift_factor + weekend,
           data = model_df, family = Gamma(link = "log"))
m4_out <- print_gamma(m4,
  "M4: TBC Age ~ System Beds (Shift 1) + Staffing + Shift + Weekend")

# M5: Shift 3 (afternoon count — post-placement)
m5 <- glm(age_hours ~ system_available_s3 + pairs_24h + disp_24h +
             shift_factor + weekend,
           data = model_df, family = Gamma(link = "log"))
m5_out <- print_gamma(m5,
  "M5: TBC Age ~ System Beds (Shift 3) + Staffing + Shift + Weekend")

# M6: Shift Latest Count
m6 <- glm(age_hours ~ system_latest + pairs_24h + disp_24h +
             shift_factor + weekend,
           data = model_df, family = Gamma(link = "log"))
m6_out <- print_gamma(m6,
  "M6: TBC Age ~ System Beds (Latest) + Staffing + Shift + Weekend")


# --- Layer 3: Within-Day Dynamics (Exploratory) ------------------------------
cat("  ===== LAYER 3: Within-Day Dynamics (Exploratory) =====\n\n")

# How many TBC cases close per day?
daily_closures <- case_df %>%
  count(closure_date, name = "tbc_closures")

delta_analysis <- system_beds %>%
  filter(date >= as.Date("2026-01-10"), date <= as.Date("2026-02-23")) %>%
  left_join(daily_closures, by = c("date" = "closure_date"))

delta_analysis$tbc_closures[is.na(delta_analysis$tbc_closures)] <- 0

# Regress delta_23 on daily TBC closures
if (sum(!is.na(delta_analysis$system_delta_23)) >= 5) {
  m_delta <- lm(system_delta_23 ~ tbc_closures + weekend, data = delta_analysis)
  cat("  WITHIN-DAY MODEL: delta_23 ~ TBC closures + weekend\n")
  cat("  (Shift 2→3 bed decline ~ ASH dispatch activity)\n")
  print(summary(m_delta)$coefficients, digits = 4)
  cat("\n")
} else {
  cat("  Insufficient data for within-day dynamics model\n\n")
}


# =============================================================================
# STEP 5 — Export Model Coefficients
# =============================================================================
cat("--- Step 5: Export Model Coefficients ---\n\n")

all_coefs <- bind_rows(
  extract_coefs(m1, "M1_TBC_age_shift2", "gamma"),
  extract_coefs(m2, "M2_TBC_star_shift2", "logistic"),
  extract_coefs(m4, "M4_TBC_age_shift1", "gamma"),
  extract_coefs(m5, "M5_TBC_age_shift3", "gamma"),
  extract_coefs(m6, "M6_TBC_age_latest", "gamma")
)

coef_path <- file.path(out_dir, "bed_constraint_model_coefficients.csv")
write_csv(all_coefs, coef_path)
cat("  All model coefficients written to:", coef_path, "\n\n")


# =============================================================================
# STEP 6 — Visualizations
# =============================================================================
cat("--- Step 6: Generating Visualizations ---\n\n")

# --- Fig 1: System availability time series -----------------------------------
plot_beds <- system_beds %>%
  filter(date >= as.Date("2026-01-10"), date <= as.Date("2026-02-23"))

p1 <- ggplot(plot_beds, aes(x = date)) +
  geom_line(aes(y = system_available_s2, color = "Shift 2 (9am, primary)"),
            linewidth = 1.2) +
  geom_line(aes(y = system_available_s1, color = "Shift 1 (pre-dawn)"),
            linewidth = 0.7, alpha = 0.6) +
  geom_line(aes(y = system_available_s3, color = "Shift 3 (afternoon)"),
            linewidth = 0.7, alpha = 0.6) +
  scale_color_manual(values = c(
    "Shift 2 (9am, primary)" = "#2166ac",
    "Shift 1 (pre-dawn)"     = "#92c5de",
    "Shift 3 (afternoon)"    = "#f4a582"
  )) +
  geom_point(aes(y = system_available_s2), color = "#2166ac", size = 1.5) +
  labs(
    title    = "System-Wide Available Shelter Beds by Shift",
    subtitle = "All Chicago shelter sites | Jan 10 \u2013 Feb 23, 2026",
    x = NULL, y = "Available Beds", color = NULL
  ) +
  theme_report +
  theme(legend.position = "top")

ggsave(file.path(fig_dir, "20_bed_availability_timeseries.png"),
       p1, width = 10, height = 6, dpi = 150)
cat("  Saved: 20_bed_availability_timeseries.png\n")

# --- Fig 2: Within-day shift dynamics -----------------------------------------
delta_long <- plot_beds %>%
  select(date, day_of_week, system_delta_12, system_delta_23) %>%
  pivot_longer(
    cols = c(system_delta_12, system_delta_23),
    names_to = "delta_type",
    values_to = "delta_beds"
  ) %>%
  mutate(
    delta_label = case_when(
      delta_type == "system_delta_12" ~ "Shift 1\u21922 (overnight)",
      delta_type == "system_delta_23" ~ "Shift 2\u21923 (daytime)"
    )
  )

p2 <- ggplot(delta_long, aes(x = day_of_week, y = delta_beds,
                               fill = delta_label)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c(
    "Shift 1\u21922 (overnight)" = "#92c5de",
    "Shift 2\u21923 (daytime)"   = "#f4a582"
  )) +
  labs(
    title    = "Within-Day Bed Count Changes by Day of Week",
    subtitle = "Positive = beds depleted | Negative = beds added",
    x = NULL, y = "Net Bed Change", fill = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "21_within_day_shift_dynamics.png"),
       p2, width = 10, height = 6, dpi = 150)
cat("  Saved: 21_within_day_shift_dynamics.png\n")

# --- Fig 3: Scatter — system beds vs TBC case age ----------------------------
p3 <- ggplot(case_df, aes(x = system_available_s2, y = age_hours)) +
  geom_jitter(aes(shape = factor(weekend)), width = 2, alpha = 0.5, size = 2,
              color = "#2166ac") +
  geom_smooth(method = "glm",
              method.args = list(family = Gamma(link = "log")),
              se = TRUE, color = "#d6604d", linewidth = 1) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "#2ca25f") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#d7191c") +
  annotate("text", x = min(case_df$system_available_s2) + 5, y = 3,
           label = "STAR (3h)", hjust = 0, vjust = -0.5,
           color = "#2ca25f", size = 3) +
  annotate("text", x = min(case_df$system_available_s2) + 5, y = 20,
           label = "City (20h)", hjust = 0, vjust = -0.5,
           color = "#d7191c", size = 3) +
  scale_shape_manual(values = c("0" = 16, "1" = 17),
                     labels = c("Weekday", "Weekend")) +
  coord_cartesian(ylim = c(0, min(max(case_df$age_hours), 50))) +
  labs(
    title    = "System Bed Availability vs. TBC Case Age",
    subtitle = "Gamma regression fit | Shift 2 (9am) bed count",
    x = "System Available Beds (Shift 2)", y = "Case Age (hours)",
    shape = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "22_scatter_beds_vs_age.png"),
       p3, width = 9, height = 6, dpi = 150)
cat("  Saved: 22_scatter_beds_vs_age.png\n")

# --- Fig 4: Forest plot — bed constraint model coefficients -------------------
# Extract non-intercept terms from M1 (primary model)
forest_data <- m1_out %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_label = case_when(
      term == "system_available_s2" ~ "System Beds (Shift 2)",
      term == "pairs_24h"           ~ "Intake Pairs (24h)",
      term == "disp_24h"            ~ "Dispatchers (24h)",
      term == "shift_factorPM"      ~ "PM Shift",
      term == "shift_factorOVN"     ~ "OVN Shift",
      term == "weekend"             ~ "Weekend",
      TRUE                          ~ term
    ),
    term_label = factor(term_label, levels = rev(unique(term_label)))
  )

p4 <- ggplot(forest_data, aes(x = exp_beta, y = term_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = exp_lo95, xmax = exp_hi95),
                 height = 0.2, linewidth = 0.8, color = "#2166ac") +
  geom_point(size = 3, color = "#2166ac") +
  labs(
    title    = "Bed Constraint Effects on TBC Case Age (M1)",
    subtitle = "Gamma GLM | exp(\u03B2): <1 = faster closure, >1 = slower closure",
    x = "Multiplicative Effect on Mean Age", y = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "23_forest_bed_constraint.png"),
       p4, width = 9, height = 5, dpi = 150)
cat("  Saved: 23_forest_bed_constraint.png\n")

# --- Fig 5: Sensitivity comparison — bed coefficient across models ------------
# Extract the bed availability coefficient from each model
sensitivity_data <- bind_rows(
  m1_out %>% filter(term == "system_available_s2") %>%
    mutate(model = "M1: Shift 2 (primary)", measure = "Shift 2"),
  m4_out %>% filter(term == "system_available_s1") %>%
    mutate(model = "M4: Shift 1", measure = "Shift 1"),
  m5_out %>% filter(term == "system_available_s3") %>%
    mutate(model = "M5: Shift 3", measure = "Shift 3"),
  m6_out %>% filter(term == "system_latest") %>%
    mutate(model = "M6: Latest", measure = "Latest")
) %>%
  mutate(model = factor(model, levels = c("M1: Shift 2 (primary)",
                                           "M4: Shift 1",
                                           "M5: Shift 3",
                                           "M6: Latest")))

p5 <- ggplot(sensitivity_data, aes(x = exp_beta, y = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = exp_lo95, xmax = exp_hi95),
                 height = 0.2, linewidth = 0.8, color = "#2166ac") +
  geom_point(size = 3, color = "#2166ac") +
  labs(
    title    = "Sensitivity: Bed Availability Effect Across Shift Measures",
    subtitle = "exp(\u03B2) for bed availability from Gamma GLMs on TBC case age",
    x = "Multiplicative Effect on Mean Age", y = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "24_sensitivity_shift_measures.png"),
       p5, width = 9, height = 5, dpi = 150)
cat("  Saved: 24_sensitivity_shift_measures.png\n")

# --- Fig 6: Population heatmap — availability by population × day -------------
if (!is.null(pop_beds)) {
  pop_plot <- pop_beds %>%
    filter(date >= as.Date("2026-01-10"), date <= as.Date("2026-02-23"))

  p6 <- ggplot(pop_plot, aes(x = date, y = population,
                               fill = pop_utilization_s2)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient2(
      low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
      midpoint = 0.9, limits = c(0.5, 1),
      labels = percent_format(),
      na.value = "grey80"
    ) +
    labs(
      title    = "Shelter Utilization by Population Group",
      subtitle = "Shift 2 (9am) | Darker red = higher utilization (fewer beds)",
      x = NULL, y = NULL, fill = "Utilization"
    ) +
    theme_report +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid   = element_blank()
    )

  ggsave(file.path(fig_dir, "25_population_utilization_heatmap.png"),
         p6, width = 12, height = 5, dpi = 150)
  cat("  Saved: 25_population_utilization_heatmap.png\n")
} else {
  cat("  Population heatmap skipped — no population-level data\n")
}


# =============================================================================
# STEP 7 — Limitations & Summary
# =============================================================================
cat("\n--- Step 7: Limitations & Summary ---\n\n")

cat("  LIMITATIONS (document for stakeholder audience):\n")
cat("  1. SIMULTANEITY: Bed availability affects ASH operations AND\n")
cat("     ASH operations affect bed counts. Shift 2 (9am) partially\n")
cat("     addresses this by capturing availability before most dispatch.\n")
cat("  2. SAMPLE SIZE: Only 45 overlapping days with staffing data.\n")
cat("     Coefficients should be interpreted cautiously.\n")
cat("  3. REPORTING: Not all sites report all shifts. Shift 2 has\n")
cat("     the best reporting coverage (consistent staffing at 9am).\n")
cat("  4. AGGREGATION: System-level count masks site-level variation.\n")
cat("     A site with 0 beds might be offset by one with 50.\n")
cat("  5. POPULATION MATCHING: TBC placements are population-matched,\n")
cat("     but system-level aggregate doesn't differentiate. Population-\n")
cat("     specific models (M3) are deferred until mapping is confirmed.\n")

cat("\n==========================================================\n")
cat("  Bed Constraint Analysis Complete\n")
cat("  Primary model: M1 (Shift 2, Gamma GLM)\n")
cat("  Sensitivity: M4 (Shift 1), M5 (Shift 3), M6 (Latest)\n")
cat("  STAR compliance: M2 (binary logistic)\n")
cat(sprintf("  Figures saved to: %s\n", fig_dir))
cat("==========================================================\n")
