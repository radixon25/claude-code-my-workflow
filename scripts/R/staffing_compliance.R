# =============================================================================
# staffing_compliance.R — Staffing Levels → Case Age & Compliance Analysis
# =============================================================================
# Companion to staffing_impact.R: same staffing inputs + 24h window design,
# but the dependent variables are case AGE (time to closure) and COMPLIANCE
# tier instead of closure counts.
#
# OUTCOMES:
#   Age         — days from creation to closure (Gamma GLM, continuous)
#   Compliance  — STAR (<3h) | City (<20h) | Non-Compliant (20h+)
#                 (Ordinal logistic + binary logistic for STAR target)
#
# SCOPE: TBH (Senior Well-Being) and TBG (Crisis Referral) only
#
# SHIFT SCHEDULE:  AM = 7am-3pm | PM = 3pm-11pm | OVN = 11pm-7am
# FIELD ROLES:     Intake Drivers (paired), Dispatchers
#                  (Supervisors constant at 1/shift — excluded from models)
#
# CAVEATS:
#   - Created Date lacks timestamps — 24h window anchored to closure shift
#   - TBG has only 8 non-compliant cases — ordinal model may be unstable
#   - TBH has 92 non-compliant — more reliable for compliance modeling
#   - Age is right-skewed with zero-inflated floor (cases closed same hour)
#   - Staffing = scheduled minus call-offs, not actual hours worked
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(MASS)  # polr() for ordinal logistic

# Project root
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

cat("\n==========================================================\n")
cat("  Staffing Impact on Case Age & Compliance\n")
cat("  TBH (Senior) + TBG (Crisis) | Field Ops Staffing\n")
cat("  Shift Schedule: AM 7a-3p | PM 3p-11p | OVN 11p-7a\n")
cat("==========================================================\n\n")


# =============================================================================
# STEP 1 — Data Preparation (mirrors staffing_impact.R)
# =============================================================================
cat("--- Step 1: Data Preparation ---\n\n")

# --- 1a: Shift-level staffing ------------------------------------------------
staffing_path <- here("data", "inputs", "staffing_log", "clean",
                      "staffing_daily_levels_2026.csv")
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

# --- 1b: Shift sequence + 24h window -----------------------------------------
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

cat("  Shift-level staffing:", nrow(shift_staffing), "shift-days loaded\n")

# --- 1c: Case-level data (TBH + TBG, closed, with age) -----------------------
sf_path <- "C:/Users/sagea/Desktop/A Safe Haven/Data/Raw Base Exports/SR_FQ_Y2026.csv"
sf_raw <- read_csv(sf_path, show_col_types = FALSE)

cases <- sf_raw %>%
  distinct(`Work Order Number`, .keep_all = TRUE) %>%
  filter(
    Status == "Closed",
    !is.na(`End Date`),
    `Work Order: Type Short Code` %in% c("TBH", "TBG")
  ) %>%
  mutate(
    closure_dt   = mdy_hm(`End Date`),
    closure_date = as.Date(closure_dt),
    closure_hour = hour(closure_dt),
    case_type    = `Work Order: Type Short Code`,
    age_days     = as.numeric(Age)
  ) %>%
  filter(!is.na(closure_date), !is.na(age_days))

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

# Compliance tiers (from SLA_Compliance_Map.xlsx)
cases <- cases %>%
  mutate(
    compliance = case_when(
      age_days < 3/24  ~ "STAR Compliant",
      age_days < 20/24 ~ "City Compliant",
      TRUE             ~ "Non-Compliant"
    ),
    # Ordered factor: higher = better
    compliance_ord = factor(compliance,
                            levels = c("Non-Compliant", "City Compliant",
                                       "STAR Compliant"),
                            ordered = TRUE),
    # Binary: STAR vs not
    is_star = as.integer(compliance == "STAR Compliant"),
    # Binary: compliant (STAR or City) vs non-compliant
    is_compliant = as.integer(compliance != "Non-Compliant"),
    # Age in hours for interpretability
    age_hours = age_days * 24
  )

cat("  TBH + TBG closed cases:", nrow(cases), "\n")
cat("  Case type:\n")
print(table(cases$case_type))

# --- 1d: Join cases to shift-level staffing -----------------------------------
case_df <- cases %>%
  inner_join(
    shift_staffing %>% select(date, shift_code, intake_field_pairs, dispatcher,
                               field_staff, pairs_24h, disp_24h, field_24h),
    by = c("staffing_date" = "date", "closure_shift" = "shift_code")
  ) %>%
  mutate(
    weekend      = as.integer(wday(closure_date) %in% c(1, 7)),
    shift_factor = factor(closure_shift, levels = c("AM", "PM", "OVN"))
  ) %>%
  filter(!is.na(pairs_24h))  # need complete 24h window

cat("  Cases with staffing linkage:", nrow(case_df), "\n")
cat("  By type:\n")
print(table(case_df$case_type))
cat("\n")

# Split for type-specific models
tbh <- case_df %>% filter(case_type == "TBH")
tbg <- case_df %>% filter(case_type == "TBG")


# =============================================================================
# STEP 2 — Descriptive Summary
# =============================================================================
cat("--- Step 2: Descriptive Summary ---\n\n")

# Age distribution by type
cat("  AGE (hours) by Case Type:\n")
case_df %>%
  group_by(case_type) %>%
  summarise(
    n          = n(),
    mean_hrs   = mean(age_hours),
    median_hrs = median(age_hours),
    sd_hrs     = sd(age_hours),
    p25_hrs    = quantile(age_hours, 0.25),
    p75_hrs    = quantile(age_hours, 0.75),
    min_hrs    = min(age_hours),
    max_hrs    = max(age_hours),
    .groups    = "drop"
  ) %>%
  as.data.frame() %>%
  print(digits = 2, row.names = FALSE)
cat("\n")

# Compliance breakdown by type
cat("  COMPLIANCE by Case Type:\n")
case_df %>%
  count(case_type, compliance) %>%
  group_by(case_type) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("\n")

# Age by shift
cat("  AGE (hours) by Closure Shift:\n")
case_df %>%
  group_by(case_type, closure_shift) %>%
  summarise(n = n(), mean_hrs = mean(age_hours), median_hrs = median(age_hours),
            .groups = "drop") %>%
  as.data.frame() %>%
  print(digits = 2, row.names = FALSE)
cat("\n")

# Compliance by shift
cat("  STAR Compliance Rate by Shift:\n")
case_df %>%
  group_by(case_type, closure_shift) %>%
  summarise(n = n(), star_rate = mean(is_star), compliant_rate = mean(is_compliant),
            .groups = "drop") %>%
  mutate(star_pct = round(star_rate * 100, 1),
         compliant_pct = round(compliant_rate * 100, 1)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("\n")

# Age by 24h staffing quartiles
cat("  AGE (hours) by 24h Field Staffing Quartile:\n")
case_df %>%
  mutate(staff_q = cut(field_24h,
                       breaks = quantile(field_24h, probs = c(0, .25, .5, .75, 1)),
                       include.lowest = TRUE, labels = c("Q1 (low)", "Q2", "Q3", "Q4 (high)"))) %>%
  group_by(case_type, staff_q) %>%
  summarise(n = n(), mean_hrs = mean(age_hours), median_hrs = median(age_hours),
            star_rate = round(mean(is_star) * 100, 1), .groups = "drop") %>%
  as.data.frame() %>%
  print(digits = 2, row.names = FALSE)
cat("\n")


# =============================================================================
# STEP 3 — Correlation Analysis
# =============================================================================
cat("--- Step 3: Correlations: Staffing × Age ---\n\n")

staff_vars <- c("pairs_24h", "disp_24h", "field_24h",
                "intake_field_pairs", "dispatcher")

cor_age <- expand.grid(
  staff_var = staff_vars,
  outcome   = c("age_hours_TBH", "age_hours_TBG"),
  stringsAsFactors = FALSE
)
cor_age$r <- NA_real_
cor_age$p_value <- NA_real_

for (i in seq_len(nrow(cor_age))) {
  dtype <- ifelse(grepl("TBH", cor_age$outcome[i]), "TBH", "TBG")
  subset <- case_df %>% filter(case_type == dtype)
  x <- subset[[cor_age$staff_var[i]]]
  y <- subset$age_hours
  if (length(unique(x)) > 1 && length(unique(y)) > 1) {
    test <- cor.test(x, y, method = "pearson")
    cor_age$r[i]       <- test$estimate
    cor_age$p_value[i] <- test$p.value
  }
}

cor_age$sig <- ifelse(is.na(cor_age$p_value), "",
               ifelse(cor_age$p_value < 0.001, "***",
               ifelse(cor_age$p_value < 0.01,  "**",
               ifelse(cor_age$p_value < 0.05,  "*",
               ifelse(cor_age$p_value < 0.10,  ".",  "")))))

cor_age_wide <- cor_age %>%
  mutate(label = ifelse(is.na(r), "—", sprintf("%.3f%s", r, sig))) %>%
  select(staff_var, outcome, label) %>%
  pivot_wider(names_from = outcome, values_from = label)

cat("  Pearson r: Staffing × Case Age (hours)\n")
print(as.data.frame(cor_age_wide), row.names = FALSE)
cat("  Signif: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")


# =============================================================================
# STEP 4 — Case Age Models (Gamma GLM, log link)
# =============================================================================
cat("--- Step 4: Case Age Models (Gamma GLM) ---\n\n")
cat("  DV = age in hours | Gamma distribution with log link\n")
cat("  Coefficients: exp(β) = multiplicative effect on mean age\n")
cat("  exp(β) < 1 → faster closure | exp(β) > 1 → slower closure\n\n")

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

# Add small constant to zero-age cases to avoid Gamma issues
tbh_age <- tbh %>% mutate(age_hours = pmax(age_hours, 0.01))
tbg_age <- tbg %>% mutate(age_hours = pmax(age_hours, 0.01))

# M1: TBH Age ~ 24h window + shift + weekend
m1 <- glm(age_hours ~ pairs_24h + disp_24h + shift_factor + weekend,
           data = tbh_age, family = Gamma(link = "log"))
m1_out <- print_gamma(m1, "TBH Age (hours) ~ 24h Staffing + Shift + Weekend")

# M2: TBG Age ~ 24h window + shift + weekend
m2 <- glm(age_hours ~ pairs_24h + disp_24h + shift_factor + weekend,
           data = tbg_age, family = Gamma(link = "log"))
m2_out <- print_gamma(m2, "TBG Age (hours) ~ 24h Staffing + Shift + Weekend")

# M3: Combined TBH+TBG with type interaction
combined_age <- bind_rows(tbh_age, tbg_age)
m3 <- glm(age_hours ~ (pairs_24h + disp_24h) * case_type +
             shift_factor + weekend,
           data = combined_age, family = Gamma(link = "log"))
m3_out <- print_gamma(m3,
  "TBH+TBG Age ~ (24h Staffing × Type) + Shift + Weekend")


# =============================================================================
# STEP 5 — Compliance Models
# =============================================================================
cat("--- Step 5: Compliance Models ---\n\n")

print_logistic <- function(model, label, ordinal = FALSE) {
  cat(sprintf("  MODEL: %s\n", label))
  if (ordinal) {
    cat(sprintf("  Formula: %s\n",
                paste(deparse(model$call$formula), collapse = " ")))
    coefs <- coef(summary(model))
    or <- exp(coefs[, "Value"])
    or_lo <- exp(coefs[, "Value"] - 1.96 * coefs[, "Std. Error"])
    or_hi <- exp(coefs[, "Value"] + 1.96 * coefs[, "Std. Error"])
    p_val <- 2 * pnorm(abs(coefs[, "t value"]), lower.tail = FALSE)
    out <- data.frame(
      term     = rownames(coefs),
      estimate = coefs[, "Value"],
      std_err  = coefs[, "Std. Error"],
      OR       = or,
      OR_lo95  = or_lo,
      OR_hi95  = or_hi,
      p_value  = p_val,
      row.names = NULL
    )
  } else {
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
  }
  print(out, digits = 4, row.names = FALSE)
  cat(sprintf("  AIC: %.1f\n", AIC(model)))
  cat("\n")
  invisible(out)
}

# --- 5a: Ordinal logistic (proportional odds) --------------------------------
cat("  ORDINAL LOGISTIC: P(higher compliance tier) ~ staffing\n")
cat("  Ordered: Non-Compliant < City Compliant < STAR Compliant\n")
cat("  OR > 1 → higher staffing increases odds of better compliance\n\n")

# M4: TBH ordinal compliance ~ 24h staffing + shift + weekend
m4 <- polr(compliance_ord ~ pairs_24h + disp_24h + shift_factor + weekend,
            data = tbh, Hess = TRUE)
m4_out <- print_logistic(m4, "TBH Compliance (Ordinal) ~ 24h Staffing + Shift + Weekend",
                          ordinal = TRUE)

# M5: TBG ordinal — may warn due to small n in non-compliant
tryCatch({
  m5 <- polr(compliance_ord ~ pairs_24h + disp_24h + shift_factor + weekend,
              data = tbg, Hess = TRUE)
  m5_out <- print_logistic(m5, "TBG Compliance (Ordinal) ~ 24h Staffing + Shift + Weekend",
                            ordinal = TRUE)
}, error = function(e) {
  cat("  TBG ordinal model did not converge (only 8 non-compliant cases).\n")
  cat("  Falling back to binary models only.\n\n")
})

# --- 5b: Binary logistic — STAR Compliant (< 3h) vs rest ---------------------
cat("  BINARY LOGISTIC: P(STAR Compliant) ~ staffing\n")
cat("  OR > 1 → higher staffing increases odds of STAR compliance\n\n")

# M6: TBH P(STAR) ~ 24h staffing + shift + weekend
m6 <- glm(is_star ~ pairs_24h + disp_24h + shift_factor + weekend,
           data = tbh, family = binomial)
m6_out <- print_logistic(m6, "TBH P(STAR Compliant) ~ 24h Staffing + Shift + Weekend")

# M7: TBG P(STAR) ~ 24h staffing + shift + weekend
m7 <- glm(is_star ~ pairs_24h + disp_24h + shift_factor + weekend,
           data = tbg, family = binomial)
m7_out <- print_logistic(m7, "TBG P(STAR Compliant) ~ 24h Staffing + Shift + Weekend")

# --- 5c: Binary logistic — Compliant (STAR or City) vs Non-Compliant ---------
cat("  BINARY LOGISTIC: P(Compliant) ~ staffing\n")
cat("  Compliant = STAR or City (<20h) | Non-Compliant = 20h+\n\n")

# M8: TBH P(Compliant) ~ 24h staffing + shift + weekend
m8 <- glm(is_compliant ~ pairs_24h + disp_24h + shift_factor + weekend,
           data = tbh, family = binomial)
m8_out <- print_logistic(m8, "TBH P(Compliant) ~ 24h Staffing + Shift + Weekend")

# TBG: only 8 non-compliant — note this for completeness
if (sum(tbg$is_compliant == 0) >= 5) {
  m9 <- glm(is_compliant ~ pairs_24h + disp_24h + shift_factor + weekend,
             data = tbg, family = binomial)
  m9_out <- print_logistic(m9, "TBG P(Compliant) ~ 24h Staffing + Shift + Weekend")
} else {
  cat("  TBG P(Compliant) skipped — only", sum(tbg$is_compliant == 0),
      "non-compliant cases.\n\n")
}


# =============================================================================
# STEP 6 — Summary Export
# =============================================================================
cat("--- Step 6: Summary Export ---\n\n")

extract_coefs <- function(model, model_name, ordinal = FALSE) {
  if (ordinal) {
    coefs <- coef(summary(model))
    p_val <- 2 * pnorm(abs(coefs[, "t value"]), lower.tail = FALSE)
    data.frame(
      model     = model_name,
      term      = rownames(coefs),
      estimate  = coefs[, "Value"],
      std_error = coefs[, "Std. Error"],
      effect    = exp(coefs[, "Value"]),
      lo95      = exp(coefs[, "Value"] - 1.96 * coefs[, "Std. Error"]),
      hi95      = exp(coefs[, "Value"] + 1.96 * coefs[, "Std. Error"]),
      p_value   = p_val,
      row.names = NULL
    )
  } else {
    coefs <- summary(model)$coefficients
    p_col <- if ("Pr(>|t|)" %in% colnames(coefs)) "Pr(>|t|)" else "Pr(>|z|)"
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
}

all_coefs <- bind_rows(
  extract_coefs(m1, "M1_TBH_age_gamma"),
  extract_coefs(m2, "M2_TBG_age_gamma"),
  extract_coefs(m3, "M3_combined_age_gamma"),
  extract_coefs(m4, "M4_TBH_compliance_ordinal", ordinal = TRUE),
  extract_coefs(m6, "M6_TBH_star_binary"),
  extract_coefs(m7, "M7_TBG_star_binary"),
  extract_coefs(m8, "M8_TBH_compliant_binary")
)

# Add TBG ordinal and compliance if they exist
if (exists("m5")) all_coefs <- bind_rows(all_coefs,
  extract_coefs(m5, "M5_TBG_compliance_ordinal", ordinal = TRUE))
if (exists("m9")) all_coefs <- bind_rows(all_coefs,
  extract_coefs(m9, "M9_TBG_compliant_binary"))

csv_path <- here("data", "inputs", "staffing_log", "clean",
                 "staffing_compliance_summary_2026.csv")
write_csv(all_coefs, csv_path)
cat("  All model coefficients written to:", csv_path, "\n\n")


# =============================================================================
# STEP 7 — Visualizations
# =============================================================================
cat("--- Step 7: Generating Visualizations ---\n\n")

# --- Fig 1: Age distribution by shift (violin + jitter) ----------------------
p1 <- ggplot(case_df, aes(x = shift_factor, y = age_hours, fill = case_type)) +
  geom_violin(alpha = 0.4, position = position_dodge(width = 0.8),
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(aes(color = case_type), alpha = 0.3, size = 1,
              position = position_jitterdodge(jitter.width = 0.15,
                                              dodge.width = 0.8)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "#2ca25f",
             linewidth = 0.7) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#d7191c",
             linewidth = 0.7) +
  annotate("text", x = 0.55, y = 3, label = "STAR (3h)", hjust = 0,
           vjust = -0.5, color = "#2ca25f", size = 3) +
  annotate("text", x = 0.55, y = 20, label = "City (20h)", hjust = 0,
           vjust = -0.5, color = "#d7191c", size = 3) +
  scale_fill_manual(values = c(TBH = "#d7191c", TBG = "#fdae61")) +
  scale_color_manual(values = c(TBH = "#d7191c", TBG = "#fdae61")) +
  coord_cartesian(ylim = c(0, min(max(case_df$age_hours), 40))) +
  labs(
    title    = "Case Age by Closure Shift",
    subtitle = "TBH (Senior) + TBG (Crisis) | Dashed = compliance thresholds",
    x = "Closure Shift", y = "Age (hours)",
    fill = "Case Type", color = "Case Type"
  ) +
  theme_report

ggsave(file.path(fig_dir, "07_age_by_shift_violin.png"),
       p1, width = 9, height = 6, dpi = 150)
cat("  Saved: 07_age_by_shift_violin.png\n")

# --- Fig 2: Compliance rates by shift (stacked bar) --------------------------
comp_by_shift <- case_df %>%
  count(case_type, closure_shift, compliance) %>%
  group_by(case_type, closure_shift) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    compliance = factor(compliance,
                        levels = c("Non-Compliant", "City Compliant",
                                   "STAR Compliant")),
    closure_shift = factor(closure_shift, levels = c("AM", "PM", "OVN"))
  )

p2 <- ggplot(comp_by_shift, aes(x = closure_shift, y = pct, fill = compliance)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~case_type) +
  scale_fill_manual(values = c("STAR Compliant"  = "#2ca25f",
                                "City Compliant"   = "#fdae61",
                                "Non-Compliant"    = "#d7191c")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Compliance Rates by Closure Shift",
    subtitle = "STAR (<3h) | City (<20h) | Non-Compliant (20h+)",
    x = "Closure Shift", y = NULL, fill = NULL
  ) +
  theme_report +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(fig_dir, "08_compliance_by_shift.png"),
       p2, width = 9, height = 5, dpi = 150)
cat("  Saved: 08_compliance_by_shift.png\n")

# --- Fig 3: 24h staffing vs age scatter (TBH + TBG) --------------------------
p3 <- ggplot(case_df, aes(x = field_24h, y = age_hours)) +
  geom_jitter(aes(color = case_type, shape = factor(weekend)),
              width = 0.3, alpha = 0.5, size = 2) +
  geom_smooth(aes(color = case_type), method = "glm",
              method.args = list(family = Gamma(link = "log")),
              se = TRUE, linewidth = 1) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "#2ca25f") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#d7191c") +
  scale_color_manual(values = c(TBH = "#d7191c", TBG = "#fdae61")) +
  scale_shape_manual(values = c("0" = 16, "1" = 17),
                     labels = c("Weekday", "Weekend")) +
  coord_cartesian(ylim = c(0, min(max(case_df$age_hours), 50))) +
  labs(
    title    = "24h Field Staffing vs. Case Age",
    subtitle = "Gamma regression fit | Dashed = compliance thresholds",
    x = "Field Staff (24h Window)", y = "Case Age (hours)",
    color = "Case Type", shape = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "09_scatter_24h_staff_age.png"),
       p3, width = 9, height = 6, dpi = 150)
cat("  Saved: 09_scatter_24h_staff_age.png\n")

# --- Fig 4: Forest plot — Age model coefficients (M1 TBH, M2 TBG) -----------
forest_age <- bind_rows(
  m1_out %>% mutate(model = "TBH Age (M1)"),
  m2_out %>% mutate(model = "TBG Age (M2)")
) %>%
  filter(!term %in% c("(Intercept)")) %>%
  mutate(
    term_label = case_when(
      term == "pairs_24h"       ~ "Intake Pairs (24h)",
      term == "disp_24h"        ~ "Dispatchers (24h)",
      term == "shift_factorPM"  ~ "PM Shift",
      term == "shift_factorOVN" ~ "OVN Shift",
      term == "weekend"         ~ "Weekend",
      TRUE                      ~ term
    ),
    term_label = factor(term_label, levels = rev(unique(term_label)))
  )

p4 <- ggplot(forest_age, aes(x = exp_beta, y = term_label, color = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = exp_lo95, xmax = exp_hi95),
                 height = 0.2, linewidth = 0.8,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("TBH Age (M1)" = "#d7191c",
                                 "TBG Age (M2)" = "#fdae61")) +
  labs(
    title    = "Staffing Effects on Case Age (Time to Closure)",
    subtitle = "Gamma GLM | exp(β): <1 = faster closure, >1 = slower closure",
    x = "Multiplicative Effect on Mean Age", y = NULL, color = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "10_forest_age_models.png"),
       p4, width = 9, height = 5, dpi = 150)
cat("  Saved: 10_forest_age_models.png\n")

# --- Fig 5: Forest plot — STAR compliance ORs (M6 TBH, M7 TBG) ---------------
forest_star <- bind_rows(
  m6_out %>% mutate(model = "TBH STAR (M6)"),
  m7_out %>% mutate(model = "TBG STAR (M7)")
) %>%
  filter(!term %in% c("(Intercept)")) %>%
  mutate(
    term_label = case_when(
      term == "pairs_24h"       ~ "Intake Pairs (24h)",
      term == "disp_24h"        ~ "Dispatchers (24h)",
      term == "shift_factorPM"  ~ "PM Shift",
      term == "shift_factorOVN" ~ "OVN Shift",
      term == "weekend"         ~ "Weekend",
      TRUE                      ~ term
    ),
    term_label = factor(term_label, levels = rev(unique(term_label)))
  )

p5 <- ggplot(forest_star, aes(x = OR, y = term_label, color = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = OR_lo95, xmax = OR_hi95),
                 height = 0.2, linewidth = 0.8,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("TBH STAR (M6)" = "#d7191c",
                                 "TBG STAR (M7)" = "#fdae61")) +
  labs(
    title    = "Staffing Effects on STAR Compliance (<3h)",
    subtitle = "Binary logistic | OR > 1 = higher odds of STAR compliance",
    x = "Odds Ratio", y = NULL, color = NULL
  ) +
  theme_report

ggsave(file.path(fig_dir, "11_forest_star_compliance.png"),
       p5, width = 9, height = 5, dpi = 150)
cat("  Saved: 11_forest_star_compliance.png\n")

# --- Fig 6: Compliance mosaic — staffing quartile × compliance tier -----------
mosaic_data <- case_df %>%
  mutate(
    staff_q = cut(field_24h,
                  breaks = quantile(field_24h, probs = c(0, .25, .5, .75, 1)),
                  include.lowest = TRUE,
                  labels = c("Q1\n(lowest)", "Q2", "Q3", "Q4\n(highest)")),
    compliance = factor(compliance,
                        levels = c("STAR Compliant", "City Compliant",
                                   "Non-Compliant"))
  ) %>%
  count(case_type, staff_q, compliance) %>%
  group_by(case_type, staff_q) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

p6 <- ggplot(mosaic_data, aes(x = staff_q, y = pct, fill = compliance)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~case_type) +
  scale_fill_manual(values = c("STAR Compliant"  = "#2ca25f",
                                "City Compliant"   = "#fdae61",
                                "Non-Compliant"    = "#d7191c")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Compliance by 24h Staffing Level",
    subtitle = "Cases grouped by field staffing quartile in their 24h window",
    x = "24h Field Staffing Quartile", y = NULL, fill = NULL
  ) +
  theme_report +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(fig_dir, "12_compliance_by_staffing_quartile.png"),
       p6, width = 9, height = 5, dpi = 150)
cat("  Saved: 12_compliance_by_staffing_quartile.png\n")

cat("\n==========================================================\n")
cat("  Analysis complete.\n")
cat("==========================================================\n")
