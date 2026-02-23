# =============================================================================
# input_effects.R
# S.T.A.R. Program Reporting - Input Effects Analysis
# =============================================================================
#
# PURPOSE:
#   Functions for modeling the relationship between operational inputs
#   (staffing, weather, bed availability) and service delivery outcomes
#   (SLA compliance, demand, aging).
#
# APPROACH:
#   - Descriptive: correlation matrices, summary tables
#   - Regression: linear models for compliance/aging, Poisson GLM for demand
#   - All functions accept a pre-built analysis dataset (monthly grain)
#
# NOTE:
#   This is exploratory/research-level analysis. Model outputs should be
#   interpreted cautiously — the STAR program started January 2026,
#   so sample sizes are small in early months.
#
# USAGE:
#   source(here("R", "input_effects.R"))
#
#   inputs <- load_input_data()
#   inputs_monthly <- aggregate_inputs_monthly(inputs)
#   model_dt <- build_model_dataset(period_reports, inputs_monthly)
#   correlations <- compute_correlations(model_dt)
#
# =============================================================================

library(data.table)
library(here)


# =============================================================================
# DATA LOADING
# =============================================================================

#' Load and validate all input datasets from data/inputs/
#'
#' Reads whichever input files exist. Missing files are silently skipped
#' and noted in the output.
#'
#' @return named list with elements: staffing, weather, beds (each data.table or NULL)
load_input_data <- function() {
  staffing_path <- here("data", "inputs", "staffing_log.csv")
  weather_path  <- here("data", "inputs", "weather_daily.csv")
  beds_path     <- here("data", "inputs", "bed_tracker_monthly.csv")

  inputs <- list(staffing = NULL, weather = NULL, beds = NULL)

  if (file.exists(staffing_path)) {
    inputs$staffing <- fread(staffing_path)
    inputs$staffing[, date := as.Date(date)]
    message("  Loaded staffing data: ", nrow(inputs$staffing), " rows")
  } else {
    message("  Staffing data not found at: ", staffing_path)
  }

  if (file.exists(weather_path)) {
    inputs$weather <- fread(weather_path)
    inputs$weather[, date := as.Date(date)]
    message("  Loaded weather data: ", nrow(inputs$weather), " rows")
  } else {
    message("  Weather data not found at: ", weather_path)
  }

  if (file.exists(beds_path)) {
    inputs$beds <- fread(beds_path)
    message("  Loaded bed tracker data: ", nrow(inputs$beds), " rows")
  } else {
    message("  Bed tracker data not found at: ", beds_path)
  }

  inputs
}


# =============================================================================
# DATA AGGREGATION
# =============================================================================

#' Aggregate daily input datasets to monthly grain
#'
#' Staffing and weather are daily → collapsed to monthly averages/totals.
#' Beds are already monthly → collapsed across populations.
#'
#' @param inputs list from load_input_data()
#' @return data.table with one row per year-month, columns for each input metric
aggregate_inputs_monthly <- function(inputs) {
  result <- NULL

  # Staffing: daily → monthly averages
  if (!is.null(inputs$staffing)) {
    staffing_monthly <- inputs$staffing[, .(
      avg_daily_staff  = round(mean(staff_count, na.rm = TRUE), 1),
      total_staff_days = sum(staff_count, na.rm = TRUE)
    ), by = .(year = year(date), month = month(date))]

    result <- staffing_monthly
  }

  # Weather: daily → monthly summaries
  if (!is.null(inputs$weather)) {
    weather_monthly <- inputs$weather[, .(
      avg_temp     = round(mean((temp_high_f + temp_low_f) / 2, na.rm = TRUE), 1),
      total_precip = round(sum(precip_in, na.rm = TRUE), 2),
      total_snow   = round(sum(snow_in, na.rm = TRUE), 2),
      cold_days    = sum(temp_low_f < 20, na.rm = TRUE),
      severe_days  = sum(condition == "severe", na.rm = TRUE)
    ), by = .(year = year(date), month = month(date))]

    result <- if (is.null(result)) {
      weather_monthly
    } else {
      merge(result, weather_monthly, by = c("year", "month"), all = TRUE)
    }
  }

  # Beds: already monthly, aggregate across populations
  if (!is.null(inputs$beds)) {
    beds_monthly <- inputs$beds[, .(
      system_util_pct    = round(sum(avg_occupied) / sum(total_beds) * 100, 1),
      avg_available_beds = round(mean(avg_available, na.rm = TRUE), 1),
      total_capacity     = sum(total_beds)
    ), by = .(year, month)]

    result <- if (is.null(result)) {
      beds_monthly
    } else {
      merge(result, beds_monthly, by = c("year", "month"), all = TRUE)
    }
  }

  if (is.null(result)) {
    message("  No input data available to aggregate.")
    return(data.table(year = integer(), month = integer()))
  }

  return(result)
}


# =============================================================================
# MODEL DATASET
# =============================================================================

#' Build the modeling dataset: join monthly outcomes with monthly inputs
#'
#' Extracts key outcome metrics from archived run_report() outputs and joins
#' them with the aggregated input variables.
#'
#' @param period_reports named list of run_report() outputs, keyed by period label
#'        e.g. list("January 2026" = report_jan, "February 2026" = report_feb)
#' @param inputs_monthly data.table from aggregate_inputs_monthly()
#' @return data.table with one row per year-month-wo_tc
build_model_dataset <- function(period_reports, inputs_monthly) {

  outcomes <- rbindlist(imap(period_reports, function(rpt, label) {
    # Parse label like "January 2026" to year-month
    parts <- strsplit(label, " ")[[1]]
    mo <- match(parts[1], month.name)
    yr <- as.integer(parts[2])

    if (is.na(mo) || is.na(yr)) {
      warning("Could not parse period label: ", label)
      return(NULL)
    }

    # Compliance rate (pct for STAR-compliant levels)
    comp <- rpt$overall$compliance_summary
    comp_rate <- if (!is.null(comp)) {
      comp[grepl("STAR|Compliant", compliance_level, ignore.case = TRUE),
           .(wo_tc, star_pct = sum(pct))]
    } else {
      data.table(wo_tc = character(), star_pct = numeric())
    }

    # Demand totals
    demand <- rpt$overall$demand_by_type_status
    demand_total <- if (!is.null(demand)) {
      demand[, .(demand_N = sum(N)), by = wo_tc]
    } else {
      data.table(wo_tc = character(), demand_N = integer())
    }

    # Aging
    aging <- rpt$overall$aging_summary
    aging_dt <- if (!is.null(aging)) {
      aging[, .(wo_tc, avg_age, median_age)]
    } else {
      data.table(wo_tc = character(), avg_age = numeric(), median_age = numeric())
    }

    # Merge outcomes for this period
    dt <- Reduce(function(a, b) merge(a, b, by = "wo_tc", all = TRUE),
                 list(comp_rate, demand_total, aging_dt))
    dt[, `:=`(year = yr, month = mo)]
    dt
  }), fill = TRUE)

  if (nrow(outcomes) == 0) {
    message("  No outcome data extracted from period reports.")
    return(data.table())
  }

  # Join with input variables
  if (nrow(inputs_monthly) > 0) {
    model_dt <- merge(outcomes, inputs_monthly, by = c("year", "month"), all.x = TRUE)
  } else {
    model_dt <- outcomes
  }

  return(model_dt)
}


# =============================================================================
# DESCRIPTIVE ANALYSIS
# =============================================================================

#' Compute correlation matrix between inputs and outcomes
#'
#' @param model_dt data.table from build_model_dataset()
#' @return correlation matrix (numeric columns only, excluding year/month)
compute_correlations <- function(model_dt) {
  numeric_cols <- names(model_dt)[sapply(model_dt, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c("year", "month"))

  if (length(numeric_cols) < 2) {
    message("  Not enough numeric columns for correlation analysis.")
    return(NULL)
  }

  cor_matrix <- cor(model_dt[, ..numeric_cols], use = "pairwise.complete.obs")
  round(cor_matrix, 3)
}


#' Summarize each input variable
#'
#' @param inputs_monthly data.table from aggregate_inputs_monthly()
#' @return data.table with descriptive stats per variable
summarize_inputs <- function(inputs_monthly) {
  numeric_cols <- names(inputs_monthly)[sapply(inputs_monthly, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c("year", "month"))

  if (length(numeric_cols) == 0) return(NULL)

  rbindlist(lapply(numeric_cols, function(col) {
    vals <- inputs_monthly[[col]]
    data.table(
      variable = col,
      n        = sum(!is.na(vals)),
      mean     = round(mean(vals, na.rm = TRUE), 2),
      sd       = round(sd(vals, na.rm = TRUE), 2),
      min      = round(min(vals, na.rm = TRUE), 2),
      max      = round(max(vals, na.rm = TRUE), 2)
    )
  }))
}


# =============================================================================
# REGRESSION MODELS
# =============================================================================

#' Fit compliance model: what inputs predict STAR compliance rate?
#'
#' Linear model on star_pct (0-100 scale).
#' With sufficient data, consider betareg for bounded outcome.
#'
#' @param model_dt   data.table from build_model_dataset()
#' @param predictors character vector of column names to use as predictors
#' @return lm model object, or NULL if insufficient data
fit_compliance_model <- function(model_dt,
                                  predictors = c("avg_daily_staff", "avg_temp",
                                                 "cold_days", "system_util_pct")) {
  fit_model(model_dt, "star_pct", predictors, family = "gaussian")
}


#' Fit demand model: what inputs predict demand volume?
#'
#' Poisson GLM for count data.
#'
#' @param model_dt   data.table
#' @param predictors character vector
#' @return glm model object, or NULL if insufficient data
fit_demand_model <- function(model_dt,
                              predictors = c("avg_daily_staff", "avg_temp",
                                             "cold_days", "total_snow",
                                             "system_util_pct")) {
  fit_model(model_dt, "demand_N", predictors, family = "poisson")
}


#' Fit aging model: what inputs predict average case age?
#'
#' @param model_dt   data.table
#' @param predictors character vector
#' @return lm model object, or NULL if insufficient data
fit_aging_model <- function(model_dt,
                             predictors = c("avg_daily_staff", "system_util_pct",
                                            "demand_N")) {
  fit_model(model_dt, "avg_age", predictors, family = "gaussian")
}


# --- Internal fitting helper -------------------------------------------------

fit_model <- function(model_dt, outcome, predictors, family = "gaussian") {
  available <- intersect(predictors, names(model_dt))
  if (length(available) == 0) {
    message("  No predictor columns available for ", outcome, " model.")
    return(NULL)
  }

  cols_needed <- c(outcome, available)
  dt_complete <- na.omit(model_dt[, ..cols_needed])

  if (nrow(dt_complete) < 3) {
    message("  Insufficient data for ", outcome, " model (need 3+ rows, have ", nrow(dt_complete), ").")
    return(NULL)
  }

  formula_str <- paste(outcome, "~", paste(available, collapse = " + "))

  tryCatch({
    if (family == "poisson") {
      glm(as.formula(formula_str), data = dt_complete, family = poisson())
    } else {
      lm(as.formula(formula_str), data = dt_complete)
    }
  }, error = function(e) {
    message("  Model fitting failed for ", outcome, ": ", e$message)
    NULL
  })
}


# =============================================================================
# MODEL OUTPUT HELPERS
# =============================================================================

#' Extract tidy coefficient table from a fitted model
#'
#' @param model lm or glm object (or NULL)
#' @return data.table with term, estimate, std_error, statistic, p_value
tidy_model <- function(model) {
  if (is.null(model)) return(NULL)

  s <- summary(model)$coefficients
  data.table(
    term      = rownames(s),
    estimate  = round(s[, 1], 4),
    std_error = round(s[, 2], 4),
    statistic = round(s[, 3], 4),
    p_value   = round(s[, 4], 4)
  )
}
