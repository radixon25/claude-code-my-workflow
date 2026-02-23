# =============================================================================
# period_comparison.R
# S.T.A.R. Program Reporting - Period-over-Period Comparison
# =============================================================================
#
# PURPOSE:
#   Functions for comparing key metrics across reporting periods.
#   Supports two modes:
#     1. Two-period comparison (current vs. prior) — for monthly reports
#     2. Multi-period trend (all periods) — for yearly/6-month trajectory
#
# USAGE:
#   source(here("R", "period_comparison.R"))
#
#   # Two-period comparison
#   delta <- compare_two_periods(report_current, report_prior, "Feb 2026", "Jan 2026")
#
#   # Multi-period trend
#   archives <- load_period_archive("monthly", 2026)
#   trend <- build_trend(archives)
#
# DEPENDS ON:
#   - run_report() output shape from SR_Report_Framework.R
#   - get_period_label() from filter_period.R (for archive loading)
#
# =============================================================================

library(data.table)
library(purrr)
library(here)


# =============================================================================
# ARCHIVE LOADING
# =============================================================================

#' Load archived period reports from data/processed/periods/
#'
#' Scans the archive directory for RDS files matching the report type and year.
#' Returns a named list keyed by period label (e.g., "January 2026").
#'
#' @param report_type character — "monthly", "quarterly", or "weekly"
#' @param year        integer  — reporting year
#' @return named list of run_report() outputs
load_period_archive <- function(report_type, year) {
  archive_dir <- here("data", "processed", "periods")

  if (!dir.exists(archive_dir)) {
    warning("Archive directory not found: ", archive_dir)
    return(list())
  }

  # Match files like monthly_2026_01.rds, quarterly_2026_1.rds
  pattern <- sprintf("^%s_%d_\\d+\\.rds$", report_type, year)
  files <- list.files(archive_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    message("No archived ", report_type, " reports found for ", year)
    return(list())
  }

  # Extract period number from filename
  reports <- list()
  for (f in files) {
    basename_f <- basename(f)
    # Parse: type_year_period.rds
    period_num <- as.integer(sub(sprintf("^%s_%d_(\\d+)\\.rds$", report_type, year), "\\1", basename_f))
    label <- get_period_label(report_type, period_num, year)
    reports[[label]] <- readRDS(f)
  }

  # Sort by period number (chronological order)
  period_nums <- as.integer(sub(sprintf("^%s_%d_(\\d+)\\.rds$", report_type, year), "\\1", basename(files)))
  reports <- reports[order(period_nums)]

  message("Loaded ", length(reports), " archived ", report_type, " reports: ",
          paste(names(reports), collapse = ", "))
  return(reports)
}


# =============================================================================
# TWO-PERIOD COMPARISON
# =============================================================================

#' Compare two single-period report objects (current vs. prior)
#'
#' @param current       list — output of run_report() for the current period
#' @param prior         list — output of run_report() for the prior period
#' @param current_label character — e.g. "February 2026"
#' @param prior_label   character — e.g. "January 2026"
#' @return named list of comparison data.tables
compare_two_periods <- function(current, prior, current_label, prior_label) {
  list(
    compliance = compare_compliance(current, prior, current_label, prior_label),
    demand     = compare_demand(current, prior, current_label, prior_label),
    aging      = compare_aging(current, prior, current_label, prior_label),
    outcomes   = compare_outcomes(current, prior, current_label, prior_label),
    status     = compare_status(current, prior, current_label, prior_label)
  )
}


# --- Compliance Comparison ---------------------------------------------------

#' Compare SLA compliance rates between two periods
#' @return data.table: wo_tc, compliance_level, N/pct current vs prior,
#'         delta_N, delta_pct, direction ("improved"|"declined"|"unchanged")
compare_compliance <- function(current, prior, current_label, prior_label) {
  cur <- copy(current$overall$compliance_summary)
  pri <- copy(prior$overall$compliance_summary)
  if (is.null(cur) || is.null(pri)) return(NULL)

  setnames(cur, c("N", "pct"), c("N_current", "pct_current"))
  setnames(pri, c("N", "pct"), c("N_prior", "pct_prior"))

  merged <- merge(cur, pri, by = c("wo_tc", "compliance_level"), all = TRUE)
  for (col in c("N_current", "pct_current", "N_prior", "pct_prior")) {
    set(merged, which(is.na(merged[[col]])), col, 0)
  }

  merged[, `:=`(
    delta_N   = N_current - N_prior,
    delta_pct = round(pct_current - pct_prior, 1)
  )]

  # Direction: "improved" if STAR-compliant pct went up or non-compliant went down
  merged[, direction := fifelse(
    delta_pct == 0, "unchanged",
    fifelse(
      grepl("STAR|Compliant", compliance_level, ignore.case = TRUE),
      fifelse(delta_pct > 0, "improved", "declined"),
      fifelse(delta_pct < 0, "improved", "declined")
    )
  )]

  merged[, `:=`(period_current = current_label, period_prior = prior_label)]
  return(merged)
}


# --- Demand Comparison -------------------------------------------------------

#' Compare demand volumes between two periods
#' @return data.table: wo_tc, N_current, N_prior, delta_N, pct_change
compare_demand <- function(current, prior, current_label, prior_label) {
  cur <- copy(current$overall$demand_by_type_status)
  pri <- copy(prior$overall$demand_by_type_status)
  if (is.null(cur) || is.null(pri)) return(NULL)

  cur_total <- cur[, .(N_current = sum(N)), by = wo_tc]
  pri_total <- pri[, .(N_prior = sum(N)), by = wo_tc]

  merged <- merge(cur_total, pri_total, by = "wo_tc", all = TRUE)
  for (col in c("N_current", "N_prior")) {
    set(merged, which(is.na(merged[[col]])), col, 0L)
  }

  merged[, `:=`(
    delta_N    = N_current - N_prior,
    pct_change = fifelse(N_prior == 0, NA_real_,
                         round((N_current - N_prior) / N_prior * 100, 1))
  )]

  merged[, `:=`(period_current = current_label, period_prior = prior_label)]
  return(merged)
}


# --- Aging Comparison --------------------------------------------------------

#' Compare aging metrics between two periods
#' @return data.table: wo_tc, avg/median/max age current vs prior, deltas
compare_aging <- function(current, prior, current_label, prior_label) {
  cur <- copy(current$overall$aging_summary)
  pri <- copy(prior$overall$aging_summary)
  if (is.null(cur) || is.null(pri)) return(NULL)

  setnames(cur, c("count", "avg_age", "median_age", "max_age"),
           c("count_current", "avg_age_current", "median_age_current", "max_age_current"))
  setnames(pri, c("count", "avg_age", "median_age", "max_age"),
           c("count_prior", "avg_age_prior", "median_age_prior", "max_age_prior"))

  merged <- merge(cur, pri, by = "wo_tc", all = TRUE)
  merged[, `:=`(
    delta_avg    = round(avg_age_current - avg_age_prior, 1),
    delta_median = round(median_age_current - median_age_prior, 1)
  )]

  merged[, `:=`(period_current = current_label, period_prior = prior_label)]
  return(merged)
}


# --- Outcomes Comparison -----------------------------------------------------

#' Compare activity outcome distributions between two periods
#' @return data.table: wo_tc, activity_outcome, N/pct current vs prior, deltas
compare_outcomes <- function(current, prior, current_label, prior_label) {
  cur <- copy(current$overall$activity_outcomes)
  pri <- copy(prior$overall$activity_outcomes)
  if (is.null(cur) || is.null(pri)) return(NULL)

  setnames(cur, c("N", "pct"), c("N_current", "pct_current"))
  setnames(pri, c("N", "pct"), c("N_prior", "pct_prior"))

  merged <- merge(cur, pri, by = c("wo_tc", "activity_outcome"), all = TRUE)
  for (col in c("N_current", "pct_current", "N_prior", "pct_prior")) {
    set(merged, which(is.na(merged[[col]])), col, 0)
  }

  merged[, `:=`(
    delta_N   = N_current - N_prior,
    delta_pct = round(pct_current - pct_prior, 1)
  )]

  merged[, `:=`(period_current = current_label, period_prior = prior_label)]
  return(merged)
}


# --- Status Comparison -------------------------------------------------------

#' Compare status distributions between two periods
#' @return data.table: wo_tc, status, N/pct current vs prior, deltas
compare_status <- function(current, prior, current_label, prior_label) {
  cur <- copy(current$overall$demand_by_type_status)
  pri <- copy(prior$overall$demand_by_type_status)
  if (is.null(cur) || is.null(pri)) return(NULL)

  setnames(cur, c("N", "pct"), c("N_current", "pct_current"))
  setnames(pri, c("N", "pct"), c("N_prior", "pct_prior"))

  merged <- merge(cur, pri, by = c("wo_tc", "status"), all = TRUE)
  for (col in c("N_current", "pct_current", "N_prior", "pct_prior")) {
    set(merged, which(is.na(merged[[col]])), col, 0)
  }

  merged[, `:=`(
    delta_N   = N_current - N_prior,
    delta_pct = round(pct_current - pct_prior, 1)
  )]

  merged[, `:=`(period_current = current_label, period_prior = prior_label)]
  return(merged)
}


# =============================================================================
# MULTI-PERIOD TREND
# =============================================================================

#' Build wide-format trend tables from a list of period report objects
#'
#' Each trend table has one row per metric grouping (e.g., wo_tc) and one
#' column per period, ordered chronologically. A pct_change column compares
#' the last two periods.
#'
#' @param period_reports named list of run_report() outputs, keyed by label
#'        e.g. list("January 2026" = report_jan, "February 2026" = report_feb)
#' @return named list of wide-format trend data.tables
build_trend <- function(period_reports) {
  if (length(period_reports) < 2) {
    message("Need at least 2 periods for trend analysis; only ",
            length(period_reports), " available.")
    return(NULL)
  }

  # Period order is determined by the input list order (chronological from load_period_archive)
  period_order <- names(period_reports)

  list(
    demand_trend     = pivot_trend_wide(
      collect_demand_long(period_reports), "N", "wo_tc", period_order, "pct_change"
    ),
    compliance_trend = pivot_trend_wide(
      collect_compliance_long(period_reports), "pct", c("wo_tc", "compliance_level"), period_order, "pp_diff"
    ),
    aging_trend      = pivot_trend_wide(
      collect_aging_long(period_reports), "avg_age", "wo_tc", period_order, "pct_change"
    ),
    outcomes_trend   = pivot_trend_wide(
      collect_outcomes_long(period_reports), "N", c("wo_tc", "activity_outcome"), period_order, "pct_change"
    ),
    status_trend     = pivot_trend_wide(
      collect_status_long(period_reports), "pct", c("wo_tc", "status"), period_order, "pp_diff"
    )
  )
}


# --- Wide Pivot Utility ------------------------------------------------------

#' Pivot a long trend table to wide format with period columns + change column
#'
#' @param long_dt      data.table with columns: id_cols, "period", value_col
#' @param value_col    character — the metric column to spread (e.g., "N", "pct", "avg_age")
#' @param id_cols      character vector — grouping columns that define each row
#' @param period_order character vector — chronological period labels for column ordering
#' @param change_type  character — how to compute the change column:
#'                     "pct_change" = (new - old) / old * 100 — for counts and raw values
#'                     "pp_diff"    = new - old              — for metrics already in %
#' @return wide data.table with one column per period + change column
pivot_trend_wide <- function(long_dt, value_col, id_cols, period_order,
                             change_type = "pct_change") {
  if (is.null(long_dt) || nrow(long_dt) == 0) return(NULL)

  # Ensure period is ordered factor for correct column sequence
  long_dt[, period := factor(period, levels = period_order)]

  # Pivot wide: one column per period
  formula_str <- paste(paste(id_cols, collapse = " + "), "~ period")
  wide <- dcast(long_dt, as.formula(formula_str), value.var = value_col, fill = 0)

  # Calculate change between last two periods
  period_cols <- intersect(period_order, names(wide))
  if (length(period_cols) >= 2) {
    last_col <- period_cols[length(period_cols)]
    prev_col <- period_cols[length(period_cols) - 1]

    if (change_type == "pp_diff") {
      # Percentage point difference: new - old (for metrics already in %)
      wide[, pp_diff := round(get(last_col) - get(prev_col), 1)]
    } else {
      # Percent change: (new - old) / old * 100 (for counts and raw values)
      wide[, pct_change := fifelse(
        get(prev_col) == 0, NA_real_,
        round((get(last_col) - get(prev_col)) / abs(get(prev_col)) * 100, 1)
      )]
    }
  }

  return(wide)
}


# --- Long-Format Collectors (internal) ---------------------------------------

collect_demand_long <- function(period_reports) {
  rbindlist(imap(period_reports, function(rpt, label) {
    dt <- copy(rpt$overall$demand_by_type_status)
    if (is.null(dt)) return(NULL)
    totals <- dt[, .(N = sum(N)), by = wo_tc]
    totals[, period := label]
    totals
  }), fill = TRUE)
}

collect_compliance_long <- function(period_reports) {
  rbindlist(imap(period_reports, function(rpt, label) {
    dt <- copy(rpt$overall$compliance_summary)
    if (is.null(dt)) return(NULL)
    dt[, period := label]
    dt
  }), fill = TRUE)
}

collect_aging_long <- function(period_reports) {
  rbindlist(imap(period_reports, function(rpt, label) {
    dt <- copy(rpt$overall$aging_summary)
    if (is.null(dt)) return(NULL)
    dt[, period := label]
    dt
  }), fill = TRUE)
}

collect_outcomes_long <- function(period_reports) {
  rbindlist(imap(period_reports, function(rpt, label) {
    dt <- copy(rpt$overall$activity_outcomes)
    if (is.null(dt)) return(NULL)
    dt[, period := label]
    dt
  }), fill = TRUE)
}

collect_status_long <- function(period_reports) {
  rbindlist(imap(period_reports, function(rpt, label) {
    dt <- copy(rpt$overall$demand_by_type_status)
    if (is.null(dt)) return(NULL)
    dt[, period := label]
    dt
  }), fill = TRUE)
}
