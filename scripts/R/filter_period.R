# ============================================================================
# filter_period.R
# S.T.A.R. Program Reporting - Period Filtering
# ============================================================================
# Purpose: Filter full dataset to specific reporting periods
# Source this AFTER Reshape.R and BEFORE run_report()
#
# Integration point in build_data.R:
#   source(here("R", "config.R"))
#   source(here("R", "SR_FQ_Reshape.R"))
#   source(here("R", "filter_period.R"))
#   source(here("R", "SR_Report_Framework.R"))
#
#   df_wide <- reshape_flex_data(...)
#   df_wide <- assign_compliance(...)
#   df_wide <- filter_by_period(...)
#   report  <- run_report(df_wide)
#
# Supported report_type values:
#   "weekly"    — report_period = 1-53 (ISO week number)
#   "monthly"   — report_period = 1-12 (month number)
#   "quarterly" — report_period = 1-4  (quarter number)
#   "yearly"    — report_period = NULL (uses report_year only)
# ============================================================================

library(data.table)
library(lubridate)

# ============================================================================
# MAIN FILTERING FUNCTION
# ============================================================================

filter_by_period <- function(dt,
                             report_type,
                             report_period,
                             report_year = 2026,
                             date_col = "created_date") {

  # Validate inputs
  valid_types <- c("weekly", "monthly", "quarterly", "yearly")
  if (!report_type %in% valid_types) {
    stop("report_type must be one of: ", paste(valid_types, collapse = ", "))
  }

  # Ensure date column exists
  if (!date_col %in% names(dt)) {
    stop("Date column '", date_col, "' not found in data")
  }

  # Work on a copy to avoid modifying original
  dt <- copy(dt)

  # Ensure date column is Date type (handle both Date and character)
  if (!inherits(dt[[date_col]], "Date")) {
    dt[, (date_col) := as.Date(get(date_col), format = "%m/%d/%Y")]
  }

  # Store original count for logging
  n_original <- nrow(dt)

  # Apply filter based on report type
  filtered <- switch(report_type,

    "weekly" = {
      if (!report_period %in% 1:53) stop("For weekly reports, report_period must be 1-53")
      dt[lubridate::year(get(date_col)) == report_year &
           lubridate::isoweek(get(date_col)) == report_period]
    },

    "monthly" = {
      if (!report_period %in% 1:12) stop("For monthly reports, report_period must be 1-12")
      dt[lubridate::year(get(date_col)) == report_year &
           lubridate::month(get(date_col)) == report_period]
    },

    "quarterly" = {
      if (!report_period %in% 1:4) stop("For quarterly reports, report_period must be 1-4")
      start_month <- (report_period - 1) * 3 + 1
      end_month <- report_period * 3
      dt[lubridate::year(get(date_col)) == report_year &
           lubridate::month(get(date_col)) >= start_month &
           lubridate::month(get(date_col)) <= end_month]
    },

    "yearly" = {
      dt[lubridate::year(get(date_col)) == report_year]
    }
  )

  # Log filtering results
  message(sprintf("Filtered to %s: %d records (from %d total)",
                  get_period_label(report_type, report_period, report_year),
                  nrow(filtered),
                  n_original))

  return(filtered)
}

# ============================================================================
# PERIOD LABEL FUNCTIONS
# ============================================================================

get_period_label <- function(report_type, report_period, report_year = 2026) {

  month_names <- c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")

  switch(report_type,
         "weekly"    = paste0("Week ", report_period, ", ", report_year),
         "monthly"   = paste(month_names[report_period], report_year),
         "quarterly" = paste0("Q", report_period, " ", report_year),
         "yearly"    = as.character(report_year)
  )
}

get_folder_name <- function(report_type, report_period, report_year = 2026) {

  month_names <- c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")

  switch(report_type,
         "weekly"    = paste0(report_year, "-W", sprintf("%02d", report_period)),
         "monthly"   = paste0(report_year, "-", sprintf("%02d", report_period), "_", month_names[report_period]),
         "quarterly" = paste0(report_year, "-Q", report_period),
         "yearly"    = as.character(report_year)
  )
}

get_report_title <- function(report_type, report_period, report_year = 2026) {

  label <- get_period_label(report_type, report_period, report_year)

  switch(report_type,
         "weekly"    = paste0("S.T.A.R. Program Weekly Report: ", label),
         "monthly"   = paste0("S.T.A.R. Program Monthly Report: ", label),
         "quarterly" = paste0("S.T.A.R. Program Quarterly Report: ", label),
         "yearly"    = paste0("S.T.A.R. Program Annual Report: ", label)
  )
}

# ============================================================================
# DATE RANGE FUNCTIONS (useful for report text)
# ============================================================================

get_period_start <- function(report_type, report_period, report_year = 2026) {

  switch(report_type,
         "weekly" = {
           # ISO week 1 starts on the Monday of the week containing Jan 4
           jan4 <- as.Date(paste(report_year, 1, 4, sep = "-"))
           week1_monday <- jan4 - (lubridate::wday(jan4, week_start = 1) - 1)
           week1_monday + (report_period - 1) * 7
         },
         "monthly"   = as.Date(paste(report_year, report_period, 1, sep = "-")),
         "quarterly" = as.Date(paste(report_year, (report_period - 1) * 3 + 1, 1, sep = "-")),
         "yearly"    = as.Date(paste(report_year, 1, 1, sep = "-"))
  )
}

get_period_end <- function(report_type, report_period, report_year = 2026) {

  switch(report_type,
         "weekly" = {
           get_period_start(report_type, report_period, report_year) + 6
         },
         "monthly"   = ceiling_date(as.Date(paste(report_year, report_period, 1, sep = "-")), "month") - 1,
         "quarterly" = ceiling_date(as.Date(paste(report_year, report_period * 3, 1, sep = "-")), "month") - 1,
         "yearly"    = as.Date(paste(report_year, 12, 31, sep = "-"))
  )
}

get_period_range_text <- function(report_type, report_period, report_year = 2026) {

  start_date <- get_period_start(report_type, report_period, report_year)
  end_date <- get_period_end(report_type, report_period, report_year)

  paste(format(start_date, "%B %d, %Y"), "to", format(end_date, "%B %d, %Y"))
}

# ============================================================================
# COMPARISON PERIOD FUNCTION (for prior period analysis)
# ============================================================================

get_prior_period <- function(report_type, report_period, report_year = 2026) {

  if (report_type == "weekly") {
    if (report_period == 1) {
      # Roll back to last ISO week of prior year
      dec31 <- as.Date(paste(report_year - 1, 12, 31, sep = "-"))
      last_week <- lubridate::isoweek(dec31)
      return(list(report_period = last_week, report_year = report_year - 1))
    } else {
      return(list(report_period = report_period - 1, report_year = report_year))
    }

  } else if (report_type == "monthly") {
    if (report_period == 1) {
      return(list(report_period = 12, report_year = report_year - 1))
    } else {
      return(list(report_period = report_period - 1, report_year = report_year))
    }

  } else if (report_type == "quarterly") {
    if (report_period == 1) {
      return(list(report_period = 4, report_year = report_year - 1))
    } else {
      return(list(report_period = report_period - 1, report_year = report_year))
    }

  } else if (report_type == "yearly") {
    return(list(report_period = NULL, report_year = report_year - 1))
  }
}

# ============================================================================
# VALIDATION FUNCTION
# ============================================================================

validate_period_params <- function(report_type, report_period, report_year) {

  errors <- c()

  if (!report_type %in% c("weekly", "monthly", "quarterly", "yearly")) {
    errors <- c(errors, "Invalid report_type")
  }

  if (report_type == "weekly" && !report_period %in% 1:53) {
    errors <- c(errors, "Weekly report_period must be 1-53")
  }

  if (report_type == "monthly" && !report_period %in% 1:12) {
    errors <- c(errors, "Monthly report_period must be 1-12")
  }

  if (report_type == "quarterly" && !report_period %in% 1:4) {
    errors <- c(errors, "Quarterly report_period must be 1-4")
  }

  if (!is.numeric(report_year) || report_year < 2026) {
    errors <- c(errors, "report_year must be 2026 or later (program start)")
  }

  if (length(errors) > 0) {
    stop(paste(errors, collapse = "; "))
  }

  return(TRUE)
}
