# =============================================================================
# SR Reporting Framework
# =============================================================================
#
# PURPOSE:
#   Generates service request reports with two sections:
#     1. Overall Data      — standard metrics across all case types
#     2. Data by Case Type — metrics specific to each wo_tc
#
# STRUCTURE:
#   Section 1: Utilities         — shared helpers used across all functions
#   Section 2: Default Analyses  — wo_tc agnostic, run on full dataset
#   Section 3: Case-Type Analyses — wo_tc specific, run on per-type subsets
#   Section 4: Registries        — named lists that drive the report
#   Section 5: Orchestrator      — run_report() ties everything together
#
# HOW TO EXTEND:
#   - New default analysis:    write function → add to default_analyses list
#   - New case-type analysis:  write function → add to tc_analyses under the wo_tc key
#   - No changes needed to run_report() — it reads from the registries
#
# =============================================================================


library(data.table)
library(purrr)





# =============================================================================
# SECTION 1: UTILITIES
# =============================================================================
# Shared helpers. Keep these small and reusable.
# Any logic used by 2+ analysis functions belongs here.
# -----------------------------------------------------------------------------

#' Remove columns that are entirely NA or blank
drop_blank_cols <- function(dt) {
  blank_cols <- names(dt)[sapply(dt, function(x) all(is.na(x) | x == ""))]
  if (length(blank_cols) > 0) dt[, (blank_cols) := NULL]
  return(dt)
}

#' Count rows by grouping columns, sorted descending
count_by <- function(dt, by_cols) {
  dt[, .N, by = by_cols][order(-N)]
}

#' Add a percentage column to a count summary
add_pct <- function(dt, count_col = "N") {
  total <- sum(dt[[count_col]], na.rm = TRUE)
  dt[, pct := round(get(count_col) / total * 100, 1)]
}

#' Coerce age to numeric safely
parse_age <- function(dt) {
  dt <- copy(dt)
  dt[, age := as.numeric(age)]
  return(dt)
}

#' Coerce created_date to Date safely
parse_dates <- function(dt) {
  dt <- copy(dt)

  parse_any <- function(x) {
    if (inherits(x, "Date")) return(x)
    out <- as.Date(x, format = "%m/%d/%Y")
    if (all(is.na(out))) out <- as.Date(x, format = "%m/%d/%y")
    if (all(is.na(out))) out <- as.Date(x)  # fallback
    out
  }

  dt[, created_date := parse_any(created_date)]
  if ("end_date" %in% names(dt)) dt[, end_date := parse_any(end_date)]
  return(dt)
}


# =============================================================================
# SECTION 2: DEFAULT ANALYSIS FUNCTIONS
# =============================================================================
# These are wo_tc AGNOSTIC. They accept the full dataset and summarize
# across all case types. They should never reference type-specific columns
# like shelter_placed or senior_age.
#
# Contract:
#   Input:  data.table (full wide dataset)
#   Output: data.table (or named list of data.tables)
# -----------------------------------------------------------------------------

#' Demand volume by case type and status
#' Answers: How many requests exist per type, and what status are they in?
#' Percentages are calculated WITHIN each case type (not overall).
default_demand_by_type_status <- function(df) {
  result <- count_by(df, c("wo_tc", "status"))
  result[, pct := round(N / sum(N) * 100, 1), by = wo_tc]
  result
}

#' Demand volume over time
#' Answers: What's the monthly trend of incoming requests per type?
default_demand_over_time <- function(df) {
  dt <- parse_dates(df)
  dt[, created_month := format(created_date, "%Y-%m")]
  count_by(dt, c("wo_tc", "created_month"))
}

#' Aging summary by case type
#' Answers: How old are requests by type? Where are the backlogs?
default_aging_summary <- function(df) {
  dt <- parse_age(df)
  dt[, .(
    count      = .N,
    avg_age    = round(mean(age, na.rm = TRUE), 1),
    median_age = round(median(age, na.rm = TRUE), 1),
    max_age    = max(age, na.rm = TRUE)
  ), by = wo_tc][order(-avg_age)]
}

#' Overall status distribution
#' Answers: What share of all requests are in each status?
default_status_distribution <- function(df) {
  result <- count_by(df, "status")
  add_pct(result)
}

#' Volume by police district and case type
#' Answers: Where is demand concentrated geographically?
default_volume_by_pd <- function(df) {
  count_by(df, c("pd", "wo_tc"))
}

#' Activity outcome distribution (shared across most types)
#' Answers: What are the outcomes across all types that track activity_outcome?
default_activity_outcomes <- function(df) {
  if (!"activity_outcome" %in% names(df)) return(NULL)
  dt <- df[!is.na(activity_outcome) & activity_outcome != ""]
  result <- count_by(dt, c("wo_tc", "activity_outcome"))
  add_pct(result)
}

#' Demographic summary — gender distribution by case type
#' Answers: What is the gender breakdown across types?
default_gender_distribution <- function(df) {
  if (!"client_gender" %in% names(df)) return(NULL)
  dt <- df[!is.na(client_gender) & client_gender != ""]
  result <- count_by(dt, c("wo_tc", "client_gender"))
  add_pct(result)
}

#' Demographic summary — language distribution by case type
#' Answers: What languages are clients speaking across types?
default_language_distribution <- function(df) {
  if (!"client_language" %in% names(df)) return(NULL)
  dt <- df[!is.na(client_language) & client_language != ""]
  result <- count_by(dt, c("wo_tc", "client_language"))
  add_pct(result)
}

#' Veteran status by case type
#' Answers: What share of clients are veterans?
default_veteran_summary <- function(df) {
  if (!"veteran" %in% names(df)) return(NULL)
  dt <- df[!is.na(veteran) & veteran != ""]
  result <- count_by(dt, c("wo_tc", "veteran"))
  add_pct(result)
}

default_compliance_summary <- function(df) {
  if (!"compliance_level" %in% names(df)) return(NULL)
  result <- count_by(df, c("wo_tc", "compliance_level"))
  result[, pct := round(N / sum(N) * 100, 1), by = wo_tc]
  result
}

# =============================================================================
# SECTION 3: CASE-TYPE SPECIFIC ANALYSIS FUNCTIONS
# =============================================================================
# These functions are designed for a SINGLE wo_tc subset. They reference
# columns that only exist for that type.
#
# Contract:
#   Input:  data.table (a single type's subset from split_by_type)
#   Output: data.table (or named list of data.tables)
#
# Naming convention: tc_{wo_tc}_{analysis_name}
# -----------------------------------------------------------------------------


# --- TBC: Shelter Placement --------------------------------------------------

#' Shelter placement summary
#' Answers: Where are clients being placed?
tc_tbc_shelter_placement <- function(df) {
  if (!"shelter_placed" %in% names(df)) return(NULL)
  dt <- df[!is.na(shelter_placed) & shelter_placed != ""]
  result <- count_by(dt, "shelter_placed")
  add_pct(result)
}

#' Shelter denial tracking
#' Answers: How many clients were denied shelter before being served?
tc_tbc_shelter_denials <- function(df) {
  if (!"denied_shelter_today" %in% names(df)) return(NULL)
  dt <- df[!is.na(denied_shelter_today) & denied_shelter_today != ""]
  result <- count_by(dt, "denied_shelter_today")
  add_pct(result)
}

#' Household composition
#' Answers: What do households seeking shelter look like?
tc_tbc_household_composition <- function(df) {
  if (!"hh_makeup" %in% names(df)) return(NULL)
  dt <- df[!is.na(hh_makeup) & hh_makeup != ""]
  result <- count_by(dt, "hh_makeup")
  add_pct(result)
}

#' Recent release from detention
#' Answers: What share of clients were recently incarcerated?
tc_tbc_recent_release <- function(df) {
  if (!"recent_release" %in% names(df)) return(NULL)
  dt <- df[!is.na(recent_release) & recent_release != ""]
  result <- count_by(dt, "recent_release")
  add_pct(result)
}

#' Youth client breakdown
#' Answers: How many clients are youth (18-24)?
tc_tbc_youth_clients <- function(df) {
  if (!"youth_client" %in% names(df)) return(NULL)
  dt <- df[!is.na(youth_client) & youth_client != ""]
  result <- count_by(dt, "youth_client")
  add_pct(result)
}

#' TBC activity outcomes
#' Answers: What are the outcomes for shelter placement cases?
tc_tbc_activity_outcomes <- function(df) {
  if (!"activity_outcome" %in% names(df)) return(NULL)
  dt <- df[!is.na(activity_outcome) & activity_outcome != ""]
  result <- count_by(dt, "activity_outcome")
  add_pct(result)
}


# --- TBH: Senior Services ---------------------------------------------------

#' Senior age distribution
#' Answers: What is the age profile of seniors being served?
tc_tbh_senior_age <- function(df) {
  if (!"senior_age" %in% names(df)) return(NULL)
  dt <- df[!is.na(senior_age) & senior_age != ""]
  result <- count_by(dt, "senior_age")
  add_pct(result)
}

#' CHA property breakdown
#' Answers: How many senior requests involve CHA properties?
tc_tbh_cha_property <- function(df) {
  if (!"cha_property" %in% names(df)) return(NULL)
  dt <- df[!is.na(cha_property) & cha_property != ""]
  result <- count_by(dt, "cha_property")
  add_pct(result)
}

#' Caller relationship to senior
#' Answers: Who is calling on behalf of seniors?
tc_tbh_caller_relationship <- function(df) {
  if (!"caller_relationship" %in% names(df)) return(NULL)
  dt <- df[!is.na(caller_relationship) & caller_relationship != ""]
  result <- count_by(dt, "caller_relationship")
  add_pct(result)
}

#' Request nature for senior services
#' Answers: What kinds of help are seniors requesting?
tc_tbh_request_nature <- function(df) {
  if (!"request_nature" %in% names(df)) return(NULL)
  dt <- df[!is.na(request_nature) & request_nature != ""]
  result <- count_by(dt, "request_nature")
  add_pct(result)
}


# --- TBA: General/Family Intake ----------------------------------------------

#' Ambulatory status
#' Answers: Can clients move independently?
tc_tba_ambulatory <- function(df) {
  if (!"ambulatory" %in% names(df)) return(NULL)
  dt <- df[!is.na(ambulatory) & ambulatory != ""]
  result <- count_by(dt, "ambulatory")
  add_pct(result)
}

#' Family size summary
#' Answers: How large are the families requesting help?
tc_tba_family_size <- function(df) {
  if (!"total_people_n" %in% names(df)) return(NULL)
  dt <- df[!is.na(total_people_n) & total_people_n != ""]
  result <- count_by(dt, "total_people_n")
  add_pct(result)
}

#' TBA activity outcomes
tc_tba_activity_outcomes <- function(df) {
  if (!"activity_outcome" %in% names(df)) return(NULL)
  dt <- df[!is.na(activity_outcome) & activity_outcome != ""]
  result <- count_by(dt, "activity_outcome")
  add_pct(result)
}


# --- TBE: Family/Transport ---------------------------------------------------

#' Special assistance needs
#' Answers: What special assistance is being requested?
tc_tbe_special_assistance <- function(df) {
  if (!"special_assistance" %in% names(df)) return(NULL)
  dt <- df[!is.na(special_assistance) & special_assistance != ""]
  result <- count_by(dt, "special_assistance")
  add_pct(result)
}

#' Family size for transport
tc_tbe_family_size <- function(df) {
  if (!"total_families_n" %in% names(df)) return(NULL)
  dt <- df[!is.na(total_families_n) & total_families_n != ""]
  result <- count_by(dt, "total_families_n")
  add_pct(result)
}

#' TBE activity outcomes
tc_tbe_activity_outcomes <- function(df) {
  if (!"activity_outcome" %in% names(df)) return(NULL)
  dt <- df[!is.na(activity_outcome) & activity_outcome != ""]
  result <- count_by(dt, "activity_outcome")
  add_pct(result)
}


# --- TBG: Individual Outreach ------------------------------------------------

#' Mobility status
#' Answers: Can the client walk on their own?
tc_tbg_mobility <- function(df) {
  if (!"can_walk" %in% names(df)) return(NULL)
  dt <- df[!is.na(can_walk) & can_walk != ""]
  result <- count_by(dt, "can_walk")
  add_pct(result)
}

#' Request nature for outreach
tc_tbg_request_nature <- function(df) {
  if (!"request_nature" %in% names(df)) return(NULL)
  dt <- df[!is.na(request_nature) & request_nature != ""]
  result <- count_by(dt, "request_nature")
  add_pct(result)
}

#' TBG activity outcomes
tc_tbg_activity_outcomes <- function(df) {
  if (!"activity_outcome" %in% names(df)) return(NULL)
  dt <- df[!is.na(activity_outcome) & activity_outcome != ""]
  result <- count_by(dt, "activity_outcome")
  add_pct(result)
}

tc_compliance_detail <- function(df) {
  if (!"compliance_level" %in% names(df)) return(NULL)
  result <- count_by(df, "compliance_level")
  add_pct(result)
}
# =============================================================================
# SECTION 4: REGISTRIES
# =============================================================================
# These lists drive the report. Add or remove entries here to control
# what analyses run. The orchestrator reads these — no other code changes needed.
# -----------------------------------------------------------------------------

# --- Default analyses: run on full dataset ---
default_analyses <- list(
  demand_by_type_status  = default_demand_by_type_status,
  demand_over_time       = default_demand_over_time,
  aging_summary          = default_aging_summary,
  status_distribution    = default_status_distribution,
  volume_by_pd           = default_volume_by_pd,
  activity_outcomes      = default_activity_outcomes,
  gender_distribution    = default_gender_distribution,
  language_distribution  = default_language_distribution,
  veteran_summary        = default_veteran_summary,
  compliance_summary     = default_compliance_summary
)

# --- Case-type analyses: run on per-type subsets ---
# Each key is a wo_tc value. Each value is a named list of functions.
# Only the functions registered under a given wo_tc will run for that type.
tc_analyses <- list(
  TBC = list(
    shelter_placement   = tc_tbc_shelter_placement,
    compliance          = tc_compliance_detail,
    shelter_denials     = tc_tbc_shelter_denials,
    household_comp      = tc_tbc_household_composition,
    recent_release      = tc_tbc_recent_release,
    youth_clients       = tc_tbc_youth_clients,
    activity_outcomes   = tc_tbc_activity_outcomes
  ),
  TBH = list(
    senior_age          = tc_tbh_senior_age,
    compliance          = tc_compliance_detail,
    cha_property        = tc_tbh_cha_property,
    caller_relationship = tc_tbh_caller_relationship,
    request_nature      = tc_tbh_request_nature
  ),
  TBA = list(
    ambulatory          = tc_tba_ambulatory,
    compliance          = tc_compliance_detail,
    family_size         = tc_tba_family_size,
    activity_outcomes   = tc_tba_activity_outcomes
  ),
  TBE = list(
    special_assistance  = tc_tbe_special_assistance,
    compliance          = tc_compliance_detail,
    family_size         = tc_tbe_family_size,
    activity_outcomes   = tc_tbe_activity_outcomes
  ),
  TBG = list(
    mobility            = tc_tbg_mobility,
    compliance          = tc_compliance_detail,
    request_nature      = tc_tbg_request_nature,
    activity_outcomes   = tc_tbg_activity_outcomes
  )
)


# =============================================================================
# COMPLIANCE LEVEL ASSIGNMENT
# =============================================================================
# Reads the SLA compliance map and assigns a compliance_level to each record
# based on its wo_tc and age. The map is evaluated top-down per type,
# so the FIRST matching rule wins (most stringent tier first).
#
# The map file has three columns:
#   wo_tc            — case type code
#   compliance_level — label for the tier
#   Age_Calc         — R expression using 'age' (in days), e.g. "age < (3/24)"
# -----------------------------------------------------------------------------

#' Assign compliance levels based on the SLA map
#'
#' @param df_wide       data.table — the wide dataset with age column (in days)
#' @param sla_map_path  file path to the SLA compliance Excel file
#' @return df_wide with a new compliance_level column
assign_compliance <- function(df_wide, sla_map_path) {
  library(readxl)
  
  if (!file.exists(sla_map_path)) stop("SLA map not found: ", sla_map_path)
  
  # Read the compliance map
  sla_map <- as.data.table(read_xlsx(sla_map_path))
  
  # Ensure age is numeric (in days)
  df_wide[, age := as.numeric(age)]
  
  # Initialize compliance_level as NA
  df_wide[, compliance_level := NA_character_]
  
  # Get unique case types from the map
  tc_codes <- unique(sla_map$wo_tc)
  
  # For each case type, evaluate rules in ORDER (most stringent first)
  for (tc in tc_codes) {
    rules <- sla_map[wo_tc == tc]
    
    for (i in seq_len(nrow(rules))) {
      rule_expr <- rules$Age_Calc[i]
      rule_label <- rules$compliance_level[i]
      
      # Apply rule only to rows of this type that haven't been assigned yet
      df_wide[
        wo_tc == tc & is.na(compliance_level) & eval(parse(text = rule_expr)),
        compliance_level := rule_label
      ]
    }
  }
  
  return(df_wide)
}

# =============================================================================
# SECTION 5: ORCHESTRATOR
# =============================================================================
# run_report() executes the full pipeline. It reads from the registries
# and returns a structured list. You never need to modify this function
# when adding new analyses — just update the registries in Section 4.
# -----------------------------------------------------------------------------

#' Run the full report
#'
#' @param df_wide  The wide-format data.table (output of reshape_flex_data)
#' @return A named list with two elements:
#'   $overall    — named list of data.tables from default analyses
#'   $by_type    — named list (per wo_tc) of named lists of data.tables
#'
run_report <- function(df_wide) {

  # --- Overall section: run all default analyses on full dataset ---
  overall <- map(default_analyses, function(fn) {
    tryCatch(fn(df_wide), error = function(e) {
      warning("Default analysis failed: ", e$message)
      NULL
    })
  })
  # Remove any NULLs (analyses that returned nothing)
  overall <- compact(overall)

  # --- By-type section: split data, run registered analyses per type ---
  type_list <- split(df_wide, by = "wo_tc") %>%
    map(~ {
      dt <- copy(.x)
      blank_cols <- names(dt)[sapply(dt, function(x) all(is.na(x) | x == ""))]
      blank_cols <- blank_cols[!is.na(blank_cols)]
      if (length(blank_cols) > 0) dt[, (blank_cols) := NULL]
      dt
    })

  by_type <- imap(type_list, function(dt, tc) {
    # Get registered functions for this type (empty list if none registered)
    fns <- tc_analyses[[tc]] %||% list()

    results <- map(fns, function(fn) {
      tryCatch(fn(dt), error = function(e) {
        warning("Analysis failed for ", tc, ": ", e$message)
        NULL
      })
    })
    compact(results)
  })

  return(list(
    overall = overall,
    by_type = by_type
  ))
}

