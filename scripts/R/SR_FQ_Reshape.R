# =============================================================================
# SR Flex Question (FQ) Data Reshaping Pipeline
# =============================================================================
#
# PURPOSE:
#   This script takes raw service request (SR) flex question data — where each 
#   row is a single question/answer pair — and reshapes it into a wide format 
#   where each question becomes its own column. It then splits the data by 
#   work order type (wo_tc) and saves each type as a separate CSV file.
#
# WHAT THE RAW DATA LOOKS LIKE:
#   Each service request (identified by wo_number) has multiple rows — one per 
#   question asked. For example, a shelter request might have 10 rows for 
#   10 different questions. All requests of the same type (wo_tc) share the 
#   same set of questions.
#
# WHAT THIS SCRIPT DOES:
#   1. Reads the raw report and a column name mapping file
#   2. Replaces long question text with short, readable column names
#   3. Reshapes from long (one row per question) to wide (one row per request)
#   4. Splits the wide data by work order type
#   5. Removes empty columns that don't apply to each type
#   6. Saves each type as its own CSV file
#
# INPUTS:
#   - Raw FQ export   : CSV from the SR system (one row per question/answer)
#   - Column map       : CSV with three columns (wo_tc, fq, fq_short) that maps 
#                        full question text to short column names
#
# OUTPUTS:
#   - One CSV per work order type, saved to the specified output directory
#
# DEPENDENCIES:
#   - data.table : fast reading, reshaping, and joining
#   - dplyr      : used for initial column selection and filtering
#   - purrr      : used for iterating over the split list (iwalk)
#
# =============================================================================


# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(purrr)


# -----------------------------------------------------------------------------
# Default File Paths — override these before calling the pipeline functions
# -----------------------------------------------------------------------------

if (!exists("col_map_path")) col_map_path <- here::here("data", "mapping", "fq_col_map.csv")
if (!exists("fq_raw_path"))  fq_raw_path  <- here::here("data", "raw", "SR_FQ_Y2026.csv")
if (!exists("output_dir"))   output_dir   <- here::here("data", "processed", "FQ_By_Type")


# =============================================================================
# FUNCTION: reshape_flex_data
# =============================================================================
# Takes the raw long-format FQ data and reshapes it to wide format.
#
# How it works:
#   - Reads both the raw report and the column mapping file
#   - Selects and renames relevant columns from the raw report
#   - Filters out rows with blank answers
#   - Joins the column map to replace full question text (fq) with short 
#     readable names (fq_short). Any unmapped questions get an auto-generated 
#     name as a fallback.
#   - Extracts metadata columns (dates, address, etc.) before pivoting, since 
#     these are the same for every row within a work order
#   - Pivots the data wide: one row per work order, one column per question
#   - If a work order has duplicate answers to the same question, they are 
#     concatenated with " | " so no data is lost
#   - Joins the metadata back onto the wide result
#
# Parameters:
#   report_path   - file path to the raw FQ CSV export
#   col_map_path  - file path to the column mapping CSV (wo_tc, fq, fq_short)
#
# Returns:
#   A data.table in wide format with one row per work order
# -----------------------------------------------------------------------------

reshape_flex_data <- function(report_path, col_map_path) {
  
  # Validate that files exist before reading
  if (!file.exists(report_path)) stop("Report not found: ", report_path)
  if (!file.exists(col_map_path)) stop("Col map not found: ", col_map_path)
  
  # Read raw data and select/rename columns by position
  # Column positions are based on the standard SR FQ export layout
  df <- fread(report_path) %>%
    select(
      wo_tn        = 3,   # Work order type name
      wo_tc        = 4,   # Work order type code (used for grouping)
      wo_number    = 2,   # Work order ID (unique per request)
      sr_number    = 5,   # Service request number
      status       = 1,   # Current status of the work order
      created_date = 6,   # Date the request was created
      end_date     = 7,   # Date the request was closed
      age          = 8,   # Age of the request in days
      lm_by        = 9,   # Last modified by (user)
      lm_date      = 10,  # Last modified date
      wo_addr      = 15,  # Work order address
      xcoord       = 11,  # X coordinate (longitude)
      ycoord       = 12,  # Y coordinate (latitude)
      pd           = 14,  # Police district
      fq           = 16,  # Flex question (full text)
      fa           = 17   # Flex answer (response value)
    ) %>%
    filter(fa != "")  # Drop rows with no answer
  
  # Read the column name mapping
  col_map <- fread(col_map_path)
  
  # Normalize column names to lowercase in case of inconsistency
  setnames(df, tolower(trimws(names(df))))
  setnames(col_map, tolower(trimws(names(col_map))))
  
  # Join: map full question text to short column names
  # Uses a keyed join for performance on large datasets
  setkey(col_map, wo_tc, fq)
  df[col_map, fq_short := i.fq_short, on = .(wo_tc, fq)]
  
  # Fallback: auto-generate a name for any unmapped questions
  df[is.na(fq_short), fq_short := make.names(fq)]
  
  # Extract metadata — one row per work order
  # These columns are constant across all question rows for a given wo_number,
  # so we pull them out before pivoting to keep the reshape clean
  meta <- unique(df, by = "wo_number")[, .(
    wo_number, wo_tc, wo_tn, sr_number, status,
    created_date, end_date, age, lm_by, lm_date,
    wo_addr, xcoord, ycoord, pd
  )]
  
  # Pivot from long to wide: one row per work order, one column per question
  # fun.aggregate handles the rare case of duplicate answers by concatenating
  df_wide <- suppressWarnings(
    dcast(df, wo_number + wo_tc ~ fq_short,
          value.var = "fa",
          fun.aggregate = function(x) paste(x, collapse = " | "))
  )
  
  # Rejoin metadata to the wide result
  df_wide <- meta[df_wide, on = .(wo_number, wo_tc)]
  
  return(df_wide)
}


# =============================================================================
# FUNCTION: split_by_type
# =============================================================================
# Splits the wide dataframe into a list of dataframes, one per work order type.
# Removes any columns that are entirely NA within each type — these represent 
# questions that are not asked for that particular type.
#
# Parameters:
#   df_wide - the wide-format data.table from reshape_flex_data()
#
# Returns:
#   A named list of data.tables, keyed by wo_tc
#     e.g. result_list$TBH, result_list$TBC
# -----------------------------------------------------------------------------

split_by_type <- function(df_wide) {
  split(df_wide, by = "wo_tc") %>%
    lapply(function(dt) {
      # Identify columns where every value is NA (irrelevant questions)
      na_cols <- names(dt)[sapply(dt, function(x) all(is.na(x)))]
      # Remove those columns in place
      dt[, (na_cols) := NULL]
      dt
    })
}


# =============================================================================
# FUNCTION: save_by_type
# =============================================================================
# Saves each data.table in the list as a separate CSV file, named after its 
# work order type.
#
# Parameters:
#   result_list - named list of data.tables (output of split_by_type)
#   output_dir  - directory path where CSVs will be saved
#
# Output:
#   One CSV per work order type, e.g. Shelter.csv, ELECTRICAL.csv
# -----------------------------------------------------------------------------

save_by_type <- function(result_list, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  iwalk(result_list, function(dt, name) {
    # Remove columns that are entirely NA or blank
    blank_cols <- names(dt)[sapply(dt, function(x) all(is.na(x) | x == ""))]
    dt[, (blank_cols) := NULL]
    
    file_name <- paste0(make.names(name), ".csv")
    fwrite(dt, file.path(output_dir, file_name))
  })
  
  message("Saved ", length(result_list), " files to ", output_dir)
}


# =============================================================================
# RUN THE PIPELINE (disabled; call from build_data.R)
# =============================================================================
#
# result <- reshape_flex_data(fq_raw_path, col_map_path)  # Step 1: Reshape
# result_list <- split_by_type(result)                   # Step 2: Split by type
# save_by_type(result_list, output_dir)                  # Step 3: Save to CSV
#
# To inspect a specific type interactively:
# names(result_list)        # See all available types
# result_list$TBH           # View a specific type
