# ----------------------------
# Staffing Sheets -> Tidy Data
#
# SCHEDULE WORKBOOKS (data/inputs/staffing_log/*.xlsx):
# - Each workbook is one week (Sat-Fri), tabs: AM, PM, OVN.
# - Row 2 header: A2 = "Positions", B2:H2 = date strings.
# - Schedule block ends at first sentinel row ("Color Coding", etc.).
# - Only PERSON assignments kept (BLANK/ON_CALL excluded).
# - HR role excluded.
#
# ATTENDANCE TRACKER (data/inputs/staffing_log/Attendance Tracker.csv):
# - Rows with "call off", "called off", or "no show" in Title
#   are matched by staff name + date and removed from the schedule.
# - Unmatched call-offs are logged to the QA file.
#
# OUTPUTS (to {input_dir}/clean/):
# - staffing_long_2026.csv      (one row per person-role-day-shift)
# - staffing_daily_levels_2026.csv (count + staff list per role-day-shift)
# - staffing_qa_log_2026.csv
# ----------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(lubridate)
})

# ====== PROJECT ROOT ======
# Walk up from this script's directory to find .git (same logic as here::here())
find_project_root <- function() {
  # Try rstudioapi first (if running in RStudio)
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
  # Fallback: walk up from working directory
  d <- getwd()
  while (d != dirname(d)) {
    if (file.exists(file.path(d, ".git"))) return(d)
    d <- dirname(d)
  }
  stop("Could not find project root (.git directory)")
}

project_root <- find_project_root()

# ====== USER SETTINGS ======
input_dir <- file.path(project_root, "data", "inputs", "staffing_log")
out_dir   <- file.path(input_dir, "clean")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

output_csv_long  <- file.path(out_dir, "staffing_long_2026.csv")
output_csv_daily <- file.path(out_dir, "staffing_daily_levels_2026.csv")
output_qa_log    <- file.path(out_dir, "staffing_qa_log_2026.csv")

analysis_year <- 2026L
ignore_before <- as.Date("2026-01-01")

expected_shifts <- c("AM", "PM", "OVN")

# Schedule block: A1:H500 (adjust row count upward if needed)
MAX_ROWS <- 500
MAX_COLS <- 8   # A..H

HEADER_ROW <- 2 # header row index in the read block
ROLE_START_ROW <- 3

# ====== HELPERS ======

normalize_sheet_name <- function(x) {
  x %>% as.character() %>% str_squish() %>% str_to_upper()
}

# Exclude Excel lock/temp files (~$...)
is_real_workbook <- function(path) {
  !grepl("(^|/|\\\\)~\\$", path)
}

# Convert a header cell (B2:H2) to a Date
# Handles:
# - numeric Excel date serials (days since 1899-12-30)
# - Date/POSIXct
# - character dates (fallback)
to_date_from_excel_header <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_Date_)

  # Excel often provides numeric serials
  if (is.numeric(x) && !is.na(x[1])) {
    return(as.Date(x[1], origin = "1899-12-30"))
  }

  # Already Date/POSIX
  if (inherits(x, "Date")) return(as.Date(x[1]))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x[1]))

  # Fallback: try parse "m/d/yyyy" or "m/d/yy" from character
  s <- str_squish(as.character(x[1]))
  if (s == "" || is.na(s)) return(NA_Date_)

  m <- str_extract(s, "\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b")
  if (is.na(m)) return(NA_Date_)
  suppressWarnings(mdy(m))
}

# Clean role (keep role_raw; role_clean used for analysis)
clean_role <- function(role_raw) {
  role_raw %>%
    str_squish() %>%
    str_replace_all("\\s+", " ") %>%
    # drop trailing "(...)" (e.g., "(9a)", "(PTO)", etc.) for rollups
    str_replace("\\s*\\([^\\)]*\\)\\s*$", "") %>%
    str_squish()
}

# Classify assignment cell values
# - BLANK: empty cell
# - ON_CALL: equals "On-Call" / "On Call" / "ONCALL" (case-insensitive)
# - PERSON: anything else
classify_assignment <- function(cell) {
  disp <- ifelse(is.na(cell), "", str_squish(as.character(cell)))
  low  <- str_to_lower(disp)

  assignment_type <- case_when(
    disp == "" ~ "BLANK",
    str_detect(low, "^on\\s*-?\\s*call$|^oncall$") ~ "ON_CALL",
    TRUE ~ "PERSON"
  )

  # For PERSON, strip trailing notes in parentheses (optional but helpful)
  staff_clean <- case_when(
    assignment_type == "PERSON" ~ disp %>%
      str_replace("\\s*\\([^\\)]*\\)\\s*$", "") %>%
      str_squish(),
    TRUE ~ NA_character_
  )

  tibble(
    staff_display = disp,
    staff_clean = staff_clean,
    assignment_type = assignment_type
  )
}

# Read only the schedule block A1:H{MAX_ROWS}
read_schedule_block <- function(path, sheet) {
  read_excel(
    path,
    sheet = sheet,
    range = cell_limits(c(1, 1), c(MAX_ROWS, MAX_COLS)),
    col_names = FALSE,
    .name_repair = "unique"
  )
}

# Empty output with correct columns (prevents 0x0 bind issues)
empty_long <- function() {
  tibble(
    source_file = character(),
    sheet_name_raw = character(),
    shift_code = character(),
    shift_name = character(),
    role_raw = character(),
    role_clean = character(),
    date = as.Date(character()),
    staff_display = character(),
    staff_clean = character(),
    assignment_type = character()
  )
}

# Sentinel patterns that mark the end of the schedule block
SENTINEL_PATTERN <- "^(color\\s*coding|phone\\s*numbers|supervisor\\s*phone|notes)$"

# Validate + extract header dates and data rows for one sheet
validate_and_extract <- function(df) {
  if (nrow(df) < HEADER_ROW) stop("Sheet has fewer than 2 rows; cannot read header row 2.")
  if (ncol(df) < 8) stop("Sheet has fewer than 8 columns (A..H).")

  # Header cells B2:H2 (columns 2..8)
  header_cells <- df[HEADER_ROW, 2:8] %>% unlist(use.names = FALSE)

  dates <- map(header_cells, to_date_from_excel_header) %>% unlist()
  dates <- as.Date(dates, origin = "1970-01-01")

  if (any(is.na(dates))) stop("Could not parse one or more dates from B2:H2.")
  if (length(unique(dates)) != 7) stop("Header dates in B2:H2 are not 7 unique dates.")

  # Data rows (A3:H...)
  data_df <- df[ROLE_START_ROW:nrow(df), 1:8, drop = FALSE] %>%
    mutate(across(everything(), as.character))

  names(data_df) <- c("role_raw", paste0("d", 1:7))

  # Truncate at first sentinel row (Color Coding, Phone Numbers, etc.)
  role_col <- str_squish(data_df$role_raw)
  role_col <- ifelse(is.na(role_col), "", role_col)
  sentinel_idx <- which(str_detect(str_to_lower(role_col), SENTINEL_PATTERN))

  if (length(sentinel_idx) > 0) {
    data_df <- data_df[seq_len(sentinel_idx[1] - 1), , drop = FALSE]
  }

  data_df <- data_df %>%
    mutate(role_raw = str_squish(role_raw)) %>%
    filter(!is.na(role_raw), role_raw != "") %>%
    filter(str_to_lower(role_raw) != "positions")

  if (nrow(data_df) == 0) stop("No role rows found starting at A3.")

  list(dates = dates, data = data_df)
}

shift_name_from_code <- function(code) {
  case_when(
    code == "AM"  ~ "1st",
    code == "PM"  ~ "2nd",
    code == "OVN" ~ "3rd",
    TRUE ~ NA_character_
  )
}

# Classify a sheet name to a shift code by prefix.
# Handles both "AM" and "AM 7a-330p" style names.
classify_shift <- function(sheet_norm) {
  case_when(
    str_detect(sheet_norm, "^OVN")  ~ "OVN",
    str_detect(sheet_norm, "^AM")   ~ "AM",
    str_detect(sheet_norm, "^PM")   ~ "PM",
    TRUE ~ NA_character_
  )
}

# Parse one workbook (one file) -> long data + QA log
parse_workbook <- function(path) {
  qa <- list()

  sheets_raw <- excel_sheets(path)
  sheets_norm <- normalize_sheet_name(sheets_raw)

  sheet_map <- tibble(sheet_raw = sheets_raw, sheet_norm = sheets_norm) %>%
    mutate(shift_code = classify_shift(sheet_norm)) %>%
    filter(!is.na(shift_code)) %>%
    group_by(shift_code) %>%
    slice(1) %>%
    ungroup()

  # Log missing expected shifts
  missing <- setdiff(expected_shifts, sheet_map$shift_code)
  if (length(missing) > 0) {
    qa[[length(qa) + 1]] <- tibble(
      source_file = basename(path),
      sheet = NA_character_,
      issue = paste0("Missing expected shift tab(s): ", paste(missing, collapse = ", "))
    )
  }

  to_parse <- sheet_map

  parsed <- map_dfr(seq_len(nrow(to_parse)), function(i) {
    sh_raw  <- to_parse$sheet_raw[i]
    sh_code <- to_parse$shift_code[i]

    tryCatch({
      df <- read_schedule_block(path, sh_raw)
      extracted <- validate_and_extract(df)

      dates <- extracted$dates
      data_df <- extracted$data

      long <- data_df %>%
        mutate(role_clean = clean_role(role_raw)) %>%
        pivot_longer(
          cols = starts_with("d"),
          names_to = "day_index",
          values_to = "cell_value"
        ) %>%
        mutate(
          day_num = as.integer(str_remove(day_index, "^d")),
          date = dates[day_num],
          source_file = basename(path),
          sheet_name_raw = sh_raw,
          shift_code = sh_code,
          shift_name = shift_name_from_code(sh_code)
        ) %>%
        select(source_file, sheet_name_raw, shift_code, shift_name,
               role_raw, role_clean, date, cell_value)

      long <- long %>%
        bind_cols(classify_assignment(long$cell_value)) %>%
        select(-cell_value)

      long
    }, error = function(e) {
      qa[[length(qa) + 1]] <<- tibble(
        source_file = basename(path),
        sheet = sh_raw,
        issue = paste0("Parse error: ", conditionMessage(e))
      )
      empty_long()
    })
  })

  qa_df <- if (length(qa) > 0) bind_rows(qa) else tibble(
    source_file = character(), sheet = character(), issue = character()
  )

  list(data = parsed, qa = qa_df)
}

# Normalize a staff name for matching: squish whitespace, strip trailing
# shift codes ("1st", "2nd", "3rd"), lowercase for comparison
normalize_staff_name <- function(x) {
  x %>%
    str_squish() %>%
    str_replace("\\s+(1st|2nd|3rd)$", "") %>%
    str_to_lower()
}

# ====== RUN PIPELINE ======

xlsx_files <- list.files(input_dir, pattern = "\\.xls[xm]$", full.names = TRUE)
xlsx_files <- xlsx_files[is_real_workbook(xlsx_files)]

if (length(xlsx_files) == 0) {
  stop("No Excel files found in input_dir: ", input_dir)
}

results <- map(xlsx_files, parse_workbook)

staffing_long <- bind_rows(map(results, "data"))
qa_log <- bind_rows(map(results, "qa"))

# Filter to operating period, assigned persons only, exclude HR
staffing_long <- staffing_long %>%
  filter(!is.na(date), date >= ignore_before) %>%
  filter(assignment_type == "PERSON") %>%
  filter(role_clean != "HR")

# ====== ATTENDANCE TRACKER (call-offs & no-shows) ======

attendance_file <- file.path(input_dir, "Attendance Tracker.csv")

if (file.exists(attendance_file)) {
  attendance <- read.csv(attendance_file, stringsAsFactors = FALSE) %>%
    mutate(title_lower = str_to_lower(str_squish(Title)))

  # Keep only call-off and no-show entries
  calloffs <- attendance %>%
    filter(str_detect(title_lower, "call\\s*-?\\s*off|called\\s*-?\\s*off|no\\s*-?\\s*show")) %>%
    mutate(
      calloff_date = as.Date(suppressWarnings(mdy_hm(Date.Time.Called.Off))),
      name_norm    = normalize_staff_name(Staff.Name)
    ) %>%
    filter(!is.na(calloff_date))

  # Fix obvious year typos (2025 -> 2026 for dates that fall outside analysis year)
  calloffs <- calloffs %>%
    mutate(calloff_date = if_else(
      year(calloff_date) < analysis_year,
      calloff_date + years(analysis_year - year(calloff_date)),
      calloff_date
    ))

  message("Attendance tracker: ", nrow(calloffs), " call-off/no-show entries found")

  # Add normalized name to staffing for matching
  staffing_long <- staffing_long %>%
    mutate(name_norm = normalize_staff_name(staff_clean))

  schedule_names <- sort(unique(staffing_long$name_norm))

  # Build canonical name groups from all names across both datasets.
  # This handles typos in both the schedule AND the attendance tracker,
  # as well as inconsistent spelling within the schedule itself.
  all_names <- sort(unique(c(schedule_names, unique(calloffs$name_norm))))
  canon_map <- tibble(name_norm = all_names, canonical = all_names)

  # Greedy grouping: for each name, if a close match (edit dist <= 3) exists

  # that already has a canonical form, adopt it
  for (i in seq_along(all_names)) {
    nm <- all_names[i]
    dists <- adist(nm, canon_map$canonical)[1, ]
    # Find closest canonical name (prefer schedule names as canonical)
    candidates <- which(dists > 0 & dists <= 3)
    if (length(candidates) > 0) {
      # Prefer a name that's in the schedule; break ties alphabetically
      cand_names <- canon_map$canonical[candidates]
      in_sched <- cand_names %in% schedule_names
      if (any(in_sched)) {
        best <- sort(cand_names[in_sched])[1]
      } else {
        best <- sort(cand_names)[1]
      }
      # Merge: set both to the same canonical
      old_canon <- canon_map$canonical[i]
      canon_map$canonical[canon_map$canonical == old_canon] <- best
    }
  }

  canon_map <- canon_map %>% distinct()
  fuzzy_resolved <- canon_map %>% filter(name_norm != canonical)
  if (nrow(fuzzy_resolved) > 0) {
    message("Fuzzy name matches: ",
            paste(sprintf("'%s' -> '%s'", fuzzy_resolved$name_norm,
                          fuzzy_resolved$canonical), collapse = ", "))
  }

  # Apply canonical mapping to both call-offs and schedule
  calloffs <- calloffs %>%
    left_join(canon_map, by = "name_norm") %>%
    mutate(name_norm = canonical) %>%
    select(-canonical)

  staffing_long <- staffing_long %>%
    left_join(canon_map, by = "name_norm") %>%
    mutate(name_norm = canonical) %>%
    select(-canonical)

  # Build the anti-join key
  calloff_keys <- calloffs %>%
    distinct(name_norm, calloff_date) %>%
    rename(date = calloff_date)

  n_before <- nrow(staffing_long)

  staffing_long <- staffing_long %>%
    anti_join(calloff_keys, by = c("name_norm", "date"))

  n_removed <- n_before - nrow(staffing_long)
  message("Removed ", n_removed, " rows matching call-off/no-show entries")

  # Log unmatched call-offs (date outside schedule range or truly unknown name)
  all_schedule_keys <- bind_rows(map(results, "data")) %>%
    filter(assignment_type == "PERSON") %>%
    mutate(name_norm = normalize_staff_name(staff_clean)) %>%
    left_join(canon_map, by = "name_norm") %>%
    mutate(name_norm = canonical) %>%
    select(-canonical) %>%
    distinct(name_norm, date)

  canonical_schedule_names <- sort(unique(all_schedule_keys$name_norm))

  unmatched <- calloff_keys %>%
    anti_join(all_schedule_keys, by = c("name_norm", "date"))

  if (nrow(unmatched) > 0) {
    unmatched_detail <- unmatched %>%
      left_join(calloffs %>% distinct(name_norm, Staff.Name), by = "name_norm") %>%
      mutate(
        in_schedule = name_norm %in% canonical_schedule_names,
        issue = if_else(
          in_schedule,
          paste0("Call-off date outside schedule range: ",
                 Staff.Name, " on ", date),
          paste0("Call-off name not found in schedule: ",
                 Staff.Name, " on ", date)
        )
      )
    qa_log <- bind_rows(qa_log, tibble(
      source_file = "Attendance Tracker.csv",
      sheet = NA_character_,
      issue = unmatched_detail$issue
    ))
    message("Unmatched call-offs logged to QA: ", nrow(unmatched_detail))
  }

  staffing_long <- staffing_long %>% select(-name_norm)

} else {
  message("No Attendance Tracker.csv found; skipping call-off removal")
}

# Write long output
write.csv(staffing_long, output_csv_long, row.names = FALSE)

# Daily staffing levels
staffing_daily <- staffing_long %>%
  group_by(date, shift_code, shift_name, role_clean) %>%
  summarise(
    scheduled_staff = n(),
    unique_staff    = n_distinct(staff_clean),
    staff_list      = paste(sort(unique(staff_clean)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(date, shift_code, role_clean)

write.csv(staffing_daily, output_csv_daily, row.names = FALSE)
write.csv(qa_log, output_qa_log, row.names = FALSE)

message(
  "\nDone.\n",
  "- Long data:    ", output_csv_long, "\n",
  "- Daily rollup: ", output_csv_daily, "\n",
  "- QA log:       ", output_qa_log, "\n",
  "- Files read:   ", length(xlsx_files), "\n",
  "- Long rows:    ", nrow(staffing_long), "\n",
  "- Daily rows:   ", nrow(staffing_daily)
)
