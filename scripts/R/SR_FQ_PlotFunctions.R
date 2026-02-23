# =============================================================================
# LEVEL ORDERING SYSTEM
# =============================================================================

# --- Fixed ordinal orders (meaning-based, never changes) ---
ordinal_orders <- list(
  compliance_level = c(
    "STAR Compliant (< 3 hours)",
    "City Compliant (< 20 hours)",
    "Non-Compliant (20+ hours)"
  ),
  status = c(
    "OPEN",
    "IN PROGRESS",
    "PENDING",
    "CANCELLED",
    "CLOSED"
    # adjust to match your actual status values
  )
)

#' Apply ordering to a variable
#' - Ordinal variables: use fixed order from ordinal_orders
#' - Everything else: order by frequency (most common first)
#' 
#' @param dt    data.table
#' @param col   column name to order
#' @return dt with the column converted to an ordered factor
order_levels <- function(dt, col) {
  dt <- copy(dt)
  
  if (col %in% names(ordinal_orders)) {
    # Fixed meaningful order
    valid_levels <- intersect(ordinal_orders[[col]], unique(dt[[col]]))
    dt[, (col) := factor(get(col), levels = valid_levels)]
  } else if (is.character(dt[[col]]) || is.factor(dt[[col]])) {
    # Frequency order — most common first
    freq <- dt[!is.na(get(col)) & get(col) != "", .N, by = col][order(-N)]
    dt[, (col) := factor(get(col), levels = freq[[col]])]
  }
  
  return(dt)
}

#' Get palette that respects the current factor ordering
get_palette <- function(dt, group, from = "#05cda3", to = "#dc8616") {
  groups <- levels(dt[[group]])
  if (is.null(groups)) groups <- unique(dt[[group]])
  groups <- groups[!is.na(groups) & groups != ""]
  n <- length(groups)
  
  pal <- colorRampPalette(c(from, to))(n)
  setNames(pal, groups)
}


# =============================================================================
# PLOT TEMPLATES
# =============================================================================
# Each template handles a specific chart type. They call order_levels
# internally so the caller never thinks about ordering.
# 
# Pattern:
#   1. order_levels on the relevant variables
#   2. get_palette based on the fill/color variable
#   3. ggplot with consistent theme
# =============================================================================


#' Stacked bar — shows proportional breakdown
#' Use for: compliance by type, status by type
#' Fill variable gets gradient, x-axis ordered by total volume
plot_stacked_bar <- function(dt, x, fill, title = NULL) {
  dt <- order_levels(dt, fill)
  dt <- order_levels(dt, x)
  pal <- get_palette(dt, fill)
  
  ggplot(dt, aes(x = .data[[x]], y = N, fill = .data[[fill]])) +
    geom_col(position = "fill") +
    scale_fill_manual(values = pal) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Grouped bar — shows absolute counts side by side
#' Use for: demand by type and status
plot_grouped_bar <- function(dt, x, fill, title = NULL) {
  dt <- order_levels(dt, fill)
  dt <- order_levels(dt, x)
  pal <- get_palette(dt, fill)
  
  ggplot(dt, aes(x = .data[[x]], y = N, fill = .data[[fill]])) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = pal) +
    labs(title = title, x = NULL, y = "Count", fill = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Horizontal bar — shows ranked categories
#' Use for: outcomes, shelter placement, language, any single-variable count
plot_horizontal_bar <- function(dt, y, title = NULL, from = "#05cda3", to = "#dc8616") {
  dt <- as.data.table(order_levels(dt, y))
  # Use base R assignment instead of :=
  dt[[y]] <- factor(dt[[y]], levels = rev(levels(dt[[y]])))
  pal <- get_palette(dt, y, from, to)
  
  ggplot(dt, aes(x = N, y = .data[[y]], fill = .data[[y]])) +
    geom_col() +
    scale_fill_manual(values = pal) +
    labs(title = title, x = "Count", y = NULL) +
    theme_minimal() +
    theme(legend.position = "none")
}

#' Line chart — shows trends over time
#' Use for: demand over time
plot_trend_line <- function(dt, x = "created_month", color = "wo_tc", title = NULL) {
  dt <- order_levels(dt, color)
  pal <- get_palette(dt, color)
  
  ggplot(dt, aes(x = .data[[x]], y = N, color = .data[[color]], group = .data[[color]])) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = pal) +
    labs(title = title, x = NULL, y = "Count", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
}


# =============================================================================
# REPORT-SPECIFIC PLOT FUNCTIONS
# =============================================================================
# These are thin wrappers that map a specific analysis output to the
# correct plot template with the right column names.
# The analyst doesn't need to remember which template to use.
# =============================================================================

# --- Overall plots ---
plot_compliance_by_type <- function(dt) {
  plot_stacked_bar(dt, x = "wo_tc", fill = "compliance_level",
                   title = "SLA Compliance by Case Type")
}

plot_demand_by_type_status <- function(dt) {
  plot_grouped_bar(dt, x = "wo_tc", fill = "status",
                   title = "Demand by Case Type and Status")
}

plot_demand_trend <- function(dt) {
  plot_trend_line(dt, title = "Monthly Demand by Case Type")
}

plot_activity_outcomes_overall <- function(dt) {
  plot_stacked_bar(dt, x = "wo_tc", fill = "activity_outcome",
                   title = "Activity Outcomes by Case Type")
}

plot_volume_by_pd <- function(dt) {
  plot_horizontal_bar(dt, y = "pd", title = "Volume by Police District")
}

# --- Case-type plots (work on single-type subsets) ---
plot_tc_compliance <- function(dt) {
  plot_horizontal_bar(dt, y = "compliance_level",
                      title = "Compliance Breakdown")
}

plot_tc_activity_outcomes <- function(dt) {
  plot_horizontal_bar(dt, y = "activity_outcome",
                      title = "Activity Outcomes")
}

plot_tc_shelter_placement <- function(dt) {
  plot_horizontal_bar(dt, y = "shelter_placed",
                      title = "Shelter Placement")
}

plot_tc_household_comp <- function(dt) {
  plot_horizontal_bar(dt, y = "hh_makeup",
                      title = "Household Composition")
}

plot_tc_request_nature <- function(dt) {
  plot_horizontal_bar(dt, y = "request_nature",
                      title = "Request Nature")
}


x <- plot_tc_shelter_placement(report$by_type$TBC$shelter_placement)
y <- plot_tc_activity_outcomes(report$by_type$TBC$activity_outcomes)
ggsave("Shelter Placements.png",x, width = 20, height = 15)
ggsave("Activity Outcomes.png",y, width = 20, height = 15)
       
