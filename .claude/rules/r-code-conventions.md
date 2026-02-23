---
paths:
  - "**/*.R"
  - "Figures/**/*.R"
  - "scripts/**/*.R"
---

# R Code Standards

**Standard:** Senior Principal Data Engineer + PhD researcher quality

---

## 1. Reproducibility

- `set.seed()` called ONCE at top (YYYYMMDD format)
- All packages loaded at top via `library()` (not `require()`)
- All paths relative to repository root
- `dir.create(..., recursive = TRUE)` for output directories

## 2. Function Design

- `snake_case` naming, verb-noun pattern
- Roxygen-style documentation
- Default parameters, no magic numbers
- Named return values (lists or tibbles)

## 3. Domain Correctness

- Salesforce date fields: `Created_Date` lacks timestamps; only `End_Date` has time
- Compliance tiers are ordinal: STAR (<3h) > City (<20h) > Non-Compliant (20h+)
- Staffing = scheduled minus call-offs, NOT actual hours worked
- Case age is right-skewed with zero-inflated floor (same-hour closures)
- Shift schedule: AM=7am-3pm, PM=3pm-11pm, OVN=11pm-7am
- Supervisor count is constant (1/shift) — exclude from regression models
- Intake Drivers operate in pairs; model pair count, not individual count
- TBG (Crisis Referral) has very few non-compliant cases (~8) — ordinal models may be unstable

## 4. Visual Identity

```r
# --- ASH Institutional Palette ---
# TODO: User to provide official ASH brand colors
# Placeholder values below — update when brand guide is available
ash_navy       <- "#1B365D"   # Primary (headings, key elements)
ash_accent     <- "#4A90D9"   # Secondary (accents, links)
accent_gray    <- "#525252"   # Neutral (de-emphasized text)
positive_green <- "#15803d"   # Good/compliant
negative_red   <- "#b91c1c"   # Bad/non-compliant
warning_amber  <- "#D97706"   # Warning/approaching threshold
```

### Custom Theme
```r
theme_ash <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", color = ash_navy),
      plot.subtitle = element_text(color = accent_gray),
      legend.position = "bottom"
    )
}
```

### Figure Dimensions
```r
# For Beamer slides (4:3 aspect ratio)
ggsave(filepath, width = 12, height = 5, bg = "transparent")
# For Quarto HTML reports (wider layout)
ggsave(filepath, width = 14, height = 6, bg = "white")
```

## 5. RDS Data Pattern

**Heavy computations saved as RDS; slide rendering loads pre-computed data.**

```r
saveRDS(result, file.path(out_dir, "descriptive_name.rds"))
```

## 6. Common Pitfalls

| Pitfall | Impact | Prevention |
|---------|--------|------------|
| Missing `bg = "transparent"` | White boxes on Beamer slides | Always include in ggsave() for slides |
| Hardcoded paths | Breaks on other machines | Use here() + config.R |
| Using Created_Date for shift assignment | Wrong shift linkage | Use End_Date timestamps |
| Including Supervisors in regression | Constant predictor, singular matrix | Filter to Intake + Dispatch only |
| Not accounting for call-offs | Overstates available staffing | Subtract Attendance Tracker call-offs |
| Small sample ordinal model for TBG | Unstable coefficients | Flag n < 20 per category, use binary instead |
| Treating compliance tiers as nominal | Loses ordering information | Use ordinal logistic or binary for STAR target |

## 7. Line Length & Mathematical Exceptions

**Standard:** Keep lines <= 100 characters.

**Exception: Mathematical Formulas** -- lines may exceed 100 chars **if and only if:**

1. Breaking the line would harm readability of the math (influence functions, matrix ops, finite-difference approximations, formula implementations matching paper equations)
2. An inline comment explains the mathematical operation:
   ```r
   # Sieve projection: inner product of residuals onto basis functions P_k
   alpha_k <- sum(r_i * basis[, k]) / sum(basis[, k]^2)
   ```
3. The line is in a numerically intensive section (simulation loops, estimation routines, inference calculations)

**Quality Gate Impact:**
- Long lines in non-mathematical code: minor penalty (-1 to -2 per line)
- Long lines in documented mathematical sections: no penalty

## 8. Code Quality Checklist

```
[ ] Packages at top via library()
[ ] set.seed() once at top
[ ] All paths relative
[ ] Functions documented (Roxygen)
[ ] Figures: transparent bg, explicit dimensions
[ ] RDS: every computed object saved
[ ] Comments explain WHY not WHAT
```
