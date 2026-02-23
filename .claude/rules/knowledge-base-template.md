---
paths:
  - "Slides/**/*.tex"
  - "Quarto/**/*.qmd"
  - "scripts/**/*.R"
---

# ASH Constraint Analysis Knowledge Base

## Notation Registry

| Rule | Convention | Example | Anti-Pattern |
|------|-----------|---------|-------------|
| Case type codes | 3-letter uppercase | TBC, TBH, TBG, TBA, TBE | "shelter", "senior" (use codes) |
| Compliance tiers | Title case label | STAR, City, Non-Compliant | "level 1", "tier A" |
| Shift codes | Uppercase abbreviation | AM, PM, OVN | "morning", "day shift" |
| Staffing roles | Full name in text, abbreviation in code | Intake Driver (intake_driver) | "driver", "ID" |
| Time thresholds | Hours with unit | 3h, 20h | "3", "twenty hours" |

## Symbol Reference

| Symbol | Meaning | Introduced |
|--------|---------|------------|
| `wo_tc` | Work order type/case type code | SR Report Framework |
| `age` | Case age in days (time since creation) | SR Report Framework |
| `compliance_level` | SLA tier: STAR / City / Non-Compliant | Staffing Compliance |
| `intake_pairs` | Number of Intake Driver pairs per shift | Staffing Impact |
| `dispatchers` | Number of Dispatchers per shift | Staffing Impact |
| `call_offs` | Staff absences from scheduled shift | Staffing Levels |
| `pd` | Police district (geographic area) | SR Report Framework |

## Analysis Module Progression

| # | Module | Core Question | Key Variables | Key Method |
|---|--------|--------------|--------------|------------|
| 1 | SR Report Framework | What is the demand and status landscape? | wo_tc, status, age | Registry-driven aggregation |
| 2 | Staffing Pipeline | What staffing is actually available per shift? | schedule, call-offs, role | Excel reshape + deduction |
| 3 | Staffing Impact | Does staffing affect case closure speed? | intake_pairs, dispatchers, age | GLM (Gamma, Poisson) |
| 4 | Staffing Compliance | Does staffing affect SLA compliance? | staffing, compliance_level | Ordinal logistic |
| 5 | Input Effects | What external factors affect operations? | weather, beds, staffing | Multi-factor regression |
| 6 | Weather Effects | Does weather drive demand or slow closure? | temperature, precipitation, age | TBD |
| 7 | Bed Availability | Does shelter capacity constrain placements? | beds_available, placements | TBD |

## Analysis Components

| Component | Data Source | Script(s) | Module | Purpose |
|-----------|-----------|-----------|--------|---------|
| SR demand analysis | Salesforce CSV | SR_Report_Framework.R | 1 | Baseline demand metrics |
| Staffing reshape | Weekly Excel logs | staffing_levels.R | 2 | Clean staffing data |
| Closure speed model | SR + staffing | staffing_impact.R | 3 | Staffing effect on age |
| SLA compliance model | SR + staffing + SLA map | staffing_compliance.R | 4 | Staffing effect on tiers |
| Period comparison | SR across periods | period_comparison.R | 1 | Trend detection |

## Design Principles

| Principle | Evidence | Modules Applied |
|-----------|----------|----------------|
| Registry-driven analysis | Adding new analysis = adding to list, not changing orchestrator | 1 (SR Report Framework) |
| Config as single source | All paths, periods, years in config.R | All modules |
| RDS intermediate pattern | Heavy compute saved; reports load pre-computed | All modules |

## Anti-Patterns (Don't Do This)

| Anti-Pattern | What Happened | Correction |
|-------------|---------------|-----------|
| Treating Supervisors as variable | Constant predictor caused singular matrix | Exclude from regression |
| Using Created_Date for shift | Wrong shift assignment (no timestamp) | Use End_Date |
| Ordinal model on TBG | Only ~8 non-compliant cases, unstable fit | Use binary (STAR vs not) |

## R Code Pitfalls

| Bug | Impact | Fix |
|-----|--------|-----|
| `as.numeric(age)` on character column | Silent NA introduction | Parse with explicit format check |
| Missing call-off deduction | Staffing overstated by ~15-20% | Always join Attendance Tracker |
| `read_xlsx` on .csv file | Silent failure or wrong parse | Check file extension in config.R |
| Percentage within vs across types | Misleading comparison | Always specify `by = wo_tc` in data.table |
