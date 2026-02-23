---
paths:
  - "Figures/**/*"
  - "Quarto/**/*.qmd"
  - "Slides/**/*.tex"
---

# Single Source of Truth: Enforcement Protocol

**R analysis scripts are the authoritative source for ALL data and results. Beamer and Quarto are derived presentation layers.**

## The SSOT Chain

```
R Analysis Scripts (DATA & RESULTS SOURCE OF TRUTH)
  ├── scripts/R/*.R → data/processed/*.rds (computed results)
  │
  ├── Beamer .tex (SLIDE SOURCE OF TRUTH for presentations)
  │   ├── extract_tikz.tex → PDF → SVGs (derived)
  │   └── Quarto .qmd → HTML (derived from Beamer where both exist)
  │
  └── Quarto .qmd (REPORT SOURCE OF TRUTH for HTML reports)
      └── May exist independently from Beamer for report-only modules

Bibliography_base.bib (shared across both presentation formats)
Figures/ModuleN/*.rds → plotly charts (data source)

NEVER edit derived artifacts independently.
ALWAYS propagate changes from source → derived.
```

### When Both Beamer and Quarto Exist

The Beamer `.tex` file is the presentation source of truth. Quarto `.qmd` derives from it. See `beamer-quarto-sync.md` for the mandatory sync rule.

### When Only Quarto Exists

For report-only modules (no corresponding Beamer deck), the Quarto `.qmd` is the sole presentation artifact. No sync is required.

### R Scripts Are Always Upstream

If a result changes in an R script, ALL downstream presentation artifacts (both Beamer and Quarto) must be updated to reflect the new result.

---

## TikZ Freshness Protocol (MANDATORY)

**Before using ANY TikZ SVG in a Quarto slide, verify it matches the current Beamer source.**

### Diff-Check Procedure

1. Read the TikZ block from the Beamer `.tex` file
2. Read the corresponding block from `Figures/LectureN/extract_tikz.tex`
3. Compare EVERY coordinate, label, color, opacity, and anchor point
4. If ANY difference exists: update `extract_tikz.tex` from Beamer, recompile, regenerate SVGs
5. Only then reference the SVG in the QMD

### When to Re-Extract

Re-extract ALL TikZ diagrams when:
- The Beamer `.tex` file has been modified since last extraction
- Starting a new Quarto translation
- Any TikZ-related quality issue is reported
- Before any commit that includes QMD changes

---

## Environment Parity (MANDATORY)

**Every Beamer environment MUST have a CSS equivalent before translation begins.**

1. Scan the Beamer source for all custom environments
2. Check each against your theme SCSS file
3. If ANY environment is missing from SCSS, create it BEFORE translating

---

## Content Fidelity Checklist

```
[ ] Frame count: Beamer frames == Quarto slides (where both exist)
[ ] Math check: every equation appears with identical notation
[ ] Citation check: every \cite has a @key in Quarto
[ ] Environment check: every Beamer box has CSS equivalent
[ ] Figure check: every \includegraphics has SVG or plotly equivalent
[ ] No added content: Quarto does not invent slides not in Beamer
[ ] No dropped content: every Beamer idea appears in Quarto
[ ] R results match: all numbers trace back to the same .rds file
```
