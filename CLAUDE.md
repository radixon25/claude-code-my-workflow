# CLAUDE.MD -- Academic Project Development with Claude Code

**Project:** ASH Constraint Analysis
**Institution:** A Safe Haven
**Branch:** main

---

## Core Principles

- **Plan first** -- enter plan mode before non-trivial tasks; save plans to `quality_reports/plans/`
- **Verify after** -- compile/render and confirm output at the end of every task
- **Single source of truth** -- R analysis scripts (`scripts/R/`) are authoritative for data and results; Beamer slides and Quarto reports are derived presentation layers
- **Quality gates** -- nothing ships below 80/100
- **[LEARN] tags** -- when corrected, save `[LEARN:category] wrong → right` to MEMORY.md

---

## Folder Structure

```
ASH-Constraint-Analysis/
├── CLAUDE.MD                    # This file
├── .claude/                     # Rules, skills, agents, hooks
├── Bibliography_base.bib        # Centralized bibliography
├── Figures/                     # Generated plots and images
├── Preambles/header.tex         # LaTeX headers (when created)
├── Slides/                      # Beamer .tex files (presentations)
├── Quarto/                      # RevealJS .qmd + HTML reports + theme
├── docs/                        # GitHub Pages (auto-generated)
├── scripts/
│   ├── R/                       # Analysis pipeline
│   │   ├── config.R             # Centralized config (year, periods, paths)
│   │   ├── SR_Report_Framework.R # Registry-driven report orchestrator
│   │   ├── SR_FQ_Reshape.R      # Salesforce data reshaping
│   │   ├── SR_FQ_PlotFunctions.R # Plotting utilities
│   │   ├── filter_period.R      # Period filtering
│   │   ├── period_comparison.R  # Period-over-period comparison
│   │   ├── input_effects.R      # Input effects modeling
│   │   ├── staffing_levels.R    # Excel schedule → tidy staffing
│   │   ├── staffing_impact.R    # Staffing → closure analysis
│   │   ├── staffing_compliance.R # Staffing → compliance analysis
│   │   └── Data/                # Raw data + mappings (gitignored)
│   ├── quality_score.py         # Quality scoring (0-100)
│   └── sync_to_docs.sh          # Quarto → GitHub Pages deployment
├── quality_reports/             # Plans, session logs, merge reports
├── explorations/                # Research sandbox
├── templates/                   # Session log, quality report templates
└── master_supporting_docs/      # Papers and reference materials
```

---

## Commands

```bash
# R analysis pipeline
Rscript scripts/R/config.R

# LaTeX (3-pass, XeLaTeX only)
cd Slides
xelatex -interaction=nonstopmode file.tex
bibtex file
xelatex -interaction=nonstopmode file.tex
xelatex -interaction=nonstopmode file.tex

# Quarto render
quarto render Quarto/file.qmd

# Quality score
python scripts/quality_score.py Quarto/file.qmd
```

---

## Quality Thresholds

| Score | Gate | Meaning |
|-------|------|---------|
| 80 | Commit | Good enough to save |
| 90 | PR | Ready for deployment |
| 95 | Excellence | Aspirational |

---

## Skills Quick Reference

| Command | What It Does |
|---------|-------------|
| `/compile-latex [file]` | 3-pass XeLaTeX + bibtex |
| `/deploy [LectureN]` | Render Quarto + sync to docs/ |
| `/extract-tikz [LectureN]` | TikZ → PDF → SVG |
| `/proofread [file]` | Grammar/typo/overflow review |
| `/visual-audit [file]` | Slide layout audit |
| `/pedagogy-review [file]` | Narrative, notation, pacing review |
| `/review-r [file]` | R code quality review |
| `/qa-quarto [LectureN]` | Adversarial Quarto vs Beamer QA |
| `/slide-excellence [file]` | Combined multi-agent review |
| `/translate-to-quarto [file]` | Beamer → Quarto translation |
| `/validate-bib` | Cross-reference citations |
| `/devils-advocate` | Challenge slide design |
| `/create-lecture` | Full lecture creation |
| `/commit [msg]` | Stage, commit, PR, merge |
| `/lit-review [topic]` | Literature search + synthesis |
| `/research-ideation [topic]` | Research questions + strategies |
| `/interview-me [topic]` | Interactive research interview |
| `/review-paper [file]` | Manuscript review |
| `/data-analysis [dataset]` | End-to-end R analysis |

---

## Beamer Custom Environments

<!-- TBD: Will define environments as slides are built.
     Candidate environments: findingbox, constraintbox, recommendationbox -->

| Environment       | Effect        | Use Case       |
|-------------------|---------------|----------------|
| TBD | -- | -- |

## Quarto CSS Classes

<!-- TBD: Will define classes as reports/slides are built.
     Theme file: Quarto/ash-theme.scss -->

| Class              | Effect        | Use Case       |
|--------------------|---------------|----------------|
| TBD | -- | -- |

---

## Current Project State

| Module | R Script(s) | Beamer | Quarto | Status |
|--------|-------------|--------|--------|--------|
| SR Report Framework | `SR_Report_Framework.R`, `SR_FQ_Reshape.R` | -- | -- | Production |
| Period Filtering | `filter_period.R`, `period_comparison.R` | -- | -- | Production |
| Staffing Pipeline | `staffing_levels.R` | -- | -- | Production |
| Staffing Impact | `staffing_impact.R`, `staffing_compliance.R` | -- | -- | Production |
| Input Effects | `input_effects.R` | -- | -- | Exploratory |
| Weather Effects | TBD | -- | -- | Not started |
| Bed Availability | TBD | -- | -- | Not started |
| Executive Summary | -- | TBD | TBD | Not started |
