# The Concessional Shield: Financing Structure and the Cost of Sovereign Debt in Sub-Saharan Africa

Master thesis, M2 International Economics and Development, Université Paris Dauphine-PSL (2025-2026).
Author: Hawa Kebe. Supervisor: Marc Raffinot (LEDa-DIAL). Grade: 16/20.

## Summary

The thesis asks why Sub-Saharan African sovereigns borrow at such different costs across creditor segments, and how the erosion of concessional finance changes the average cost of their debt. It builds a panel of developing countries over 2000-2024 and aggregates the African Debt Database to the country-year-segment level across five creditor segments (multilateral, bilateral, China, external market, domestic). Four hypotheses are tested with panel regressions (year fixed effects in selected specifications, standard errors clustered by country).

## Contents

| File | Description |
|---|---|
| `Master_Thesis_KEBE_The_Concessional_Shield.pdf` | Full thesis (English version, translated from the French original) |
| `script_R_ENG.R` | Replication script: data construction, descriptive statistics, figures, regressions and LaTeX tables |

## Data

The raw data are not redistributed here. They are public and can be downloaded from:

- World Bank DataBank: International Debt Statistics, World Development Indicators, Worldwide Governance Indicators (exports named `*_Data.csv`)
- UNU-WIDER Government Revenue Dataset (`grd-data.csv`)
- African Debt Database, Kiel Institute, May 2026 version (`ADD_v2026May_a.xlsx`)
- OECD-DAC official development assistance statistics (`Long-term-ODA.xlsx`, `2025-preliminary-data.xlsx`)

## How to run

Software: R with the packages `tidyverse`, `readxl`, `scales`, `countrycode`, `fixest` and `ggcorrplot` (installed automatically if missing).

1. Put the raw files in one folder.
2. Edit the `setwd()` line at the top of the script so that it points to that folder.
3. Run the script. Tables and figures are written to an `output` sub-folder.
