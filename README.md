# Epidemiology in R: CHD/MI risk factors (BRFSS 2025)

Survey-weighted analysis of self-reported coronary heart disease or myocardial infarction (CHD/MI) using CDC BRFSS 2025 data.

## What's in here
- `01_clean_prevalence.R`: load, recode, weighted prevalence
- `02_table1.R`: weighted Table 1 by CHD/MI status
- `03_models.R`: crude and adjusted odds ratios, sex x smoking interaction
- `03b_bmi.R`: BMI model without clinical mediators
- `04_plots.R`: forest plot and prevalence by age
- `app/`: Shiny app
- `epidemiology.Rmd`: starter notebook
- `codebook25_llcp-v2-508.html`, `meaning_in_codebook.csv`: codebook files

## Setup
Install R packages once (`survey` must be version 4.5 or newer):

~~r
install.packages(c("tidyverse", "haven", "survey", "gtsummary", "broom", "shiny"))
~~

## Run the Shiny app
No raw data needed, the app ships with its own data file:

~~r
shiny::runApp("app")
~~

## Rerun the analysis
1. Put `LLCP2025.XPT` (CDC BRFSS 2025) in the project folder. It is too large for GitHub.
2. Open the folder as an RStudio project, so paths resolve.
3. Run in order: `01_clean_prevalence.R`, `02_table1.R`, `03_models.R`, `03b_bmi.R`, `04_plots.R`

`01` creates `brfss_clean.rds`, which the other scripts read.

## Methods notes
- Outcome: `_MICHD` (1 = CHD/MI, 2 = none). Respondents with a missing outcome are excluded.
- Codes 7, 9 and blanks are recoded to missing.
- Estimates are weighted with `_LLCPWT`. Variance is estimated with weights only (no strata or clusters), so confidence intervals may be slightly off.
- Cross-sectional, self-reported data: associations only, no causal claims.
