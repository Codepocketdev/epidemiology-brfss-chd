# Epidemiology in R: CHD/MI risk factors (BRFSS 2025)

Survey-weighted analysis of self-reported coronary heart disease / myocardial infarction using CDC BRFSS 2025.

## Run order
1. Put `LLCP2025.XPT` (from CDC) in this folder
2. `01_clean_prevalence.R`: clean, recode, weighted prevalence
3. `02_table1.R`: weighted Table 1
4. `03_models.R`: crude and adjusted ORs, sex x smoking interaction
5. `03b_bmi.R`: BMI model without clinical mediators
6. `04_plots.R`: forest plot, prevalence by age
7. `app/`: Shiny app, run with `shiny::runApp("app")`

Note: variance is estimated with weights only (no strata or clusters).
