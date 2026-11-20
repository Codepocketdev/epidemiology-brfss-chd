library(tidyverse)
library(haven)
library(survey)
library(gtsummary)

options(survey.lonely.psu = "adjust")

# ---- 1. Load only the columns we need ----
vars <- c("_MICHD","SEXVAR","_AGEG5YR","_RACE","_EDUCAG","_INCOMG1",
          "MARITAL","EMPLOY1","_SMOKER3","_TOTINDA","DRNKANY6",
          "_RFBING6","_RFDRHV9","BPHIGH6","BPMEDS1","TOLDHI3",
          "CHOLMED3","DIABETE4","_BMI5","_BMI5CAT","GENHLTH",
          "_LLCPWT","_STSTR","_PSU")

raw <- read_xpt("LLCP2025.XPT", col_select = all_of(vars))

# ---- 2. Recode (7/9/blank -> NA, first level = reference) ----
d <- raw %>%
  rename_with(~ sub("^_", "", .x)) %>%
  mutate(
    chd   = case_when(MICHD == 1 ~ 1, MICHD == 2 ~ 0, TRUE ~ NA_real_),
    chd_f = factor(chd, levels = c(0, 1), labels = c("No CHD/MI", "CHD/MI")),
    sex   = factor(SEXVAR, 1:2, c("Male", "Female")),
    age   = factor(AGEG5YR, 1:13,
                   c("18-24","25-29","30-34","35-39","40-44","45-49","50-54",
                     "55-59","60-64","65-69","70-74","75-79","80+")),
    race  = factor(RACE, 1:8,
                   c("White NH","Black NH","AI/AN NH","Asian NH","NHOPI NH",
                     "Other NH","Multiracial NH","Hispanic")),
    educ  = factor(EDUCAG, 1:4,
                   c("<High school","High school","Some college","College grad")),
    income = factor(INCOMG1, 1:7,
                   c("<15k","15-25k","25-35k","35-50k","50-100k","100-200k","200k+")),
    marital = factor(MARITAL, 1:6,
                   c("Married","Divorced","Widowed","Separated","Never married","Unmarried couple")),
    employ = factor(EMPLOY1, 1:8,
                   c("Employed","Self-employed","Out of work 1y+","Out of work <1y",
                     "Homemaker","Student","Retired","Unable to work")),
    smoker = factor(SMOKER3, c(4, 3, 2, 1),
                   c("Never","Former","Current some days","Current daily")),
    active = factor(TOTINDA, 1:2, c("Active","Inactive")),
    anydrink = factor(DRNKANY6, 1:2, c("No drink","Any drink")) %>% fct_relevel("No drink"),
    binge  = factor(RFBING6, 1:2, c("No","Yes")),
    heavy  = factor(RFDRHV9, 1:2, c("No","Yes")),
    htn    = factor(BPHIGH6, c(3, 1), c("No","Yes")),
    chol   = factor(TOLDHI3, c(2, 1), c("No","Yes")),
    diab   = factor(DIABETE4, c(3, 4, 1), c("No","Prediabetes","Yes")),
    bmi    = BMI5 / 100,
    bmicat = factor(BMI5CAT, c(2, 1, 3, 4),
                   c("Normal","Underweight","Overweight","Obese")),
    genhlth = factor(GENHLTH, 1:5, c("Excellent","Very good","Good","Fair","Poor"))
  )

# BPMEDS1 / CHOLMED3 are only asked of people who answered yes to
# high BP / cholesterol, so their blanks are structural. Left out of Table 1.

saveRDS(d, "brfss_clean.rds")

# ---- 3. Survey design, then restrict to the study population ----
des <- svydesign(ids = ~1, weights = ~LLCPWT,
                 data = d)
des_sub <- subset(des, !is.na(chd))

cat("\nStudy population (non-missing outcome):", nrow(d %>% filter(!is.na(chd))), "\n")

# ---- 4. Prevalence: unweighted vs weighted ----
cat("\nUnweighted prevalence of CHD/MI (%):\n")
print(round(100 * mean(d$chd, na.rm = TRUE), 2))

cat("\nWeighted prevalence of CHD/MI (%), 95% CI:\n")
p <- svymean(~chd, des_sub, na.rm = TRUE)
print(round(100 * cbind(prev = coef(p), confint(p)), 2))

cat("\nWeighted prevalence by sex (%):\n")
print(svyby(~chd, ~sex, des_sub, svymean, na.rm = TRUE) %>%
        mutate(across(c(chd, se), ~ round(100 * .x, 2))))

cat("\nWeighted prevalence by age group (%):\n")
print(svyby(~chd, ~age, des_sub, svymean, na.rm = TRUE) %>%
        mutate(across(c(chd, se), ~ round(100 * .x, 2))))

# ---- 5. Weighted Table 1 by outcome ----
t1 <- tbl_svysummary(
  des_sub,
  by = chd_f,
  include = c(sex, age, race, educ, income, marital, employ, smoker, active,
              anydrink, binge, heavy, htn, chol, diab, bmi, bmicat, genhlth),
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  digits = list(all_continuous() ~ 1),
  missing = "ifany"
) %>% add_overall()

write_csv(as_tibble(t1), "table1.csv")
cat("\nTable 1 saved to table1.csv\n")
