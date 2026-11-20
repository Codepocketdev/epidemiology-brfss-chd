suppressMessages({library(tidyverse); library(survey); library(gtsummary)})
options(survey.lonely.psu = "adjust")

d <- readRDS("brfss_clean.rds") %>%
  filter(!is.na(chd)) %>%
  select(chd, chd_f, sex, age, race, educ, income, marital, employ, smoker,
         active, anydrink, binge, heavy, htn, chol, diab, bmi, bmicat,
         genhlth, STSTR, LLCPWT)

des <- svydesign(ids = ~1, weights = ~LLCPWT,
                 data = d)

cat("\nWeighted prevalence by age group (%):\n")
print(svyby(~chd, ~age, des, svymean, na.rm = TRUE) %>%
        mutate(across(c(chd, se), ~ round(100 * .x, 2))))

t1 <- tbl_svysummary(
  des,
  by = chd_f,
  include = c(sex, age, race, educ, income, marital, employ, smoker, active,
              anydrink, binge, heavy, htn, chol, diab, bmi, bmicat, genhlth),
  statistic = list(all_continuous() ~ "{mean} ({sd})"),
  digits = list(all_continuous() ~ 1),
  missing = "ifany"
) %>% add_overall()

write_csv(as_tibble(t1), "table1.csv")
cat("\nTable 1 saved to table1.csv\n")
