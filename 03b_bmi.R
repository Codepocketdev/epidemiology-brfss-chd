suppressMessages({library(tidyverse); library(survey); library(broom)})

d <- readRDS("brfss_clean.rds") %>% filter(!is.na(chd))
des <- svydesign(ids = ~1, weights = ~LLCPWT, data = d)

m_bmi <- svyglm(chd ~ bmicat + age + sex + race + educ + income + smoker +
                  active + binge,
                design = des, family = quasibinomial())

res <- tidy(m_bmi, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(str_starts(term, "bmicat")) %>%
  transmute(model = "BMI model (no clinical mediators)", term,
            OR = round(estimate, 2), lo = round(conf.low, 2),
            hi = round(conf.high, 2), p = signif(p.value, 3))

print(res)
write_csv(res, "bmi_results.csv")
