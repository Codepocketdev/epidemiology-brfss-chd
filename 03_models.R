suppressMessages({library(tidyverse); library(survey); library(broom)})
options(survey.lonely.psu = "adjust")

d <- readRDS("brfss_clean.rds") %>% filter(!is.na(chd))
des <- svydesign(ids = ~1, weights = ~LLCPWT,
                 data = d)

fit <- function(f) svyglm(f, design = des, family = quasibinomial())
or_tab <- function(m, label) {
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(model = label, term, OR = round(estimate, 2),
              lo = round(conf.low, 2), hi = round(conf.high, 2),
              p = signif(p.value, 3))
}

# ---- 1. Crude ORs, one exposure at a time ----
exposures <- c("sex","age","race","educ","income","marital","employ","smoker",
               "active","anydrink","binge","heavy","htn","chol","diab",
               "bmicat","genhlth")
crude <- map_dfr(exposures, ~ or_tab(fit(reformulate(.x, "chd")), "Crude"))
cat("\nCrude done\n")

# ---- 2. Nested adjusted models ----
m1 <- fit(chd ~ age + sex + race + educ + income + smoker + active + binge)
m2 <- update(m1, . ~ . + htn + chol + diab + bmicat)
adj <- bind_rows(or_tab(m1, "Adj 1: demographic + behavioural"),
                 or_tab(m2, "Adj 2: + clinical"))
cat("\nAdjusted done\n")

# ---- 3. Effect modification: sex x smoking ----
m_int <- update(m2, . ~ . + sex:smoker)
cat("\nInteraction test (sex x smoker):\n")
print(regTermTest(m_int, ~ sex:smoker))

strat <- map_dfr(c("Male","Female"), function(s) {
  m <- svyglm(chd ~ age + race + educ + income + smoker + active + binge +
                htn + chol + diab + bmicat,
              design = subset(des, sex == s), family = quasibinomial())
  or_tab(m, paste("Smoking OR,", s)) %>% filter(str_starts(term, "smoker"))
})

write_csv(bind_rows(crude, adj, strat), "model_results.csv")
cat("\nSaved model_results.csv\n")
print(strat)
