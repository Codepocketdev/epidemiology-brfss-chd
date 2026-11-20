suppressMessages({library(tidyverse)})

# ---- Forest plot: adjusted model 2 ----
r <- read_csv("model_results.csv", show_col_types = FALSE) %>%
  filter(model == "Adj 2: + clinical") %>%
  mutate(term = fct_rev(fct_inorder(term)),
         group = case_when(
           str_starts(term, "age") ~ "Age",
           str_starts(term, "sex") ~ "Sex",
           str_starts(term, "race") ~ "Race/ethnicity",
           str_starts(term, "educ") ~ "Education",
           str_starts(term, "income") ~ "Income",
           str_starts(term, "smoker") ~ "Smoking",
           str_starts(term, "active|binge") ~ "Behaviour",
           TRUE ~ "Clinical"))

p1 <- ggplot(r, aes(x = OR, y = term, xmin = lo, xmax = hi, colour = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_pointrange(size = 0.3) +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8, 16)) +
  labs(title = "Adjusted odds ratios for CHD/MI (BRFSS 2025)",
       x = "Odds ratio (log scale, 95% CI)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 10)
ggsave("forest.png", p1, width = 8, height = 10, dpi = 150)

# ---- Weighted prevalence by age ----
d <- readRDS("brfss_clean.rds") %>% filter(!is.na(chd), !is.na(age))
pa <- d %>% group_by(age) %>%
  summarise(prev = 100 * weighted.mean(chd, LLCPWT), .groups = "drop")

p2 <- ggplot(pa, aes(age, prev)) +
  geom_col(fill = "#c0392b") +
  geom_text(aes(label = round(prev, 1)), vjust = -0.4, size = 3) +
  labs(title = "Weighted CHD/MI prevalence by age group",
       x = "Age group", y = "Prevalence (%)") +
  theme_minimal(base_size = 11)
ggsave("prev_age.png", p2, width = 8, height = 5, dpi = 150)

cat("Saved forest.png and prev_age.png\n")
