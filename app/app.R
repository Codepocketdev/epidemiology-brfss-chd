library(shiny)
library(tidyverse)

d   <- readRDS("data.rds")
res <- read_csv("model_results.csv", show_col_types = FALSE)

exposures <- c("Age group" = "age", "Sex" = "sex", "Race/ethnicity" = "race",
               "Education" = "educ", "Income" = "income", "Smoking" = "smoker",
               "Physical activity" = "active", "Binge drinking" = "binge",
               "High blood pressure" = "htn", "High cholesterol" = "chol",
               "Diabetes" = "diab", "BMI category" = "bmicat")
age_lv <- levels(d$age)

ui <- fluidPage(
  titlePanel("CHD/MI risk factors: BRFSS 2025"),
  tabsetPanel(
    tabPanel("Prevalence explorer",
      sidebarLayout(
        sidebarPanel(
          selectInput("exposure", "Group by", exposures, selected = "smoker"),
          checkboxGroupInput("sex", "Sex", c("Male", "Female"),
                             selected = c("Male", "Female")),
          sliderInput("age", "Age groups", min = 1, max = length(age_lv),
                      value = c(1, length(age_lv)), step = 1),
          textOutput("age_txt")
        ),
        mainPanel(plotOutput("prev_plot"), tableOutput("prev_tab"))
      )),
    tabPanel("Adjusted odds ratios",
      selectInput("model", "Model",
                  c("Crude", "Adj 1: demographic + behavioural",
                    "Adj 2: + clinical"),
                  selected = "Adj 2: + clinical"),
      plotOutput("forest", height = "800px")),
    tabPanel("About",
      p("Data: CDC BRFSS 2025. Outcome: self-reported history of coronary heart disease or myocardial infarction (_MICHD)."),
      p("Prevalence is survey-weighted (_LLCPWT). Confidence intervals use the Kish effective sample size."),
      p("Limitations: cross-sectional design, self-reported data, residual confounding, variance estimated without strata or clusters."))
  )
)

server <- function(input, output, session) {
  output$age_txt <- renderText(
    paste("Ages:", age_lv[input$age[1]], "to", age_lv[input$age[2]]))

  dat <- reactive({
    req(input$sex)
    d %>% filter(sex %in% input$sex,
                 as.integer(age) >= input$age[1],
                 as.integer(age) <= input$age[2])
  })

  prev <- reactive({
    v <- input$exposure
    dat() %>%
      filter(!is.na(.data[[v]])) %>%
      group_by(level = .data[[v]]) %>%
      summarise(n = n(),
                prev = weighted.mean(chd, LLCPWT),
                neff = sum(LLCPWT)^2 / sum(LLCPWT^2),
                .groups = "drop") %>%
      mutate(se = sqrt(prev * (1 - prev) / neff),
             lo = pmax(0, prev - 1.96 * se),
             hi = prev + 1.96 * se,
             across(c(prev, lo, hi), ~ 100 * .x))
  })

  output$prev_plot <- renderPlot({
    ggplot(prev(), aes(level, prev)) +
      geom_col(fill = "#c0392b") +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
      labs(x = NULL, y = "Weighted prevalence of CHD/MI (%)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$prev_tab <- renderTable({
    prev() %>% transmute(Level = as.character(level), N = n,
                         `Prevalence %` = round(prev, 2),
                         `95% CI` = paste0(round(lo, 2), " - ", round(hi, 2)))
  })

  output$forest <- renderPlot({
    r <- res %>% filter(model == input$model) %>%
      mutate(term = fct_rev(fct_inorder(term)))
    ggplot(r, aes(OR, term, xmin = lo, xmax = hi)) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
      geom_pointrange(size = 0.3) +
      scale_x_log10() +
      labs(x = "Odds ratio (log scale, 95% CI)", y = NULL) +
      theme_minimal(base_size = 12)
  })
}

shinyApp(ui, server)
