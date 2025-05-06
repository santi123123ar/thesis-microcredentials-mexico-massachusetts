# Heterogeneous Effects: DiD models by period (pre, post-2020, post-2021)
# This script estimates models with period-by-treatment interactions.

library(dplyr)
library(fixest)
library(modelsummary)
library(readr)

# Load panel data
panel <- read_csv("panel_data_final.csv")

# Define periods and create interaction terms
panel <- panel %>%
  mutate(
    Period = case_when(
      Year < 2020 ~ "Pre",
      Year == 2020 ~ "Post2020",
      Year > 2020 ~ "Post2021"
    ),
    Period = factor(Period, levels = c("Pre", "Post2020", "Post2021")),
    Period_Treatment = interaction(Treatment, Period)
  )

# Estimate heterogeneous DiD models (fixed effects by region, clustered SEs)
modelo_hetero_wages <- feols(
  avg_wage_tech ~ i(Period_Treatment, ref = "1.Pre") +
    higher_ed_enrollment + unemployment_rate + university_cost_usd | Region,
  data = panel,
  cluster = ~Region
)

modelo_hetero_empleo <- feols(
  tech_workers ~ i(Period_Treatment, ref = "1.Pre") +
    higher_ed_enrollment + unemployment_rate + university_cost_usd | Region,
  data = panel,
  cluster = ~Region
)

# Display results with clear labels in the Viewer
modelsummary(
  list(
    "Tech Wages – Extended Periods" = modelo_hetero_wages,
    "Tech Employment – Extended Periods" = modelo_hetero_empleo
  ),
  coef_rename = c(
    "Period_Treatment1.Pre" = "CDMX × Pre",
    "Period_Treatment1.Post2020" = "CDMX × Post-2020",
    "Period_Treatment1.Post2021" = "CDMX × Post-2021",
    "Period_Treatment0.Pre" = "Massachusetts × Pre",
    "Period_Treatment0.Post2020" = "Massachusetts × Post-2020",
    "Period_Treatment0.Post2021" = "Massachusetts × Post-2021",
    "higher_ed_enrollment" = "Education Enrollment",
    "unemployment_rate" = "Unemployment Rate",
    "university_cost_usd" = "University Cost (USD)"
  ),
  stars = TRUE,
  gof_omit = "Adj|Within|Pseudo|Log|FE",
  title = "Table: Heterogeneous Effects across Pre / Post-2020 / Post-2021",
)

