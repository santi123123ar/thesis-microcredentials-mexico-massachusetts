# Difference-in-Differences Models and Robustness Checks
# This script estimates DiD models for wages and employment, and runs robustness checks.

library(dplyr)
library(broom)
library(flextable)
library(officer)
library(ggplot2)
library(fixest)
library(modelsummary)
library(readr)

# Load panel data
panel <- read_csv("panel_data_final.csv")

# Baseline DiD models
modelo_salarios <- lm(avg_wage_tech ~ Interaction + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel)
modelo_empleo <- lm(tech_workers ~ Interaction + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel)

# Robustness: log transformations and exclusion of certain years
panel <- panel %>%
  mutate(
    log_avg_wage = log(avg_wage_tech),
    log_tech_workers = log(tech_workers)
  )

log_model_wages <- lm(log_avg_wage ~ Interaction + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel)
log_model_employment <- lm(log_tech_workers ~ Interaction + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel)

# Exclude 2020 (pandemic shock year)
panel_no_2020 <- panel %>% filter(Year != 2020)
model_no2020_wages <- lm(avg_wage_tech ~ Interaction + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel_no_2020)
model_no2020_employment <- lm(tech_workers ~ Interaction + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel_no_2020)

# Exclude 2016–2019 for CDMX (data gaps)
panel_clean <- panel %>% filter(!(Region == "CDMX" & Year %in% 2016:2019))
model_clean_wages <- lm(avg_wage_tech ~ Interaction + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel_clean)
model_clean_employment <- lm(tech_workers ~ Interaction + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel_clean)

# Models without controls (just DiD)
model_simple_wages <- lm(avg_wage_tech ~ Interaction, data = panel)
model_simple_employment <- lm(tech_workers ~ Interaction, data = panel)

# Collect all results for summary table
models <- list(
  "Baseline – Wages" = modelo_salarios,
  "Baseline – Employment" = modelo_empleo,
  "Log – Wages" = log_model_wages,
  "Log – Employment" = log_model_employment,
  "No 2020 – Wages" = model_no2020_wages,
  "No 2020 – Employment" = model_no2020_employment,
  "Clean Panel – Wages" = model_clean_wages,
  "Clean Panel – Employment" = model_clean_employment,
  "No Controls – Wages" = model_simple_wages,
  "No Controls – Employment" = model_simple_employment
)

robust_table <- lapply(names(models), function(name) {
  model <- models[[name]]
  coef <- tidy(model, conf.int = TRUE) %>% filter(term == "Interaction")
  fit <- glance(model)
  data.frame(
    Model = name,
    Estimate = round(coef$estimate, 2),
    Std.Error = round(coef$std.error, 2),
    P.Value = signif(coef$p.value, 6),
    CI_Low = round(coef$conf.low, 2),
    CI_High = round(coef$conf.high, 2),
    R2 = round(fit$r.squared, 3),
    N = fit$nobs
  )
}) %>% bind_rows()

# Create a summary table for reporting
flextable(robust_table) %>%
  set_caption("Robustness Checks – DiD Coefficients and Model Fit") %>%
  autofit()

# Parallel trends graphs (pre-treatment years only)
panel_pretrend <- panel %>% filter(Year < 2020)
ggplot(panel_pretrend, aes(x = Year, y = avg_wage_tech, color = Region)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  labs(
    title = "Parallel Trends in Tech Wages (Pre-2020)",
    x = "Year",
    y = "Average Tech Wage (USD)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("CDMX" = "#0072B2", "Massachusetts" = "#D55E00"))

ggplot(panel_pretrend, aes(x = Year, y = tech_workers, color = Region)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  labs(
    title = "Parallel Trends in Tech Workforce Size (Pre-2020)",
    x = "Year",
    y = "Number of Tech Workers"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("CDMX" = "#0072B2", "Massachusetts" = "#D55E00"))

# Placebo test: simulate a fake treatment in 2018 to check for spurious effects
panel <- panel %>%
  mutate(
    Post2018 = ifelse(Year >= 2018, 1, 0),
    DiD_placebo = Treatment * Post2018
  )
panel_pre2020 <- panel %>% filter(Year < 2020)

modelo_placebo_wages_simple <- lm(avg_wage_tech ~ DiD_placebo + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel_pre2020)
modelo_placebo_empleo_simple <- lm(tech_workers ~ DiD_placebo + higher_ed_enrollment + unemployment_rate + university_cost_usd, data = panel_pre2020)

# Display placebo coefficients
tidy(modelo_placebo_wages_simple, conf.int = TRUE) %>% filter(term == "DiD_placebo")
tidy(modelo_placebo_empleo_simple, conf.int = TRUE) %>% filter(term == "DiD_placebo")
