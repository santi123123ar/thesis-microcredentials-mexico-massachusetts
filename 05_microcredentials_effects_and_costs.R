# Microcredentials: Interaction Effects and Cost Comparison
# This script estimates models with microcredential exposure and visualizes cost differences.

library(fixest)
library(dplyr)
library(readr)
library(modelsummary)
library(ggplot2)

# Load panel and microcredentials data
panel <- read_csv("panel_data_final.csv")
micro <- read_csv("MICROCREDENTIALS_FINAL_FILTERED_UPDATED.csv")

# Create normalized exposure indicator for microcredentials
micro <- micro %>%
  mutate(MC_Exposure = Registered_Learners_M_Coursera / max(Registered_Learners_M_Coursera, na.rm = TRUE)) %>%
  select(Year, MC_Exposure)

# Merge microcredential exposure into panel
panel <- panel %>%
  left_join(micro, by = "Year") %>%
  mutate(CDMX = ifelse(Region == "CDMX", 1, 0),
         MC_Treatment = CDMX * MC_Exposure)

# DiD models with microcredential interaction
modelo_mc_wages <- feols(
  avg_wage_tech ~ MC_Treatment + higher_ed_enrollment + unemployment_rate + university_cost_usd | Region,
  data = panel,
  cluster = ~Region
)
modelo_mc_empleo <- feols(
  tech_workers ~ MC_Treatment + higher_ed_enrollment + unemployment_rate + university_cost_usd | Region,
  data = panel,
  cluster = ~Region
)

# Display results with clear labels
modelsummary(
  list(
    "Tech Wages – MC Effect" = modelo_mc_wages,
    "Tech Employment – MC Effect" = modelo_mc_empleo
  ),
  coef_rename = c(
    "MC_Treatment" = "Microcredentials × CDMX",
    "higher_ed_enrollment" = "Education Enrollment",
    "unemployment_rate" = "Unemployment Rate",
    "university_cost_usd" = "University Cost (USD)"
  ),
  stars = TRUE,
  gof_omit = "Adj|Within|Pseudo|Log|FE",
  title = "Table: Interaction Effects – Microcredential Exposure and CDMX"
)

# Visualize total degree cost comparison (universities vs microcredentials)
costo_comparativo <- tibble::tibble(
  Category = c(
    "Massachusetts – Traditional University",
    "CDMX – High-end (Tec de Monterrey)",
    "Microcredentials (Global Average)",
    "CDMX – Average Private University",
    "CDMX – Low-cost (UNAM/IPN)"
  ),
  Total_Cost_USD = c(
    239183,   # 4 years in Massachusetts
    60000,    # Tec de Monterrey estimate
    19058,    # Microcredentials average
    18442,    # CDMX private average
    400       # UNAM/IPN estimate
  )
)

ggplot(costo_comparativo, aes(x = reorder(Category, Total_Cost_USD), y = Total_Cost_USD, fill = Category)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Total Degree Cost Comparison",
    subtitle = "Traditional Universities vs. Microcredentials (2012–2024)",
    x = NULL,
    y = "Total Cost (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(
    "#00BFFF", "#BDB300", "#EE82EE", "#FF7F7F", "#00C27C"
  )) +
  scale_y_continuous(labels = scales::dollar)
