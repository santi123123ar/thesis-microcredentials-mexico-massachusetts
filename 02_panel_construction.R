# Panel Construction: Combine CDMX and Massachusetts data for DiD analysis
# This script builds the final panel dataset with harmonized variables and treatment indicators.

library(readr)
library(dplyr)

# Load cleaned datasets
cdmx_data <- read_csv("CDMX_FINAL_FILTERED_UPDATED.csv")
mass_data <- read_csv("Massachusetts_Final_Filtered.csv")

# Prepare CDMX panel: set treatment group and create DiD indicators
cdmx_panel <- cdmx_data %>%
  transmute(
    Region = "CDMX",
    Year,
    avg_wage_tech = Avg_Wage_Tech,
    tech_workers = Tech_Workers_CDMX,
    unemployment_rate = Unemployment_Rate,
    higher_ed_enrollment = HigherEd_Enrollment,
    informality_rate = Informality_Rate_Tech,
    university_cost_usd = University_Annual_RealPrice_USD,
    Treatment = 1,
    Post = ifelse(Year >= 2020, 1, 0),
    Interaction = Treatment * Post
  )

# Prepare Massachusetts panel: set control group and DiD indicators
mass_panel <- mass_data %>%
  transmute(
    Region = "Massachusetts",
    Year = year,
    avg_wage_tech,
    tech_workers,
    unemployment_rate,
    higher_ed_enrollment = higher_ed_enrollment,
    informality_rate = NA,
    university_cost_usd = tuition_private,
    Treatment = 0,
    Post = ifelse(Year >= 2020, 1, 0),
    Interaction = Treatment * Post
  )

# Combine both regions into a single panel
panel <- bind_rows(cdmx_panel, mass_panel)

# Save final panel for modeling
write_csv(panel, "panel_data_final.csv")
