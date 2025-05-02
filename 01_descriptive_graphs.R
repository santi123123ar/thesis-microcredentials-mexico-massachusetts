# Descriptive Graphs for Wage, Workforce, and Enrollment Trends
# This script generates 5 core graphs using only:
# - CDMX_FINAL_FILTERED_UPDATED.csv
# - MASSACHUSETTS_FINAL_FILTERED.csv

#1. Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)

# 2. Load main datasets
cdmx_data <- read_csv("CDMX_FINAL_FILTERED_UPDATED.csv")
mass_data <- read_csv("MASSACHUSETTS_FINAL_FILTERED.csv")

# 3. CDMX – Tech Wage Trends (converted from monthly MXN to annual USD)
#The exchange rate of 18.32 MXN/USD reflects the 2024 daily average compiled from Banco de México’s official data.
exchange_rate <- 18.32
wage_cdmx <- cdmx_data %>%
  filter(Year >= 2012) %>%
  mutate(Tech_Wage_CDMX_USD = Avg_Wage_Tech * 12 / exchange_rate) %>%
  select(Year, Tech_Wage_CDMX_USD)

plot_wage_cdmx <- ggplot(wage_cdmx, aes(x = as.factor(Year), y = Tech_Wage_CDMX_USD)) +
  geom_col(fill = "red") +
  labs(
    title = "Tech Wage Trends – CDMX (Annual USD)",
    x = "Year",
    y = "Wage (USD)",
    caption = "Source: ENOE; Exchange Rate: 18.32"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# 4. Massachusetts – Tech Wage Trends
wage_mass <- mass_data %>%
  select(Year = year, Tech_Wage_MA = avg_wage_tech) %>%
  filter(Year >= 2012)

plot_wage_mass <- ggplot(wage_mass, aes(x = as.factor(Year), y = Tech_Wage_MA)) +
  geom_col(fill = "blue") +
  labs(
    title = "Tech Wage Trends – Massachusetts",
    x = "Year",
    y = "Wage (USD)",
    caption = "Source: US Bureau of Labor Statistics"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# 5. Tech Workforce Growth Rate – CDMX vs. Massachusetts (2013–2023)
cdmx_growth <- cdmx_data %>%
  arrange(Year) %>%
  mutate(Tech_Workforce_GrowthRate = 100 * (Tech_Workers_CDMX - lag(Tech_Workers_CDMX)) / lag(Tech_Workers_CDMX)) %>%
  filter(Year >= 2013, Year <= 2023) %>%
  select(Year, Tech_Workforce_GrowthRate) %>%
  mutate(Location = "CDMX")

mass_growth <- mass_data %>%
  arrange(year) %>%
  mutate(Tech_Workforce_GrowthRate = 100 * (tech_workers - lag(tech_workers)) / lag(tech_workers)) %>%
  filter(year >= 2013, year <= 2023) %>%
  select(Year = year, Tech_Workforce_GrowthRate) %>%
  mutate(Location = "Massachusetts")

all_growth <- bind_rows(cdmx_growth, mass_growth)

plot_growth <- ggplot(all_growth, aes(x = Year, y = Tech_Workforce_GrowthRate, color = Location)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Tech Workforce Growth Rate – CDMX vs. Massachusetts",
    x = "Year",
    y = "Growth Rate (%)",
    caption = "Sources: ENOE (CDMX), BLS (Massachusetts)"
  ) +
  scale_color_manual(values = c("CDMX" = "darkred", "Massachusetts" = "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# 6. Higher Education Enrollment – CDMX
enroll_cdmx <- cdmx_data %>%
  select(Year, Enrollment = HigherEd_Enrollment) %>%
  filter(!is.na(Enrollment))

plot_enroll_cdmx <- ggplot(enroll_cdmx, aes(x = as.factor(Year), y = Enrollment)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Higher Education Enrollment – CDMX",
    x = "Year",
    y = "Enrollment",
    caption = "Source: INEGI"
  ) +
  scale_y_continuous(labels = comma_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# 7. Higher Education Enrollment – Massachusetts (fixed y-axis scale)
enroll_mass <- mass_data %>%
  select(Year = year, Enrollment = higher_ed_enrollment) %>%
  filter(!is.na(Enrollment))

plot_enroll_mass <- ggplot(enroll_mass, aes(x = as.factor(Year), y = Enrollment)) +
  geom_col(fill = "purple") +
  labs(
    title = "Higher Education Enrollment – Massachusetts",
    x = "Year",
    y = "Enrollment (FTE)",
    caption = "Source: IPEDS"
  ) +
  scale_y_continuous(labels = comma_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# 8. Export all graphs to a single PDF
pdf("Graphs_final.pdf", height = 8, width = 10)
print(plot_wage_cdmx)
print(plot_wage_mass)
print(plot_growth)
print(plot_enroll_cdmx)
print(plot_enroll_mass)
dev.off()
