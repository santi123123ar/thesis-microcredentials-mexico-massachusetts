# Descriptive Graphs for Wage, Workforce, and Enrollment Trends
# This script generates all main graphs for the thesis using cleaned datasets.

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)

# Set color consistency
region_colors <- c("CDMX" = "darkred", "Massachusetts" = "blue")

setwd("~/CVS TESIS/CDMX:MASSACHUSETTS:MICROCREDENTIALS")
cdmx_data <- read_csv("CDMX_FINAL_FILTERED_UPDATED.csv")
mass_data <- read_csv("MASSACHUSETTS_FINAL_FILTERED.csv")
fte_mass <- read_csv("~/CVS TESIS/Massachusetts/IPEDS - Integrated Postsecondary Education Data System/12-MONTH ENROLLMENT/massachusetts_fte_enrollment_final.csv")

# 1. Tech Wage Trends – CDMX (converted to USD at 1 USD = 18.33 MXN)
exchange_rate <- 18.33

wage_cdmx <- cdmx_data %>%
  select(Year, Tech_Wage_CDMX = Avg_Wage_Tech) %>%
  filter(Year >= 2012) %>%
  mutate(Tech_Wage_CDMX_USD = Tech_Wage_CDMX / exchange_rate)

plot_wage_cdmx <- ggplot(wage_cdmx, aes(x = as.factor(Year), y = Tech_Wage_CDMX)) +
  geom_col(fill = "darkred") +
  labs(
    title = "Tech Wage Trends – CDMX (USD)",
    x = "Year",
    y = "Wage (USD)",
    caption = "Source: ENOE. Exchange rate: 1 USD = 18.33 MXN"
  ) +
  scale_y_continuous(labels = label_dollar()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# 2. Tech Wage Trends – Massachusetts
wage_mass <- mass_data %>%
  select(Year = year, Tech_Wage_MA = avg_wage_tech) %>%
  filter(Year >= 2012)
plot_wage_mass <- ggplot(wage_mass, aes(x = as.factor(Year), y = Tech_Wage_MA)) +
  geom_col(fill = region_colors["Massachusetts"]) +
  labs(
    title = "Tech Wage Trends – Massachusetts",
    x = "Year",
    y = "Wage (USD)",
    caption = "Source: US Bureau of Labor Statistics"
  ) +
  scale_y_continuous(labels = comma_format(scale = 1e-3, suffix = "k")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# 3. Tech Workforce Growth Rate – CDMX vs Massachusetts
cdmx_growth <- cdmx_data %>%
  arrange(Year) %>%
  mutate(Tech_Workforce_GrowthRate = 100 * (Tech_Workers_CDMX - lag(Tech_Workers_CDMX)) / lag(Tech_Workers_CDMX)) %>%
  filter(Year >= 2012, Year <= 2023) %>%
  select(Year, Tech_Workforce_GrowthRate) %>%
  mutate(Location = "CDMX")
mass_growth <- mass_data %>%
  arrange(year) %>%
  mutate(Tech_Workforce_GrowthRate = 100 * (tech_workers - lag(tech_workers)) / lag(tech_workers)) %>%
  filter(year >= 2012, year <= 2023) %>%
  select(Year = year, Tech_Workforce_GrowthRate) %>%
  mutate(Location = "Massachusetts")
all_growth <- bind_rows(cdmx_growth, mass_growth)
plot_growth <- ggplot(all_growth, aes(x = Year, y = Tech_Workforce_GrowthRate, color = Location)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Tech Workforce Growth Rate – CDMX vs Massachusetts",
    x = "Year",
    y = "Growth Rate (%)",
    caption = "Sources: CDMX: ENOE; Massachusetts: US Bureau of Labor Statistics"
  ) +
  scale_color_manual(values = region_colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# 4. Higher Education Enrollment – CDMX
enroll_cdmx <- cdmx_data %>%
  select(Year, Enrollment = HigherEd_Enrollment) %>%
  filter(!is.na(Enrollment))
plot_enroll_cdmx <- ggplot(enroll_cdmx, aes(x = as.factor(Year), y = Enrollment)) +
  geom_col(fill = region_colors["CDMX"]) +
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

# 5. Higher Education Enrollment – Massachusetts
fte_mass <- fte_mass %>%
  rename(Year = year, Enrollment = higher_ed_enrollment)
plot_enroll_mass <- ggplot(fte_mass, aes(x = as.factor(Year), y = Enrollment)) +
  geom_col(fill = region_colors["Massachusetts"]) +
  labs(
    title = "Higher Education Enrollment – Massachusetts",
    x = "Year",
    y = "Enrollment (FTE)",
    caption = "Source: IPEDS – Integrated Postsecondary Education Data System"
  ) +
  scale_y_continuous(labels = comma_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# Save all plots to a single PDF for thesis appendix
pdf("Graphs_final.pdf", height = 8, width = 10)
print(plot_wage_cdmx)
print(plot_wage_mass)
print(plot_growth)
print(plot_enroll_cdmx)
print(plot_enroll_mass)
dev.off()

