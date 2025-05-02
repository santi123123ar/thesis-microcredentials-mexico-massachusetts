# Massachusetts Labor Market Panel Construction (2012-2024)
# This script builds an annual panel of labor market indicators for Massachusetts,
# used as macroeconomic controls in the empirical model.

library(readxl)
library(dplyr)
library(stringr)

# Load monthly labor market data from BLS Excel file (headers start at row 11)
raw_unemp <- read_excel("SeriesReport-20250419113547_dd624c.xlsx", skip = 10)
names(raw_unemp) <- tolower(names(raw_unemp))
names(raw_unemp) <- str_replace_all(names(raw_unemp), "\\s+", "_")
raw_unemp <- raw_unemp %>% mutate(year = as.numeric(year))

# Compute annual averages and growth rates for main labor indicators
panel_unemp <- raw_unemp %>%
  group_by(year) %>%
  summarise(
    avg_labor_force_participation = mean(labor_force_participation_rate, na.rm = TRUE),
    avg_employment_population_ratio = mean(employment_population_ratio, na.rm = TRUE),
    avg_labor_force = mean(labor_force, na.rm = TRUE),
    avg_employment = mean(employment, na.rm = TRUE),
    avg_unemployment = mean(unemployment, na.rm = TRUE),
    avg_unemployment_rate = mean(unemployment_rate, na.rm = TRUE)
  ) %>%
  arrange(year) %>%
  mutate(
    growth_labor_force = (avg_labor_force - lag(avg_labor_force)) / lag(avg_labor_force),
    growth_employment = (avg_employment - lag(avg_employment)) / lag(avg_employment),
    growth_unemployment = (avg_unemployment - lag(avg_unemployment)) / lag(avg_unemployment),
    participation_gap = avg_labor_force_participation - avg_employment_population_ratio,
    post_treatment = if_else(year >= 2018, 1, 0)
  )

# Extract Massachusetts UNITIDs from IPEDS for each year
library(readr)
library(purrr)
library(tidyr)

data_folder <- "~/CVS TESIS/Massachusetts/IPEDS - Integrated Postsecondary Education Data System"
years <- 2012:2023
ma_unitids <- list()
for (year in years) {
  file_path <- file.path(data_folder, paste0("hd", year, ".csv"))
  df <- read_csv(file_path, show_col_types = FALSE)
  df_ma <- df %>% filter(STABBR == "MA")
  ma_unitids[[as.character(year)]] <- unique(df_ma$UNITID)
}

# Aggregate annual IPEDS data (enrollment, completions, etc.) for Massachusetts
final_data <- list()
for (year in years) {
  year_str <- as.character(year)
  unitids <- ma_unitids[[year_str]]
  process_file <- function(file_name, prefix) {
    file_path <- file.path(data_folder, file_name)
    if (!file.exists(file_path)) return(NULL)
    df <- read_csv(file_path, show_col_types = FALSE)
    df <- df %>% filter(UNITID %in% unitids)
    numeric_df <- df %>% select(where(is.numeric))
    if (nrow(numeric_df) == 0) return(NULL)
    summarized <- numeric_df %>% summarise(across(everything(), mean, na.rm = TRUE))
    names(summarized) <- paste0(prefix, "_", names(summarized))
    return(summarized)
  }
  efia <- process_file(paste0("efia", year, "_rv.csv"), "efia")
  effy <- process_file(paste0("effy", year, "_rv.csv"), "effy")
  dist <- if (year >= 2020) process_file(paste0("effy", year, "_dist_rv.csv"), "dist") else NULL
  year_data <- bind_cols(efia, effy, dist) %>%
    mutate(year = year) %>%
    relocate(year)
  final_data[[year_str]] <- year_data
}
final_df <- bind_rows(final_data)
write_csv(final_df, file.path(data_folder, "massachusetts_summary_2012_2023.csv"))

# Rename variables for clarity and select only those needed for DiD modeling
df <- read_csv(file.path(data_folder, "massachusetts_summary_2012_2023.csv"))
df_clean <- df %>%
  rename(
    year = year,
    total_students = effy_EFYTOTLT,
    total_men = effy_EFYTOTLM,
    total_women = effy_EFYTOTLW,
    asian_students = effy_EFYASIAT,
    black_students = effy_EFYBKAAT,
    hispanic_students = effy_EFYHISPT,
    unknown_undergrad = effy_EFYGUUN,
    nondegree_undergrad = effy_EFYGUAN,
    known_undergrad = effy_EFYGUKN,
    fte_undergrad = efia_FTEUG,
    fte_grad = efia_FTEGD,
    institution_type = efia_ACTTYPE
  )
unitid_counts <- data.frame(
  year = as.integer(names(ma_unitids)),
  total_institutions = sapply(ma_unitids, length)
)
df_final <- df_clean %>%
  select(
    year,
    total_students,
    total_men,
    total_women,
    asian_students,
    black_students,
    hispanic_students,
    unknown_undergrad,
    nondegree_undergrad,
    known_undergrad,
    fte_undergrad,
    fte_grad
  ) %>%
  left_join(unitid_counts, by = "year") %>%
  relocate(total_institutions, .after = year)
write_csv(df_final, file.path(data_folder, "massachusetts_DiD_final_clean.csv"))
