# Data Preprocessing and Variable Selection for Econometric Modeling
# This script handles missing values and selects only relevant variables for each dataset.

# Impute missing values for numeric variables using mean (low variation) or median (high variation)
impute_numeric <- function(x) {
  if (is.numeric(x)) {
    if (sum(is.na(x)) > 0) {
      if (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) < 1) {
        return(replace(x, is.na(x), mean(x, na.rm = TRUE)))
      } else {
        return(replace(x, is.na(x), median(x, na.rm = TRUE)))
      }
    }
  }
  return(x)
}

# Impute missing values for categorical variables with "Unknown"
impute_categorical <- function(x) {
  if (is.character(x) | is.factor(x)) {
    return(replace(x, is.na(x), "Unknown"))
  }
  return(x)
}

# Remove rows with more than 50% missing values
remove_rows_with_too_many_na <- function(df, threshold = 0.5) {
  na_percentage <- rowSums(is.na(df)) / ncol(df)
  df_cleaned <- df[na_percentage <= threshold, ]
  return(df_cleaned)
}

# Apply imputation and cleaning to all datasets
cdmx_data <- as.data.frame(lapply(cdmx_data, impute_numeric))
massachusetts_data <- as.data.frame(lapply(massachusetts_data, impute_numeric))
microcredentials_data <- as.data.frame(lapply(microcredentials_data, impute_numeric))
cdmx_data <- as.data.frame(lapply(cdmx_data, impute_categorical))
massachusetts_data <- as.data.frame(lapply(massachusetts_data, impute_categorical))
microcredentials_data <- as.data.frame(lapply(microcredentials_data, impute_categorical))
cdmx_data <- remove_rows_with_too_many_na(cdmx_data)
massachusetts_data <- remove_rows_with_too_many_na(massachusetts_data)
microcredentials_data <- remove_rows_with_too_many_na(microcredentials_data)

# Select only variables relevant for modeling
cdmx_data_filtered <- cdmx_data[, c(
  "Year",
  "Avg_Wage_CDMX",
  "Avg_Wage_Tech",
  "Unemployment_Rate",
  "HigherEd_Enrollment",
  "Informality_Rate_General",
  "Informality_Rate_HigherEd",
  "Informality_Rate_Tech",
  "Tech_Workforce_GrowthRate",
  "Enrollment_Workforce_Ratio",
  "Tech_Workforce_Ratio",
  "Tech_Wage_Premium",
  "University_Annual_RealPrice_USD"
)]
massachusetts_data_filtered <- massachusetts_data[, c(
  "year",
  "avg_wage_tech",
  "unemployment_rate",
  "higher_ed_enrollment",
  "tech_workers",
  "labor_force",
  "pct_pell",
  "avg_loan",
  "avg_grant",
  "grad_rate_150_4yr",
  "tuition_in_state",
  "tuition_out_state",
  "tuition_private",
  "tuition_public_in_state",
  "tuition_public_out_state"
)]
microcredentials_data_filtered <- microcredentials_data[, c(
  "Year",
  "Registered_Learners_M_Coursera",
  "Registered_Learners_M_Edx",
  "Course_Enrollments_M_Coursera",
  "Course_Enrollments_M_Edx",
  "Avg_Certificate_Price_USD_Coursera",
  "Avg_Certificate_Price_USD_Edx",
  "Avg_Degree_Price_USD_Coursera",
  "Avg_Degree_Price_USD_Edx",
  "Avg_MasterTrack_Price_USD",
  "Avg_MicroMasters_Price_USD",
  "Avg_MicroBachelors_Price_USD",
  "Completion_Rate_Perc",
  "employer_recognition_us"
)]

# Convert price variables from text ranges to numeric averages
convert_to_numeric_average <- function(x) {
  x <- gsub("[^0-9\\-]", "", x)
  sapply(x, function(val) {
    if (grepl("-", val)) {
      nums <- unlist(strsplit(val, "-"))
      nums <- as.numeric(nums)
      return(mean(nums, na.rm = TRUE))
    } else {
      return(as.numeric(val))
    }
  })
}
microcredentials_data_filtered$Avg_Degree_Price_USD_Coursera <- convert_to_numeric_average(microcredentials_data_filtered$Avg_Degree_Price_USD_Coursera)
microcredentials_data_filtered$Avg_Degree_Price_USD_Edx <- convert_to_numeric_average(microcredentials_data_filtered$Avg_Degree_Price_USD_Edx)

# Create combined average price indicators for certificates and degrees
microcredentials_data_filtered$Avg_Certificate_Price_USD_Combined <- rowMeans(
  cbind(
    microcredentials_data_filtered$Avg_Certificate_Price_USD_Coursera,
    microcredentials_data_filtered$Avg_Certificate_Price_USD_Edx
  ), na.rm = TRUE
)
microcredentials_data_filtered$Avg_Degree_Price_USD_Combined <- rowMeans(
  cbind(
    microcredentials_data_filtered$Avg_Degree_Price_USD_Coursera,
    microcredentials_data_filtered$Avg_Degree_Price_USD_Edx
  ), na.rm = TRUE
)

# Fix employer recognition variable for key years using validated sources
microcredentials_data_filtered$employer_recognition_us <- with(microcredentials_data_filtered, ifelse(
  Year == 2021, 59,
  ifelse(Year == 2022, 86,
         ifelse(Year == 2024, 72, NA)
  )
))

# Export cleaned and filtered datasets for next steps
write.csv(cdmx_data_filtered, "CDMX_FINAL_FILTERED.csv", row.names = FALSE)
write.csv(massachusetts_data_filtered, "MASSACHUSETTS_FINAL_FILTERED.csv", row.names = FALSE)
write.csv(microcredentials_data_filtered, "MICROCREDENTIALS_FINAL_FILTERED.csv", row.names = FALSE)
