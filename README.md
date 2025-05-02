# Thesis: Microcredentials in Mexico and Massachusetts

This repository contains all the code and data used for my thesis:  
**"The Macroeconomic Impact of Microcredentials: A Comparative Analysis of Massachusetts and Mexico City"**

## Repository Structure
- `01_data_preprocessing_variable_selection.R` – Impute missing values and select variables for analysis
- `02_descriptive_graphs.R` – Generate descriptive graphs (wages, enrollment, workforce, costs)
- `03_panel_construction.R` – Build the combined panel dataset for DiD models
- `04_did_models_and_robustness.R` – Run DiD models and robustness checks
- `05_heterogeneous_effects.R` – Estimate models with heterogeneous effects by period
- `06_microcredentials_effects_and_costs.R` – Analyze microcredentials effects and compare costs
- `data/` – Contains the processed CSV files needed for the analysis

## How to Reproduce the Analysis

1. Clone or download this repository.
2. Make sure you have R and the required packages installed (see below).
3. Place the CSV data files in the `data/` folder.
4. Run the scripts in order (from `01_...R` to `07_...R`) for a full reproduction of the results and figures.

## Required R Packages

- tidyverse
- readxl
- fixest
- modelsummary
- flextable
- officer
- ggplot2
- broom

Install all with:
install.packages(c("tidyverse", "readxl", "fixest", "modelsummary", "flextable", "officer", "ggplot2", "broom"))


## Data

The repository includes only processed/cleaned CSV files. For raw data or large files, please contact the author or follow instructions in the scripts to download from public sources.

## Author

Santiago Avalos 
[santi123123ar](https://github.com/santi123123ar)

---



