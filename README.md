# Thesis: Microcredentials in Mexico and Massachusetts

This repository contains all the code and data used for my thesis:  
**"The Macroeconomic Impact of Microcredentials: A Comparative Analysis of Massachusetts and Mexico City"**

---

## Repository Structure

- `01_data_preprocessing_variable_selection.R` – Impute missing values and select variables for analysis
- `02_descriptive_graphs.R` – Generate descriptive graphs (wages, enrollment, workforce, costs)
- `03_panel_construction.R` – Build the combined panel dataset for DiD models
- `04_did_models_and_robustness.R` – Run DiD models and robustness checks
- `05_heterogeneous_effects.R` – Estimate models with heterogeneous effects by period
- `06_microcredentials_effects_and_costs.R` – Analyze microcredentials effects and compare costs
- `data/` – Contains the processed CSV files needed for the analysis

---

## Data Processing Note

This repository starts from cleaned and filtered CSV files:
- `CDMX_FINAL_FILTERED_UPDATED.csv`
- `MASSACHUSETTS_FINAL_FILTERED.csv`
- `MICROCREDENTIALS_FINAL_FILTERED.csv`

For reproducibility, all scripts begin with these processed datasets in the `data/` folder. The original data cleaning and preparation steps are not included in this repository.

---

## Data Sources

### Massachusetts
- Employment and Wages: [Bureau of Labor Statistics](https://data.bls.gov/cew/apps/data_views/data_views.htm#tab=Tables)
- Enrollment, Graduation and Financial Aid: [IPEDS Data Center](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&sid=a5cf1b5e-c854-4c2d-8da6-96d34bab521b&rtid=7)
- Unemployment Statistics: [Bureau of Labor Statistics](https://www.bls.gov/lau/)
- University Costs: [NCES Digest of Education Statistics](https://nces.ed.gov/programs/digest/)

### Mexico City
- Labor Market Indicators (Informality, Employment, Wages): [INEGI](https://www.inegi.org.mx/programas/enoe/15ymas/)
- Higher Education Statistics: [ANUIES](https://www.anuies.mx/informacion-y-servicios/informacion-estadistica-de-educacion-superior/anuario-estadistico-de-educacion-superior)

### Microcredentials
- Platform Data: [Class Central](https://www.classcentral.com), [Coursera](https://www.coursera.org), [edX](https://www.edx.org)

---

## How to Reproduce the Analysis

1. Clone or download this repository.
2. Make sure you have R and the required packages installed (see below).
3. Place the CSV data files in the `data/` folder.
4. Run the scripts in order (from `01_...R` to `06_...R`) for a full reproduction of the results and figures.

---

## Required R Packages

install.packages(c("tidyverse", "readxl", "fixest", "modelsummary", "flextable", "officer", "ggplot2", "broom"))

---

## Key Findings

The analysis compares educational and labor market outcomes before and after the introduction of microcredentials, using a Difference-in-Differences approach to estimate causal effects. Key findings explore:

- Wage differentials in technology sectors
- Employment growth patterns
- Cost-effectiveness of traditional vs. alternative credentials
- Labor market integration

---

## Author

Santiago Avalos  
[santi123123ar](https://github.com/santi123123ar)

---

**Note:** If you need access to the original raw data or have questions about the data processing methodology, please contact the author.



