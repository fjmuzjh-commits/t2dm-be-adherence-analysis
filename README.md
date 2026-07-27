# T2DM behavioral economics and treatment adherence analysis

This repository contains the R analysis code and reproducibility materials for the manuscript:

> Behavioral economic factors associated with treatment adherence among patients with type 2 diabetes: a cross-sectional study in China

## What is included

- `T2DM_full_analysis.R`: one-click analysis script.
- `functions.R`: helper functions used by the analysis script.
- `data_dictionary.csv`: variable definitions.
- `variable_coding.csv`: coding and reference categories.
- `output/tables/`: aggregate tables exported from the frozen analysis.
- `output/diagnostics/`: model diagnostic plots exported from the frozen analysis.
- `output/logs/`: model/CFA summaries and analysis log.
- `validation_report.txt` and `output/tables/validation_checks.csv`: frozen-result validation records.

## Data availability

The individual-level survey dataset is not publicly included in this repository because it contains participant-level research data from a hospital-based questionnaire study. The dataset may be made available from the corresponding author upon reasonable request and after approval under applicable ethics and data-governance requirements.

To rerun the analysis after obtaining approved access to the dataset, place the data file here:

```text
input/T2DM_SCI_final_data_v1.2.csv
```

The expected CSV variable names are provided in `input/T2DM_SCI_final_data_v1.2_TEMPLATE_HEADER_ONLY.csv`, `data_dictionary.csv`, and `variable_coding.csv`.

## How to rerun

Open a clean R session at the repository root and run:

```r
rm(list = ls())
source("T2DM_full_analysis.R", encoding = "UTF-8")
```

The script expects the approved dataset at `input/T2DM_SCI_final_data_v1.2.csv`. It creates `output/` subfolders as needed and exports tables, diagnostics, logs, checksums and validation results.

## Required R packages

The analysis script installs missing CRAN packages if `INSTALL_MISSING_PACKAGES <- TRUE`.

Required packages:

- `psych`
- `lavaan`
- `writexl`
- `lmtest`
- `sandwich`

The helper file contains legacy helper code for `openxlsx`, but the final workbook export is performed with `writexl`; `openxlsx` is not required for the final analysis path.

## Fixed analysis scope

- Study design: single-center cross-sectional association study.
- Final sample size: 232 participants.
- Primary outcome: continuous treatment adherence total score.
- Sensitivity outcome: treatment adherence total score >=40.
- CFA estimator: WLSMV, with questionnaire items treated as ordered categorical indicators.
- Primary linear model: 22 predictor parameters, including residence.
- Supplementary sensitivity model: 21 predictor parameters, excluding residence.
- Logistic sensitivity analysis: binary adherence category as the outcome.

## Public repository exclusions

The following files are intentionally not included:

- `input/T2DM_SCI_final_data_v1.2.csv` (individual-level survey data).
- `output/model_objects/*.rds` (model objects may contain fitted-object environments).

## Citation and manuscript text

Suggested manuscript wording:

> The R analysis code and aggregate reproducibility materials are available at: [GitHub URL to be inserted after repository creation]. Individual-level survey data are not publicly available because they contain participant-level research data; they may be made available from the corresponding author upon reasonable request and after approval under applicable ethics and data-governance requirements.
