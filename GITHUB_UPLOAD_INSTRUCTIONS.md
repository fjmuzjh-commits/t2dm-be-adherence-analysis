# GitHub upload instructions

This folder is prepared as a public reproducibility repository for the manuscript:

Behavioral-economic characteristics and medication adherence among patients with type 2 diabetes mellitus.

## Before uploading

Confirm that the repository does not contain individual-level participant data.

Included public materials:

- R analysis scripts: `T2DM_full_analysis.R`, `functions.R`
- Data dictionary and variable coding files.
- Aggregate result tables, model summaries, diagnostics and validation checks.
- Header-only dataset template: `input/T2DM_SCI_final_data_v1.2_TEMPLATE_HEADER_ONLY.csv`

Excluded materials:

- `input/T2DM_SCI_final_data_v1.2.csv`
- `output/model_objects/*.rds`
- Any participant-level raw or processed dataset.

## Suggested GitHub repository name

`t2dm-be-adherence-analysis`

## Upload by command line

Create an empty GitHub repository first, then run these commands in this folder:

```bash
git remote add origin https://github.com/<owner>/t2dm-be-adherence-analysis.git
git push -u origin main
```

If the remote already exists:

```bash
git remote set-url origin https://github.com/<owner>/t2dm-be-adherence-analysis.git
git push -u origin main
```

## Manuscript wording after upload

Data availability statement:

The R analysis code, aggregate output tables, diagnostics, and reproducibility documentation are available at: https://github.com/fjmuzjh-commits/t2dm-be-adherence-analysis. Individual-level survey data are not publicly available because they contain participant-level clinical and questionnaire information; de-identified data may be made available from the corresponding author upon reasonable request and subject to institutional approval.

Methods reproducibility sentence:

All statistical analyses were conducted in R. The reproducible R scripts and aggregate validation materials are available in the public GitHub repository: https://github.com/fjmuzjh-commits/t2dm-be-adherence-analysis.
