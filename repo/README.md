# Loan Default Risk Analysis

**IT3081 Statistical Modelling Consultancy Project**
- **Group:** 2026-DS-37
- **Technology:** R, RStudio, R Shiny
**Repository:** [Hariluxshun1/Loan-Default-Risk-Analysis](https://github.com/Hariluxshun1/Loan-Default-Risk-Analysis)

An academic analysis of loan records using exploratory analysis, statistical inference, predictive models, principal component analysis (PCA), and **simulated** experimental designs. The project also includes an interactive Shiny dashboard for explaining results to nontechnical stakeholders.

**Live dashboard:** https://loanrisk-analytics.shinyapps.io/loanrisk-analytics/

> **Academic-use limitation:** The project treats `Status = 1` as **default** and `Status = 0` as **non-default**. This is the project's working target definition; original-source documentation and observed repayment histories are still needed to validate it for banking use. No model in this repository should be used to approve, reject, or score real loan applicants.

## Data

- **Source:** Kaggle, `yasserh/loan-default-dataset` (`Loan_Default.csv`).
- **Size:** 148,670 observations and 34 columns in the raw file.
- **After `scripts/00_setup.R`:** 32 columns; the `ID` field and constant `year` field are removed. The available records are from **2019 only**.
- **Outcome rate under the project's coding:** 24.64% `Status = 1`.
- **Availability:** The raw CSV is **not included in Git**. Download it separately from the source and place it at `repo/data/Loan_Default.csv`.

The single-year data do **not** support a real monthly loan-default time-series study. Any time-series extension discussed in the project is conceptual, not measured on longitudinal records.

## Repository layout

```text
Loan-Default-Risk-Analysis/
├── Loan-Default-Risk-Analysis.Rproj
└── repo/
    ├── README.md                      # This guide
    ├── data/                          # Place Loan_Default.csv here (ignored by Git)
    ├── scripts/                       # R analyses and evaluation scripts
    ├── results/                       # Model/experiment CSVs and figures
    │   └── figures/
    ├── outputs/                       # EDA/PCA exports and reports
    │   ├── descriptive/
    │   └── PCA/
    ├── docs/                          # Task 1 framing and research notes
    └── Dashboard/
        ├── README.md                  # Dashboard-specific guide
        ├── app.R                      # Shiny app (Version 4)
        └── data/                       # Aggregated CSVs used by the dashboard
```

## Installation and setup

1. Install a recent version of **R** and **RStudio**.
2. Clone this repository or download it as a ZIP.
3. Obtain `Loan_Default.csv` from the dataset source and place it in **`repo/data/`** (not in the root-level folder).
4. Open the `.Rproj` file in RStudio, then set the working directory to the **inner `repo/`** folder before running analysis scripts.
5. Install the packages used across the scripts:

```r
install.packages(c(
  "dplyr", "ggplot2", "car", "glmnet", "pROC",
  "smotefamily", "e1071", "shiny"
))
# Only if you intend to publish the Shiny app:
# install.packages("rsconnect")
```

Verify that your R session is in `repo/` and load the shared data setup:

```r
getwd()                         # Should end in /repo
source("scripts/00_setup.R")     # Reads data/Loan_Default.csv
```

If `data/Loan_Default.csv` cannot be found, the working directory or dataset location is incorrect. **Do not run the scripts from the repository root** unless you first change into `repo/`.

## Scripts and task coverage

| Script | Purpose | Assignment task |
|---|---|---|
| `00_setup.R` | Shared dataset loading and basic preprocessing | Common setup |
| `03_descriptive.R` | EDA, group comparisons, missingness, outliers, correlations | Task 3 |
| `04_inference.R` | Income t-test, categorical association, loan-purpose ANOVA, variance tests | Task 4 |
| `05_modelling.R` | Logistic modelling and initial data-quality/model diagnostics | Task 5 |
| `05_missing_data_check.R` | Missing-data sensitivity investigation | Task 5 |
| `05_threshold_validation.R` | Stratified 60/20/20 partitions, median imputation, validation threshold selection | Task 5 |
| `05_final_model_comparison.R` | Baseline, ridge, LASSO, SMOTE, and weighted logistic comparison | Task 5 |
| `05_model_visualizations.R` | Plots of the six-model results | Tasks 5 and 8 |
| `06_experimental_design.R` | **Simulated** CRD and RCBD repayment-reminder experiments | Task 6 |
| `07_pca.R` | Seven-variable PCA and explained-variance analysis | Task 7 |
| `08_naive_bayes.R` | Naive Bayes evaluation on the comparable holdout split | Task 8 |

**Tasks 1–2** have supporting material in `docs/`; this repository does not establish that the full literature-review requirement is complete. **Tasks 9, 11, and 12** are primarily discussion, expert-review, and recommendations activities rather than standalone R scripts. Task 11's anonymized feedback summary is presented in the dashboard; private response records are not intended for the public repository.

## Recommended execution order

Run the commands in the **R Console**, with your working directory set to `repo/`:

```r
# Data inspection, inference, and PCA
source("scripts/03_descriptive.R")
source("scripts/04_inference.R")
source("scripts/07_pca.R")

# Predictive modelling and checks (some computations may take time)
source("scripts/05_modelling.R")
source("scripts/05_missing_data_check.R")
source("scripts/05_threshold_validation.R")

# Final comparisons: this order matters
source("scripts/05_final_model_comparison.R")  # Generates results/task5_model_comparison.csv
source("scripts/08_naive_bayes.R")             # Reads that file; creates six-model CSV
source("scripts/05_model_visualizations.R")   # Reads the six-model CSV

# Separate, explicitly simulated experiments
source("scripts/06_experimental_design.R")
```

The scripts generally source `00_setup.R` themselves, so running it at the start is a useful sanity check but not a substitute for placing the raw CSV correctly.

**Output-path note:** The committed EDA and PCA snapshots are organized under `outputs/descriptive/` and `outputs/PCA/`. The current `03_descriptive.R` and `07_pca.R` scripts write their new outputs directly into `outputs/`. Rerunning them does **not** automatically update those two organized snapshot folders; compare or organize newly generated files before treating them as replacements. The Task 5/8 and Task 6 scripts export into `results/`.

## Main research findings

- **Statistical inference:** Income differed across the project-defined outcome groups (Welch t-test, *p* < 2.2 × 10^-16), although the standardized difference was small (Cohen's *d* ≈ 0.15). Group association and loan-purpose differences were also statistically significant; these are **associations, not causal effects**.
- **PCA:** The current analysis uses **seven numerical variables**. The first five PCs explain approximately **91.42%** of their observed variation; that percentage is **not** loan-default prediction accuracy.
- **Model comparison:** Five logistic approaches and Naive Bayes were evaluated using comparable train/validation/test partitions. Naive Bayes had the highest **validation F1** among the six. Its **test F1 ≈ 0.4381** and **ROC-AUC ≈ 0.6208**, indicating limited discrimination, not production readiness.
- **Trade-off:** The selected Naive Bayes test threshold was **0.29**. It flagged 5,715 default-labelled loans correctly but also produced **13,044 false alarms** on the held-out test data.
- **Experiments:** CRD and RCBD were simulated to demonstrate design and analysis principles. They are **not empirical evidence** of repayment-reminder effectiveness.

Missing predictors, potentially outcome-associated missingness, highly associated categorical fields, and outlying LTV values require particular caution when interpreting the model results.

## Shiny dashboard (Version 4)

The dashboard can run **without the raw Kaggle CSV** because its `Dashboard/data/` directory contains the aggregated outputs it needs.

```r
# From repo/
shiny::runApp("Dashboard")
```

Tabs: **Overview**, **Model Comparison**, **Statistical Findings**, **PCA & Experiments**, and **Expert Review**.

To update the deployed app, install and configure `rsconnect` for your own shinyapps.io account, then run from `repo/`:

```r
rsconnect::deployApp(
  appDir = "Dashboard",
  appName = "loanrisk-analytics",
  account = "loanrisk-analytics"
)
```

A GitHub push does **not** update the published Shiny app; redeployment is separate. See [`Dashboard/README.md`](Dashboard/README.md) for app-specific guidance.

## Expert review and limitations

One senior banking professional with more than 20 years' experience reviewed an **earlier dashboard version** and rated its relevance **2/5** and clarity **2/5**. The reviewer requested a clearer purpose and less technical language. Version 4 includes simpler explanations and more transparent limitations in response. **The updated version has not yet been re-rated** by the expert.

Before any operational use, the target definition must be verified against primary records and supplemented with actual repayment histories, independent/temporal testing, calibration, fairness and regulatory reviews, and human oversight. The application is an **educational analytics demonstration**, not a live lending system.

## Collaboration and privacy

Use feature branches and pull requests for team changes; review alterations to shared scripts before merging into `main`. Do not commit raw borrower records, identifiable expert feedback, passwords, API tokens, `.Renviron`, `.Rhistory`, `.RData`, or RStudio temporary files. Only aggregated data are distributed with the dashboard.
