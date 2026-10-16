# LoanRisk Analytics — Shiny Dashboard Version 4

**IT3081 Statistical Modelling Consultancy Project | Group 2026-DS-37**

**Live dashboard:** https://loanrisk-analytics.shinyapps.io/loanrisk-analytics/

This R Shiny application presents the group's **academic Loan Default Risk Analysis** in a form intended to be understandable to nontechnical stakeholders. It explores loan-record patterns, compares six research models, summarizes statistical tests and PCA, and documents feedback from a banking-industry reviewer.

> **Target definition and scope:** The project uses `Status = 1` for **default** and `Status = 0` for **non-default**. This is the working classification in the analyses. The original data supplier's outcome definition still warrants independent verification with documented repayment outcomes. The application is **not** suitable for real credit decisions.

## What's new in Version 4

Version 4 builds on the earlier user-friendliness revisions and uses **loan default / non-default** wording consistently in the dashboard interface.

- **Overview:** A plain-language explanation of the dashboard purpose and how to navigate its findings, plus a clear target-definition explanation.
- **Model Comparison:** Six-model comparison with descriptions of F1, ROC-AUC, recall, precision, and false-alarm trade-offs.
- **Statistical Findings:** Accessible summaries of Task 4 hypothesis tests, effect sizes, and missing-data concerns.
- **PCA & Experiments:** Seven-variable PCA and the **simulated** CRD and RCBD research exercises.
- **Expert Review:** Actual anonymized preliminary feedback, changes made in response, questionnaire download, and methodological cautions.

## Key figures shown

| Measure | Research result |
|---|---:|
| Loan observations | 148,670 |
| Project-defined default rate | 24.64% |
| Highest validation F1 model | Naive Bayes |
| Naive Bayes held-out test F1 | 0.4381 |
| Naive Bayes held-out test ROC-AUC | 0.6208 |
| Naive Bayes test true positives | 5,715 |
| Naive Bayes test false positives | 13,044 |
| First five PCs: variance explained | 91.42% |

These metrics are **not** evidence of operational model validity. In particular, ROC-AUC **0.6208 is not 62.08% accuracy**, and 91.42% PCA variance explained is **not** prediction accuracy.

## Files and data

```text
Dashboard/
├── README.md
├── app.R
└── data/
    ├── model_comparison.csv
    ├── pca_variance.csv
    ├── by_region.csv
    ├── by_gender.csv
    ├── by_age.csv
    ├── by_loan_purpose.csv
    └── by_credit_type.csv
```

All shipped CSV files are **aggregated analysis outputs**. The Shiny app does **not** require or publish the raw `Loan_Default.csv` dataset or individual borrower-level records. Do not replace the shipped CSVs with confidential records.

## Run the application locally

1. Install **R**, **RStudio**, and the **`shiny`** package:

   ```r
   install.packages("shiny")
   ```

2. Open the main RStudio project and set the working directory to the inner **`repo/`** folder.
3. Start the application:

   ```r
   shiny::runApp("Dashboard")
   ```

4. Open all five tabs and verify that the main KPI reads **Loan default rate (24.64%)**. The folder name is `Dashboard` with a capital **D**.

For local testing, no shinyapps.io credentials are needed.

## Redeploy to shinyapps.io

Install `rsconnect` if needed, and connect your own account using its official authentication workflow (do not commit tokens or secrets). From the `repo/` working directory:

```r
rsconnect::deployApp(
  appDir = "Dashboard",
  appName = "loanrisk-analytics",
  account = "loanrisk-analytics"
)
```

If prompted, choose to **update the existing application**, not create a new one. Successful redeployment should retain the current URL. A **GitHub push alone does not update** the deployed app.

### Optional external expert-feedback link

The app reads an optional environment variable named `LOANRISK_FEEDBACK_URL`. If set to a valid `https://` URL, a link to the external questionnaire is shown. Otherwise, the dashboard offers its downloadable questions. It does not itself store survey responses.

Keep any environment settings and survey-response files private. Do not publish personal or identifying details of the expert.

## Banking expert evaluation

A senior-level banking officer with **over 20 years' experience** reviewed an earlier version. The reviewer rated:

- **Relevance for exploring loan-risk patterns:** 2/5.
- **Clarity of charts and explanations:** 2/5.

The reviewer highlighted uncertainty about the outcome definition, requested more understandable descriptions, and identified **repayment history** and **employment/income history** as useful additional data. Version 4 improves the introduction, metric descriptions, error examples, and disclosure of the target coding and research limitations.

**Important:** These ratings apply to the **earlier prototype**, not to Version 4. There is no confirmed follow-up usability score or second independent expert validation.

## Research limitations

- The dataset contains one recorded year (**2019**), without the longitudinal repayment history needed for genuine time-series default forecasting.
- Outcome meaning requires independent source-level verification before business interpretation.
- Outcome-associated missingness and unusually high LTV observations may affect estimates; the models are exploratory.
- Naive Bayes has limited discriminative performance and many false positives at the selected threshold.
- The CRD and RCBD plots illustrate **simulated**, not observed, intervention outcomes.
- PCA describes variation in input variables, not the proportion of defaults predicted.
- No automated accept/reject decisions, personal loan scoring, regulatory compliance claim, or production service is provided.

For the full research workflow, see the [main project README](../README.md).
