# LoanRisk Analytics — Dashboard Version 3 (expert-informed)

This R Shiny app updates Version 2 based on feedback from one senior-level banking officer with more than 20 years of experience. The reviewer gave relevance and clarity ratings of 2/5 for the earlier version and asked for a simpler, more welcoming interface and clearer terminology.

## What changed
- New **What does this dashboard do?** introduction and step-by-step navigation overview.
- Plain-language choices and descriptions for precision, recall, F1, and ROC-AUC.
- Specific example of the Naive Bayes trade-off: 5,715 correctly flagged Status 1 records, 13,044 incorrectly flagged Status 0 records.
- Simple summaries before the statistics and PCA charts.
- Expert-review tab now contains anonymized original ratings and feedback, plus a completed change log.

## Important limitations
- The **Status 1** label remains unverified as a repayment-default outcome. Do not represent model predictions as actual credit default risk or use them for decisions.
- The dataset has no monthly borrower repayment history. Time-series enhancement remains conceptual.
- One expert's rating is preliminary feedback, not a representative survey or post-revision validation. No post-update rating is claimed.
- Only aggregated data are distributed with this application.

## Install in your existing RStudio project

1. Stop your currently running Shiny app.
2. Back up `repo/Dashboard/` (or `repo/dashboard/`).
3. Extract this ZIP, then copy **the contents** of `LoanRisk_Dashboard_v3/` into the existing `repo/Dashboard/` folder. `app.R` and `data/` should sit directly inside `Dashboard/`.
4. With `getwd()` pointing to `repo/`, run `shiny::runApp("Dashboard")`.
5. Check Overview, Model Comparison, Statistical Findings, PCA & Experiments and Expert Review.
6. Republish the updated Shiny app with `rsconnect::deployApp(appDir="Dashboard", appName="loanrisk-analytics", account="loanrisk-analytics")` from `repo/`, after confirming the proper account is configured.

If your folder is lowercase (`dashboard`), use that spelling consistently.

The published dashboard does **not** update when you only change local R files or push to GitHub; you must redeploy to shinyapps.io.

## Notes for the presentation

Our group developed an R Shiny dashboard and sought feedback from a senior-level banking officer with over 20 years of experience. The expert rated relevance and clarity 2/5 and asked for less technical wording and a clearer purpose. We updated the overview, chart explanations, and expert-review transparency. We have **not** yet shown that those changes improve usability scores; a follow-up review would be required.
