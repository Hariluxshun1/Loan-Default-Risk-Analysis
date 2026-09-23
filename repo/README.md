# Loan Default Risk Analysis — IT3081 Group Assignment
Group 2026-DS-37 | Statistical Modelling Consultancy Project

## Dataset
`yasserh/loan-default-dataset` from Kaggle (148,670 rows, 34 columns).
Download it yourself from Kaggle and place `Loan_Default.csv` inside the `data/` folder.
The raw CSV is NOT tracked in Git (see .gitignore) — everyone downloads their own copy so the repo stays small.

## Folder structure
```
data/        -> place Loan_Default.csv here (not tracked by git)
scripts/     -> one R script per task, see below
outputs/     -> save any plots/tables you want to keep (not tracked by git)
```

## Script ownership
| File | Task | Owner |
|---|---|---|
| 00_setup.R | Load libraries + shared data-cleaning steps everyone uses | Whole group agrees, then commit once |
| 03_descriptive.R | Task 3 — Descriptive Analysis | Person B |
| 04_inference.R | Task 4 — Statistical Inference | Person C |
| 05_modelling.R | Task 5 — Predictive Modelling | Person C |
| 07_pca.R | Task 7 — PCA Evaluation | Person B |
| 08_naive_bayes.R | Task 8 — Bayesian Methods | Person D |

## Workflow
1. `git pull` before you start working, every session.
2. Work only in YOUR OWN script file to avoid merge conflicts.
3. `git add .` -> `git commit -m "short message"` -> `git push` when you're done for the session.
4. If two people edited the same file and you get a conflict, ping the group on chat before resolving it.
