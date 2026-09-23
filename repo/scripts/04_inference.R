# 04_inference.R — Task 4: Statistical Inference
# Owner: Person C
source("scripts/00_setup.R")

# ---- Example: comparison of means (income by default status) ----
t.test(income ~ Status, data = df)

# ---- Example: comparison of proportions (default rate by Gender) ----
table_gender <- table(df$Gender, df$Status)
prop.test(table_gender)

# ---- Example: ANOVA (loan_amount across loan_purpose) ----
anova_model <- aov(loan_amount ~ loan_purpose, data = df)
summary(anova_model)

# TODO: add remaining hypothesis tests with H0/H1 stated in comments
