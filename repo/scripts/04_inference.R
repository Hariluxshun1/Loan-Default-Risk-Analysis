# 04_inference.R — Task 4: Statistical Inference
# Owner: Person C

source("scripts/00_setup.R")


# =========================================================
# Test 1: Comparison of Means — Income by Default Status
# =========================================================
# H0: mean income is equal between defaulters and non-defaulters
# H1: mean income differs between defaulters and non-defaulters
# Test chosen: Welch Two Sample t-test — comparing the mean of one
# numeric variable (income) across two independent groups (Status).

t_test_income <- t.test(income ~ Status, data = df)
print(t_test_income)

# Result: t = 22.584, df = 54076, p < 2.2e-16 -> reject H0
# Mean income: non-defaulters $7,204 vs defaulters $6,232
# (95% CI for the difference: $887.83 - $1,056.58, does not cross 0)
#
# Business implication: Lower income is significantly associated with
# higher default risk. Supports weighting income more heavily in the
# bank's risk-scoring model, and considering income-based loan caps or
# extra verification for lower-income applicants.


# =========================================================
# Test 2: Comparison of Proportions — Default Rate by Gender
# =========================================================
# H0: default rate is equal across gender groups
# H1: default rate differs across gender groups
# Test chosen: prop.test() — comparing proportions (default rate)
# across more than two independent categorical groups.

gender_table <- table(df$Gender, df$Status)
print(gender_table)

prop_test_gender <- prop.test(gender_table)
print(prop_test_gender)

# Result: X-squared = 1043.6, df = 3, p < 2.2e-16 -> reject H0
# Default rate by group: Joint 19.2%, Female 25.1%, Male 26.2%,
# Sex Not Available 28.6%
#
# Business implication: Joint/co-signed applications show meaningfully
# lower default risk, supporting continued encouragement of co-applicants
# for higher-risk profiles. The "Sex Not Available" group's higher rate
# more likely reflects a data quality/reporting gap than a genuine gender
# effect, and should be flagged as a limitation rather than acted on
# directly — using gender as a predictor also raises fairness/regulatory
# concerns (see Task 12 ethical considerations).


# =========================================================
# Test 3: ANOVA — Loan Amount Across Loan Purpose
# =========================================================
# H0: mean loan amount is equal across all loan purpose categories
# H1: at least one loan purpose category has a different mean loan amount
# Test chosen: one-way ANOVA — comparing means across more than two
# independent groups (loan_purpose has 5 categories including blank).

anova_model <- aov(loan_amount ~ loan_purpose, data = df)
summary(anova_model)

# Result: F(4, 148665) = 1682, p < 2.2e-16 -> reject H0

aggregate(loan_amount ~ loan_purpose, data = df, mean)
TukeyHSD(anova_model)

# Post-hoc (Tukey): p2 is significantly smaller than every other category
# ($92k-$169k lower, p < 0.0001 in all p2 comparisons). p1 and p4 are not
# significantly different from the blank category (p = 0.89, 0.78).
#
# Business implication: Loan purpose p2 is associated with substantially
# smaller loan amounts than p1/p3/p4. Recommend the bank map these coded
# purposes back to descriptive categories (e.g. auto, home, business,
# personal) to check whether smaller p2 loans also carry a different risk
# profile, enabling purpose-specific approval thresholds instead of a
# one-size-fits-all rule. The blank category likely reflects missing/
# unrecorded purpose codes — noted as a data quality issue in Task 3.


# =========================================================
# Test 4: Comparison of Variances — Loan Amount by Default Status
# =========================================================
# H0: the variance of loan amount is equal between defaulters and
#     non-defaulters
# H1: the variance of loan amount differs between defaulters and
#     non-defaulters
# Test chosen: F test (var.test()) — the standard test for comparing
# variances between two independent groups.

var.test(loan_amount ~ Status, data = df)

# Result: F = 0.703, p < 2.2e-16 -> reject H0
# Ratio of variances (group0/group1) = 0.703, i.e. defaulters have MORE
# variable loan amounts than non-defaulters.
#
# Business implication: Defaults occur across both small and large loans
# rather than being concentrated in one loan-size bracket. Loan amount
# alone is a weak standalone risk signal — the bank should rely on a
# combination of factors (income, credit type, LTV, etc.) rather than
# treating large loans as automatically higher risk.
