
# =========================================================
# IT3081 - STATISTICAL MODELLING
# TASK 4: STATISTICAL INFERENCE
# Project: Loan Default Risk Analysis
# Owner: Person C
# Working Directory: repo/
# =========================================================

source("scripts/00_setup.R")

# Statistical significance level
alpha <- 0.05

# Check that both outcome groups exist
stopifnot(
  all(c("0", "1") %in% as.character(unique(df$Status)))
)

# IMPORTANT:
# Project documentation defines:
# Status 0 = Non-default
# Status 1 = Default
#
# Confirm the original dataset's outcome definition.
# Statistical associations do not establish causation.

# Required package for Levene's test
if (!requireNamespace("car", quietly = TRUE)) {
  stop("Install the car package using install.packages('car')")
}

# Function to display hypothesis-test decisions
report_decision <- function(p_value) {

  if (is.finite(p_value) && p_value < alpha) {
    cat("Decision: Reject H0 at alpha =", alpha, "\n")
  } else {
    cat("Decision: Fail to reject H0 at alpha =", alpha, "\n")
  }

}


# =========================================================
# TEST 1: COMPARISON OF MEANS
# Income by Default Status
# =========================================================

# Hypotheses:
# H0: Mean income is equal between non-defaulters
#     and defaulters.
# H1: Mean income differs between the two groups.
#
# Selected test: Welch Two Sample t-test
#
# Reason:
# Income is a numerical variable.
# Default Status contains two independent groups.
# Welch's t-test does not assume equal group variances.
#
# Assumptions:
# 1. Observations are independent.
# 2. Income is numerical.
# 3. Large group sizes support approximate inference.
# 4. Extreme outliers may influence the results.

cat("\n========== TEST 1: WELCH T-TEST ==========\n")

# Perform Welch's t-test
t_test_income <- t.test(
  income ~ Status,
  data = df
)

print(t_test_income)

# Calculate mean income for each group
income_means <- aggregate(
  income ~ Status,
  data = df,
  FUN = mean
)

cat("\nMean income by Status:\n")
print(income_means)

# ---------------------------------------------------------
# Effect Size: Cohen's d
# ---------------------------------------------------------

# Extract income values for each group
income_0 <- na.omit(df$income[df$Status == "0"])
income_1 <- na.omit(df$income[df$Status == "1"])

# Group sample sizes
n0 <- length(income_0)
n1 <- length(income_1)

# Pooled standard deviation
pooled_sd <- sqrt(
  ((n0 - 1) * var(income_0) +
     (n1 - 1) * var(income_1)) /
    (n0 + n1 - 2)
)

# Calculate Cohen's d
cohens_d <- (
  mean(income_0) - mean(income_1)
) / pooled_sd

cat("\nCohen's d:", round(cohens_d, 5), "\n")

# Hypothesis decision
report_decision(t_test_income$p.value)

# ---------------------------------------------------------
# VERIFIED RESULTS
# ---------------------------------------------------------

# Welch t-statistic = 22.584
# Degrees of freedom = 54076
# p-value < 2.2e-16
#
# Mean income:
# Non-defaulters = 7204.014
# Defaulters = 6231.807
#
# Mean difference = 972.207
# 95% Confidence Interval = [887.8314, 1056.5835]
#
# Cohen's d = 0.14997
#
# Statistical interpretation:
# Since p < 0.05, reject H0.
#
# Average income differs significantly between
# non-defaulters and defaulters.
#
# However, Cohen's d indicates a small effect size.
# Therefore, the statistically significant difference
# is relatively small compared with income variability.
#
# Business implication:
# Lower income is associated with default status.
# Income may contribute to a multivariable loan-risk
# assessment, but should not be used alone to make
# lending decisions.
#
# Other relevant financial characteristics, such as
# LTV and debt-to-income ratio, should be considered.
#
# This result indicates association, not causation.


# =========================================================
# TEST 2: COMPARISON OF PROPORTIONS
# Default Rate by Recorded Gender Category
# =========================================================

# Hypotheses:
# H0: Default proportions are equal across all
#     recorded Gender categories.
# H1: At least one recorded category has a
#     different default proportion.
#
# Selected test: Chi-square-based proportion test
#
# Reason:
# Status is a binary categorical variable.
# Gender contains four recorded categories.
# We compare default proportions across these groups.
#
# Assumptions:
# 1. Observations are independent.
# 2. Categories are mutually exclusive.
# 3. Expected cell frequencies are sufficiently large.
#
# IMPORTANT:
# The Gender field contains "Joint" and
# "Sex Not Available".
# Therefore, it is not a purely demographic
# gender classification.

cat("\n========== TEST 2: PROPORTION TEST ==========\n")

# Create contingency table
gender_table <- table(
  df$Gender,
  df$Status
)

cat("\nGender by Status:\n")
print(gender_table)

# ---------------------------------------------------------
# Expected Frequency Check
# ---------------------------------------------------------

chi_sq <- chisq.test(gender_table)

cat("\nMinimum expected cell frequency:",
    min(chi_sq$expected), "\n")

# ---------------------------------------------------------
# Comparison of Proportions
# ---------------------------------------------------------

prop_test_gender <- prop.test(gender_table)

print(prop_test_gender)

# IMPORTANT:
# prop.test() reports the proportion corresponding
# to the first column of the contingency table.
#
# Here Status = 0 is the first column.
# Calculate Status = 1 proportions separately.

default_rates <- prop.table(
  gender_table,
  margin = 1
)[, "1"] * 100

cat("\nDefault rates by category (%):\n")
print(round(default_rates, 2))

# ---------------------------------------------------------
# Effect Size: Cramer's V
# ---------------------------------------------------------

cramers_v <- sqrt(
  as.numeric(chi_sq$statistic) /
    (
      sum(gender_table) *
        min(dim(gender_table) - 1)
    )
)

cat("\nCramer's V:",
    round(cramers_v, 5), "\n")

# Hypothesis decision
report_decision(prop_test_gender$p.value)

# ---------------------------------------------------------
# VERIFIED RESULTS
# ---------------------------------------------------------

# Chi-square statistic = 1043.6
# Degrees of freedom = 3
# p-value < 2.2e-16
#
# Default rates:
# Female = 25.12%
# Joint = 19.16%
# Male = 26.19%
# Sex Not Available = 28.59%
#
# Cramer's V = 0.08378
#
# Statistical interpretation:
# Since p < 0.05, reject H0.
#
# Default proportions differ significantly
# across the recorded Gender categories.
#
# However, Cramer's V indicates a weak association
# between recorded Gender category and Status.
#
# Business implication:
# Joint applications have the lowest observed
# default proportion in this dataset.
#
# This does not prove that adding a co-applicant
# causes a reduction in default risk.
#
# The bank should investigate this relationship
# while controlling for other financial variables.
#
# Gender-related information must not be used
# for discriminatory lending decisions.
#
# "Sex Not Available" should be investigated
# as a potential data-quality issue.


# =========================================================
# TEST 3: ONE-WAY ANOVA
# Loan Amount Across Loan Purpose Categories
# =========================================================

# Hypotheses:
# H0: Mean loan amount is equal across all
#     loan-purpose categories.
# H1: At least one category has a different
#     mean loan amount.
#
# Selected tests:
# 1. Classical One-way ANOVA
# 2. Levene's test
# 3. Welch ANOVA
# 4. Pairwise Welch t-tests with Holm correction
#
# Reason:
# Loan amount is numerical.
# Loan purpose contains more than two groups.
#
# Assumptions of classical ANOVA:
# 1. Independent observations.
# 2. Approximately normal within-group residuals.
# 3. Equal population variances.
#
# Levene's test checks the equal-variance assumption.
#
# Since unequal variances were detected, Welch ANOVA
# is used for the main statistical conclusion.
#
# IMPORTANT:
# This analysis compares loan amounts.
# It does NOT compare default probabilities.

cat("\n========== TEST 3: ANOVA ==========\n")

# ---------------------------------------------------------
# Classical One-way ANOVA
# ---------------------------------------------------------

anova_model <- aov(
  loan_amount ~ loan_purpose,
  data = df
)

anova_results <- summary(anova_model)

cat("\nClassical ANOVA results:\n")
print(anova_results)

# ---------------------------------------------------------
# Mean Loan Amount by Purpose
# ---------------------------------------------------------

purpose_means <- aggregate(
  loan_amount ~ loan_purpose,
  data = df,
  FUN = mean
)

cat("\nMean loan amount by purpose:\n")
print(purpose_means)

# ---------------------------------------------------------
# Effect Size: Eta-squared
# ---------------------------------------------------------

anova_table <- anova_results[[1]]

eta_squared <- (
  anova_table[1, "Sum Sq"] /
    sum(anova_table[, "Sum Sq"])
)

cat("\nEta-squared:",
    round(eta_squared, 5), "\n")

cat("Percentage of variation explained:",
    round(eta_squared * 100, 2), "%\n")

# ---------------------------------------------------------
# Levene's Test for Equal Variances
# ---------------------------------------------------------

levene_purpose <- car::leveneTest(
  loan_amount ~ as.factor(loan_purpose),
  data = df,
  center = median
)

cat("\nLevene's test across loan purposes:\n")
print(levene_purpose)

# ---------------------------------------------------------
# Welch ANOVA
# ---------------------------------------------------------

# Welch ANOVA does not assume equal variances.

welch_anova <- oneway.test(
  loan_amount ~ loan_purpose,
  data = df,
  var.equal = FALSE
)

cat("\nWelch ANOVA results:\n")
print(welch_anova)

# ---------------------------------------------------------
# Pairwise Welch t-tests
# ---------------------------------------------------------

# Remove actual NA observations.
# Keep blank-string categories separately for review.

purpose_data <- df[
  !is.na(df$loan_amount) &
    !is.na(df$loan_purpose),
  c("loan_amount", "loan_purpose")
]

# Use Welch pairwise tests because group variances differ.
# Holm adjustment controls family-wise error rates.

pairwise_purpose <- pairwise.t.test(
  x = purpose_data$loan_amount,
  g = factor(purpose_data$loan_purpose),
  p.adjust.method = "holm",
  pool.sd = FALSE
)

cat("\nPairwise Welch tests (Holm-adjusted):\n")
print(pairwise_purpose)

# Use Welch ANOVA for the final hypothesis decision.
report_decision(welch_anova$p.value)

# ---------------------------------------------------------
# VERIFIED RESULTS
# ---------------------------------------------------------

# Classical ANOVA:
# F(4,148665) = 1682
# p < 2.2e-16
#
# Eta-squared = 0.04329
#
# Loan purpose accounts for approximately 4.33%
# of the observed variation in loan amounts.
#
# Levene's test:
# F(4,148665) = 257.85
# p < 2.2e-16
#
# Equal-variance assumption is violated.
#
# Welch ANOVA:
# F(4,1021.7) = 1819.5
# p < 2.2e-16
#
# Mean loan amounts:
# Blank category = 353067.2
# p1 = 338754.3
# p2 = 202462.1
# p3 = 294748.3
# p4 = 371061.6
#
# Holm-adjusted pairwise Welch comparisons:
# Eight of ten comparisons are statistically
# significant at alpha = 0.05.
#
# Non-significant comparisons:
# Blank vs p1: adjusted p = 0.6041
# Blank vs p4: adjusted p = 0.6041
#
# All other category pairs have adjusted p < 0.05.
#
# Statistical interpretation:
# Since Welch ANOVA p < 0.05, reject H0.
#
# Mean loan amounts differ significantly
# among loan-purpose categories.
#
# Levene's test indicates unequal variances,
# so Welch ANOVA is more appropriate than
# classical ANOVA for the main conclusion.
#
# Eta-squared indicates that the association
# between loan purpose and loan amount is modest.
#
# Business implication:
# Different loan purposes are associated with
# different typical loan amounts.
#
# Category p2 has the lowest mean loan amount.
#
# The bank should verify the definitions of
# coded categories p1, p2, p3 and p4.
#
# The blank category requires data-quality review.
#
# This test does NOT determine whether certain
# purposes have higher default risk.
#
# Further investigation is required before
# implementing purpose-specific lending policies.


# =========================================================
# TEST 4: COMPARISON OF VARIANCES
# Loan Amount by Default Status
# =========================================================

# Hypotheses:
# H0: Population variance of loan amount is equal
#     between default and non-default groups.
# H1: Population variances differ between groups.
#
# Selected tests:
# 1. Classical F-test for equality of variances.
# 2. Median-centred Levene's test.
#
# Reason:
# Loan amount is numerical.
# Status defines two independent groups.
#
# Classical F-test assumptions:
# 1. Independent observations.
# 2. Approximately normal distributions
#    within each group.
#
# The F-test is sensitive to non-normality.
# Levene's test provides a more robust comparison.

cat("\n========== TEST 4: VARIANCE TEST ==========\n")

# ---------------------------------------------------------
# Classical F-test
# ---------------------------------------------------------

var_test_loan <- var.test(
  loan_amount ~ Status,
  data = df
)

print(var_test_loan)

# ---------------------------------------------------------
# Calculate Variance for Each Group
# ---------------------------------------------------------

loan_variances <- tapply(
  df$loan_amount,
  df$Status,
  var,
  na.rm = TRUE
)

cat("\nObserved loan amount variance by Status:\n")
print(loan_variances)

# ---------------------------------------------------------
# Calculate Variance Ratio
# ---------------------------------------------------------

# Variance of Status 1 divided by variance of Status 0.

variance_ratio <- unname(
  loan_variances["1"] /
    loan_variances["0"]
)

cat("\nVariance ratio (Status 1 / Status 0):",
    round(variance_ratio, 5), "\n")

# ---------------------------------------------------------
# Levene's Test
# ---------------------------------------------------------

levene_status <- car::leveneTest(
  loan_amount ~ Status,
  data = df,
  center = median
)

cat("\nLevene's test by Status:\n")
print(levene_status)

# ---------------------------------------------------------
# Hypothesis Decisions
# ---------------------------------------------------------

cat("\nClassical F-test:\n")
report_decision(var_test_loan$p.value)

cat("Levene's test:\n")
report_decision(levene_status[1, "Pr(>F)"])

# ---------------------------------------------------------
# VERIFIED RESULTS
# ---------------------------------------------------------

# Classical F-test:
# F = 0.70328
# Numerator df = 112030
# Denominator df = 36638
# p < 2.2e-16
#
# Variance ratio (Status 0 / Status 1) = 0.70328
#
# 95% Confidence Interval:
# [0.6916231, 0.7150875]
#
# Variance ratio (Status 1 / Status 0)
# is approximately 1.42.
#
# Levene's test:
# F(1,148668) = 174.68
# p < 2.2e-16
#
# Statistical interpretation:
# Since p < 0.05 for both tests, reject H0.
#
# Loan amount variance differs significantly
# between defaulters and non-defaulters.
#
# Defaulters have approximately 1.42 times the
# observed loan amount variance of non-defaulters.
#
# Both classical F-test and median-centred
# Levene's test support the variance difference.
#
# Business implication:
# The bank should investigate differences in
# loan-size distributions between status groups.
#
# Loan amount should be evaluated alongside
# income, LTV, DTI and other relevant variables.
#
# Differences in variance do not establish
# that loan amount causes default.
#
# These results do not prove that larger loans
# necessarily have greater default risk.


# =========================================================
# FINAL TASK 4 SUMMARY
# =========================================================

cat("\n========================================\n")
cat("TASK 4: STATISTICAL INFERENCE SUMMARY\n")
cat("========================================\n")

cat("\nTEST 1: Welch's t-test\n")
cat("Cohen's d:",
    round(cohens_d, 5), "\n")
cat("p-value:", t_test_income$p.value, "\n")

cat("\nTEST 2: Comparison of Proportions\n")
cat("Cramer's V:",
    round(cramers_v, 5), "\n")
cat("p-value:", prop_test_gender$p.value, "\n")

cat("\nTEST 3: Welch ANOVA\n")
cat("Eta-squared:",
    round(eta_squared, 5), "\n")
cat("Welch p-value:", welch_anova$p.value, "\n")

cat("\nTEST 4: Comparison of Variances\n")
cat("Variance ratio (Status 1 / Status 0):",
    round(variance_ratio, 5), "\n")
cat("F-test p-value:", var_test_loan$p.value, "\n")
cat("Levene p-value:",
    levene_status[1, "Pr(>F)"], "\n")

cat("\n========================================\n")
cat("ALL FOUR STATISTICAL TESTS COMPLETED\n")
cat("========================================\n")

# FINAL CONCLUSION:
#
# 1. Average income differs significantly between
#    recorded default and non-default groups,
#    but Cohen's d indicates a small effect.
#
# 2. Default proportions differ across recorded
#    Gender categories, but Cramer's V indicates
#    a weak association.
#
# 3. Average loan amounts differ significantly
#    across loan-purpose categories. Eta-squared
#    indicates that purpose accounts for about
#    4.33% of observed loan-amount variability.
#
# 4. Loan-amount variances differ significantly
#    between status groups. Both the F-test and
#    Levene's test support this conclusion.
#
# These findings describe statistical associations
# within the dataset, not causal relationships.
#
# The bank should consider several relevant
# financial variables together and validate
# the dataset before making lending decisions.
