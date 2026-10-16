
# ============================================================
# IT3081 - STATISTICAL MODELLING
# TASK 8 - CRITICAL EVALUATION OF BAYESIAN METHODS
# Project: Loan Default Risk Analysis
#
# Corrected Naive Bayes Evaluation
# Comparable with Task 5 logistic regression models
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

required_packages <- c("e1071", "pROC")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(e1071)
library(pROC)


# ============================================================
# 2. LOAD SAME DATA SPLIT AS TASK 5
# ============================================================

# This script recreates the exact 60/20/20 stratified
# partitions and training-median imputation used in Task 5.
#
# It also provides:
# train, validation, test, predictors,
# thresholds, calculate_metrics()

source("scripts/05_threshold_validation.R")

# Confirm binary outcome levels
train$Status <- factor(train$Status, levels = c("0", "1"))
validation$Status <- factor(
  validation$Status,
  levels = c("0", "1")
)
test$Status <- factor(test$Status, levels = c("0", "1"))

cat("\n===== NAIVE BAYES DATA SPLIT =====\n")

cat("Training rows:", nrow(train), "\n")
cat("Validation rows:", nrow(validation), "\n")
cat("Test rows:", nrow(test), "\n")


# ============================================================
# 3. TRAIN NAIVE BAYES MODEL
# ============================================================

cat("\n===== TRAINING NAIVE BAYES =====\n")

# e1071::naiveBayes fits a Gaussian Naive Bayes
# model for these numerical predictors.
#
# Train only on training observations.
# Do NOT train using validation or test data.

nb_formula <- as.formula(
  paste(
    "Status ~",
    paste(predictors, collapse = " + ")
  )
)

nb_model <- e1071::naiveBayes(
  formula = nb_formula,
  data = train,
  laplace = 0
)

print(nb_model)


# ============================================================
# 4. PREDICT VALIDATION PROBABILITIES
# ============================================================

cat("\n===== VALIDATION PREDICTIONS =====\n")

nb_validation_prob <- predict(
  nb_model,
  newdata = validation,
  type = "raw"
)[, "1"]

# Check probability distribution
print(summary(nb_validation_prob))

# Ensure probabilities are valid
if (
  anyNA(nb_validation_prob) ||
  any(!is.finite(nb_validation_prob))
) {
  stop("Invalid Naive Bayes validation probabilities.")
}


# ============================================================
# 5. SELECT THRESHOLD USING VALIDATION F1
# ============================================================

# Use the same threshold search grid as Task 5:
# 0.10 to 0.50, increasing by 0.01.

nb_validation_results <- do.call(
  rbind,
  lapply(
    thresholds,
    function(t) {
      calculate_metrics(
        validation$Status,
        nb_validation_prob,
        t
      )
    }
  )
)

# Highest validation F1 wins.
# If tied, select the higher threshold.

nb_best_index <- order(
  -nb_validation_results$F1,
  -nb_validation_results$Threshold
)[1]

nb_best_threshold <-
  nb_validation_results$Threshold[nb_best_index]

nb_best_validation_f1 <-
  nb_validation_results$F1[nb_best_index]

cat("\n===== TOP VALIDATION RESULTS =====\n")

print(
  head(
    nb_validation_results[
      order(-nb_validation_results$F1),
    ],
    10
  )
)

cat("\nSelected Naive Bayes threshold:",
    nb_best_threshold, "\n")

cat("Validation F1:",
    round(nb_best_validation_f1, 4), "\n")


# ============================================================
# 6. FINAL TEST-SET EVALUATION
# ============================================================

cat("\n===== NAIVE BAYES TEST RESULTS =====\n")

nb_test_prob <- predict(
  nb_model,
  newdata = test,
  type = "raw"
)[, "1"]

if (
  anyNA(nb_test_prob) ||
  any(!is.finite(nb_test_prob))
) {
  stop("Invalid Naive Bayes test probabilities.")
}

nb_test_metrics <- calculate_metrics(
  test$Status,
  nb_test_prob,
  nb_best_threshold
)

nb_roc <- pROC::roc(
  response = test$Status,
  predictor = nb_test_prob,
  levels = c("0", "1"),
  direction = "<",
  quiet = TRUE
)

nb_auc <- as.numeric(pROC::auc(nb_roc))

nb_test_metrics$Model <- "Naive Bayes"
nb_test_metrics$Validation_F1 <- nb_best_validation_f1
nb_test_metrics$ROC_AUC <- nb_auc

print(nb_test_metrics)

cat("\nNaive Bayes Test ROC-AUC:",
    round(nb_auc, 4), "\n")


# ============================================================
# 7. COMPARE WITH DEFAULT THRESHOLD 0.50
# ============================================================

cat("\n===== NAIVE BAYES AT THRESHOLD 0.50 =====\n")

print(
  calculate_metrics(
    test$Status,
    nb_test_prob,
    0.50
  )
)


# ============================================================
# 8. SIX-MODEL COMPARISON
# ============================================================

# Load the existing five-model results.
# They must come from 05_final_model_comparison.R.

comparison_path <- "results/task5_model_comparison.csv"

if (!file.exists(comparison_path)) {
  stop(
    "Run scripts/05_final_model_comparison.R first."
  )
}

existing_results <- read.csv(
  comparison_path,
  stringsAsFactors = FALSE
)

nb_comparison_row <- nb_test_metrics[, c(
  "Model",
  "Threshold",
  "Validation_F1",
  "Accuracy",
  "Precision",
  "Recall",
  "Specificity",
  "F1",
  "ROC_AUC",
  "TP",
  "FP",
  "TN",
  "FN"
)]

# Combine into one comparison table
six_model_comparison <- rbind(
  existing_results,
  nb_comparison_row
)

# Rank according to validation F1 only
six_model_comparison <- six_model_comparison[
  order(-six_model_comparison$Validation_F1),
]

rownames(six_model_comparison) <- NULL

cat("\n============================================\n")
cat("FINAL SIX-MODEL COMPARISON\n")
cat("============================================\n")

print(
  six_model_comparison,
  digits = 4,
  row.names = FALSE
)

cat("\nHighest validation F1 model:\n")
print(six_model_comparison$Model[1])


# ============================================================
# 9. SAVE RESULTS
# ============================================================

dir.create(
  "results",
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  six_model_comparison,
  "results/task5_six_model_comparison.csv",
  row.names = FALSE
)

cat("\nSaved comparison to:\n")
cat("results/task5_six_model_comparison.csv\n")


# ============================================================
# 10. CRITICAL EVALUATION OF BAYESIAN METHODS
# ============================================================

# NAIVE BAYES
#
# Naive Bayes applies Bayes' theorem and assumes
# conditional independence between predictors
# given the target class.
#
# For numerical variables, this implementation
# models predictor values using class-conditional
# Gaussian distributions.
#
# The independence assumption is a simplification.
# Low VIF scores do not prove conditional independence.
#
# Advantages:
# - Computationally efficient
# - Easy to implement
# - Produces class probability estimates
#
# Limitations:
# - Independence assumption may be unrealistic
# - Gaussian assumptions may not fit financial data
# - Probability estimates may require calibration
# - Performance depends on data quality
#
# The earlier in-sample ROC-AUC of 0.676 from the
# original script is NOT a valid held-out
# comparison against Task 5 models.


# BAYESIAN REGRESSION
#
# Bayesian regression specifies prior distributions
# for model parameters and updates them with data
# to obtain posterior distributions.
#
# Domain knowledge can inform prior selection,
# but priors must be justified and tested.
#
# Compared with frequentist logistic regression,
# Bayesian regression can represent parameter
# uncertainty using posterior distributions.
#
# Bayesian regression is discussed conceptually
# in this task. It is not implemented here.


# BAYESIAN DECISION MAKING
#
# Bayesian decision theory combines estimated
# outcome probabilities with the consequences
# of different actions.
#
# For example:
# - False negatives may represent missed defaults.
# - False positives may represent reliable borrowers
#   incorrectly flagged as high risk.
#
# Decision thresholds should account for these
# costs and probability calibration.
#
# Maximizing F1 is an assignment evaluation
# objective, not necessarily the optimal banking
# decision policy.


# ============================================================
# 11. FINAL CONCLUSIONS
# ============================================================

cat("\n===== TASK 8 CONCLUSION =====\n")

cat("Naive Bayes was fitted using training data only.\n")
cat("Its threshold was selected on validation data.\n")
cat("It was evaluated on the Task 5 test observations.\n")

cat("Naive Bayes validation F1:",
    round(nb_best_validation_f1, 4), "\n")

cat("Naive Bayes test F1:",
    round(nb_test_metrics$F1, 4), "\n")

cat("Naive Bayes ROC-AUC:",
    round(nb_auc, 4), "\n")

cat("Best model by validation F1:",
    six_model_comparison$Model[1], "\n")

cat("\nIMPORTANT LIMITATIONS:\n")
cat("- Status=1 is an unverified default label assumption.\n")
cat("- Missingness is associated with loan status.\n")
cat("- Dataset analyses have already informed model choices.\n")
cat("- All comparisons are exploratory.\n")
cat("- Do not use these models for automated lending decisions.\n")

cat("\nTASK 8 NAIVE BAYES EVALUATION COMPLETE.\n")
