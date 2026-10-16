
# =========================================================
# IT3081 - STATISTICAL MODELLING
# TASK 5: MISSING-DATA SENSITIVITY ANALYSIS
# =========================================================

source("scripts/00_setup.R")

# Required package
library(pROC)

set.seed(42)

# ---------------------------------------------------------
# 1. SELECT THE SAME FIVE PREDICTORS
# ---------------------------------------------------------

predictors <- c(
  "loan_amount",
  "income",
  "LTV",
  "dtir1",
  "Credit_Score"
)

columns <- c("Status", predictors)

data_all <- df[, columns]

# ---------------------------------------------------------
# 2. STRATIFIED TRAIN/TEST SPLIT
# ---------------------------------------------------------

# Split the full dataset BEFORE imputing missing values.

train_idx <- unlist(
  lapply(split(seq_len(nrow(data_all)), data_all$Status),
         function(i) sample(i, floor(0.7 * length(i))))
)

train <- data_all[train_idx, ]
test <- data_all[-train_idx, ]

cat("Training rows:", nrow(train), "\n")
cat("Testing rows:", nrow(test), "\n")

# ---------------------------------------------------------
# 3. FIT MEDIAN VALUES ON TRAINING DATA ONLY
# ---------------------------------------------------------

medians <- sapply(
  train[, predictors],
  median,
  na.rm = TRUE
)

# Add missingness indicator variables.
# These indicate whether a value was originally missing.
# IMPORTANT:
# These indicators may reflect outcome-related
# data collection, not genuine borrower risk.

for (col in predictors) {

  missing_col <- paste0(col, "_missing")

  train[[missing_col]] <- as.integer(is.na(train[[col]]))
  test[[missing_col]] <- as.integer(is.na(test[[col]]))

  train[[col]][is.na(train[[col]])] <- medians[col]
  test[[col]][is.na(test[[col]])] <- medians[col]
}

# ---------------------------------------------------------
# 4. TRAIN IMPUTED LOGISTIC REGRESSION
# ---------------------------------------------------------

# Use the same five predictors without missing indicators
# for the initial comparison.
# Missingness indicators are retained for later diagnostics.

formula_model <- as.formula(
  paste("Status ~", paste(predictors, collapse = " + "))
)

imputed_model <- glm(
  formula_model,
  data = train,
  family = binomial()
)

summary(imputed_model)

# ---------------------------------------------------------
# 5. EVALUATE ON UNTOUCHED TEST ROWS
# ---------------------------------------------------------

# Test predictors are imputed using training medians.
# Test labels were never used in preprocessing.

prob <- predict(
  imputed_model,
  newdata = test,
  type = "response"
)

actual <- as.character(test$Status)
predicted <- ifelse(prob >= 0.5, "1", "0")

conf <- table(
  Predicted = factor(predicted, levels = c("0", "1")),
  Actual = factor(actual, levels = c("0", "1"))
)

print(conf)

TP <- conf["1", "1"]
TN <- conf["0", "0"]
FP <- conf["1", "0"]
FN <- conf["0", "1"]

accuracy <- (TP + TN) / sum(conf)
precision <- if ((TP + FP) == 0) NA_real_ else TP / (TP + FP)
recall <- if ((TP + FN) == 0) NA_real_ else TP / (TP + FN)

roc_obj <- roc(
  response = factor(actual, levels = c("0", "1")),
  predictor = prob,
  levels = c("0", "1"),
  direction = "<",
  quiet = TRUE
)

cat("\n===== IMPUTED MODEL RESULTS =====\n")
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall:", round(recall, 4), "\n")
cat("ROC-AUC:", round(as.numeric(auc(roc_obj)), 4), "\n")

# ---------------------------------------------------------
# 6. DATA DISTRIBUTION COMPARISON
# ---------------------------------------------------------

cat("\nOriginal default rate:\n")
print(prop.table(table(data_all$Status)))

cat("\nImputed test-set default rate:\n")
print(prop.table(table(test$Status)))

cat("\nTraining medians:\n")
print(medians)

cat("\nMissingness by Status (full dataset):\n")
for (col in predictors) {
  cat("\n", col, "\n")
  print(table(data_all$Status, is.na(data_all[[col]])))
}

# ---------------------------------------------------------
# INTERPRETATION GUIDELINES
# ---------------------------------------------------------

# Compare the imputed model against the earlier
# complete-case logistic regression.
#
# Earlier complete-case baseline:
# Test ROC-AUC = 0.6210
# Accuracy = 0.8368
# Recall = 0.0004921
#
# NOTE:
# This script uses a different evaluation population.
# AUCs can be compared descriptively, but this is not
# a matched head-to-head test of the same observations.
#
# Median imputation preserves more observations but
# does not necessarily eliminate selection bias.
#
# Missingness may be outcome-dependent.
# Confirm predictor availability and target meaning
# before recommending operational use.
