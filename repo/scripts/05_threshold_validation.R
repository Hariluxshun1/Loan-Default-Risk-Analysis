
# =========================================================
# IT3081 - STATISTICAL MODELLING
# TASK 5: VALIDATION-BASED THRESHOLD SELECTION
# Project: Loan Default Risk Analysis
# Owner: Person C
# Working directory: repo/
# =========================================================

source("scripts/00_setup.R")

set.seed(42)

# =========================================================
# 1. SELECT MODEL VARIABLES
# =========================================================

predictors <- c(
  "loan_amount",
  "income",
  "LTV",
  "dtir1",
  "Credit_Score"
)

model_data <- df[, c("Status", predictors)]

# IMPORTANT:
# Status = 1 is treated as default according to
# the project's working interpretation.
#
# Missingness can be strongly outcome-associated.
# Missing-value indicators are not used as predictors
# in this initial sensitivity analysis.


# =========================================================
# 2. STRATIFIED 60/20/20 SPLIT
# =========================================================

# Create partitions separately within each Status
# category to preserve class proportions.

status_groups <- split(
  seq_len(nrow(model_data)),
  model_data$Status
)

train_idx <- integer(0)
validation_idx <- integer(0)
test_idx <- integer(0)

for (group in status_groups) {

  shuffled <- sample(group)

  n <- length(shuffled)

  n_train <- floor(0.60 * n)
  n_validation <- floor(0.20 * n)

  train_idx <- c(
    train_idx,
    shuffled[seq_len(n_train)]
  )

  validation_idx <- c(
    validation_idx,
    shuffled[
      seq.int(n_train + 1, n_train + n_validation)
    ]
  )

  test_idx <- c(
    test_idx,
    shuffled[
      seq.int(n_train + n_validation + 1, n)
    ]
  )
}

train <- model_data[train_idx, ]
validation <- model_data[validation_idx, ]
test <- model_data[test_idx, ]

cat("\n===== DATA SPLIT =====\n")

cat("Training rows:", nrow(train), "\n")
cat("Validation rows:", nrow(validation), "\n")
cat("Testing rows:", nrow(test), "\n")

cat("\nClass proportions:\n")
print(rbind(
  Train = prop.table(table(train$Status)),
  Validation = prop.table(table(validation$Status)),
  Test = prop.table(table(test$Status))
))


# =========================================================
# 3. MEDIAN IMPUTATION
# =========================================================

# Calculate medians using TRAINING DATA ONLY.
# Never calculate imputation statistics on
# validation or test observations.

train_medians <- sapply(
  train[, predictors, drop = FALSE],
  median,
  na.rm = TRUE
)

for (col in predictors) {

  if (!is.finite(train_medians[col])) {
    stop(paste("Cannot impute predictor:", col))
  }

  train[[col]][is.na(train[[col]])] <-
    train_medians[col]

  validation[[col]][is.na(validation[[col]])] <-
    train_medians[col]

  test[[col]][is.na(test[[col]])] <-
    train_medians[col]
}

cat("\nTraining medians:\n")
print(train_medians)


# =========================================================
# 4. TRAIN LOGISTIC REGRESSION
# =========================================================

model_formula <- as.formula(
  paste(
    "Status ~",
    paste(predictors, collapse = " + ")
  )
)

log_model <- glm(
  model_formula,
  data = train,
  family = binomial()
)

cat("\n===== LOGISTIC REGRESSION =====\n")
print(summary(log_model))


# =========================================================
# 5. PREDICT ON VALIDATION DATA
# =========================================================

validation_prob <- predict(
  log_model,
  newdata = validation,
  type = "response"
)

# Function to calculate classification metrics
calculate_metrics <- function(actual, probabilities, threshold) {

  actual <- as.character(actual)
  predicted <- ifelse(probabilities >= threshold, "1", "0")

  TP <- sum(predicted == "1" & actual == "1")
  FP <- sum(predicted == "1" & actual == "0")
  TN <- sum(predicted == "0" & actual == "0")
  FN <- sum(predicted == "0" & actual == "1")

  precision <- if ((TP + FP) == 0) {
    0
  } else {
    TP / (TP + FP)
  }

  recall <- if ((TP + FN) == 0) {
    0
  } else {
    TP / (TP + FN)
  }

  specificity <- if ((TN + FP) == 0) {
    0
  } else {
    TN / (TN + FP)
  }

  accuracy <- (TP + TN) / (TP + TN + FP + FN)

  f1 <- if ((precision + recall) == 0) {
    0
  } else {
    2 * precision * recall / (precision + recall)
  }

  data.frame(
    Threshold = threshold,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    Specificity = specificity,
    F1 = f1,
    TP = TP,
    FP = FP,
    TN = TN,
    FN = FN
  )
}


# =========================================================
# 6. SELECT THRESHOLD USING VALIDATION DATA
# =========================================================

# F1 is used as an illustrative selection objective.
# An operational bank should instead consider
# false-positive and false-negative business costs.

thresholds <- seq(0.10, 0.50, by = 0.01)

validation_results <- do.call(
  rbind,
  lapply(
    thresholds,
    function(t) calculate_metrics(
      validation$Status,
      validation_prob,
      t
    )
  )
)

# Select threshold with highest validation F1.
# In a tie, choose the higher threshold.
best_index <- order(
  -validation_results$F1,
  -validation_results$Threshold
)[1]

best_threshold <- validation_results$Threshold[best_index]

cat("\n===== VALIDATION RESULTS =====\n")
print(validation_results[
  order(-validation_results$F1),
][1:10, ])

cat("\nSelected threshold:",
    round(best_threshold, 3), "\n")


# =========================================================
# 7. FINAL TEST-SET EVALUATION
# =========================================================

# The threshold was selected WITHOUT using test labels.

test_prob <- predict(
  log_model,
  newdata = test,
  type = "response"
)

test_metrics <- calculate_metrics(
  test$Status,
  test_prob,
  best_threshold
)

cat("\n===== FINAL TEST RESULTS =====\n")
print(test_metrics)

# Calculate ROC-AUC
if (!requireNamespace("pROC", quietly = TRUE)) {
  stop("Install pROC: install.packages('pROC')")
}

test_roc <- pROC::roc(
  response = test$Status,
  predictor = test_prob,
  levels = c("0", "1"),
  direction = "<",
  quiet = TRUE
)

cat("Test ROC-AUC:",
    round(as.numeric(pROC::auc(test_roc)), 4), "\n")

# Compare with default threshold 0.50
cat("\n===== TEST AT DEFAULT THRESHOLD 0.50 =====\n")

print(
  calculate_metrics(test$Status, test_prob, 0.50)
)


# =========================================================
# 8. FINAL INTERPRETATION
# =========================================================

cat("\n===== INTERPRETATION =====\n")
cat("Threshold selected using validation F1.\n")
cat("Test outcomes were not used during selection.\n")
cat("Missing predictor values were imputed using\n")
cat("training medians only.\n")
cat("High recall may produce many false positives.\n")
cat("AUC and F1 measure different properties.\n")
cat("The chosen threshold is not necessarily\n")
cat("optimal for operational bank decisions.\n")
cat("Status coding and predictor availability\n")
cat("must be verified before deployment.\n")
