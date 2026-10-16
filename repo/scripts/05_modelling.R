# =============================================================
# IT3081 - TASK 5: PREDICTIVE STATISTICAL MODELLING
# Project: Loan Default Risk Analysis | Owner: Person C
# Run from the repo/ working directory.
# =============================================================
source("scripts/00_setup.R")

required_packages <- c("glmnet", "pROC", "smotefamily")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                            logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}

# The project's interpretation is Status 0 = non-default, 1 = default.
# Confirm the original dataset's meaning before making business claims.
features <- c("loan_amount", "income", "LTV", "dtir1", "Credit_Score")
needed <- c("Status", features)
stopifnot(all(needed %in% names(df)))

cat("\n===== 1. DATA QUALITY AND POSSIBLE LEAKAGE =====\n")
for (variable in c("rate_of_interest", "Interest_rate_spread",
                   "Upfront_charges", "dtir1")) {
  if (variable %in% names(df)) {
    cat("\nMissingness by Status:", variable, "\n")
    print(table(Status = df$Status, Missing = is.na(df[[variable]])))
  }
}
# Exclude rate-related columns because they may be recorded after loan
# processing and their missingness may reveal the Status outcome.
# Exclusion is precautionary; it is not proof of the data-generation mechanism.
# dtir1 is retained for continuity with the initial model, but its
# outcome-dependent missingness is a major selection-bias limitation.

model_data <- df[, needed]
model_data$Status <- factor(as.character(model_data$Status), levels = c("0", "1"))
stopifnot(!anyNA(model_data$Status))

original_n <- nrow(model_data)
model_data <- model_data[complete.cases(model_data), , drop = FALSE]
cat("\nOriginal rows:", original_n,
    "| Complete cases:", nrow(model_data),
    "| Dropped:", original_n - nrow(model_data), "\n")
cat("Full-data default rate:", mean(df$Status == "1"), "\n")
cat("Complete-case default rate:", mean(model_data$Status == "1"), "\n")
# Complete-case results describe the subset with all five predictors.
# They may not generalize to applicants with missing income/LTV/DTI.

cat("\n===== 2. STRATIFIED 70/30 TRAIN / TEST SPLIT =====\n")
set.seed(3081)
indices_0 <- which(model_data$Status == "0")
indices_1 <- which(model_data$Status == "1")
train_indices <- sort(c(sample(indices_0, floor(0.70 * length(indices_0))),
                        sample(indices_1, floor(0.70 * length(indices_1)))))
train <- model_data[train_indices, , drop = FALSE]
test <- model_data[-train_indices, , drop = FALSE]
rownames(train) <- NULL
rownames(test) <- NULL
cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")
cat("Training status distribution:\n")
print(prop.table(table(train$Status)))
cat("Test status distribution:\n")
print(prop.table(table(test$Status)))
# The test data must remain untouched: no SMOTE, reweighting or fitting.

formula_model <- Status ~ loan_amount + income + LTV + dtir1 + Credit_Score

cat("\n===== 3. BASELINE LOGISTIC REGRESSION =====\n")
log_model <- glm(formula_model, data = train, family = binomial())
print(summary(log_model))
prob_logistic <- as.numeric(predict(log_model, newdata = test, type = "response"))
# Coefficients reflect conditional associations, not causal effects.
# Linearity of each numeric predictor on the log-odds scale should be checked.

cat("\n===== 4. RIDGE AND LASSO (CV ON TRAINING DATA ONLY) =====\n")
x_train <- model.matrix(formula_model, data = train)[, -1, drop = FALSE]
x_test <- model.matrix(formula_model, data = test)[, -1, drop = FALSE]
y_train <- as.integer(as.character(train$Status))
set.seed(3081)
cv_ridge <- glmnet::cv.glmnet(x_train, y_train, family = "binomial",
                              alpha = 0, nfolds = 10, type.measure = "deviance")
set.seed(3081)
cv_lasso <- glmnet::cv.glmnet(x_train, y_train, family = "binomial",
                              alpha = 1, nfolds = 10, type.measure = "deviance")
cat("Ridge lambda.min:", cv_ridge$lambda.min, "\n")
cat("LASSO lambda.min:", cv_lasso$lambda.min, "\n")
cat("Ridge coefficients:\n")
print(coef(cv_ridge, s = "lambda.min"))
cat("LASSO coefficients:\n")
print(coef(cv_lasso, s = "lambda.min"))
prob_ridge <- as.numeric(predict(cv_ridge, newx = x_test,
                                 s = "lambda.min", type = "response"))
prob_lasso <- as.numeric(predict(cv_lasso, newx = x_test,
                                 s = "lambda.min", type = "response"))
# glmnet standardizes numeric predictors internally using training data.

cat("\n===== 5. CLASS-WEIGHTED LOGISTIC REGRESSION =====\n")
train_counts <- table(train$Status)
minority_weight <- as.numeric(train_counts["0"] / train_counts["1"])
weights_train <- ifelse(train$Status == "1", minority_weight, 1)
cat("Default class weight:", round(minority_weight, 4), "\n")
# A non-integer-successes warning may occur with non-integer case weights;
# it is expected for this weighted binomial GLM, not a test-data issue.
weighted_model <- glm(formula_model, data = train, family = binomial(),
                      weights = weights_train)
prob_weighted <- as.numeric(predict(weighted_model, newdata = test,
                                    type = "response"))

cat("\n===== 6. SMOTE + LOGISTIC REGRESSION (TRAINING ONLY) =====\n")
set.seed(3081)
smote_result <- smotefamily::SMOTE(
  X = train[, features, drop = FALSE],
  target = train$Status,
  K = 5,
  dup_size = 2
)
smote_train <- as.data.frame(smote_result$data)
# smotefamily places the output class label in the last column.
names(smote_train)[ncol(smote_train)] <- "Status"
smote_train$Status <- factor(as.character(smote_train$Status), levels = c("0", "1"))
stopifnot(!anyNA(smote_train$Status), all(features %in% names(smote_train)))
cat("SMOTE training distribution:\n")
print(table(smote_train$Status))
smote_model <- glm(formula_model, data = smote_train, family = binomial())
prob_smote <- as.numeric(predict(smote_model, newdata = test,
                                 type = "response"))
# NEVER evaluate on the synthetic SMOTE training set.

cat("\n===== 7. COMMON UNTOUCHED TEST-SET EVALUATION =====\n")
# Status 1 is always the positive class.
# Thresholds 0.50 and 0.30 are prespecified illustrations, not optimized
# on the test set. Operational thresholds require cost and calibration review.
evaluate_model <- function(name, actual, probabilities, threshold = 0.5) {
  stopifnot(length(actual) == length(probabilities),
            !anyNA(probabilities),
            all(is.finite(probabilities)),
            all(probabilities >= 0 & probabilities <= 1))
  actual <- factor(as.character(actual), levels = c("0", "1"))
  predicted <- factor(ifelse(probabilities >= threshold, "1", "0"),
                      levels = c("0", "1"))
  cm <- table(Predicted = predicted, Actual = actual)
  tn <- unname(cm["0", "0"])
  fp <- unname(cm["1", "0"])
  fn <- unname(cm["0", "1"])
  tp <- unname(cm["1", "1"])
  safe_ratio <- function(numerator, denominator) {
    if (denominator == 0) return(NA_real_)
    numerator / denominator
  }
  precision <- safe_ratio(tp, tp + fp)
  recall <- safe_ratio(tp, tp + fn)
  specificity <- safe_ratio(tn, tn + fp)
  f1 <- if (is.na(precision) || is.na(recall) ||
            precision + recall == 0) NA_real_ else
    2 * precision * recall / (precision + recall)
  roc_obj <- pROC::roc(response = actual, predictor = probabilities,
                        levels = c("0", "1"), direction = "<", quiet = TRUE)
  result <- data.frame(Model = name, Threshold = threshold,
                       Accuracy = (tp + tn) / sum(cm),
                       Precision = precision, Recall = recall,
                       Specificity = specificity, F1 = f1,
                       AUC = as.numeric(pROC::auc(roc_obj)),
                       TP = tp, FP = fp, TN = tn, FN = fn)
  list(metrics = result, confusion_matrix = cm, roc = roc_obj)
}

predictions <- list(
  "Baseline logistic" = prob_logistic,
  "Ridge logistic" = prob_ridge,
  "LASSO logistic" = prob_lasso,
  "Weighted logistic" = prob_weighted,
  "SMOTE logistic" = prob_smote
)
results_05 <- lapply(names(predictions), function(name) {
  evaluate_model(name, test$Status, predictions[[name]], threshold = 0.5)
})
comparison_05 <- do.call(rbind, lapply(results_05, `[[`, "metrics"))
comparison_05 <- comparison_05[order(-comparison_05$AUC), ]
rownames(comparison_05) <- NULL
cat("\nTEST-SET COMPARISON AT THRESHOLD 0.50:\n")
print(comparison_05, row.names = FALSE, digits = 4)

cat("\nCONFUSION MATRICES (TEST SET, THRESHOLD 0.50):\n")
for (i in seq_along(results_05)) {
  cat("\n", names(predictions)[i], "\n", sep = "")
  print(results_05[[i]]$confusion_matrix)
}

results_03 <- lapply(names(predictions), function(name) {
  evaluate_model(name, test$Status, predictions[[name]], threshold = 0.3)
})
comparison_03 <- do.call(rbind, lapply(results_03, `[[`, "metrics"))
cat("\nTEST-SET COMPARISON AT THRESHOLD 0.30 (ILLUSTRATIVE):\n")
print(comparison_03, row.names = FALSE, digits = 4)

cat("\n===== 8. DIAGNOSTICS / RECOMMENDATION NOTES =====\n")
if (requireNamespace("car", quietly = TRUE)) {
  cat("Training-set logistic VIFs:\n")
  print(car::vif(log_model))
} else {
  cat("Optional: install 'car' to calculate VIFs.\n")
}
cat("\nIMPORTANT INTERPRETATION NOTES:\n")
cat("- Compare TEST AUC and default-class recall, precision and F1.\n")
cat("- Accuracy can mislead when default is the minority class.\n")
cat("- AUC measures ranking, not probability calibration or business value.\n")
cat("- Weighted/SMOTE probabilities may require recalibration.\n")
cat("- Examine linearity of continuous predictors in the logit and fairness.\n")
cat("- Do NOT recommend a final model until these test results are reviewed.\n")
cat("- Confirm what Status means in the original Kaggle source.\n")
cat("- Missing dtir1 may bias the complete-case sample.\n")
cat("- Person D should evaluate Naive Bayes on the SAME split.\n")
