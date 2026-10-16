
# ============================================================
# IT3081 - STATISTICAL MODELLING
# TASK 5 - FINAL FIVE-MODEL COMPARISON
# Project: Loan Default Risk Analysis
# ============================================================

# STEP 1 - PACKAGES
required_packages <- c("glmnet", "smotefamily", "pROC")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(glmnet)
library(smotefamily)
library(pROC)

# STEP 2 - LOAD IDENTICAL TRAIN/VALIDATION/TEST SPLITS
# Reuses the same reproducible split and training-only
# median imputation from our threshold-validation script.

source("scripts/05_threshold_validation.R")

set.seed(42)

# Status = 1 is the project's working definition of default.
# Verify this coding with the original data documentation.

train$Status <- factor(train$Status, levels = c("0", "1"))
validation$Status <- factor(validation$Status, levels = c("0", "1"))
test$Status <- factor(test$Status, levels = c("0", "1"))

x_train <- as.matrix(train[, predictors, drop = FALSE])
x_val <- as.matrix(validation[, predictors, drop = FALSE])
x_test <- as.matrix(test[, predictors, drop = FALSE])

y_train <- as.integer(as.character(train$Status))

# STEP 3 - METRIC FUNCTION
# calculate_metrics() is loaded by the script above.

score_model <- function(
    name,
    validation_prob,
    test_prob
) {

  # Select threshold using validation data ONLY.
  validation_scores <- do.call(
    rbind,
    lapply(
      thresholds,
      function(t) {
        calculate_metrics(
          validation$Status,
          validation_prob,
          t
        )
      }
    )
  )

  # Maximize validation F1;
  # ties are resolved using the higher threshold.
  best_row <- order(
    -validation_scores$F1,
    -validation_scores$Threshold
  )[1]

  chosen_threshold <-
    validation_scores$Threshold[best_row]

  best_validation_f1 <-
    validation_scores$F1[best_row]

  # Score the same test observations for every model.
  results <- calculate_metrics(
    test$Status,
    test_prob,
    chosen_threshold
  )

  test_auc <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = test$Status,
        predictor = as.numeric(test_prob),
        levels = c("0", "1"),
        direction = "<",
        quiet = TRUE
      )
    )
  )

  results$Model <- name
  results$Validation_F1 <- best_validation_f1
  results$ROC_AUC <- test_auc

  return(results)
}

all_results <- list()

# ============================================================
# MODEL 1 - BASELINE LOGISTIC REGRESSION
# ============================================================

cat("\nTraining Model 1: Baseline Logistic...\n")

# log_model was fitted using the same training split
# in 05_threshold_validation.R.

baseline_val_prob <- predict(
  log_model,
  newdata = validation,
  type = "response"
)

baseline_test_prob <- predict(
  log_model,
  newdata = test,
  type = "response"
)

all_results[[1]] <- score_model(
  "Baseline Logistic",
  baseline_val_prob,
  baseline_test_prob
)

# ============================================================
# MODEL 2 - RIDGE LOGISTIC REGRESSION
# ============================================================

cat("\nTraining Model 2: Ridge Logistic...\n")

# Stratified cross-validation folds within training data.
foldid <- integer(length(y_train))

for (class_value in c(0, 1)) {
  idx <- which(y_train == class_value)
  foldid[idx] <- sample(
    rep(1:5, length.out = length(idx))
  )
}

ridge_cv <- cv.glmnet(
  x = x_train,
  y = y_train,
  family = "binomial",
  alpha = 0,
  foldid = foldid,
  type.measure = "deviance",
  standardize = TRUE
)

ridge_val_prob <- as.numeric(
  predict(
    ridge_cv,
    newx = x_val,
    s = "lambda.min",
    type = "response"
  )
)

ridge_test_prob <- as.numeric(
  predict(
    ridge_cv,
    newx = x_test,
    s = "lambda.min",
    type = "response"
  )
)

all_results[[2]] <- score_model(
  "Ridge Logistic",
  ridge_val_prob,
  ridge_test_prob
)

# ============================================================
# MODEL 3 - LASSO LOGISTIC REGRESSION
# ============================================================

cat("\nTraining Model 3: LASSO Logistic...\n")

lasso_cv <- cv.glmnet(
  x = x_train,
  y = y_train,
  family = "binomial",
  alpha = 1,
  foldid = foldid,
  type.measure = "deviance",
  standardize = TRUE
)

lasso_val_prob <- as.numeric(
  predict(
    lasso_cv,
    newx = x_val,
    s = "lambda.min",
    type = "response"
  )
)

lasso_test_prob <- as.numeric(
  predict(
    lasso_cv,
    newx = x_test,
    s = "lambda.min",
    type = "response"
  )
)

all_results[[3]] <- score_model(
  "LASSO Logistic",
  lasso_val_prob,
  lasso_test_prob
)

# ============================================================
# MODEL 4 - SMOTE LOGISTIC REGRESSION
# ============================================================

cat("\nTraining Model 4: SMOTE Logistic...\n")

# SMOTE is applied ONLY to the training set.
# Validation and test data remain unchanged.
#
# SMOTE uses Euclidean-distance nearest neighbors,
# so predictors are scaled with training statistics.

smote_means <- colMeans(x_train)

smote_sds <- apply(x_train, 2, sd)

if (any(!is.finite(smote_sds) | smote_sds == 0)) {
  stop("Cannot standardize one or more SMOTE predictors.")
}

scale_smote <- function(data_matrix) {
  result <- sweep(
    data_matrix,
    2,
    smote_means,
    "-"
  )
  sweep(result, 2, smote_sds, "/")
}

smote_x_train <- scale_smote(x_train)
smote_x_val <- scale_smote(x_val)
smote_x_test <- scale_smote(x_test)

set.seed(42)

smote_result <- smotefamily::SMOTE(
  X = as.data.frame(smote_x_train),
  target = train$Status,
  K = 5,
  dup_size = 2
)

smote_train <- smote_result$data

# smotefamily stores the outcome in the final column.
names(smote_train)[ncol(smote_train)] <- "Status"

smote_train$Status <- factor(
  as.character(smote_train$Status),
  levels = c("0", "1")
)

if (anyNA(smote_train$Status)) {
  stop("SMOTE returned unexpected Status labels.")
}

smote_model <- glm(
  model_formula,
  data = smote_train,
  family = binomial()
)

smote_val <- as.data.frame(smote_x_val)
smote_test <- as.data.frame(smote_x_test)

smote_val_prob <- predict(
  smote_model,
  newdata = smote_val,
  type = "response"
)

smote_test_prob <- predict(
  smote_model,
  newdata = smote_test,
  type = "response"
)

all_results[[4]] <- score_model(
  "SMOTE Logistic",
  smote_val_prob,
  smote_test_prob
)

# ============================================================
# MODEL 5 - CLASS-WEIGHTED LOGISTIC REGRESSION
# ============================================================

cat("\nTraining Model 5: Weighted Logistic...\n")

n_nondefault <- sum(train$Status == "0")
n_default <- sum(train$Status == "1")

default_weight <- n_nondefault / n_default

sample_weights <- ifelse(
  train$Status == "1",
  default_weight,
  1
)

# Non-integer success warning can occur with
# fractional case weights in a binomial GLM.
weighted_model <- suppressWarnings(
  glm(
    model_formula,
    data = train,
    family = binomial(),
    weights = sample_weights
  )
)

weighted_val_prob <- predict(
  weighted_model,
  newdata = validation,
  type = "response"
)

weighted_test_prob <- predict(
  weighted_model,
  newdata = test,
  type = "response"
)

all_results[[5]] <- score_model(
  "Weighted Logistic",
  weighted_val_prob,
  weighted_test_prob
)

# ============================================================
# FINAL COMPARISON TABLE
# ============================================================

comparison <- do.call(rbind, all_results)

comparison <- comparison[, c(
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

# Rank by VALIDATION F1, not by test performance.
comparison <- comparison[
  order(-comparison$Validation_F1),
]

rownames(comparison) <- NULL

cat("\n============================================\n")
cat("FINAL FIVE-MODEL COMPARISON\n")
cat("============================================\n")

print(
  comparison,
  digits = 4,
  row.names = FALSE
)

cat("\nBest model by validation F1:\n")

print(comparison$Model[1])

cat("\nSelected threshold:\n")
print(comparison$Threshold[1])

cat("\nRidge selected lambda:\n")
print(ridge_cv$lambda.min)

cat("\nLASSO selected lambda:\n")
print(lasso_cv$lambda.min)

cat("\nTraining distribution before SMOTE:\n")
print(table(train$Status))

cat("\nTraining distribution after SMOTE:\n")
print(table(smote_train$Status))

# ============================================================
# SAVE OUTPUTS
# ============================================================

dir.create(
  "results",
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  comparison,
  "results/task5_model_comparison.csv",
  row.names = FALSE
)

cat("\nResults saved to:\n")
cat("results/task5_model_comparison.csv\n")

cat("\nNOTE:\n")
cat("Model ranking is based on validation F1.\n")
cat("Test metrics describe out-of-sample performance.\n")
cat("This remains exploratory due to earlier data\n")
cat("analysis and model-selection experiments.\n")
cat("SMOTE/weighted probabilities may need calibration.\n")
cat("Business costs, target definition, and fairness\n")
cat("must be assessed before real-world deployment.\n")
