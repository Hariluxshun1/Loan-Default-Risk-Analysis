# 05_modelling.R — Task 5: Predictive Statistical Modelling
# Owner: Person C

source("scripts/00_setup.R")


# =========================================================
# Data leakage check: why rate_of_interest, Interest_rate_spread
# and Upfront_charges are excluded from the model
# =========================================================
# An initial model including rate_of_interest failed to converge
# (huge standard errors, AIC of 12 for 100k+ rows). This diagnostic
# check explains why:

table(df$Status, is.na(df$rate_of_interest))
table(df$Status, is.na(df$dtir1))

# Finding: rate_of_interest is missing for 0% of non-defaulters but
# ~99.5% of defaulters — missingness is essentially a perfect proxy
# for default status (likely because this field is only recorded once
# a loan is fully processed, which never happens for defaulted loans).
# This is data leakage, not a usable predictor, so it — along with
# Interest_rate_spread and Upfront_charges, which share the same
# missingness pattern — is excluded from the model below.
# dtir1 shows a milder version of the same pattern but is kept, since
# it's still meaningfully populated for both groups.


# =========================================================
# Baseline Logistic Regression
# =========================================================
log_model <- glm(Status ~ loan_amount + income + LTV + dtir1 + Credit_Score,
                 data = df, family = binomial)
summary(log_model)

# Result: converges properly (AIC = 108,651). Significant predictors
# (p < 0.001): loan_amount (-), income (-), LTV (+), dtir1 (+).
# Credit_Score is NOT significant (p = 0.137).
#
# Business implication: LTV and DTI ratio are the strongest drivers of
# default risk, followed by income and loan amount. Credit_Score shows
# no significant relationship with default in this dataset — consistent
# with its near-zero correlation with every other variable found during
# data exploration, suggesting a data quality issue with this field
# rather than a genuine absence of predictive value for credit scores
# in general.


# =========================================================
# Ridge & LASSO Regression (comparison against baseline)
# =========================================================
library(glmnet)

model_data <- df[, c("Status", "loan_amount", "income", "LTV", "dtir1", "Credit_Score")]
model_data <- na.omit(model_data)

x <- model.matrix(Status ~ loan_amount + income + LTV + dtir1 + Credit_Score,
                  data = model_data)[, -1]
y <- model_data$Status

ridge_model <- glmnet(x, y, family = "binomial", alpha = 0)
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)

cv_ridge <- cv.glmnet(x, y, family = "binomial", alpha = 0)
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)

best_ridge_lambda <- cv_ridge$lambda.min
best_lasso_lambda <- cv_lasso$lambda.min

coef(cv_ridge, s = best_ridge_lambda)
coef(cv_lasso, s = best_lasso_lambda)

# Result: Ridge and LASSO produce coefficients nearly identical to the
# baseline logistic regression for loan_amount, income, LTV and dtir1 —
# confirming the baseline model is stable, not overfitting. LASSO shrinks
# Credit_Score's coefficient by more than half (9.9e-05 -> 4.2e-05),
# independently confirming it as the weakest predictor.
#
# Business implication: since regularization barely changes the model,
# added complexity isn't earning its keep here — plain logistic
# regression is preferred for its interpretability and regulatory
# transparency, without sacrificing performance.


# =========================================================
# Model Evaluation: AUC & Confusion Matrix
# =========================================================
library(pROC)

predicted_probs <- predict(log_model, type = "response")

roc_obj <- roc(log_model$model$Status, predicted_probs)
auc(roc_obj)
plot(roc_obj, main = "ROC Curve - Logistic Regression")

predicted_class <- ifelse(predicted_probs > 0.5, 1, 0)
conf_matrix <- table(Predicted = predicted_class, Actual = log_model$model$Status)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy, 4), "\n")

# Result: AUC = 0.6136 (modest — only somewhat better than random).
# Accuracy = 83.68% looks strong but is misleading: the model predicts
# "default" only 7 times out of 124,437 rows, catching just 7 of 20,319
# actual defaulters (0.03% recall). This is a classic accuracy-paradox
# caused by class imbalance (~24.6% default rate).


# ---- Testing a lower threshold (0.3 instead of 0.5) ----
predicted_class_03 <- ifelse(predicted_probs > 0.3, 1, 0)
conf_matrix_03 <- table(Predicted = predicted_class_03, Actual = log_model$model$Status)
print(conf_matrix_03)

accuracy_03 <- sum(diag(conf_matrix_03)) / sum(conf_matrix_03)
cat("Accuracy at 0.3 threshold:", round(accuracy_03, 4), "\n")

recall_03 <- conf_matrix_03["1", "1"] / sum(conf_matrix_03[, "1"])
cat("Recall at 0.3 threshold:", round(recall_03, 4), "\n")

# Result: lowering the threshold to 0.3 barely improves recall (0.03%
# -> 0.94%) while accuracy stays flat (~83.7-83.8%).
#
# Business implication: the limitation isn't threshold choice, it's that
# these 5 predictors don't carry enough discriminative power (AUC=0.61)
# to reliably separate defaulters from non-defaulters. For production
# use, the bank would need additional variables (e.g. credit history
# depth, employment stability, loan-purpose-specific risk factors from
# Task 4) rather than relying on threshold tuning alone.


# =========================================================
# Checking Assumptions: Multicollinearity
# =========================================================
# Logistic regression assumes predictors are not highly correlated with
# each other. Checked using Variance Inflation Factor (VIF).
library(car)

vif(log_model)

# Result: all VIF values well below the problem threshold of 5
# (highest: income at 1.73; Credit_Score ~1.0). Confirms the
# no-multicollinearity assumption is satisfied — consistent with
# excluding rate_of_interest/Interest_rate_spread/Upfront_charges
# earlier, and with loan_amount/property_value (the most correlated
# pair overall, r=0.75) not both appearing in this model.


# =========================================================
# Final Model Recommendation
# =========================================================
# We recommend plain logistic regression as the baseline model for its
# interpretability and regulatory transparency. Ridge and LASSO produce
# nearly identical coefficients, confirming model stability rather than
# overfitting, so their added complexity isn't justified here. However,
# the model's discriminative power is modest (AUC = 0.61), and
# Credit_Score contributes no significant signal. We recommend the bank
# expand the feature set beyond these five variables before relying on
# this model for real credit decisions.