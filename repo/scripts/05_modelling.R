# 05_modelling.R — Task 5: Predictive Statistical Modelling
# Owner: Person C
source("scripts/00_setup.R")

# ---- Baseline logistic regression ----
log_model <- glm(Status ~ loan_amount + income + rate_of_interest + LTV + dtir1,
                  data = df, family = binomial)
summary(log_model)

# TODO: Ridge / LASSO comparison using glmnet
# TODO: model evaluation (confusion matrix, ROC/AUC via pROC)
