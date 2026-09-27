# 08_naive_bayes.R — Task 8: Critical Evaluation of Bayesian Methods

source("scripts/00_setup.R")
library(e1071)


# =========================================================
# Naive Bayes Model (comparison against Task 5's logistic regression)
# =========================================================
# Uses the same predictors and complete-case data as the Task 5 model,
# for a fair comparison.

nb_data <- df[, c("Status", "loan_amount", "income", "LTV", "dtir1", "Credit_Score")]
nb_data <- na.omit(nb_data)

nb_model <- naiveBayes(Status ~ loan_amount + income + LTV + dtir1 + Credit_Score,
                       data = nb_data)
print(nb_model)


# =========================================================
# Evaluation — same method as Task 5, for direct comparison
# =========================================================
nb_pred <- predict(nb_model, nb_data, type = "raw")[, "1"]
nb_class <- ifelse(nb_pred > 0.5, 1, 0)

conf_matrix_nb <- table(Predicted = nb_class, Actual = nb_data$Status)
print(conf_matrix_nb)

accuracy_nb <- sum(diag(conf_matrix_nb)) / sum(conf_matrix_nb)
cat("Naive Bayes Accuracy:", round(accuracy_nb, 4), "\n")

library(pROC)
roc_nb <- roc(nb_data$Status, nb_pred)
auc(roc_nb)

# Result: AUC = 0.676 (vs logistic regression's 0.614 from Task 5).
# Recall on defaulters = 613/20319 = 3.02% (vs logistic regression's
# 0.03%) — Naive Bayes catches roughly 100x more actual defaulters.
# Accuracy is nearly identical between the two models (83.19% vs
# 83.68%), reinforcing that accuracy alone is the wrong metric to
# judge either model on.
#
# Why Naive Bayes does better despite its independence assumption:
# Naive Bayes is documented to remain robust even when its independence
# assumption is technically violated, particularly when predictors show
# low multicollinearity — which is exactly what we found in Task 5's
# VIF check (all values below 1.8). Classification only requires the
# relative ranking between classes to be roughly correct, not perfectly
# calibrated probabilities, which is why the independence violation
# here doesn't hurt performance much.


# =========================================================
# Critical Discussion: Naive Bayes, Bayesian Regression,
# Bayesian Decision Making
# =========================================================

# Naive Bayes:
# Despite assuming full independence between predictors — an assumption
# not strictly true here — Naive Bayes outperformed logistic regression
# on both AUC and recall. This suggests it may be a genuinely useful
# complementary model for this problem, not merely a theoretical
# exercise, particularly as a lightweight, fast-to-train first-pass
# risk flag.

# Bayesian Regression:
# Unlike Naive Bayes' rigid independence assumption, Bayesian regression
# could incorporate prior banking knowledge — for example, treating
# "LTV above 90% is historically high-risk" as a prior belief — while
# still updating estimates from the bank's own data. This could improve
# estimates without needing a larger dataset, especially given the
# modest signal strength found in both models so far.

# Bayesian Decision Making:
# Given that both models struggle with recall (missing most actual
# defaulters), a Bayesian decision-theoretic framework could formally
# weigh the cost of a false negative (approving a defaulter) against a
# false positive (rejecting a good borrower), rather than relying on an
# arbitrary 0.5 probability cutoff — directly addressing the threshold
# limitation identified in Task 5.

# Overall recommendation:
# Naive Bayes' unexpectedly strong performance suggests the bank should
# consider it as a lightweight complement to logistic regression for an
# initial risk flag, while reserving logistic regression for decisions
# requiring full interpretability and regulatory scrutiny.