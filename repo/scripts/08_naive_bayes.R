# 08_naive_bayes.R — Task 8: Critical Evaluation of Bayesian Methods
# Owner: Person D
source("scripts/00_setup.R")
library(e1071)

nb_model <- naiveBayes(Status ~ Gender + loan_purpose + Credit_Worthiness + income + loan_amount,
                        data = df)

# TODO: evaluate + discuss independence assumption, compare briefly to logistic regression
