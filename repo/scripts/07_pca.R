# 07_pca.R — Task 7: Critical Evaluation of PCA
# Owner: Person B
source("scripts/00_setup.R")

num_df <- df[, c("loan_amount","rate_of_interest","Interest_rate_spread",
                  "Upfront_charges","term","property_value","income",
                  "Credit_Score","LTV","dtir1")]

num_df <- na.omit(num_df)

pca_result <- prcomp(num_df, scale. = TRUE)
summary(pca_result)

# TODO: scree plot, discuss whether PCA is worth using given
# known multicollinearity (loan_amount~property_value etc.)



plot(pca_result, type = "l")
