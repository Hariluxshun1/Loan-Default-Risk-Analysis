# 07_pca.R — Task 7: Critical Evaluation of PCA
# Owner: Person B

source("scripts/00_setup.R")


# ---- Select numerical variables for PCA ----
# rate_of_interest and Upfront_charges are excluded
# because they were identified as data leakage variables
# in Task 5.

num_df <- df[, c(
  "loan_amount",
  "Interest_rate_spread",
  "term",
  "property_value",
  "income",
  "Credit_Score",
  "LTV",
  "dtir1"
)]


# ---- Remove rows with missing values ----
num_df <- na.omit(num_df)


# ---- Standardized PCA ----
pca_result <- prcomp(
  num_df,
  scale. = TRUE
)


# ---- PCA summary ----
summary(pca_result)


# ---- Scree plot ----
plot(
  pca_result,
  type = "l",
  main = "PCA Scree Plot"
)


# ---- PC1 and PC2 loadings ----
pca_result$rotation[, 1:2]


# ---- Correlation matrix ----
cor(num_df, use = "complete.obs")

