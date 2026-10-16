
# 07_pca.R - Task 7: Critical Evaluation of PCA

source("scripts/00_setup.R")

library(ggplot2)
dir.create("outputs", showWarnings = FALSE)

# 1. Check required variables
pca_vars <- c(
  "loan_amount", "term", "property_value", "income",
  "Credit_Score", "LTV", "dtir1"
)

if (!all(pca_vars %in% names(df))) {
  stop("Some PCA variables are missing from the dataset.")
}

if (!all(sapply(df[pca_vars], is.numeric))) {
  stop("All PCA variables must be numeric.")
}

# 2. Missingness check
for (v in intersect(
  c("rate_of_interest", "Interest_rate_spread",
    "Upfront_charges", "dtir1"), names(df)
)) {
  cat("\nMissing values in", v, "by Status:\n")
  print(table(df$Status, is.na(df[[v]])))
}

# 3. Prepare PCA data
pca_data <- df[, pca_vars, drop = FALSE]

valid <- complete.cases(pca_data) &
  apply(pca_data, 1, function(x) all(is.finite(x)))

pca_data <- pca_data[valid, , drop = FALSE]

cat("\nOriginal rows:", nrow(df))
cat("\nRows retained:", nrow(pca_data))
cat("\nRows removed:", sum(!valid))
cat("\nRetention (%):", round(mean(valid) * 100, 2), "\n")

if (nrow(pca_data) < 2)
  stop("Not enough complete rows for PCA.")

if (any(sapply(pca_data, sd) == 0))
  stop("A PCA variable has zero variance.")

# 4. Correlation matrix
cm <- cor(pca_data)
print(round(cm, 3))
write.csv(cm, "outputs/pca_correlation.csv")

# 5. Perform standardized PCA
pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)

eig <- pca$sdev^2
variance <- eig / sum(eig) * 100
cumulative <- cumsum(variance)

pca_table <- data.frame(
  PC = paste0("PC", seq_along(eig)),
  Eigenvalue = round(eig, 4),
  Explained_Percent = round(variance, 2),
  Cumulative_Percent = round(cumulative, 2)
)

print(pca_table)
cat("\nComponents with eigenvalue > 1:", sum(eig > 1), "\n")
cat("Components for 80% variance:",
    which(cumulative >= 80)[1], "\n")

write.csv(pca_table, "outputs/pca_variance_table.csv",
          row.names = FALSE)
write.csv(round(pca$rotation, 4),
          "outputs/pca_loadings.csv")
write.csv(as.data.frame(pca$x),
          "outputs/pca_scores.csv", row.names = FALSE)

# 6. Scree plot
png("outputs/pca_scree_plot.png", width = 900, height = 600)
plot(variance, type = "b", pch = 19,
     xlab = "Principal Component",
     ylab = "Variance Explained (%)",
     main = "PCA Scree Plot")
dev.off()

# 7. Cumulative variance plot
png("outputs/pca_cumulative_variance.png",
    width = 900, height = 600)
plot(cumulative, type = "b", pch = 19,
     ylim = c(0, 100),
     xlab = "Principal Component",
     ylab = "Cumulative Variance (%)",
     main = "PCA Cumulative Variance")
abline(h = 80, lty = 2)
dev.off()

cat("\nPC1 and PC2 loadings:\n")
print(round(pca$rotation[, 1:2, drop = FALSE], 3))

cat("\nPCA analysis completed. Check outputs/ folder.\n")
