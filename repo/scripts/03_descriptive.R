# 03_descriptive.R — Task 3: Dataset Understanding & Descriptive Analysis
# Owner: Person B

source("scripts/00_setup.R")


# ---- Dataset description ----
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")
cat("Target variable: Status\n")


# ---- Variable descriptions / data types ----
variable_info <- data.frame(
  Variable = names(df),
  Type = sapply(df, class)
)
print(variable_info)


# ---- Data quality assessment ----
cat("Duplicate rows:", sum(duplicated(df)), "\n")


# ---- Missing value analysis ----
missing_values <- colSums(is.na(df))
print(missing_values)


# ---- Descriptive statistics ----
summary(df)


# ---- Default status ----
print(table(df$Status))
cat("Default rate:", mean(as.numeric(as.character(df$Status))) * 100, "%\n")


# ---- Outlier detection using IQR ----
num_vars <- names(df)[sapply(df, is.numeric)]


outlier_counts <- sapply(df[num_vars], function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  sum(x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr), na.rm = TRUE)
})

print(sort(outlier_counts, decreasing = TRUE))


# ---- Visualizations ----

# Income distribution
ggplot(df, aes(x = income)) +
  geom_histogram(bins = 50, na.rm = TRUE) +
  labs(title = "Distribution of Income")


# Loan amount distribution
ggplot(df, aes(x = loan_amount)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Loan Amount")

#Loan Default Status bar chart
ggplot(df, aes(x = Status)) +
  geom_bar() +
  labs(title = "Loan Default Status")


# Default status
ggplot(df, aes(x = Status)) +
  geom_bar() +
  labs(title = "Loan Default Status")