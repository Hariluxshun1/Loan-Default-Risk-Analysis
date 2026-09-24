# 03_descriptive.R — Task 3: Dataset Understanding & Descriptive Analysis
# Owner: Person B

# Load shared setup
source("scripts/00_setup.R")

# ---- Data types ----
str(df)

# ---- Missing value analysis ----
colSums(is.na(df))

# ---- Descriptive statistics ----
summary(df)

# ---- Income distribution ----
ggplot(df, aes(x = income)) +
  geom_histogram(bins = 50, na.rm = TRUE) +
  labs(title = "Distribution of Income")

# ---- Loan amount distribution ----
ggplot(df, aes(x = loan_amount)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Loan Amount")