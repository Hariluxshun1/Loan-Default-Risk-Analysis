# 03_descriptive.R — Task 3: Dataset Understanding & Descriptive Analysis
# Owner: Person B
source("scripts/00_setup.R")

# ---- Missing value analysis ----
colSums(is.na(df))

# ---- Descriptive statistics ----
summary(df)

# ---- Visualizations (add more as you go) ----
ggplot(df, aes(x = income)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Income")

# TODO: outlier detection (boxplots), default rate by category, etc.
