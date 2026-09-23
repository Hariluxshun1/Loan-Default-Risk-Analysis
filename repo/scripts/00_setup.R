# 00_setup.R
# Run this first, every session, before working in your own script.
# Loads libraries and the cleaned dataset so everyone starts from the same base.

# ---- Libraries ----
# install.packages(c("dplyr","ggplot2","glmnet","pROC","e1071")) # run once if not installed
library(dplyr)
library(ggplot2)

# ---- Load raw data ----
df <- read.csv("data/Loan_Default.csv")

# ---- Basic shared cleaning (agree on this as a group before changing it) ----
df$ID <- NULL     # unique identifier, not useful for analysis
df$year <- NULL   # constant value (2019) across all rows, no variance

# age comes in as a text bracket like "25-34" -> treat as a factor
df$age <- as.factor(df$age)

# Status is the target variable: 1 = defaulted, 0 = did not default
df$Status <- as.factor(df$Status)

cat("Data loaded:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Default rate:", round(mean(df$Status == "1"), 4), "\n")
