
# Task 3: Dataset Understanding & Descriptive Analysis
# Owner: Person B

# Set project working directory

# Set working directory
setwd("C:/Users/ASUS/Desktop/Loan-Default-Risk-Analysis-main (1)/Loan-Default-Risk-Analysis-main/repo")

# Load dataset
df <- read.csv(
  "data/Loan_Default.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# Check dataset
print(dim(df))
print(head(df))
print(names(df))


# Set working directory
setwd("C:/Users/ASUS/Desktop/Loan-Default-Risk-Analysis-main (1)/Loan-Default-Risk-Analysis-main/repo")

# Load dataset
df <- read.csv(
  "data/Loan_Default.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# Checkdataset

print(dim(df))
print(head(df))
print(names(df))


# Check dataset
file.exists("data/Loan_Default.csv")
library(ggplot2)
library(dplyr)
dir.create("outputs", showWarnings = FALSE)

# 1. Clean blank values
df[] <- lapply(df, function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA")] <- NA
    return(factor(x))
  }
  x
})

N <- nrow(df)
status <- as.character(df$Status)

# 2. Dataset and variable information
cat("Rows:", N, "\nColumns:", ncol(df), "\n")
variable_info <- data.frame(
  Variable = names(df),
  Type = sapply(df, function(x) class(x)[1]),
  row.names = NULL
)
print(variable_info)
cat("Duplicate rows:", sum(duplicated(df)), "\n")

# 3. Missing values
missing <- data.frame(
  Variable = names(df),
  Count = colSums(is.na(df)),
  Percentage = round(colMeans(is.na(df)) * 100, 2),
  row.names = NULL
)
print(missing)
write.csv(missing, "outputs/task3_missing.csv", row.names = FALSE)

m <- subset(missing, Count > 0)
if (nrow(m) > 0) {
  p <- ggplot(m, aes(reorder(Variable, Percentage), Percentage)) +
    geom_col() + coord_flip() +
    labs(title = "Missing Values by Variable",
         x = "Variable", y = "Missing (%)") + theme_minimal()
  print(p)
  ggsave("outputs/task3_missing.png", p)
}

# 4. Missingness by Status (possible leakage check)
for (v in intersect(c("rate_of_interest", "Interest_rate_spread",
                      "Upfront_charges", "dtir1"), names(df))) {
  cat("\nMissingness:", v, "\n")
  tab <- table(Status = status, Missing = is.na(df[[v]]),
               useNA = "ifany")
  print(tab)
  print(round(100 * prop.table(tab, 1), 2))
}

# 5. Descriptive statistics and default status
print(summary(df))
print(table(status, useNA = "ifany"))
valid <- !is.na(status)

if (any(valid)) {
  cat("Default rate (if Status 1 = default):",
      round(mean(status[valid] == "1") * 100, 2), "%\n")
}

# 6. Default rates by group
group_vars <- intersect(c("loan_purpose", "credit_type",
                          "occupancy_type", "age", "Region", "Gender"), names(df))

for (v in group_vars) {
  temp <- data.frame(Group = as.character(df[[v]]), Status = status)
  temp$Group[is.na(temp$Group)] <- "Missing"
  
  res <- temp %>% group_by(Group) %>%
    summarise(Total = n(), Known = sum(!is.na(Status)),
              Defaults = sum(Status == "1", na.rm = TRUE),
              Default_Rate = ifelse(Known > 0,
                                    round(100 * Defaults / Known, 2), NA_real_),
              .groups = "drop")
  
  names(res)[1] <- v
  print(res)
  write.csv(res, paste0("outputs/default_by_", v, ".csv"),
            row.names = FALSE)
}
# Gender note: "Joint" and "Sex Not Available" are not clean
# individual-sex categories. Discuss fairness and confounding.

# 7. Numeric summary
num_vars <- names(df)[sapply(df, is.numeric)]
key <- intersect(c("income", "loan_amount", "property_value",
                   "LTV", "dtir1", "Credit_Score", "term", "rate_of_interest",
                   "Interest_rate_spread", "Upfront_charges"), num_vars)

num_summary <- do.call(rbind, lapply(key, function(v) {
  x <- df[[v]]
  x <- x[is.finite(x)]
  data.frame(Variable = v, N = length(x),
             Missing = sum(is.na(df[[v]])),
             Mean = if (length(x)) mean(x) else NA_real_,
             Median = if (length(x)) median(x) else NA_real_,
             SD = if (length(x) > 1) sd(x) else NA_real_,
             IQR = if (length(x)) IQR(x) else NA_real_,
             Min = if (length(x)) min(x) else NA_real_,
             Max = if (length(x)) max(x) else NA_real_)
}))
print(num_summary)
write.csv(num_summary, "outputs/task3_numeric.csv", row.names = FALSE)

# 8. Potential outliers (IQR rule)
outliers <- do.call(rbind, lapply(num_vars, function(v) {
  x <- df[[v]]
  x <- x[is.finite(x)]
  if (length(x) < 2)
    return(data.frame(Variable = v, Outliers = 0, Percent = 0))
  q <- quantile(x, c(.25, .75))
  i <- q[2] - q[1]
  n <- sum(x < q[1] - 1.5*i | x > q[2] + 1.5*i)
  data.frame(Variable = v, Outliers = n,
             Percent = round(100*n/length(x), 2))
}))
outliers <- outliers[order(-outliers$Outliers), ]
print(outliers)
write.csv(outliers, "outputs/task3_outliers.csv", row.names = FALSE)

# 9. Histograms
for (v in intersect(c("income", "loan_amount"), num_vars)) {
  x <- df[[v]]
  p <- ggplot(df, aes(x = .data[[v]])) +
    geom_histogram(bins = 50, na.rm = TRUE) +
    labs(title = paste("Distribution of", v),
         subtitle = paste("Records:", sum(is.finite(x))),
         x = v, y = "Count") + theme_minimal()
  
  if (all(x[!is.na(x)] > 0) && any(!is.na(x)))
    p <- p + scale_x_log10()
  
  print(p)
  ggsave(paste0("outputs/hist_", v, ".png"), p)
}

# 10. Boxplots by Status
for (v in intersect(c("income", "loan_amount", "LTV",
                      "dtir1", "Credit_Score"), num_vars)) {
  p <- ggplot(df, aes(x = factor(Status), y = .data[[v]])) +
    geom_boxplot(na.rm = TRUE) +
    labs(title = paste(v, "by Status"),
         subtitle = paste("Records:", sum(
           !is.na(df$Status) & !is.na(df[[v]]))),
         x = "Status", y = v) + theme_minimal()
  
  x <- df[[v]]
  if (v %in% c("income", "loan_amount") &&
      all(x[!is.na(x)] > 0) && any(!is.na(x)))
    p <- p + scale_y_log10()
  
  print(p)
  ggsave(paste0("outputs/box_", v, ".png"), p)
}

# 11. Correlation matrix and heatmap
cor_vars <- key[sapply(key, function(v) {
  x <- df[[v]]
  x <- x[is.finite(x)]
  length(x) > 1 && sd(x) > 0
})]

if (length(cor_vars) >= 2) {
  cm <- cor(df[cor_vars], use = "pairwise.complete.obs")
  print(round(cm, 2))
  write.csv(round(cm, 3), "outputs/task3_correlation.csv")
  
  d <- as.data.frame(as.table(cm))
  p <- ggplot(d, aes(Var1, Var2, fill = Freq)) +
    geom_tile() + geom_text(aes(label = round(Freq, 2))) +
    scale_fill_gradient2(low = "blue", mid = "white",
                         high = "red", limits = c(-1, 1)) +
    labs(title = "Correlation Heatmap",
         x = NULL, y = NULL, fill = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  ggsave("outputs/task3_correlation_heatmap.png", p,
         width = 9, height = 7)
}

cat("\nTask 3 analysis completed. Check outputs/ folder.\n")

