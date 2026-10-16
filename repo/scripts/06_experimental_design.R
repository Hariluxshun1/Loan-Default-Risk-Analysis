
# ============================================================
# IT3081 - STATISTICAL MODELLING
# TASK 6 - EXPERIMENTAL DESIGN: CRD AND RCBD
# Project: Loan Default Risk Analysis
# ============================================================

set.seed(42)

# ============================================================
# PART A - COMPLETELY RANDOMIZED DESIGN (CRD)
# ============================================================

cat("\n===== PART A: CRD =====\n")

# Three loan repayment reminder treatments
treatments <- c(
  "Standard_SMS",
  "Personalized_SMS",
  "SMS_Plus_Call"
)

# Simulated experiment with 90 borrowers
n_per_treatment <- 30

# Random assignment to treatments
crd_treatment <- sample(
  rep(treatments, each = n_per_treatment)
)

# Simulated percentage of payments made on time
# These treatment means are hypothetical assumptions.
treatment_means <- c(
  Standard_SMS = 70,
  Personalized_SMS = 76,
  SMS_Plus_Call = 83
)

crd_data <- data.frame(
  Treatment = factor(crd_treatment, levels = treatments)
)

crd_data$OnTimePayment <- pmin(
  100,
  pmax(
    0,
    rnorm(
      nrow(crd_data),
      mean = treatment_means[
        as.character(crd_data$Treatment)
      ],
      sd = 12
    )
  )
)

cat("\nCRD descriptive statistics:\n")

print(
  aggregate(
    OnTimePayment ~ Treatment,
    data = crd_data,
    FUN = mean
  )
)

# One-way ANOVA
crd_model <- aov(
  OnTimePayment ~ Treatment,
  data = crd_data
)

cat("\nCRD ANOVA results:\n")
print(summary(crd_model))

cat("\nCRD Tukey pairwise comparisons:\n")
print(TukeyHSD(crd_model))

# ============================================================
# PART B - RANDOMIZED COMPLETE BLOCK DESIGN (RCBD)
# ============================================================

cat("\n===== PART B: RCBD =====\n")

# Pre-treatment repayment-risk groups:
# Low, Medium, High.
#
# This demonstration uses 30 blocks, with
# 3 borrowers per block (one per treatment).
#
# Within each block, treatment assignment is randomized.
# This represents a matched-group blocked experiment,
# not repeated treatments on one borrower.

n_blocks <- 30

block_risk <- rep(
  c("Low", "Medium", "High"),
  each = 10
)

rcbd_data <- data.frame(
  Block = factor(rep(seq_len(n_blocks), each = 3)),
  RiskGroup = factor(
    rep(block_risk, each = 3),
    levels = c("Low", "Medium", "High")
  )
)

# Randomize the three treatments within every block
rcbd_data$Treatment <- factor(
  unlist(
    lapply(
      seq_len(n_blocks),
      function(i) sample(treatments)
    )
  ),
  levels = treatments
)

# Simulated block-level baseline payment rates
risk_baseline <- c(
  Low = 82,
  Medium = 72,
  High = 61
)

# Treatment effects relative to standard SMS
treatment_effect <- c(
  Standard_SMS = 0,
  Personalized_SMS = 6,
  SMS_Plus_Call = 13
)

# Shared block-level variations
block_effect <- rnorm(n_blocks, mean = 0, sd = 6)

# Generate hypothetical payment percentages
rcbd_data$OnTimePayment <- pmin(
  100,
  pmax(
    0,
    risk_baseline[
      as.character(rcbd_data$RiskGroup)
    ] +
      treatment_effect[
        as.character(rcbd_data$Treatment)
      ] +
      block_effect[as.integer(rcbd_data$Block)] +
      rnorm(nrow(rcbd_data), mean = 0, sd = 7)
  )
)

cat("\nRCBD treatment means:\n")

print(
  aggregate(
    OnTimePayment ~ Treatment,
    data = rcbd_data,
    FUN = mean
  )
)

# RCBD ANOVA: Treatment + Block
rcbd_model <- aov(
  OnTimePayment ~ Treatment + Block,
  data = rcbd_data
)

cat("\nRCBD ANOVA results:\n")
print(summary(rcbd_model))

cat("\nRCBD Tukey pairwise comparisons:\n")
print(TukeyHSD(rcbd_model, which = "Treatment"))

# ============================================================
# PART C - VISUALIZATIONS
# ============================================================

dir.create(
  "results/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

png(
  "results/figures/task6_crd_boxplot.png",
  width = 1400,
  height = 900,
  res = 160
)

boxplot(
  OnTimePayment ~ Treatment,
  data = crd_data,
  col = c("lightblue", "lightgreen", "lightpink"),
  main = "Simulated CRD: Repayment Reminder Treatments",
  ylab = "On-Time Payment Percentage",
  xlab = "Reminder Treatment",
  ylim = c(0, 100)
)

dev.off()

png(
  "results/figures/task6_rcbd_boxplot.png",
  width = 1400,
  height = 900,
  res = 160
)

boxplot(
  OnTimePayment ~ Treatment,
  data = rcbd_data,
  col = c("lightblue", "lightgreen", "lightpink"),
  main = "Simulated RCBD: Repayment Reminder Treatments",
  ylab = "On-Time Payment Percentage",
  xlab = "Reminder Treatment",
  ylim = c(0, 100)
)

dev.off()

# ============================================================
# PART D - DESIGN COMPARISON
# ============================================================

cat("\n===== DESIGN COMPARISON =====\n")

cat("\nCRD: Borrowers are randomized without blocking.\n")
cat("RCBD: Treatment is randomized within matched blocks.\n")
cat("Blocking can reduce residual variation when\n")
cat("borrowers in the same block are similar.\n")

cat("\nCRD residual mean square:\n")
print(summary(crd_model)[[1]]["Residuals", "Mean Sq"])

cat("\nRCBD residual mean square:\n")
print(summary(rcbd_model)[[1]]["Residuals", "Mean Sq"])

cat("\nIMPORTANT:\n")
cat("All outcomes in this script are simulated.\n")
cat("These are demonstrations, not observed bank results.\n")
cat("The CRD and RCBD simulations are separate experiments.\n")
cat("Their residual mean squares are descriptive, not\n")
cat("a controlled estimate of RCBD efficiency gain.\n")

# ============================================================
# PART E - SAVE RESULTS
# ============================================================

dir.create("results", showWarnings = FALSE)

write.csv(
  crd_data,
  "results/task6_crd_simulated.csv",
  row.names = FALSE
)

write.csv(
  rcbd_data,
  "results/task6_rcbd_simulated.csv",
  row.names = FALSE
)

cat("\nTASK 6 SIMULATION COMPLETED.\n")
cat("Saved two datasets and two boxplots.\n")
