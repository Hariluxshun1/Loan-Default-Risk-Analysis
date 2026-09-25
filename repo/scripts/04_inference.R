source("scripts/00_setup.R")

# ---- Test 1: Comparison of Means — Income by Default Status ----
# H0: mean income is equal between defaulters and non-defaulters
# H1: mean income differs between defaulters and non-defaulters

t_test_income <- t.test(income ~ Status, data = df)
print(t_test_income)

gender_table <- table(df$Gender, df$Status)
print(gender_table)

prop_test_gender <- prop.test(gender_table)
print(prop_test_gender)


#Anova test
anova_model <- aov(loan_amount ~ loan_purpose, data = df)
summary(anova_model)


aggregate(loan_amount ~ loan_purpose, data = df, mean)
TukeyHSD(anova_model)


#Variance
var.test(loan_amount ~ Status, data = df)
