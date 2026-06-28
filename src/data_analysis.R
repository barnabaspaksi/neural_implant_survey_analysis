#setwd("C:/Users/YOUR_USER/YOUR_PATH/")
library(tidyverse)
library(stringr)
library(ggplot2)
library(dplyr)

if (!requireNamespace("janitor", quietly = TRUE)) {
  install.packages("janitor")
}

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
if (!requireNamespace("survey", quietly = TRUE)) {
  install.packages("survey")
}
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

library(janitor)
library(patchwork)
library(car)

load_orig_dataset <- function(){
  
  all_results = read.csv("Survey_results_2026.csv")
  dim(all_results)
  colnames(all_results)
  
  return (all_results)
}

create_g13_subset <- function(all_results){
  
  df = all_results %>% select(starts_with("DQ") | starts_with("G13"))
  colnames(df) = c("age", "gender", "academic_program", "Q1", "Q2", "Q3")
  
  return(df)
}


show_rundown <- function(df){
  dim(df)
  str(df)
  head(df)
}

all_results <- load_orig_dataset()
df <- create_g13_subset(all_results)
show_rundown(df)

### General Exploration before Question-specific Analysis
df %>% 
  tabyl(academic_program, gender) %>% 
  adorn_totals(where = c("row", "col"))

gender_over_age_hist <- df %>% 
  count(age, gender) %>% 
  ggplot(aes(x = age, y = n, fill = gender)) + 
  geom_col(color = "black") + 
  labs(title = "Age Distribution by Gender", 
       x = "Age Group", y = "Count", fill = "Gender") + 
  theme_minimal()

prog_over_age_hist <- df %>% 
  count(age, academic_program) %>% 
  ggplot(aes(x = age, y = n, fill = academic_program)) + 
  geom_col(color = "black") + 
  labs(title = "Age Distribution by Academic Program", 
       x = "Age Group", y = "Count", fill = "Program") + 
  theme_minimal()

# Displaying distribution of gender and program over age groups
gender_over_age_hist + prog_over_age_hist

# We observe that Data Science students span a wider range of age categories 
# compared to Business Informatics, where no students from the age groups 18-20 
# or over 35 participated in the survey. Data Science has a larger share of 
# students with 177 compared to 32 for Business Informatics.
# Furthermore, the distribution of female students has a different shape
# compared to the overall distribution, as the size of the 21-23 age group
# exceeds that of the 23-25 group for females. As male students, 
# accounting for 156 out of 209 observations of the sample, appear more in 
# 23-25 compared to 21-23, they dominate the overall shape too.

### Q1 ###
# After 10 years of successful safety testing, on a scale of 1 to 5 (1 = Strongly Disagree, 5 = Strongly Agree), would you undergo a surgical procedure to implant a cognitive-enhancement chip in your brain? #
table(df$Q1)
round(prop.table(table(df$Q1)), 4)
barplot(prop.table(table(df$Q1)), ylim = c(0,0.4))


### Q2 ###
# After 10 years of successful safety testing, what ratio of the general population (0-100%) do you believe would choose to get the implant? #
table(df$Q2)
round(prop.table(table(df$Q2))*100, 2)
hist(df$Q2, ylim = c(0, 60))
hist(df$Q2,  freq = FALSE, ylim = c(0, 0.03))


### Comparison Q1 and Q2 ###
mean(df$Q2)
median(df$Q2)

cs = cumsum(table(df$Q1)[5:1])
round(prop.table(cs)*100,2)
# --> While people estimated that around 30% of people would get the implant, 
# only 7.31% of people said that they would strongly agree or agree to get the implant.
# If we count the neutral people as well, it would still only be 17.85%


### Q3 ###
# Which of the following do you perceive as the most immediate risks of neural-link technology? (Select up to 3) #

#--> A lot of people did not listen to the question and selected more than 3,
# but the analysis stays the same regardless, so it does not matter too much.

# Count how many values each person chose
ch1 = ";"
print("Count for character ;")
count = str_count(df$Q3,ch1)
count
table(count)

df_long <- df %>%
  separate_rows(Q3, sep = ";")
df_long %>% count(Q3, sort = TRUE) %>%
  mutate(percent = (n / nrow(df_long))*100)

barplot(table(df_long$Q3), las = 2)

####################
library(survey)

base_design <- svydesign(ids = ~1, data = df)

# 2. OPTIONAL: If you want to fix the 177 vs 32 program imbalance 
# structure your known population totals (e.g., assuming a 50/50 true split)
pop_program <- data.frame(academic_program = c("Data Science", "Business Informatics"), 
                          Freq = c(105.5, 105.5)) # Total 209 split evenly

weighted_design <- rake(design = base_design,
                        sample.margins = list(~academic_program),
                        population.margins = list(pop_program))

df$Q1_ordered <- factor(df$Q1, levels = 1:5, ordered = TRUE)
df$academic_program <- as.factor(df$academic_program)
df$gender <- as.factor(df$gender)

# Re-build design with the updated dataframe
final_design <- svydesign(ids = ~1, data = df)

# Run the Ordinal Logistic Regression with an interaction term
# This tests if the effect of the program changes based on gender
q1_ordinal_model <- svyolr(Q1_ordered ~ academic_program * gender, 
                           design = final_design)

summary(q1_ordinal_model)

# Fit the Factorial Model matching your exact columns
factorial_model <- lm(Q1 ~ academic_program * gender, data = df)

# Look at the main effects and the crucial interaction effect
anova(factorial_model)

# Model Validation (Crucial due to the 177 vs 32 imbalance)
# This checks if the uneven group sizes break the variance assumptions


car::leveneTest(Q1 ~ academic_program * gender, data = df)

# homoskedasticity assumption does not hold: lm and anova above are not 
# reliable

# Runs a one-way ANOVA alternative that does not assume equal variances
oneway.test(Q1 ~ interaction(academic_program, gender), data = df, var.equal = FALSE)
# The Welch-corrected test is reliable even without homoskedasticity. It tells 
# us that there is some significant difference between groups. As it contradicts
# the previous svyolr, we need to discuss why. It is because that method takes 
# a single baseline, while this one checks everything. We can use another test 
# to find out where the significant rift was.

pairwise.t.test(df$Q1, interaction(df$academic_program, df$gender), 
                p.adjust.method = "holm", 
                pool.sd = FALSE) # FALSE forces it to use Welch's correction

# The test shows that the relevant divide is between female and male students 
# studying DS, as that is the only p-value under 0.05. Its value is 0.03. 
# Interestingly, the same does not hold between BI females and DS males. We 
# suspect that the main driver of this difference is the sample size. 
# After the correction, the two female groups across programs are not 
# statistically different either, which seems paradoxical after the previous insights.

######### DUMPING MY STUFF (cleaned up code based on insights in the repeated parts but the cleaned up part does not prove all statements like heteroskedasticity)

library(survey)
library(robustbase)
library(car) # Keep car only for Anova(risk_model, test.statistic = "Chisq") at the end
library(stringr)
library(purrr)

# ==============================================================================
# SECTION 1: Define the Survey Design & Weights
# ==============================================================================

# 1. Base design (Equal probability)
base_design <- svydesign(ids = ~1, data = df)

# 2. Rake to adjust for academic program imbalance (50/50 target split)
pop_program <- data.frame(academic_program = c("Data Science", "Business Informatics"), 
                          Freq = c(105.5, 105.5))

weighted_design <- rake(design = base_design,
                        sample.margins = list(~academic_program),
                        population.margins = list(pop_program))

# 3. Clean and prep variables safely within the weighted design framework
weighted_design <- update(weighted_design,
                          Q1_ordered = factor(Q1, levels = 1:5, ordered = TRUE),
                          gender = as.factor(gender),
                          academic_program = as.factor(academic_program))

# ==============================================================================
# SECTION 2: Initial Multi-Factor Analysis (The "Insignificant" Models)
# ==============================================================================

# Fit a survey-weighted generalized linear model for Q1
# NOTE: This uses robust sandwich standard errors, absorbing heteroskedasticity issues!
q1_glmodel <- svyglm(Q1 ~ academic_program * gender, design = weighted_design)
summary(q1_glmodel)

# Run the Ordinal Logistic Regression on the proper design
q1_ordinal_model <- svyolr(Q1_ordered ~ academic_program * gender, design = weighted_design)
summary(q1_ordinal_model)

# Narrative checkpoint: The models above mask deep trends because they evaluate 
# everything against the default baseline (Business Informatics Females).

# ==============================================================================
# SECTION 3: Uncovering the Hidden Rift (Targeted Survey Insights)
# ==============================================================================

# 1. Extract valid, weighted means for each subgroup combo to visually spot the gap
subgroup_means <- svyby(~Q1, 
                        by = ~academic_program + gender, 
                        design = weighted_design, 
                        svymean)
print(subgroup_means)

# 2. Test the specific hypothesis directly: DS Male - DS Female = 0
# This bypasses the baseline masking effect and yields your true significant p-value.
svycontrast(q1_glmodel, contrasts = list("DS_Male_vs_DS_Female" = c(0, 0, 1, 1)))


# ==============================================================================
# SECTION 4: Risk Factor Analysis (Q3 Predictors on Q1)
# ==============================================================================

# 1. Dynamically extract every unique risk factor present in Q3
all_risks <- df$Q3 %>%
  str_split(";") %>%
  flatten_chr() %>%
  unique() %>%
  discard(~ .x == "" | is.na(.x)) # Drop empty/missing entries

# 2. Build the dummy matrix dynamically for ALL found options
q3_dummy_list <- map(all_risks, function(risk) {
  # Flag presence as 1 or 0
  as.numeric(str_detect(df$Q3, fixed(risk)))
})

# 3. Combine into a clean dataframe and fix column names for formulas
q3_dummy <- as.data.frame(q3_dummy_list)
colnames(q3_dummy) <- make.names(all_risks)

# 4. (Optional but recommended) Convert these columns to explicit 0/1 factors
q3_dummy <- as.data.frame(lapply(q3_dummy, factor, levels = c(0, 1)))

# Re-align your data structure for behavioral predictor modeling
regression_data <- cbind(df %>% select(Q1, academic_program, gender), q3_dummy)
colnames(regression_data) <- make.names(colnames(regression_data))

# Run a Robust Linear Model (lmrob) to evaluate risk text selection patterns
regression_data <- cbind(df %>% select(Q1, gender), q3_dummy)

# 2. Get the clean column names of all your risk variables 
# (Excluding Q1 and gender)
risk_variables <- colnames(q3_dummy)

# 3. Build the formula string programmatically: Q1 ~ Var1 + Var2 + ... + gender
formula_string <- paste("Q1 ~", paste(risk_variables, collapse = " + "), "+ gender")
risk_formula <- as.formula(formula_string)

# Print the formula to verify it caught everything
print(risk_formula)

# 4. Run your Robust Linear Model with the complete formula
risk_model <- lmrob(risk_formula, data = regression_data)
summary(risk_model)

# Test Type-II hierarchical significance for your independent predictors
Anova(risk_model, test.statistic = "Chisq")

#################################################
library(survey)

base_design <- svydesign(ids = ~1, data = df)

# 2. OPTIONAL: If you want to fix the 177 vs 32 program imbalance 
# structure your known population totals (e.g., assuming a 50/50 true split)
pop_program <- data.frame(academic_program = c("Data Science", "Business Informatics"), 
                          Freq = c(105.5, 105.5)) # Total 209 split evenly

weighted_design <- rake(design = base_design,
                        sample.margins = list(~academic_program),
                        population.margins = list(pop_program))

df$Q1_ordered <- factor(df$Q1, levels = 1:5, ordered = TRUE)
df$academic_program <- as.factor(df$academic_program)
df$gender <- as.factor(df$gender)

# Re-build design with the updated dataframe
final_design <- svydesign(ids = ~1, data = df)

# Run the Ordinal Logistic Regression with an interaction term
# This tests if the effect of the program changes based on gender
q1_ordinal_model <- svyolr(Q1_ordered ~ academic_program * gender, 
                           design = final_design)

summary(q1_ordinal_model)

# Fit the Factorial Model matching your exact columns
factorial_model <- lm(Q1 ~ academic_program * gender, data = df)

# Look at the main effects and the crucial interaction effect
anova(factorial_model)

# Model Validation (Crucial due to the 177 vs 32 imbalance)
# This checks if the uneven group sizes break the variance assumptions


car::leveneTest(Q1 ~ academic_program * gender, data = df)

# homoskedasticity assumption does not hold: lm and anova above are not 
# reliable

# Runs a one-way ANOVA alternative that does not assume equal variances
oneway.test(Q1 ~ interaction(academic_program, gender), data = df, var.equal = FALSE)
# The Welch-corrected test is reliable even without homoskedasticity. It tells 
# us that there is some significant difference between groups. As it contradicts
# the previous svyolr, we need to discuss why. It is because that method takes 
# a single baseline, while this one checks everything. We can use another test 
# to find out where the significant rift was.

pairwise.t.test(df$Q1, interaction(df$academic_program, df$gender), 
                p.adjust.method = "holm", 
                pool.sd = FALSE) # FALSE forces it to use Welch's correction

# The test shows that the relevant divide is between female and male students 
# studying DS, as that is the only p-value under 0.05. Its value is 0.03. 
# Interestingly, the same does not hold between BI females and DS males. We 
# suspect that the main driver of this difference is the sample size. 
# After the correction, the two female groups across programs are not 
# statistically different either, which seems paradoxical after the previous insights. 

library(WRS2) # For robust ANOVA under heteroskedasticity

# 1. Prepare Factor Variables
df_clean <- df %>%
  mutate(
    academic_program = as.factor(academic_program),
    gender = as.factor(gender),
    Q1 = as.numeric(Q1)
  )

# 2. Robust 2-Way Factorial ANOVA (Trimming method - reliable when Levene's Test fails)
# This tests main effects and interactions without assuming homoskedasticity
robust_model <- t2way(Q1 ~ academic_program * gender, data = df_clean)
print(robust_model)

# 3. Non-parametric Multi-group Test (Kruskal-Wallis)
# Grouping by the interaction combination as suggested by your DataCamp curriculum
df_clean <- df_clean %>% 
  mutate(group_comb = interaction(academic_program, gender))

kruskal.test(Q1 ~ group_comb, data = df_clean)

#################
install.packages("robustbase")
library(robustbase) # For robust regression fitting

# 1. Re-align our data structure cleanly
regression_data <- cbind(df_clean %>% select(Q1, academic_program, gender), q3_dummy)

# Clean names for formulas
colnames(regression_data) <- make.names(colnames(regression_data))

# 2. Run a Robust Linear Model (lmrob) 
# This tests which specific risks actually predict lower or higher Q1 scores,
# while ignoring the problematic structural correlation of the checklist format.
risk_model <- lmrob(Q1 ~ Loss.of.personality + 
                      Social.media.addiction + 
                      Adverse.physical.health.effects + 
                      Data.collection.by.big.tech.companies +
                      gender, 
                    data = regression_data)

summary(risk_model)

library(car)

# Run a Wald test on the robust model to check for joint significance
# Type-II (testing hierarchical/conditional significance) Wald test
# measures the clean, independent contribution of gender and our Q3
# risk metrics without getting bogged down by any hidden multi-collinearity
Anova(risk_model, test.statistic = "Chisq")

#### Focusing on Survey package

# 1. Base design (Equal probability)
base_design <- svydesign(ids = ~1, data = df)

# 2. Rake to adjust for academic program imbalance
pop_program <- data.frame(academic_program = c("Data Science", "Business Informatics"), 
                          Freq = c(105.5, 105.5))

weighted_design <- rake(design = base_design,
                        sample.margins = list(~academic_program),
                        population.margins = list(pop_program))

# Fit a survey-weighted generalized linear model for Q1
q1_glmodel <- svyglm(Q1 ~ academic_program * gender, design = weighted_design)
summary(q1_glmodel)

# Safely update variables inside the weighted design
weighted_design <- update(weighted_design,
                          Q1_ordered = factor(Q1, levels = 1:5, ordered = TRUE),
                          gender = as.factor(gender),
                          academic_program = as.factor(academic_program))

# Run the Ordinal Logistic Regression on the proper design
q1_ordinal_model <- svyolr(Q1_ordered ~ academic_program * gender, design = weighted_design)
summary(q1_ordinal_model)

# Above: insignificant results due to BI Female baseline!! 

# Calculate the valid, weighted means for each subgroup combo
subgroup_means <- svyby(~Q1, 
                        by = ~academic_program + gender, 
                        design = weighted_design, 
                        svymean)
print(subgroup_means)

# Test the hypothesis: DS Male - DS Female = 0
# This extracts the exact contrast from your robust survey model
svycontrast(q1_glmodel, contrasts = list("DS_Male_vs_DS_Female" = c(0, 0, 1, 1)))

