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

library(janitor)
library(patchwork)

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
  labs(title = "Age Distribution by Gender", x = "Age Group", y = "Count", fill = "Gender") + 
  theme_minimal()

prog_over_age_hist <- df %>% 
  count(age, academic_program) %>% 
  ggplot(aes(x = age, y = n, fill = academic_program)) + 
  geom_col(color = "black") + 
  labs(title = "Age Distribution by Academic Program", x = "Age Group", y = "Count", fill = "Program") + 
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

#################
print(dim(df))

program_split <- df %>% count(academic_program, sort = TRUE)
gender_split <- df %>% count(gender, sort = TRUE)

df %>% 
  count(age) %>%  # Keeps chronological/alphabetical order
  ggplot(aes(x = age, y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age Group", y = "Count")
