#Packages
library(tidyverse)
library(scales)
library(stringi)
library(gmodels)

#Setting chart defaults
old_theme <- theme_set(theme_linedraw() +
                         theme(plot.title = element_text(size = 16, face = "bold")) +
                         theme(axis.title = element_text(size = 14,)) +
                         theme(axis.text = element_text(size = 14,)) 
) 

#Read the data
cleaned_accepted <- read.csv("./data/cleaned_accepted.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
#colnames(cleaned_accepted)

#Loans per year
cleaned_accepted$issue_y <- as.integer(stri_sub(cleaned_accepted$issue_d,-4,-1))
Loans_amnt_Year <- ggplot(data = cleaned_accepted, aes(x = issue_y, y= loan_amnt)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_number(suffix = " bn", scale = 1e-9)) +
  labs(title="Loan amounts per year", x ="Year", y = "Loan amount ($)") 
Loans_amnt_Year

Loans_num_Year <- ggplot(data = cleaned_accepted, aes(x = issue_y)) + 
  geom_bar() +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  labs(title="Number of loans per year", x ="Year", y = "Number of loans") 
Loans_num_Year

#Purposes
grade_grouped <- group_by(cleaned_accepted, purpose, grade) %>%
  mutate(., "total_funded" = sum(loan_amnt))
Loans_amnt_PurpGrade <- ggplot(data = grade_grouped, aes(x=))

CrossTable(x=cleaned_accepted$purpose, y=cleaned_accepted$grade, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)

 
#Distribution of loan amounts
Density_Amounts <- ggplot(data = cleaned_accepted, aes(x = loan_amnt)) +
  geom_histogram(breaks = seq(0,40000, by = 5000)) +
  scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  labs(title="Distribution of loan amounts applied", x ="Loan amount ($)", y = "Density") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Density_Amounts

Density_Funded_Amounts <- ggplot(data = cleaned_accepted, aes(x = funded_amnt)) +
  geom_histogram(breaks = seq(0,40000, by = 5000)) +
  scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  labs(title="Distribution of loan amounts funded", x ="Loan amount ($)", y = "Density") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Density_Funded_Amounts

