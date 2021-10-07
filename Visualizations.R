#Packages
library(tidyverse)
library(scales)
library(stringi)
library(gmodels)
library(RColorBrewer)

#Setting chart defaults
old_theme <- theme_set(theme_linedraw() +
                         theme(plot.title = element_text(size = 16, face = "bold")) +
                         theme(axis.title = element_text(size = 14,)) +
                         theme(axis.text = element_text(size = 14,)) 
) 

#Read the data
cleaned_accepted <- read.csv("./data/cleaned_accepted.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
#colnames(cleaned_accepted)

#Clean term durations
cleaned_accepted$term <- as.integer(as.integer(stri_sub(cleaned_accepted$term,1,3)) / 12)

#Clean dates
cleaned_accepted$issue_date <- as.Date(paste0(as.character(cleaned_accepted$issue_d), "-01"), "%b-%Y-%d")


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
Loans_amnt_PurpGrade <- group_by(cleaned_accepted, purpose, grade) %>%
  summarize(., "total_funded" = sum(loan_amnt)) %>%
  mutate(., "prop_funded" = total_funded / sum(total_funded)) %>%
  ggplot(data = ., aes(x = grade, y = prop_funded, group = purpose, colour = purpose)) +
  geom_line() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  labs(title="Grade distribution of loans per purpose", x ="Grade", y = "Proportion of loans within grade")
Loans_amnt_PurpGrade

CrossTable(x=cleaned_accepted$purpose, y=cleaned_accepted$grade, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)

cleaned_accepted$issue_y <- as.factor(stri_sub(cleaned_accepted$issue_d,-4,-1))
Loans_amnt_YearGrade <- group_by(cleaned_accepted, issue_y, grade) %>%
  summarize(., "total_funded" = sum(loan_amnt)) %>%
  mutate(., "prop_funded" = total_funded / sum(total_funded)) %>%
  ggplot(data = ., aes(x = grade, y = prop_funded, group = issue_y, colour = issue_y)) +
  geom_line() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  labs(title="Grade distribution of loans per year", x ="Grade", y = "Proportion of loans within grade") + 
  scale_color_brewer(palette = "RdBu")
Loans_amnt_YearGrade

CrossTable(x=cleaned_accepted$issue_y, y=cleaned_accepted$grade, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)

 
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

Density_Funded_Amounts_Grade <- ggplot(data = cleaned_accepted, aes(x = funded_amnt, group = grade, colour = grade)) +
  geom_step(stat = "ecdf") +
  scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  labs(title="Cumulative distribution of loan amounts funded by grade", x ="Loan amount ($)", y = "Density") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_brewer(palette = "Blues")
Density_Funded_Amounts_Grade

Density_Funded_Amounts_Purpose <- ggplot(data = cleaned_accepted, aes(x = funded_amnt, group = purpose, colour = purpose)) +
  geom_step(stat = "ecdf") +
  scale_x_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  labs(title="Cumulative distribution of loan amounts funded by purpose", x ="Loan amount ($)", y = "Density") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_brewer(palette = "Set3")
Density_Funded_Amounts_Purpose

kruskal.test(funded_amnt ~ grade, data = cleaned_accepted)
kruskal.test(funded_amnt ~ purpose, data = cleaned_accepted)
#Groups have significantly different distributions

#Loans paid in full
subset_mature <- filter(cleaned_accepted, (term == 3 & issue_y < 2015) | (term == 5 & issue_y < 2013)) %>%
  filter(., (loan_status == "Fully Paid") | (loan_status == "Charged Off"))
#Note, filtering out the loans not adhering to the policy makes a strong bias, but information is lacking

subset_mature$issue_y <- as.integer(stri_sub(subset_mature$issue_d,-4,-1))
CrossTable(x=subset_mature$term, y=subset_mature$loan_status, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)
#Yes, longer term loans have a much higher probability of being charged off


#Interest rate analysis
cleaned_accepted$term <- as.factor(cleaned_accepted$term)
Rates_term <- ggplot(data = cleaned_accepted, aes(x = term, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates by term", x ="Term (year)", y = "Interest rate (%)")
Rates_term

Rates_grade <- ggplot(data = cleaned_accepted, aes(x = grade, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates by grade", x ="Grade", y = "Interest rate (%)")
Rates_grade

Rates_subgrade <- ggplot(data = cleaned_accepted, aes(x = sub_grade, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates by subgrade", x ="Subgrade", y = "Interest rate (%)")
Rates_subgrade

cleaned_accepted$issue_y <- as.factor(stri_sub(cleaned_accepted$issue_d,-4,-1))
Rates_issuedate_A <- filter(cleaned_accepted, grade == "A") %>%
  ggplot(data = ., aes(x = issue_y, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates A-grade loans by issue date", x ="Issue date", y = "Interest rate (%)")
Rates_issuedate_A
Rates_issuedate_B <- filter(cleaned_accepted, grade == "B") %>%
  ggplot(data = ., aes(x = issue_y, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates B-grade loans by issue date", x ="Issue date", y = "Interest rate (%)")
Rates_issuedate_B
Rates_issuedate_C <- filter(cleaned_accepted, grade == "C") %>%
  ggplot(data = ., aes(x = issue_y, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates C-grade loans by issue date", x ="Issue date", y = "Interest rate (%)")
Rates_issuedate_C
Rates_issuedate_D <- filter(cleaned_accepted, grade == "D") %>%
  ggplot(data = ., aes(x = issue_y, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates D-grade loans by issue date", x ="Issue date", y = "Interest rate (%)")
Rates_issuedate_D
Rates_issuedate_E <- filter(cleaned_accepted, grade == "E") %>%
  ggplot(data = ., aes(x = issue_y, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates E-grade loans by issue date", x ="Issue date", y = "Interest rate (%)")
Rates_issuedate_E
Rates_issuedate_F <- filter(cleaned_accepted, grade == "F") %>%
  ggplot(data = ., aes(x = issue_y, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates F-grade loans by issue date", x ="Issue date", y = "Interest rate (%)")
Rates_issuedate_F
Rates_issuedate_G <- filter(cleaned_accepted, grade == "G") %>%
  ggplot(data = ., aes(x = issue_y, y = int_rate)) +
  geom_boxplot() +
  labs(title="Distribution of interest rates G-grade loans by issue date", x ="Issue date", y = "Interest rate (%)")
Rates_issuedate_G

cleaned_accepted$term <- as.numeric(as.character(cleaned_accepted$term))
wilcox.test(int_rate ~ term, data = cleaned_accepted)
kruskal.test(int_rate ~ grade, data = cleaned_accepted)
kruskal.test(int_rate ~ sub_grade, data = cleaned_accepted)
#data not normal and sample size large so mann-whitney-wilcox / Kruskal Wallis test proving distribution of rates on short and long loans significantly different



