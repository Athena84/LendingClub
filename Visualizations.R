#Packages
library(tidyverse)
library(scales)
library(stringi)
library(gmodels)
library(RColorBrewer)
library(survminer)
library(survival)

#Setting chart defaults
old_theme <- theme_set(theme_linedraw() +
                         theme(plot.title = element_text(size = 16, face = "bold")) +
                         theme(axis.title = element_text(size = 14,)) +
                         theme(axis.text = element_text(size = 14,)) 
) 

#Read the data
cleaned_accepted <- read.csv("./data/cleaned_accepted.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#Clean term durations
cleaned_accepted$term <- as.integer(as.integer(stri_sub(cleaned_accepted$term,1,3)) / 12)

#Clean dates
cleaned_accepted$issue_date <- as.Date(paste0(as.character(cleaned_accepted$issue_d), "-01"), "%b-%Y-%d")
cleaned_accepted$last_pymnt_date <- as.Date(paste0(as.character(cleaned_accepted$last_pymnt_d), "-01"), "%b-%Y-%d")


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
  mutate(., "prop_funded" = total_funded / sum(total_funded)) %>% #works because each sumarize takes off one layer of grouping
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
cleaned_accepted$issue_y <- as.integer(stri_sub(cleaned_accepted$issue_d,-4,-1))
subset_mature <- filter(cleaned_accepted, (term == 3 & issue_y < 2015) | (term == 5 & issue_y < 2013)) %>%
  filter(., (loan_status == "Fully Paid") | (loan_status == "Charged Off"))
subset_mature$loan_status <- droplevels(subset_mature$loan_status)
subset_mature$loan_status[1]
#Note, filtering out the loans not adhering to the policy makes a strong bias, but information is lacking
#Note, in loans with this term and age there were no other statusses present, so there is no issue with long overdue loans still running

CrossTable(x = subset_mature$term, y = subset_mature$loan_status, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)
#Yes, longer term loans have a much higher probability of being charged off

subset_mature$term <- as.factor(subset_mature$term)
default_term_grade <- group_by(subset_mature, term, grade) %>%
  summarize(., default_rate = mean(loan_status == "Charged Off")) %>%
  ggplot(data = ., aes(x = grade, y = default_rate, group = term, color = term)) +
  geom_line() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  labs(title="Default rate of loans by term and grade", x ="Grade", y = "Default rate")
default_term_grade

default_term_subgrade <- group_by(subset_mature, term, sub_grade) %>%
  summarize(., default_rate = mean(loan_status == "Charged Off")) %>%
  ggplot(data = ., aes(x = sub_grade, y = default_rate, group = term, color = term)) +
  geom_line() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  labs(title="Default rate of loans by term and subgrade", x ="Subgrade", y = "Default rate")
default_term_subgrade

subset_short_mature <- filter(subset_mature, term == 3)
CrossTable(x = subset_short_mature$grade, y = subset_short_mature$loan_status, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)
subset_long_mature <- filter(subset_mature, term == 5)
CrossTable(x = subset_long_mature$grade, y = subset_long_mature$loan_status, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)

CrossTable(x = subset_mature$grade, y = subset_mature$loan_status, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)
CrossTable(x = subset_mature$sub_grade, y = subset_mature$loan_status, prop.chisq = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, chisq = TRUE)
#In general and for short or long loans separately, there is highly significant relation between grade or subgrade and default rate


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

#Settlement analysis
cleaned_accepted$settlement <- as.factor(ifelse(nzchar(as.character(cleaned_accepted$settlement_status)) == 0, 0, 1))

rate_settlement_grade <- group_by(cleaned_accepted, settlement, grade) %>%
  summarize(., av_int_rate = mean(int_rate)) %>%
  ggplot(data = ., aes(x = grade, y = av_int_rate, group = settlement, color = settlement)) +
  geom_line() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1)) +
  labs(title="Average interest rate of settled and unsettled loands by grade", x ="Grade", y = "Interest rate")
rate_settlement_grade

rate_settlement_grade <- ggplot(data = cleaned_accepted, aes(x = grade, y = int_rate, dodge = settlement)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1)) +
  labs(title="Interest rate distribution of settled and unsettled loands by grade", x ="Grade", y = "Interest rate")
rate_settlement_grade

gradeA <- filter(cleaned_accepted, grade == "A")
wilcox.test(gradeA$int_rate ~ gradeA$settlement)
gradeE <- filter(cleaned_accepted, grade == "E")
wilcox.test(gradeE$int_rate ~ gradeE$settlement)
gradeG <- filter(cleaned_accepted, grade == "G")
wilcox.test(gradeG$int_rate ~ gradeG$settlement)
#For most grades there is a significant difference, not for G


#Duration
#Needs to be analysed on only loans that could have completely paid off. If filtering on paid off and charged off statuses, the charged off would have been biased to overrepresentation 
subset_mature$duration <- as.numeric(subset_mature$last_pymnt_date - subset_mature$issue_date) / 365
subset_mature$part_term <- subset_mature$duration / subset_mature$term

Partial_Prepaid <- ggplot(data = subset_mature, aes(x = part_term, group = grade, color = grade)) +
  geom_density() +
  labs(title="Partial durations mature loans by grade", x ="Partial duration", y = "Frequency") +
  scale_x_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Partial_Prepaid

Partial_Prepaid <- filter(subset_mature, loan_status == "Fully Paid") %>%
  ggplot(data = ., aes(x = part_term, group = grade, color = grade)) +
  geom_density() +
  labs(title="Partial durations fully paid loans by grade", x ="Partial duration", y = "Frequency") +
  scale_x_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Partial_Prepaid

Partial_Prepaid <- filter(subset_mature, loan_status == "Charged Off") %>%
  ggplot(data = ., aes(x = part_term, group = grade, color = grade)) +
  geom_density() +
  labs(title="Partial durations defaulted loans by grade", x ="Partial duration", y = "Frequency") +
  scale_x_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Partial_Prepaid

filter(subset_mature, loan_status == "Fully Paid") %>%
  kruskal.test(part_term ~ grade, data = .) #significant deviation in distribution
filter(subset_mature, loan_status == "Charged Off") %>%
  kruskal.test(part_term ~ grade, data = .) #significant deviation in distribution
#No real difference by term


#Profit analysis
#Rounding all durations shorter than 1y up to 1y to avoid exploding return rates with extremely short durations
#Note: the paid off loans and charged off loans can be filtered from the total data set for profit analysis, however the combination of the two should only be filtered from the mature subset as it would otherwise bias toward charged off loans
cleaned_accepted$duration <- as.numeric(cleaned_accepted$last_pymnt_date - cleaned_accepted$issue_date) / 365
cleaned_accepted$duration <- ifelse(cleaned_accepted$duration > 1, cleaned_accepted$duration, 1)
cleaned_accepted$profit <- (cleaned_accepted$total_pymnt - cleaned_accepted$funded_amnt) / cleaned_accepted$funded_amnt
cleaned_accepted$profit_ann <- cleaned_accepted$profit  / cleaned_accepted$duration

subset_mature$duration <- as.numeric(subset_mature$last_pymnt_date - subset_mature$issue_date) / 365
subset_mature$duration <- ifelse(subset_mature$duration > 1, subset_mature$duration, 1)
subset_mature$profit <- (subset_mature$total_pymnt - subset_mature$funded_amnt) / subset_mature$funded_amnt
subset_mature$profit_ann <- subset_mature$profit  / subset_mature$duration

Profit_distributions_paidoff <- filter(cleaned_accepted, loan_status == "Fully Paid") %>%
  ggplot(data = ., aes(x = profit_ann, group = grade, color = grade)) +
  geom_density() +
  labs(title="Distributions of annual profit paid off loans by grade", x ="Annual profit", y = "Frequency") +
  scale_x_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Profit_distributions_paidoff

Profit_distributions_default <- filter(cleaned_accepted, loan_status == "Charged Off") %>%
  ggplot(data = ., aes(x = profit, group = grade, color = grade)) +
  geom_density() +
  labs(title="Distributions of total return defaulted loans by grade", x ="Profit", y = "Frequency") +
  scale_x_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Profit_distributions_default

Profit_distributions_mix <- ggplot(data = subset_mature, aes(x = profit, group = grade, color = grade)) +
  geom_density() +
  labs(title="Distributions of total profit all loans by grade", x ="Profit", y = "Frequency") +
  scale_x_continuous(labels = label_number(suffix = "%", scale = 1e2)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Profit_distributions_mix

Profit_distributions_mix_ann <- ggplot(data = subset_mature, aes(x = profit_ann, group = grade, color = grade)) +
  geom_density() +
  labs(title="Distributions of annual profit all loans by grade", x ="Profit", y = "Frequency") +
  scale_x_continuous(labels = label_number(suffix = "%", scale = 1e2), limits = c(-0.2, 0.3)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
Profit_distributions_mix_ann

cleaned_accepted$settlement <- as.factor(ifelse(nzchar(as.character(cleaned_accepted$settlement_status)) == 0, 0, 1))
subset_mature$settlement <- as.factor(ifelse(nzchar(as.character(subset_mature$settlement_status)) == 0, 0, 1))

profit_settlement_grade <- group_by(subset_mature, settlement, grade) %>%
  summarize(., av_profit = mean(profit)) %>%
  ggplot(data = ., aes(x = grade, y = av_profit, group = settlement, color = settlement)) +
  geom_line() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2), limits = c(-0.2, 0.3)) +
  labs(title="Average profit (un)settled loans by grade", x ="Grade", y = "Average total profit")
profit_settlement_grade

profit_settlement_grade <- ggplot(data = subset_mature, aes(x = grade, y = profit, dodge = settlement, color = settlement)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2), limits = c(-0.2, 0.3)) +
  labs(title="Profit distribution (non-)settled loans by grade", x ="Grade", y = "Total profit")
profit_settlement_grade

profit_grade_purpose <- group_by(subset_mature, purpose, grade) %>%
  summarize(., av_profit = mean(profit)) %>%
  ggplot(data = ., aes(x = grade, y = av_profit, group = purpose, color = purpose)) +
  geom_point() +
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2), limits = c(-0.2, 0.3)) +
  labs(title="Average profit loans by purpose and grade", x ="Grade", y = "Average total profit")
profit_grade_purpose


#Survival analysis
cleaned_accepted$part_term <- cleaned_accepted$duration / cleaned_accepted$term
subset_mature$part_term <- subset_mature$duration / subset_mature$term

paid_5 <- filter(cleaned_accepted, loan_status == "Fully Paid" & term == 5)
paid_3 <- filter(cleaned_accepted, loan_status == "Fully Paid" & term == 3)
paid <- filter(cleaned_accepted, loan_status == "Fully Paid")
fit_paid_5 <- survfit(Surv(part_term) ~ grade, data = paid_5)
fit_paid_3 <- survfit(Surv(part_term) ~ grade, data = paid_3)
fit_paid <- survfit(Surv(part_term) ~ grade, data = paid)
ggsurvplot(fit_paid, xlim = c(0,1.2)) +
  labs(title="Survival plot fully paid loans by grade", x ="Multiple of term", y = "Survival rate", fill = "Grade")

charged_off <- filter(cleaned_accepted, loan_status == "Charged Off")
fit_charged_off <- survfit(Surv(part_term) ~ grade, data = charged_off)
ggsurvplot(fit_charged_off, xlim = c(0, 1.2)) +
  labs(title="Survival plot Charged off loans by grade", x ="Multiple of term", y = "Survival rate", fill = "Grade")

fit_mature <- survfit(Surv(part_term, loan_status, type = "mstate") ~ grade, data = subset_mature)
ggcompetingrisks(fit_mature,  xlim = c(0, 1.2)) +
  labs(title="Survival plot mature loans by grade", x ="Multiple of term", y = "Survival rate")


