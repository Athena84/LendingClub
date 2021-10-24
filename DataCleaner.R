
#Packages
library(tidyverse)
library(stringi)
library(lubridate)

#Read the data
accepted <- read.csv("./data/accepted_2007_to_2018Q4.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
rejected <- read.csv("./data/rejected_2007_to_2018Q4.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#Save cleaned data set for analysis on accepted loans
selected_cols <- c("addr_state", "annual_inc", "dti", "emp_length", "fico_range_low", "fico_range_high", "grade", "id", "int_rate", "loan_amnt", "funded_amnt", "purpose", "sub_grade", "term", "issue_d", "loan_status", "settlement_status", "last_pymnt_d", "total_pymnt")
cleaned_accepted <- accepted[ , selected_cols] %>%
  filter(., int_rate > 0)
write.csv(cleaned_accepted, "./data/cleaned_accepted.csv", row.names = FALSE)

#Save combined cleaned version of rejected & accepted loans for analysis
rejected <- rename(rejected,
                   Risk.Score = Risk_Score
                   )
selected_rej_cols <- c("Amount.Requested", "Application.Date",  "Debt.To.Income.Ratio", "Employment.Length", "Risk.Score", "State") 
rejected <- rejected[ , selected_rej_cols]
rejected$Application.Date <- year(as.Date(rejected$Application.Date))
rejected$Status <- "rejected"

rejected$Employment.Length <- as.character(rejected$Employment.Length)
rejected$Employment.Length <- stri_replace_all_fixed(rejected$Employment.Length, " years", "")
rejected$Employment.Length <- stri_replace_all_fixed(rejected$Employment.Length, " year", "")
rejected$Employment.Length <- stri_replace_all_fixed(rejected$Employment.Length, "+", "")
rejected$Employment.Length <- stri_replace_all_fixed(rejected$Employment.Length, "< 1", "0")
rejected$Employment.Length <- as.numeric(rejected$Employment.Length)
rejected$Employment.Length <- ifelse(is.na(rejected$Employment.Length), 0, rejected$Employment.Length)

accepted <- rename(accepted,
                   Amount.Requested = funded_amnt,
                   Employment.Length = emp_length,
                   State = addr_state,
                   )
accepted$Application.Date <- as.integer(stri_sub(accepted$issue_d, -4, -1))
accepted$Risk.Score = (accepted$fico_range_high + accepted$fico_range_low) / 2
accepted$Debt.To.Income.Ratio <- ifelse(accepted$application_type == "Individual", accepted$dti, accepted$dti_joint)
accepted <- accepted[ , selected_rej_cols]
accepted$Status <- "accepted"

accepted$Employment.Length <- as.character(accepted$Employment.Length)
accepted$Employment.Length <- stri_replace_all_fixed(accepted$Employment.Length, " years", "")
accepted$Employment.Length <- stri_replace_all_fixed(accepted$Employment.Length, " year", "")
accepted$Employment.Length <- stri_replace_all_fixed(accepted$Employment.Length, "+", "")
accepted$Employment.Length <- stri_replace_all_fixed(accepted$Employment.Length, "< 1", "0")
accepted$Employment.Length <- as.numeric(accepted$Employment.Length)
accepted$Employment.Length <- ifelse(is.na(accepted$Employment.Length), 0, accepted$Employment.Length)

write.csv(rbind(accepted, rejected), "./data/cleaned_applications.csv", row.names = FALSE)

#ceiling(colMeans(is.na(accepted)) * 100)

