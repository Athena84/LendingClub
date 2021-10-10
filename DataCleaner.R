
#Packages
library(tidyverse)
library(caret)

#Read the data
accepted <- read.csv("./data/accepted_2007_to_2018Q4.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
rejected <- read.csv("./data/rejected_2007_to_2018Q4.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#Testing overview
#sort(colnames(accepted))
#summary(accepted)
#summary(rejected)
#apply(accepted, 2, unique)
#ceiling(colMeans(is.na(accepted)) * 100)

#Selecting key columns and removing missing value observations
selected_cols <- c("addr_state", "annual_inc", "dti", "emp_length", "fico_range_low", "fico_range_high", "grade", "id", "int_rate", "loan_amnt", "funded_amnt", "purpose", "sub_grade", "term", "issue_d", "loan_status", "settlement_status", "last_pymnt_d", "total_pymnt")
cleaned_accepted <- accepted[ , selected_cols] %>%
  filter(., int_rate > 0)

#Save cleaned data set
write.csv(cleaned_accepted, "./data/cleaned_accepted.csv", row.names = FALSE)
