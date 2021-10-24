
#Packages
library(tidyverse)
library(stringi)
library(e1071)

#Read the data
cleaned_applications <- read.csv("./data/cleaned_applications.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

set.seed(0)
sample_size = 30000
selected_cols <- c("Status", "Employment.Length")
test_sample <-cleaned_applications[sample(nrow(cleaned_applications), size = sample_size, replace = FALSE), selected_cols]

svm.linear = svm(Status ~ .,
                      data = test_sample,
                      kernel = "linear",
                      cost = 1)

summary(svm.linear)
plot(svm.linear, test_sample)
