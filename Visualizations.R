
#Packages
library(tidyverse)
library(scales)

#Setting chart defaults
old_theme <- theme_set(theme_linedraw() +
                         theme(plot.title = element_text(size = 16, face = "bold")) +
                         theme(axis.title = element_text(size = 14,)) +
                         theme(axis.text = element_text(size = 14,)) 
) 

#Read the data
cleaned_accepted <- read.csv("./data/cleaned_accepted.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
colnames(cleaned_accepted)

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