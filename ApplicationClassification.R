
#Packages
library(tidyverse)
library(stringi)

#Read the data
cleaned_applications <- read.csv("./data/cleaned_applications.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
