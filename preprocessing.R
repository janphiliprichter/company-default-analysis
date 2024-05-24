# Loading libraries
library(tidyverse)

# Reading the data set
data <- read.csv("company_data.csv",
                 tryLogical=TRUE,
                 stringsAsFactors=TRUE)

attach(data)

# company_id as factor
data$company_ID <- factor(data$company_ID)

# Convert decision_date to Date format
data$decision_date <- as.Date(data$decision_date,
                              format="%Y-%m-%d")

# Column names
colnames(data)

# Renaming columns
data <- data %>% 
  rename("date" = "decision_date",
         "score1" = "external_score_ver01",
         "score2" = "external_score_ver02",
         "score3" = "external_score_ver03",
         "default" = "target")

# Checking the data classes
glimpse(data)

# Checking for missing values
sum(is.na(data))

# Checking for infinite values
apply(data, 2, function(x) any(is.infinite(x)))

# Checking for duplicates
sum(duplicated(data))

# Removing duplicates
data <- unique(data)
