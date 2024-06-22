# Loading libraries
library(tidyverse)
library(plyr)

##### Data Preprocessing #####


# Reading the data set
data <- read.csv("company_data.csv",
                 tryLogical=TRUE,
                 stringsAsFactors=TRUE)

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

# Renaming factors
data$industry_sector <- revalue(data$industry_sector, 
                                c("Agricoltura" = "Agriculture",
                                  "Alimentare" = "Food",
                                  "Altri beni di consumo" = "Consumer Goods",
                                  "Chimica di base e intermedi" = "Chemicals",
                                  "Costruzioni e materiali per costruzioni" = "Construction",
                                  "Distribuzione" = "Retail",
                                  "Editoria e stampa" = "Publishing",
                                  "Elettrodomestici" = "Electrical Appliances",
                                  "Elettrotecnica ed elettronica" = "Electronics",
                                  "Energia ed estrazione" = "Energy",
                                  "Farmaceutica" = "Pharmaceuticals",
                                  "Holding, finanziarie ed altro" = "Finance",
                                  "Largo consumo / attività ricreativo-culturali" = "Culture",
                                  "Meccanica" = "Mechanics",
                                  "Metallurgia e prodotti in metallo" = "Metal",
                                  "Mezzi di trasporto" = "Transportation",
                                  "Servizi" = "Services",
                                  "Sistema moda" = "Fashion",
                                  "Trasporti" = "Logistics",
                                  "Utility" = "Utilities"))

data$geo_area <- revalue(data$geo_area, 
                         c("Centro" = "Centre",
                           "Isole" = "Islands",
                           "Nord-est" = "North-east",
                           "Nord-ovest" = "North-west",
                           "Sud" = "South"))

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




##### Feature Engineering #####


### Pseudo-Profits ###
data$profits <- data$gross_margin_ratio * data$revenues



### Transformations ###

## Log-transformation

# Age
data$log_age <- log(data$age + 1)

# Revenues
data$log_revenues <- log(data$revenues + 1)

# Profits
data$log_profits <- log(data$profits + 1)
mean(data$log_profits)

## Square-root-transformations

# Gross Margin Ratio
data$sqrt_gmr <- sqrt(data$gross_margin_ratio)



### Time Variables ###

## Cyclical Encoding 

# Month
data$month <- month(data$date)

data$sin_month = sin(2 * pi * data$month / 12)
data$cos_month = cos(2 * pi * data$month / 12)

# Day of the Month
data$month_day <- day(data$date)

data$sin_dom = sin(2 * pi * data$month_day / 31)
data$cos_dom = cos(2 * pi * data$month_day / 31)

# Day of the week
data$week_day <- wday(data$date)

data$sin_dow = sin(2 * pi * data$week_day / 7)
data$cos_dow = cos(2 * pi * data$week_day / 7)


## Category Year
data$year <- year(data$date)



# Saving the preprocessed data as .csv file
write.csv(x=data, 
          file="preprocessed_data.csv",
          row.names=FALSE)
