############################################################
### Check Aline Ferreira data from the Xavante, Brazil ####
############################################################

#Author: catalina fernandez ###

#starting commit: 906a51e

library(tidyverse)
library(lubridate)


setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Xavante/")

# -1. Check if data is on csv format and can be read, otherwise convert to csv
# -2. Check if there are one or more sheets in case there is a .xls file
# -3. Check if N of column names match columns of data
# -4. Check if periods are used for decimals convert LatAm comma for decimals to points)
# -5. Check that dates are in dd/mm/yyyy format
# -6. Check if  dataframe is structured  according to the guidelines; if not fix
# -7. Check if data is in the right units of measurements (cm, kg)
# -8. Modify column names to standard lowercase (person_id, sex, height_m, weight_kg)
# -9. Create observation id variable (obs_id) 

rm(list = ls())

# 1. Check if data is on csv format and can be read, otherwise convert to csv

dat <- read.csv("raw/dados_xavante_aline.csv", sep =";")
glimpse (dat)

#rename columns
colnames(dat)
dat <- dat %>%
  rename (person_id = id)


#reorder columns
dat <- dat [, c ("person_id", "sex", "age_years", "age_months", "height_cm", "weight_kg")]

dat<- dat %>%
  mutate(across(age_years:weight_kg, ~ as.numeric(gsub(",", ".", .))))

# in sex column, replace f for F and m for M

dat <- dat %>%
  mutate(sex = str_replace_all(sex, c("f" = "F", "m" = "M")))

#Create random obs_id 
generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)),
         site = "Mato_Grosso")

glimpse (dat)

dat <- dat [, c ("person_id","obs_id", "sex", "site", "age_years", "age_months", "height_cm", "weight_kg")]
write.csv(dat, "site_measurements.csv", row.names = FALSE)


