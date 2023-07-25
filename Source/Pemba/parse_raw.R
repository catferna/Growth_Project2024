#############################################################
### Check Monique Borgerhoff Mulder data from Pemba ####
#############################################################

#Author: catalina fernandez ###
#starting commit: c71eee2

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Pemba/")

library(dplyr)
library(readxl)
library(tidyxl)
library(lubridate)

rm(list = ls())
dat <- read_excel("raw/ASP with R16(May 2018 5th corrected) best anthrops_Bunce.xlsx")

# -1. Check if data is on csv format and can be read, otherwise convert to csv
# -2. Check if there are one or more sheets in case it's a .xls file
# -3. Check if N of column names match columns of data
# -4. Check if periods are used for decimals 
# -5. Check that dates are in dd/mm/yyyy format
# -6. Check if data is in the right units of measurements (cm, kg)
# -7. Check if dataframe is structured  according to the guidelines (e.g.is sex f/m?)
# -8. Modify column names to standard lowercase (person_id, sex, height_m, weight_kg)
# -9. Create observation id variable (obs_id) 
# -10 Save dataframe as "site_measurements.csv"

glimpse (dat)  #1,#2,#3,#4,#5,#6

#7 convert sex column (male 0/1) to F/M
dat <- dat %>%
  mutate(sex = ifelse(male == 0, "F", "M")) %>%
  select(-male)

# 8. Modify column names and format
dat <- dat %>%
  rename (person_id = code,
        height_cm = FLDCM,
        weight_kg = FLDKG,
        date_of_birth = `BIRTH DATE`,
        date_of_measure = datemeasured) %>%
  mutate (date_of_birth = as.Date(date_of_birth),
         date_of_measure = as.Date(date_of_measure))

head (dat)

#9. Create random obs_id 
generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)))

# reorder and keep only columns that will use

dat <- dat [, c("person_id","obs_id", "sex","date_of_birth", "date_of_measure", 
                "height_cm", "weight_kg")]


write.csv(dat, "site_measurements.csv", row.names = FALSE)
