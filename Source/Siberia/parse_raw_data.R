########################################
### Check J.Ziker data from Siberia ####
########################################

#Author: catalina fernandez ###

#starting commit: c71eee2

library(tidyverse)
library(lubridate)


setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Siberia/")

# -1. Check if data is on csv format and can be read, otherwise convert to csv
# -2. Check if there are one or more sheets in case there is a .xls file
# -3. Check if N of column names match columns of data
# -4. Check if periods are used for decimals 
# -5. Check that dates are in dd/mm/yyyy format
# -6. Check if  dataframe is structured  according to the guidelines; if not fix
# -7. Check if data is in the right units of measurements (cm, kg)
# -8. Modify column names to standard lowercase (person_id, sex, height_m, weight_kg)
# -9. Create observation id variable (obs_id) 

rm(list = ls())

# 1. Check if data is on csv format and can be read, otherwise convert to csv
dat <- read.csv("raw/Ziker_Growth_Project_Ust_Avam.csv")
glimpse (dat)

#Check 1-7 
dat <- dat %>%
  mutate (BirthDate = format(as.Date(BirthDate),"%d/%m/%Y"),
         DateDataCollection = format(as.Date(DateDataCollection),"%d/%m/%Y"), 
         Height = Height *100) #to convert to cm 

colnames(dat)
dat <- dat %>%
  rename (person_id = PersonCID,
          sex =Gender,
          height_cm = Height,
          weight_kg = Weight,
          date_of_birth= BirthDate,
          estim_date_of_measure= DateDataCollection)

dat <- dat %>%
  mutate (min_date_of_measure =  as.Date("2001-08-01"),
          max_date_of_measure = as.Date("2001-10-01"))


#Create random obs_id 
generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)))

# reorder  columns 
glimpse (dat)

dat <- dat [, c("person_id","obs_id", "sex","date_of_birth", "estim_date_of_measure", 
                "min_date_of_measure", "max_date_of_measure", "height_cm", "weight_kg")]

dat <- dat %>% 
  mutate (date_of_birth = dmy(date_of_birth),
          estim_date_of_measure = dmy(estim_date_of_measure),
 age_estimated = as.duration(interval(date_of_birth, estim_date_of_measure)) / dyears(1))

#Create random obs_id 
generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)))

write.csv(dat, "site_measurements.csv", row.names = FALSE)

rm(list = ls())

#########################
##### CLEAN DATA ########
#########################

dat <- read.csv("site_measurements.csv")

# Check height and weight values
# Data is cross-sectional so there's not much that can be done to detect anomalies

#For individual 577, obs. #mg64m, correct value (it's 15000.0)
dat$height_cm [which(dat$obs_id=="mg64m")] <- 150.0

dat <- read.csv("site_measurements.csv")


