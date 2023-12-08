#############################################################
######### Check Moseten data sent by Bret Beheim  ###########
#############################################################

#Author: catalina fernandez
#starting commit: c5ce524; 
#data downloaded from https://github.com/babeheim/tsimane-moseten-growth-curves; commit #fedb54a

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Moseten/")

library(dplyr)
library(readxl)
library(tidyxl)
library(lubridate)
library(tidyverse)

rm(list = ls())
dat <- read.csv ("raw/observations_moseten.csv") #
#first line of this data contains info, indicating:
#"Maya Children ages 3-21 annual measurements 2010-2019 (except 2015)"
#"NOTE 2011 MEASUREMENTS WERE ENTERED 2X"

# -1. Check if data is on csv format and can be read, otherwise convert to csv
# -2. Check if there are one or more sheets in case it's a .xls file
# -3. Check if N of column names match columns of data
# -4. Check if periods are used for decimals 
# -5. Check that dates are in dd/mm/yyyy format
# -6. Check if data is in the right units of measurements (cm, kg)
# -7. Check if dataframe is structured  according to the guidelines (e.g.is sex f/m?)
# -8. Check sex mismatches
# -9. Modify column names to standard lowercase (person_id, sex, height_m, weight_kg)
# -10. Add site column dat$site <- "xxx" 
# -11. Create observation id variable (obs_id) 
# -12. Save dataframe as "site_measurements.csv"

glimpse (dat)  #1 #2 #3 #4 #5 (no dates) #6 

#7. Format sex as f/m as it is as male (0/1)
dat <- dat  %>%
  rename(sex = male) %>%
  mutate(sex = ifelse(sex == 0, 'f', 'm')) 

#8. Check sex mismatch
colnames (dat)
sex_mismatch <- dat %>%
  group_by(pid) %>%
  summarise(same_sex = n_distinct(sex) == 1) %>%
  filter(!same_sex)
print (sex_mismatch) #NO individuals have sex mismatch 

#9. Check that dates of birth are consistent across observations within an individual 
#dob_mismatch <- dat %>%
 # group_by(person_id) %>%
#  filter(n_distinct(date_of_birth) > 1) %>%
#  ungroup() # NO DOB IN DATASET , only age

#dob_mismatch %>%
#  distinct(person_id) %>%
#  count() # # NO IN DATASET , only age

#10.Modify column names and format if necessary
colnames(dat)
dat <- dat %>%
  rename (person_id = pid,
          site = population, 
          height_cm = height,
          weight_kg = weight, 
          age_estimated =age)

#11. Add site column dat$site <- "xxx" 
#already in the dataset

#12-. Create random obs_id 
generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)))

glimpse (dat)

dat <- dat [, c ("person_id","obs_id", "sex", "site", "age_estimated", "height_cm", "weight_kg")]


#12-. Save dataframe as "site_measurements.csv"
#write.csv(dat, "site_measurements.csv", row.names = FALSE)
