#############################################################
####### Check Mamiraua Data already pre-cleaned by BB  ######
#############################################################

#Author: catalina fernandez
#starting commit: b27838d

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Mamiraua")

library(dplyr)
library(readxl)
library(tidyxl)
library(lubridate)

rm(list = ls())
dat <- read.csv ("site_measurements.csv")

# -1. Check if data is on csv format and can be read, otherwise convert to csv
# -2. Check if there are one or more sheets in case it's a .xls file
# -3. Check if N of column names match columns of data
# -4. Check if periods are used for decimals 
# -5. Check that dates are in dd/mm/yyyy format
# -6. Check if data is in the right units of measurements (cm, kg)
# -7. Check if dataframe is structured  according to the guidelines (e.g.is sex f/m?)
# -8. Check sex mismatches
# -9. Modify column names to standard lowercase (person_id, sex, height_m, weight_kg)
# -10. Create observation id variable (obs_id) 
# -11. Save dataframe as "site_measurements.csv"

glimpse (dat)  #1,#2,#3,#4,#5,#6, 

#7 convert sex column (female =1) to F/M
dat <- dat %>%
  mutate(sex = ifelse(sex == 1, "F", "M"))

colnames (dat)
sex_mismatch <- dat %>%
  group_by(person_id) %>%
  summarise(same_sex = n_distinct(sex) == 1) %>%
  filter(!same_sex)

print (sex_mismatch) # NO individuals have sex mismatches

#Create random obs_id 
generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)))

glimpse (dat)

dat <- dat [, c ("person_id","obs_id", "sex", "site", "date_of_birth", "date_of_measure", "reproductive_status", "height_cm", "weight_kg")]

# -10. Save dataframe as "site_measurements.csv"
write.csv(dat, "site_measurements.csv", row.names = FALSE)
