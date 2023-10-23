#############################################################
##### Check Kilimanjaro data sent by Katherine Wander ######
#############################################################

#Author: catalina fernandez
#starting commit:c865794


setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Kilimanjaro/")

library(dplyr)
library(readxl)
library(tidyxl)
library(lubridate)

rm(list = ls())
dat <- read.csv ("raw/Kilimanjaro.csv")

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

glimpse (dat)  #1 #2 #3 #4 #5 #6,#7 

#8. Check sex mismatch

colnames (dat)
sex_mismatch <- dat %>%
  group_by(participant_id) %>%
  summarise(same_sex = n_distinct(sex) == 1) %>%
  filter(!same_sex)
print (sex_mismatch) #NO individuals have sex mismatch 

#9.Modify column names and format if necessary
colnames(dat)
dat <- dat %>%
  rename (person_id = participant_id,
          height_cm = height,
          weight_kg = weight,
          date_of_measure = date,
          date_of_birth =dob)

#10. Add site column dat$site <- "xxx" 
dat$site <- "Kilimanjaro"

#11-. Create random obs_id 
generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)))

glimpse (dat)

dat <- dat [, c ("person_id","obs_id", "sex", "site", "date_of_birth","date_of_measure", "height_cm", "weight_kg")]


#12-. Save dataframe as "site_measurements.csv"
write.csv(dat, "site_measurements.csv", row.names = FALSE)



