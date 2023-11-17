#############################################################
##### Check Maya data sent by Karen Kramer ######
#############################################################

#Author: catalina fernandez
#starting commit:e59f2cc

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Maya/")

library(dplyr)
library(readxl)
library(tidyxl)
library(lubridate)
library(tidyverse)

rm(list = ls())
dat <- read.csv ("raw/Maya Anthropometry 2010-2019 for C Fernandez MPI 11 1 23.csv", skip = 1) #1729 obs.
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

glimpse (dat)  #1 #2 

#3 Remove last two columns as they contain no info
dat = dat [,1:6]

#Filter out rows for which all columns are empty 
dat <- dat[rowSums(is.na(dat) | dat== "") != ncol(dat), ]
dat <- dat %>%
  filter(!is.na(Measurment.Date) & Measurment.Date != "") #1690 obs. left after eliminating empty rows

#4.
#5. Recognize date format
dat <- dat %>%
  mutate(date_of_birth = as.Date(DOB, format = "%m/%d/%Y"), 
         date_of_measure = as.Date(Measurment.Date, format = "%m/%d/%Y"))

#6,
#7. Format sex as f/m instead of F/M
dat <- dat %>%
mutate(Sex = tolower(Sex)) 

#8. Check sex mismatch
colnames (dat)
sex_mismatch <- dat %>%
  group_by(ID) %>%
  summarise(same_sex = n_distinct(Sex) == 1) %>%
  filter(!same_sex)
print (sex_mismatch) #NO individuals have sex mismatch 

#9.Modify column names and format if necessary
colnames(dat)
dat <- dat %>%
  rename (person_id = ID,
          sex = Sex,
          height_cm = HT..cm.,
          weight_kg = WT..kg.)

#10. Add site column dat$site <- "xxx" 
dat$site <- "Maya"

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
