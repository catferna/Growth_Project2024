#############################################################
##### Check Samburu data sent by Bilinda Straight ######
#############################################################

#Author: catalina fernandez
#starting commit:e59f2cc

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Samburu/")

library(dplyr)
library(readxl)
library(tidyxl)
library(lubridate)
library(tidyverse)

rm(list = ls())
dat <- read_xlsx ("raw/2018-2022 Straight et al longitudinal.xlsx") # obs.
#GENERAL NOTE: weights adjusted based on clothing and ornaments, using matched samples for each type of clothing and ornament weighed in grams


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

glimpse (dat)  #1 #2 #4 #6

#3 Remove last three columns as they contain no info, except for a note

#GENERAL NOTE: weights adjusted based on clothing and ornaments, using matched 
#samples for each type of clothing and ornament weighed in grams

dat = dat [,1:19]

#5. Recognize date format

dat <- dat %>%
  mutate(date_of_birth  = dmy(paste(sprintf("%02d", `Birth-Date` ), sprintf("%02d", `Birth-Month` ), `Birth-Year` , sep = "/")),
         date_of_measure_2018 = dmy(paste(sprintf("%02d", `Baseline Interview-Day` ), sprintf("%02d", `Baseline Interview-Month` ), `Baseline Interview-Year`, sep = "/")),
         date_of_measure_2022= dmy(paste(sprintf("%02d", `2022 Interview-Day`), sprintf("%02d", `2022 Interview-Month`),`2022 Interview-Year` , sep = "/")), 
          Sex = tolower(Sex),
         `Lowland = 0; Highland = 1` = ifelse(`Lowland = 0; Highland = 1` == 0, "lowland", "highland"))


dat <- dat %>% rename (
  height_cm_2022 = "2022  Height",
  weight_kg_2022= "2022 Raw Weight",
    weight_adjusted_2022 = "2022 Adjusted Weight", 
    weight_kg_2018 =  "Baseline Raw Weight",
    weight_adjusted_2018 = "Baseline Adjusted Weight", 
    height_cm_2018 = "Baseline Height")

dat$weight_kg_2022 <- as.numeric(dat$weight_kg_2022)
dat$height_cm_2022 <- as.numeric(dat$height_cm_2022)
dat$weight_adjusted_2022 <- as.numeric(dat$weight_adjusted_2022)
dat <- dat %>%
  mutate(across(c( weight_kg_2022, height_cm_2022, weight_adjusted_2022, weight_kg_2018, weight_adjusted_2018, height_cm_2018 ),
                ~round(., 2)))

#7

#8. Check sex mismatch
colnames (dat)
sex_mismatch <- dat %>%
  group_by(`Sample ID`) %>%
  summarise(same_sex = n_distinct(Sex) == 1) %>%
  filter(!same_sex)
print (sex_mismatch) #NO individuals have sex mismatch 

#9.Modify column names and format if necessary
colnames(dat)
dat <- dat %>%
  rename (person_id =  "Sample ID" ,
          sex = "Sex",
         altitude = "Lowland = 0; Highland = 1" )

#10. Add site column dat$site <- "xxx" 
dat$site <- "Samburu"

colnames (dat)
dat = dat %>% 
  select ("person_id", "date_of_birth",  "sex", "site", "altitude","height_cm_2018", "weight_kg_2018", "weight_adjusted_2018",
           "height_cm_2022", "weight_kg_2022", "weight_adjusted_2022", "date_of_measure_2018", "date_of_measure_2022")

dat <- dat %>%
  pivot_longer(
    cols = starts_with(c("date_of_measure", "height_cm", "weight_kg", "weight_adjusted")),
    names_to = c(".value", "year"),
    names_pattern = "(\\D+)(_\\d+)",
    values_to = c("date_of_measure", "height_cm", "weight_kg", "weight_adjusted"))


#11-. Create random obs_id 
generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)))

glimpse (dat)

dat <- dat [, c ("person_id","obs_id", "sex", "site", "altitude", "date_of_birth","date_of_measure", "height_cm", "weight_kg", "weight_adjusted")]


#12-. Save dataframe as "site_measurements.csv"
#write.csv(dat, "site_measurements.csv", row.names = FALSE)
