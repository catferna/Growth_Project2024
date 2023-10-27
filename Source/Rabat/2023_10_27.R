###########################################################
################ Cleaning Rabat data ################
###########################################################

# starting commit:306300b
# Author: catalina fernandez

setwd ("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Rabat//")

library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv") # 184 obs. loaded

#####  Checks to ensure consistency #### 
colnames (dat)

#1. remove observations that do not have BOTH height and weight. Keep if only one is missing
sum(is.na(dat[["height_cm"]])) # 101 missing heights
sum(is.na(dat[["weight_kg"]])) # 0 missing weights
dat <- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) # NO obs. have both missing height and weight.


#2. Check if there are duplicated rows
rep <- dat %>% select(-obs_id)
sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) # 2 duplicated observations
dupl <- rep[duplicated(rep) | duplicated(rep, fromLast = TRUE), ] #ind. 24 has two identical obs. on the same day

# ind 24, obs. id #4kxa0 and #oihvi taken on 15/6/2017 are exactly the same, remove one
dat <- dat %>%
  filter(obs_id != "4kxa0")

# 3. Check how many people have X observations?
table(table(dat$person_id)) # most ind. have two obs.

# 4. Add column estimated age to make it easier to assess whether differences are likely or not.
#check the format of the input dates (month/day/year ,year-month day, etc.)

dat <- dat %>%
  mutate(date_of_birth = as.Date(date_of_birth, format = "%m/%d/%Y"),
         date_of_measure = as.Date(date_of_measure, format = "%m/%d/%Y"),
         age_estimated = as.numeric(interval(date_of_birth, date_of_measure) / dyears(1)))

#5. Check how many ind. don't have age, or don't have dob, or date of measure
sum(is.na(dat[["age_estimated"]])) # all individuals have dates, and ages.

#6. Remove rows that don't have age, or age can't be calculated bc missing dates
miss_age <- dat %>% filter(is.na(age_estimated)) # no observations removed bc no missing dates

#7. Count number of people in the dataframe
dat %>% distinct(person_id) %>%
  count() # 88 individuals

#8. Plot heights to catch evident outliers
library(ggplot2)
dev.off()  # Plot heights
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point() #everything looks fine

dev.off() # Plot weights
ggplot(dat, aes(x =age_estimated, y = weight_kg , group = person_id))+
  geom_line() +
  geom_point() #everything looks fine

# 9. Check if all individuals have sex 
miss_sex <- dat %>% filter(is.na(sex)) # all ind. have sex.

# 10. Add count of obs. per ind. as new column 
dat <- dat %>%
  add_count(person_id, name = "obs_counts")

#11. Create new df with ind. that have at least two obs. for additional checks
long <- dat %>%
  filter(obs_counts >= 2)

down <- long %>%
  arrange(person_id, age_estimated) %>%  # Arrange data by person_id and age
  group_by(person_id) %>%    # Group data by person_id
  mutate(obs_diff = height_cm - lag(height_cm, default = first(height_cm))) %>%
  ungroup() %>%
  filter (obs_diff <=-3) #no obs. go down 3 or more cm. 


#13. Check how many height obs. goes up more than 10cm in a period of 1 year or less.
# measurements in this dataset are taken a few days apart, and up to a month, so no need to check this.

#14. Indicate how many obs. were removed 
sum(is.na(dat[["height_cm"]])) # 100 missing heights, instead of 101 (deleted obs.)
sum(is.na(dat[["weight_kg"]])) # 0 ; no NA were introduced 

# 184 obs.loaded; KEPT 183  (1 obs. removed bc it was duplicated) 

## ** sex is F/M instead of f/m. Replace:
dat$sex <- tolower(dat$sex)

# 15. Check dataframe and save as site_measurements.csv
#write.csv(dat, "site_measurements.csv",row.names = FALSE)

