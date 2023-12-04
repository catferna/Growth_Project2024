###########################################################
################ Cleaning MOSETEN  data ################
###########################################################

# starting commit:62477c6
#Author: catalina fernandez


setwd ("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Moseten/")

library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv") # 1522 obs. loaded

#####  Checks to ensure consistency #### 
colnames (dat)

#1. remove observations that do not have BOTH height and weight. Keep if only one is missing
sum(is.na(dat[["height_cm"]])) # 21 height measurements missing   
sum(is.na(dat[["weight_kg"]])) # 45 weight measurements missing
dat <- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) #NO row has both vars missing. 

#2. Check if there are duplicated rows
rep <- dat %>% select(-obs_id)

sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) # NO duplicated obs.
dupl <- rep[duplicated(rep) | duplicated(rep, fromLast = TRUE), ] #NO dupl. 

# Remove duplicated obs, if any (obs_id = xxxx)
dat <- dat %>%
  filter(obs_id != "xxx") #NO dupl

# 3. Check how many people have X observations?
table(table(dat$person_id)) # most ind. have 2-3 obs. ANYTHING ELSE? 

# 4. Add column 'age_estimated' to make it easier to assess whether differences are likely or not 
### ! Check first the format of input dates, such as whether month is before day, whether numbers
# or month names are used, and n of digits for the year field, etc.
# dataset does not have dates, and has age already.
#dat <- dat %>%
#  mutate(date_of_birth = as.Date(date_of_birth, format = "%d/%m/%Y"),
#         date_of_measure = as.Date(date_of_measure, format = "%d/%m/%Y"),
#         age_estimated = as.numeric(interval(date_of_birth, date_of_measure) / dyears(1)))


#5. Check how many ind. don't have age, or don't have dob, or date of measure
sum(is.na(dat[["age_estimated"]])) # ALL obs. have age

#6. Remove rows that don't have age, or age can't be calculated bc missing dates
#miss_age <- dat %>% filter(is.na(age_estimated)) # NO missing dates
#dat <- dat %>%
#  filter(person_id != "xxx", # to remove entire individual
#        obs_id != "xxx") # to remove only a specific row

#7. Count number of people in the dataframe
dat %>% distinct(person_id) %>%
  count() # 794 individuals in the dataset

#8. Plot heights to catch evident outliers
library(ggplot2)
dev.off() 
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point() # everything looks ± fine

dev.off() 
ggplot(dat, aes(x =age_estimated, y = weight_kg , group = person_id))+
  geom_line() +
  geom_point() #definitely many weird cases, babies way too heavy 


######### check with  BRET!! 

sub20 <- dat %>% filter(age_estimated <20)


# 9. Check if all individuals have sex 

miss_sex <- dat %>% filter(is.na(sex)) # xxx cells where sex is NA
#Often, cells are empty instead of having a NA. The following script get those:
library(stringr)
fil <- dat %>%
  group_by(person_id) %>%
  filter(any(str_detect(sex, "^\\s*$"))) # xxx sex cells that are empty
empty_sex_individuals <- dat %>%
  group_by(person_id) %>%
  filter(any(str_detect(sex, "^\\s*$"))) %>% # 9 individuals have missing sex. Originally 10 
  select(person_id) %>% 
  distinct()


# 10. Add count of obs. per ind. as new column 
dat <- dat %>%
  add_count(person_id, name = "obs_counts")

# 11. Create new df with ind. that have at least two obs. for additional checks
long <- dat %>%
  filter(obs_counts >= 2)

# 12. Check obs. that go down more than 3 cm
down <- long %>%
  arrange(person_id, age_estimated) %>%  # Arrange data by person_id and age
  group_by(person_id) %>%    # Group data by person_id
  mutate(obs_diff = height_cm - lag(height_cm, default = first(height_cm))) %>%
  ungroup() %>%
  filter (obs_diff <=-3)

#13. count n of obs that goes down.
down %>%
  distinct(person_id) %>%
  count() # xxx individuals shrink >3 cm : xxx, xxx 

# a1. ind.xxx, obs #xxx height is inconsistent with trajectory. if weight seems ok, only rm height:
dat$height_cm [which(dat$obs_id=="xxx")] <-NA

# a.2. ind. xxx, obs #xxx, explain why changing the value in the cell. 

dat$height_cm [which(dat$obs_id=="xxx")] <- xxx # changed from xxx

#14. Check how many height obs. goes up more than 10cm in a period of 1 year or less.

# ---update long first:
long <- dat %>%
  filter(obs_counts >= 2) 

up <- long %>% 
  arrange(person_id, age_estimated) %>%
  group_by(person_id) %>% 
  mutate(time_elap = as.numeric (age_estimated - first(age_estimated)),
         obs_diff = height_cm - lag(height_cm, default = first(height_cm))) %>%
  ungroup() %>%
  filter(person_id %in% unique(person_id[obs_diff >= 10 & time_elap < 1])) # xxx obs, go up more than 10cm

#i <- 1
#print(long[long$person_id == up$person_id [i], ]) 
#i <- i + 1

#15. Indicate how many obs. were removed 
sum(is.na(dat[["height_cm"]])) # xxx height are NA
sum(is.na(dat[["weight_kg"]])) # xxx weight are NA

# xxx obs.loaded; KEPT xxx  (xxx obs. removed entirely) 
# xxx NA height loaded; now xxx
# xxx  weight loaded; kept xxx  weight NA


#write.csv(dat, "site_measurements.csv",row.names = FALSE)