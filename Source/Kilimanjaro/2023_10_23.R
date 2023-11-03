###########################################################
################ Cleaning Kilimanjaro data ################
###########################################################

# starting commit: a18070f
#Author: catalina fernandez


setwd ("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Kilimanjaro/")

library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv") # 2789 obs.loaded

#####  Checks to ensure consistency #### 
colnames (dat)

#1. remove observations that do not have BOTH height and weight. Keep if only one is missing
sum(is.na(dat[["height_cm"]])) # 74 height measurements missing   
sum(is.na(dat[["weight_kg"]])) # 14 weight measurements missing
nohtwt <- dat %>%
  filter((is.na(weight_kg) & is.na(height_cm))) # 6 obs. don't have both height and weight
#ind. 126 (#w7xhr), 22 (#f7rv8), 52 (#nt7iz), 87 (#cvuiy), 157 (#izxql), 158 (#0vry7) 

dat <- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) # removed 6 obs., now 2783 obs.

#2. Check if there are duplicated rows
rep <- dat %>% select(-obs_id)

sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) #2 duplicated obs.
dupl <- rep[duplicated(rep) | duplicated(rep, fromLast = TRUE), ] #ind 89

#person_id 89 has a duplicated obs.on 15/09/2021. Remove one (obs_id = pm5jx)
dat <- dat %>%
  filter(obs_id != "pm5jx")

# 3. Check how many people have X observations?
table(table(dat$person_id)) # most ind. have 13 obs.

# 4. Add column estimated age to make it easier to assess whether differences are likely or not 

dat <- dat %>%
  mutate(date_of_birth = as.Date(date_of_birth, format = "%d/%m/%Y"),
         date_of_measure = as.Date(date_of_measure, format = "%d/%m/%Y"),
         age_estimated = as.numeric(interval(date_of_birth, date_of_measure) / dyears(1)))


#5. Check how many ind. don't have age, or don't have dob, or date of measure
sum(is.na(dat[["age_estimated"]])) #5 observations don't have age

#6. Remove rows that don't have age, or age can't be calculated bc missing dates
miss_age <- dat %>% filter(is.na(age_estimated)) # Of 5 missing, 4 correspond to all the obs.for ind 88
# ind. 88 doesn't have both dob for any obs., so remove all info for ind.
# remove obs.id #scc7m for ind. 98, that doesn't have dob.
dat <- dat %>%
  filter(person_id != "88",
         obs_id != "scc7m")

#7. Count number of people in the dataframe
dat %>% distinct(person_id) %>%
  count() # 280 individuals in the dataframe

#8. Plot heights to catch evident outliers
library(ggplot2)
dev.off() 
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point() # everything looks fine

dev.off() 
ggplot(dat, aes(x =age_estimated, y = weight_kg , group = person_id))+
  geom_line() +
  geom_point() #everything looks fine, one ind. is consistently heavier than the rest.


# 9. Check if all individuals have sex 

miss_sex <- dat %>% filter(is.na(sex)) #this indicate 0 NA for sex but there are some empty cells.

library(stringr)
fil <- dat %>%
  group_by(person_id) %>%
  filter(any(str_detect(sex, "^\\s*$"))) #This detects 91 observations where sex is empty

empty_sex_individuals <- dat %>%
  group_by(person_id) %>%
  filter(any(str_detect(sex, "^\\s*$"))) %>% # 9 individuals have missing sex. Originally 10 
  select(person_id) %>% # but already removed ind. 88 above.
  distinct()

####### On Oct. 24 I emailed K. Wander to ask whether these 10 individuals have actual missing data 
### On Oct.29 she replied the e-mail indicating the sex of 3 ind"
#88: f  #ind.88 was already removed above due to missing dob
#187: f 
#188: m

# Add sex to inds. 187 and 188. 

dat <- dat %>%
  mutate(sex = ifelse(person_id == 187 & str_detect(sex, "^\\s*$"), "f", sex))

dat <- dat %>%
  mutate(sex = ifelse(person_id == 188 & str_detect(sex, "^\\s*$"), "m", sex))

# 10. Add count of obs. per ind. as new column 
dat <- dat %>%
  add_count(person_id, name = "obs_counts")

#11. Create new df with ind. that have at least two obs. for additional checks
long <- dat %>%
  filter(obs_counts >= 2)

#11.Check obs. that go down more than 3 cm
down <- long %>%
  arrange(person_id, age_estimated) %>%  # Arrange data by person_id and age
  group_by(person_id) %>%    # Group data by person_id
  mutate(obs_diff = height_cm - lag(height_cm, default = first(height_cm))) %>%
  ungroup() %>%
  filter (obs_diff <=-3)

#12. #count n of obs that goes down.
down %>%
  distinct(person_id) %>%
  count() # two individuals shrink >3 cm :144 and 249

# a1. ind.144, obs #tw4l1 height is inconsistent with trajectory. Remove this height, weight seems ok.
dat$height_cm [which(dat$obs_id=="tw4l1")] <-NA

# a2. ind. 249, obs #ok853 most likely a typo considering previous and after heights. 

dat$height_cm [which(dat$obs_id=="ok853")] <- 84.5 # changed from 81.5

#13. Check how many height obs. goes up more than 10cm in a period of 1 year or less.

# ---update long first:
long <- dat %>%
  filter(obs_counts >= 2) 

up <- long %>% 
  arrange(person_id, age_estimated) %>%
  group_by(person_id) %>% 
  mutate(time_elap = as.numeric (age_estimated - first(age_estimated)),
         obs_diff = height_cm - lag(height_cm, default = first(height_cm))) %>%
  ungroup() %>%
  filter(person_id %in% unique(person_id[obs_diff >= 10 & time_elap < 1])) # NO obs, go up more than 10cm

#i <- 1
#print(long[long$person_id == up$person_id [i], ]) 
#i <- i + 1

#14. Indicate how many obs. were removed 
sum(is.na(dat[["height_cm"]])) # 68 height are NA
sum(is.na(dat[["weight_kg"]])) #8 weight are NA

# #2789 obs.loaded; KEPT 2777  (12 obs. removed entirely) 
#  74 NA height loaded; now  68
# 14  weight loaded; kept 8  weight NA

### The numbers of NA needs to be. updated once Katherine Wander replies e-mail 


#write.csv(dat, "site_measurements.csv",row.names = FALSE)