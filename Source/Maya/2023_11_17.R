###########################################################
################ Cleaning Yucatec Maya data ################
###########################################################

# starting commit: 231a9e9
#Author: catalina fernandez

setwd ("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Maya/")

library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv ("site_measurements.csv") # 1690 obs. loaded

#####  Checks to ensure consistency #### 
colnames (dat)

#1. remove observations that do not have BOTH height and weight. Keep if only one is missing
sum(is.na(dat[["height_cm"]])) # 2 height measurements missing   
sum(is.na(dat[["weight_kg"]])) # 3 weight measurements missing
dat <- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) #removed 2 obs. that didn't have both height and weight

#2. Check if there are duplicated rows
rep <- dat %>% select(-obs_id)

sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) # 374 duplicated obs. 
# This is consistent with Kramer's note, who indicated that 2011 obs. were duplicated
dupl <- rep[duplicated(rep) | duplicated(rep, fromLast = TRUE), ] # ?? 

# Remove duplicated obs., in this case we know these are in 2011 at least.
dat <- dat %>%
  mutate(date_of_measure = as.Date (date_of_measure)) %>%
  filter(!(year(date_of_measure) == 2011 & duplicated (person_id))) #1330 obs. left

#There are 4 additional duplicates; remove 2 obs. for inds. 491 (obs. id #8dp6d) and 671 (obs id #lr6pv)
#Remove duplicated for ind.491
dat <- dat %>% 
  filter(obs_id != "8dp6d")

#Remove duplicated for ind.671
dat <- dat %>%
  filter(obs_id != "lr6pv")

# 3. Check how many people have X observations?
table(table(dat$person_id)) # most ind. have 8 obs.

# 4. Add column 'estimated age' to make it easier to assess whether differences are likely or not 
### ! Check first the format of input dates, such as whether month is before day, whether numbers
# or month names are used, and n of digits for the year field, etc.
#first check that dob are consistent  within and ind.

#4a. Check that dates of birth are consistent across observations within an individual 
dob_mismatch <- dat %>%
  group_by(person_id) %>%
  filter(n_distinct(date_of_birth) > 1) %>%
  ungroup() # 

dob_mismatch %>%
  distinct(person_id) %>%
  count() # 8 inds have dob mismatch

#4a1.ind.668, obs.id #zj9b2 has dob 2010-05-02 but other obs. for this ind. have 2010-05-25
dat$date_of_birth [which(dat$obs_id=="zj9b2")] <- "2010-05-25"

#4a2. ind.727, obs. #ddf07 has a dob 2011-07-17 but other obs for ind.have 2011-08-31
dat$date_of_birth [which(dat$obs_id=="ddf07")] <- "2011-08-31"

#4a3. ind.745, obs. #5vdjq has a dob of 2011-12-24 but other obs. for ind. have 2011-12-25
dat$date_of_birth [which(dat$obs_id=="5vdjq")] <- "2011-12-25"

#4a4. ind.751, obs.#wi7d9 has a dob of 2012-07-28 but other obs. for ind. have 2012-06-29
dat$date_of_birth [which(dat$obs_id=="wi7d9")] <- "2012-06-29"

#4a5.ind.754 obs. #frypy has a dob of 2012-05-01 but other obs. for ind. have 2012-06-12
dat$date_of_birth [which(dat$obs_id=="frypy")] <- "2012-06-12"

#4a6.ind.762 obs. #w76ce has a dob of 2012-08-22 but other obs. for ind. have 2012-08-28 
dat$date_of_birth [which(dat$obs_id=="w76ce")] <- "2012-08-28"

#4a7.ind.789 has 2 obs; obs.#1a8ok (dob=2015-01-21) and obs.#0z1c6 (dob=2015-01-22).1-day difference
#keep it as it is for now but maybe create a range. 

#4a8.ind. 740 dob is 2010-08-30, but for obs.id #s9ri4 is 2011-08-30. Modify:
dat$date_of_birth [which(dat$obs_id=="s9ri4")] <- "2010-08-30"

#Create age_estimated using dob and date of measure
dat <- dat %>%
  mutate(date_of_birth = as.Date(date_of_birth),
         date_of_measure = as.Date(date_of_measure),
         age_estimated = as.numeric(interval(date_of_birth, date_of_measure) / dyears(1)))



#5. Check how many ind. don't have age, or don't have dob, or date of measure
sum(is.na(dat[["age_estimated"]])) # All observations have age

#6. Remove rows that don't have age, or age can't be calculated bc missing dates
miss_age <- dat %>% filter(is.na(age_estimated)) # NO missing dates
#dat <- dat %>%
 # filter(person_id != "xxx", # to remove entire individual
    #     obs_id != "xxx") # to remove only a specific row

#7. Count number of people in the dataframe
dat %>% distinct(person_id) %>%
  count() # 301 individuals in the dataframe

#8. Plot heights to catch evident outliers
library(ggplot2)
dev.off() 
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point() # some weird looking trajectories, check ind. >8yo and shroter than 100cm

tooshort <- dat %>%
  filter(age_estimated > 8, height_cm < 100) # 3 ind.to check (ind.484, ind.644, ind.716)

#ind.484; obs.#0v59g clearly a typo (44.5cm), as two previous obs. are 143.2 and 143.9)
dat$height_cm [which(dat$obs_id=="0v59g")] <- 144.5  #(from 44.5cm)

#ind.644; height is quite low for age (99.4cm for 8.2yo) but it's the only obs. for ind., keep it

#ind.716; obs.id #z37e2 both height and weight are extremely off for age, 
#clearly wrong (77.9cm and 9.8 kg for a 15yo); remove entire row
dat <- dat %>%
  filter(obs_id != "z37e2")

#ind. 715, obs. #iy8to. 108 cm a d 15kg for a 14yo. Remove this observation as unreliable
dat <- dat %>%
  filter(obs_id != "iy8to")

#ind.714, obs.#yndy2 is quite short (124cm) and light (27kg) for age (16yo) but
#it's the first measurement and next is 2 years after so keep it as it is.

dev.off() 
ggplot(dat, aes(x =age_estimated, y = weight_kg , group = person_id))+
  geom_line() +
  geom_point() #everything looks Ok, except obs.#yndy2 (27kg) ind.714 referred above.


# 9. Check if all individuals have sex 

miss_sex <- dat %>% filter(is.na(sex)) # All obs. have assigned sex.

#Often, cells are empty instead of having a NA. The following script get those:
library(stringr)
fil <- dat %>%
  group_by(person_id) %>%
  filter(any(str_detect(sex, "^\\s*$"))) #no inds. have missing sex
empty_sex_individuals <- dat %>%
  group_by(person_id) %>%
  filter(any(str_detect(sex, "^\\s*$"))) %>% #no inds. have missing sex
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
  filter (obs_diff <=-3) # 3 obs. 

#13. count n of obs that goes down.
down %>%
  distinct(person_id) %>%
  count() # 3 individuals shrink >3 cm: 440, 491, and 619

# a1. #ind.440, obs.id #j0t1s goes down 3.4cm; probably a typo but can't guess height
dat$height_cm [which(dat$obs_id=="j0t1s")] <- NA #converting height to NA, weight looks OK

# a2. #ind. 491, obs.id #8c4ql goes down 6.9cm and is taken only 1month after the previous.
#remove entire observation, as weight also goes down > 4kg in a month. Probably wrong entry id
dat <- dat %>%
  filter(obs_id != "8c4ql")

# a3.#ind.619, obs.id #vhtuc; goes down 4 cm, probably typo. Modify from 141.5 to 151.5
dat$height_cm [which(dat$obs_id=="vhtuc")] <- 151.5



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
  filter(person_id %in% unique(person_id[obs_diff >= 10 & time_elap < 1])) # NO obs, go up more than 10cm/YEAR

#i <- 1
#print(long[long$person_id == up$person_id [i], ]) 
#i <- i + 1

#15. Indicate how many obs. were removed 
sum(is.na(dat[["height_cm"]])) # 1 height is NA
sum(is.na(dat[["weight_kg"]])) # 1 weight is NA

# 1690 obs. loaded; KEPT 1325  (365 obs. removed entirely, mostly '2011' duplicates) 
# 2 NA height loaded; now 1
# 3  weight loaded; kept 1  weight NA


#write.csv(dat, "site_measurements.csv",row.names = FALSE)
