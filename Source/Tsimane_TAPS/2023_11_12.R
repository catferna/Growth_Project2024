###########################################################
################ Cleaning Tsimane TAPS data ################
###########################################################

# starting commit: 
#Author: catalina fernandez


setwd ("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Tsimane_TAPS/")

library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv") # 8509 obs. loaded

#####  Checks to ensure consistency #### 
colnames (dat)

#1. remove observations that do not have BOTH height and weight. Keep if only one is missing
sum(is.na(dat[["height_cm"]])) # 324 height measurements missing   
sum(is.na(dat[["weight_kg"]])) # 330 weight measurements missing
dat <- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) #308 obs. removed 

#2. Check if there are duplicated rows
rep <- dat %>% select(-obs_id)

sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) # 4 obs.have duplicates; check 
dupl <- rep[duplicated(rep) | duplicated(rep, fromLast = TRUE), ] # check dupl.

# Remove duplicated obs, if any (obs_id = xxxx ; or person_id = xxxx)
#2a.ind. 256, obs.#xjkoc 
dat <- dat %>% filter(obs_id != "xjkoc")

#2b. ind. 464, obs.#xehwt
dat <- dat %>% filter(obs_id != "xehwt")

#dat <- dat %>% filter(person_id != "xxx")


# 3. Check how many people have X observations?
table(table(dat$person_id)) # most ind. have 6 obs. 

# 4. Add column 'estimated age' to make it easier to assess whether differences are likely or not 
### ! Check first the format of input dates, such as whether month is before day, whether numbers
# or month names are used, and n of digits for the year field, etc.

#### ----> no dates, only age

# mutate(date_of_birth = as.Date(date_of_birth, format = "%d/%m/%Y"),
  #      date_of_measure = as.Date(date_of_measure, format = "%d/%m/%Y"),
  #       age_estimated = as.numeric(interval(date_of_birth, date_of_measure) / dyears(1)))


#5. Check how many ind. don't have age, or don't have dob, or date of measure
sum(is.na(dat[["age_estimated"]])) #  2022 observations don't have age!!!

#6. Remove rows that don't have age, or age can't be calculated bc missing dates
miss_age <- dat %>% filter(is.na(age_estimated)) # 2022 obs don't have dates
ind_agemiss <- unique (miss_age$person_id) #623 ind. 

dat <- dat %>%
  filter(!person_id %in% ind_agemiss) # 2022 obs (623 ind.) removed

#dat <- dat %>%
#  filter(person_id != "xxx", # to remove entire individual
 #        obs_id != "xxx") # to remove only a specific row

#7. Count number of people in the dataframe
dat %>% distinct(person_id) %>%
  count() #1402 individuals in the dataframe

#8. Plot heights to catch evident outliers
library(ggplot2)
dev.off() 
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point() # one ind. has height of almost 2m, check:

#8a. ind. 272, obs. #xzqgs; height of f ind. is 196.8; other obs for ind. are ±146
#most likely typo of 146.8; weight for obs. looks fine.
dat$height_cm [which(dat$obs_id=="xzqgs")] <- 146.8

dev.off() 
ggplot(dat, aes(x =age_estimated, y = weight_kg , group = person_id))+
  geom_line() +
  geom_point() #everything looks fine, one ind. is consistently heavier than the rest.


# 9. Check if all individuals have sex 

miss_sex <- dat %>% filter(is.na(sex)) #   NO cells where sex is NA
#Often, cells are empty instead of having a NA. The following script get those:
library(stringr)
fil <- dat %>%
  group_by(person_id) %>%
  filter(any(str_detect(sex, "^\\s*$"))) # NO sex cells that are empty
empty_sex_individuals <- dat %>%
  group_by(person_id) %>%
  filter(any(str_detect(sex, "^\\s*$"))) %>% # NO individuals have missing sex. 
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
  count() # 61 individuals shrink >3 cm  

i <- 1
print(long[long$person_id == down$person_id [i], ]) 
i <- i + 1

#13a1.ind. 61, obs. jxvy0; both height and weight are VERY off the ind. trajectory
dat <- dat %>% filter(obs_id != "jxvy0" ) #remove entire obs.

# 13a2. ind 85, obs. #xdf27 both height and weight are VERY off the ind. trajectory; probably different ind.
dat <- dat %>% filter(obs_id != "xdf27") #remove entire obs.

# 13a3. ind 120, obs. #qkbqr both height and weight are VERY off the ind. trajectory; probably different ind.
dat <- dat %>% filter(obs_id != "qkbqr")  #remove entire obs.

# 13a4. ind 179, obs. #9ije8 is off the trajectory for both height and weight. 
dat <- dat %>% filter(obs_id != "9ije8")  #Remove entire observation.

# 13a5. ind 200, obs. #ynr6. Height of 152cm is off the trajectory; weight seems ok
dat$height_cm [which(dat$obs_id=="ynr6n")] <-NA #remove only height 

# 13a6. ind 208, obs. #b3ki2. Height of 140cm is off the trajectory; weight seems ok.
dat$height_cm [which(dat$obs_id=="b3ki2")] <-NA #remove only height 

# 13a7. ind 241, obs. #0webq. Weight seems ok but there's most likely a typo in height (160cm) according to other values
dat$height_cm [which(dat$obs_id=="0webq")] <- 168.4

# 13a8. ind.253, obs #rz5wv.  Weight seems ok but there's most likely a typo in height (159.1cm) according to other values
dat$height_cm [which(dat$obs_id=="rz5w")] <- 153.1

# 13a9. ind 339, obs. #taw3f. Weight seems ok but there's most likely a typo in height (83.4cm) according to rest of the trajectory
dat$height_cm [which(dat$obs_id=="taw3f")] <- 93.1 #changed from 83.1, considering previous and following values 

# 13a10. ind 358 , obs. #i9bvj. Both height and weight are very off the ind. trajectory. 
dat <- dat %>% filter(obs_id != "i9bvj.")  #Remove entire observation.

# 13a11. ind 366, obs. #fbs4t. Height (155cm) is off the trajectory by 10cm, most likely a typo
dat$height_cm [which(dat$obs_id=="fbs4t")] <- 165 #changed from 155; according to rest of trajectory.

# 13a12. ind 423 , obs. #k0e2c. Height (142.6cm) is about 10cm off. Change to likely value 152.6 
dat$height_cm [which(dat$obs_id=="k0e2c")] <- 152.6

# 13a13. ind 472 , obs. #w72rj. Height (180.4cm) is ~20cm off the trajectory. Change to likely value 160.4
dat$height_cm [which(dat$obs_id=="w72rj")] <- 160.4

# 13a14. ind 544 , obs. #uybde. Height is off the trajectory
dat$height_cm [which(dat$obs_id=="uybde")] <- NA #remove height observation

# 13a15. ind 558 , obs. #e53ib. Height (80.8 cm) is off the trajectory but most likely a typo, given previous and following values.
dat$height_cm [which(dat$obs_id=="e53ib")] <- 90.8 #changed from 80.8

# 13a16. ind 596, obs. #cd3wg . Probably an error measurement in height, Weight seems fine
dat$height_cm [which(dat$obs_id=="cd3wg")] <- NA #remove height observation

# 13a17. ind 614 , obs. #bkwjb and #dd3q8 are off 3.2cm from the previous height observation. 
#left as it is

# 13a18. ind 631 , obs. #4obss. Height is off the trajectory.
dat$height_cm [which(dat$obs_id=="4obss")] <- NA #remove height observation

# 13a19. ind 649, obs. #757on and #28za0 are the first and second obs. Hard to know which is wrong.
dat$height_cm [which(dat$obs_id=="757on")] <- NA #remove both height observations
dat$height_cm [which(dat$obs_id=="28za0")] <- NA #remove both height observations

# 13a20. ind 768, obs. #uzw2k. Both height and weight are very off the trajectory.Prob. different ind.
dat <- dat %>% filter(obs_id != "uzw2k")  #Remove entire observation.

# 13a21. ind 798, obs. #56i96. Both height and weight are off the trajectory. Prob. different ind.
dat <- dat %>% filter(obs_id != "56i96")  #Remove entire observation.

# 13a22. ind 799, obs. #cbsem. Height (148.7) is off the trajectory. 
dat$height_cm [which(dat$obs_id=="cbsem")] <- NA

# 13a23. ind , obs. # xx

# 13ax. ind , obs. # xx

# 13ax. ind , obs. # xx

# 13ax. ind , obs. # xx

# 13ax. ind , obs. # xx

# 13ax. ind , obs. # xx
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

