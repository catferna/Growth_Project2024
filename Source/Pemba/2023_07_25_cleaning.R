############################################################
### Cleaning Pemba, Tanzania Data  ####
###########################################################

#Author: catalina fernandez
#starting commit: da82567

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Pemba/")

library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv")

#####  checks to ensure consistency:

#1. remove observations that do not have BOTH height and weight.keep if only one is missing

sum(is.na(dat[["height_cm"]])) # NA = 0
sum(is.na(dat[["weight_kg"]])) # NA = 0


# check if there are duplicated rows.
rep <- dat %>% select(-obs_id)
sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) #NO duplicated observations
    
#count number of people in the dataframe
dat %>%
  distinct(person_id) %>%
  count() #1646 ind. 

# how many people have X observations?
table(table(dat$person_id)) # aprox. third are single obs, but most are longit.
#1   2    3   4   5    6   7   8   9  10   11 
#657 385 197 140  86  78  50  25  18   8   2

dat <- dat %>%
  mutate (date_of_birth = as.Date(date_of_birth),
          date_of_measure = as.Date(date_of_measure),
age_estimated = as.numeric(interval(date_of_birth, date_of_measure) / dyears(1)))


# plot heights to catch evident outliers

glimpse(dat)
#plot heights
library(ggplot2) 

dev.off() 
ggplot(dat, aes(x = age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point()

#Check all obs. for each individual to change/remove obs based on general trajectory
#MANY individuals have heights that are "999" Replace 999 in 705 rows with NA

sum(dat$height_cm == 999, na.rm = TRUE) 

dat <- dat %>%
  mutate(height_cm = replace(height_cm, height_cm == 999, NA)) #Replaced "999" in 705 rows with NA

#count n of obs. per ind.
dat <- dat %>%
  add_count(person_id, name = "obs_counts")

# individuals that have at least two observations

long <- dat %>%
  filter(obs_counts >= 2)

# flag observations whose heights go DOWN 3 cm or more.
# add column estimated age to make it easier to assess whether differences are likely or not 

down <- long %>%
  group_by (person_id) %>%
  arrange(age_estimated) %>%
  summarize(min_height_diffs=min(diff(height_cm))) %>%
  filter (min_height_diffs <=-3)

#count n of obs that goes down.
down %>%
  distinct(person_id) %>%
  count() # 15 individuals "shrink" 3 or more cm. 

i <- 1
print(long[long$person_id == down$person_id [i], ]) 
i <- i + 1

# -1. both height and weight for #wgjnt seem off the trajectory for ind.1603
dat <- dat %>%
  filter(obs_id != "wgjnt") 

# -2. ind. 2402 "shrink" ±4cm over 2yrs. Ind>60yrs and this is ±expected; left as it is.

# -3. ind. 7901 "shrink" 4cm over 6yrs. Ind>50yrs and this is ±expected; left as it is.

# -4. .ind. 8102 "shrink" 10.5cm. Only 2 observations, so remove individual entirely.
dat <- dat %>%
  filter(person_id != "8102")

# -5. ind. 9704 "shrink" 6cm. Only 2 observations, so remove individual entirely.
dat <- dat %>%
  filter(person_id != "9704")

# -6 ind. 10208. "shrink" and the 5 obs. are inconsistent, possibly more than one ind.
dat <- dat %>%
  filter(person_id != "10208") #remove all obs for ind.10208

# -7 ind. 10302 "shrink" 6cm. Only 2 observations, so remove all obs. for ind.
dat <- dat %>%
  filter(person_id != "10302") #remove all obs for ind.10302

# -8 ind. 11808 "shrink" 4.5 cm and is 25yo. Only 2 observations, so remove all obs. for ind.
dat <- dat %>%
  filter(person_id != "11808") #remove all obs for ind.11808

# -9 ind. 13302 obs. #b18po is off the trajectory for height and weight. RM #b18po obs.
dat <- dat %>%
  filter(obs_id != "b18po") #remove obs.#b18po

# -10 ind. 14105 obs. #x9psz off the trajectory and too close to previous measure
#probably  #x9psz is a different individual. Remove obs.# #x9psz entirely.
dat <- dat %>%
  filter(obs_id != "x9psz") #remove obs.#x9psz

# -11 ind. 15312 "shrink" 5 cm and is 25yo. Only 2 observations, so remove all obs for ind.
dat <- dat %>%
  filter(person_id != "15312") #remove all obs for ind.15312

# -12 ind. 15403 shrink" 16.5cm and is 25yo. Only 2 observations, so remove all obs for ind.
dat <- dat %>%
  filter(person_id != "15403") #remove all obs for ind.15403

# -13 ind. 17102 shrink 10cm. Both height and weight are off the trajectory for obs.#jr38j 
dat <- dat %>%
  filter(obs_id != "jr38j") #remove obs.#jr38j

# -14 ind. 17602 shrink 5cm. Remove only height for obs. #nd81a
dat$height_cm [which(dat$obs_id=="nd81a")] <-NA

# -15 ind. 20202 shrink 6cm.Only 2 observations, so remove all obs for ind.
dat <- dat %>%
  filter(person_id != "20202") #remove all obs for ind.20202

####### !!!check that all obs for a male/female are male/female, respectively

sex_mismatch <- dat %>%
  group_by(person_id) %>%
  summarise(same_sex = n_distinct(sex) == 1) %>%
  filter(!same_sex) %>%
  pull(person_id)
print (sex_mismatch) # 5 indiv. have sex mismatches across obs. Remove or fix.

ids_df <- data.frame(person_id = unlist(sex_mismatch))
check_sex <- dat %>%
  inner_join(ids_df, by = "person_id")


#-1. ind.615. Only 2 obs (one F and one M). Remove obs #2jb64 as that doesn't have height measurement. 
dat <- dat %>%
  filter(obs_id != "2jb64") #remove obs.#2jb64

#-2. ind.6806. 3 obs as M, one as F (#h3nfz) Remove female obs #h3nfz)
dat <- dat %>%
  filter(obs_id != "h3nfz") #remove obs.#h3nfz 

#-3. ind.14613 only 2 obs. (one F and one M). Remove female obs #poi6h since doesn't have height.
dat <- dat %>%
  filter(obs_id != "poi6h") #remove obs.#poi6h

#-4 ind.19003 obs #yj5gv clearly a different individual(F) than other 9 obs. Remove #yj5gv
dat <- dat %>%
  filter(obs_id != "yj5gv") #remove obs.#yj5gv

#-5. ind. 19804 has 3 obs, 2 M and one F. Remove female obs. #mqg30
dat <- dat %>%
  filter(obs_id != "mqg30") #remove obs.#mqg30

### Check inds whose heights go up >=10cm/year ###

long <- dat %>% #update "long"
  filter(obs_counts >= 2) 

up <- long %>%
  group_by(person_id) %>%
  arrange(date_of_birth) %>%
  mutate(time_elap = as.numeric (date_of_measure - first(date_of_measure)),
         obs_diff = height_cm - lag(height_cm)) %>%
  filter(person_id %in% unique(person_id[obs_diff >= 10 & time_elap < 365]))

up %>%
  distinct(person_id) %>%
  count() #only one ind. and a child so leave it as it is. 


### Check heights of individuals < 1 year old to see if there are any biologically implausible values

# height of ind. 35003, obs.#25oob seems impossible for a 10kg and 1yo. Convert #25oob to NA
dat$height_cm [which(dat$obs_id=="25oob")] <-NA

#height and weight for ind. 6325 are extremely off. Remove individual as there's only a single observation. 
dat <- dat %>%
  filter(person_id != "6325") #remove all obs for 6325

#height and weight for ind. 39306 are off. Only one observation; remove individual 
dat <- dat %>%
  filter(person_id != "39306") #remove all obs for 39306

# flag ind younger than 12yo and taller than 150cm

tall <- dat %>% 
  filter (age_estimated < 12 & height_cm > 150) #2 observations are extremely off. Remove

dat <- dat %>%
  filter(obs_id != "tsw4p") #remove obs.#tsw4p 172cm and 70.2 kg for a 10yo is off from any pop. trajectory


dat <- dat %>%
  filter(obs_id != "oaoik") ##remove obs.#oaoik 167 and 66 kg for a 9yo is off from any pop. trajectory

# flag inds older than 12yo and shorter than 100cm
short <- dat %>% 
  filter (age_estimated >12 & height_cm <100) # one individual 5433

#remove ind. 5433 height is extremely short (96cm) and weight too low for age (12.5 yo). 
dat <- dat %>%
  filter(person_id != "5433") #remove ind. 5433

#Introduced 703 NA in heigths from "999" and other implausible values
#no NA weights were added. Zero NA for height
# From 1646 ind. in original dataset, kept 1635

write.csv(dat, "site_measurements.csv",row.names = FALSE)
