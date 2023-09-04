############################################################
###### Cleaning Mamiraua data - Ribeirinhos, Brazil #######
###########################################################

#starting commit: b2d465f

#Author: catalina fernandez
setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Mamiraua//")


library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv")

#####  checks to ensure consistency:
colnames (dat)
#1. remove observations that do not have BOTH height and weight. Keep if only one is missing

sum(is.na(dat[["height_cm"]])) # 2 rows don't have height 
sum(is.na(dat[["weight_kg"]])) # 352 rows don't have height 

dat <- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) #No rows have both height and weight missing; no row removed.


#2. Check if there are duplicated rows.
rep <- dat %>% select(-obs_id)
sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) # 4 rep. measures. 

#check observations:
duplicated_rows <- rep[duplicated(rep) | duplicated(rep, fromLast = TRUE), ] # 2 rows are duplicated
#ind. BOAJO-01-03, obs.#qv9jm and #ptcra and ind.MAGUA-04-04 , obs. #oh957 and # jc443

dat <- dat %>%
  filter(obs_id != "oh957" & obs_id != "ptcra") #removing one of the dupl. for each ind. 

# 3. Check how many people have X observations?
table(table(dat$person_id)) #Most observations (n=341) are repeated observations (n=2)
#but most have only repeated height but no repeated weight.
#repeated measures are taken ON THE SAME DAY, so no actual longitudinal measurements. 

# 4. Add column estimated age to make it easier to assess whether differences are likely or not 
dat <- dat %>%
  mutate (date_of_birth = as.Date(date_of_birth),
          date_of_measure = as.Date(date_of_measure),
          age_estimated = as.numeric(interval(date_of_birth, date_of_measure) / dyears(1)))

#5. Check how many ind. don't have age, or don't have dob, or date of measure
sum(is.na(dat[["age_estimated"]])) #8 missing ages due to missing birth dates

#6. Remove rows that don't have age, or age can't be calculated bc missing dates 
dat <- dat %>%
  filter(!is.na(age_estimated)) # 8 obs. remove


#7. Count number of people in the dataframe
dat %>% distinct(person_id) %>%
  count()  #353 ind. 

#8. Plot heights to catch evident outliers

library(ggplot2)
dev.off() 
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point() #everything looks fine

# plot shows one 7yo ind. (	VIOPA-03-06) that's too tall for his age (±146cm). 
# left as it is as although extreme, could be possible. 

#9.Count n of obs. per ind.
dat <- dat %>%
  add_count(person_id, name = "obs_counts")

# 10. Create new df with ind. that have at least two obs. for additional checks
long <- dat %>%
  filter(obs_counts >= 2)

#11. Heights are taken on same day so allow only allow minimal error
down <- long %>%
  group_by (person_id) %>%
  arrange(age_estimated) %>%
  summarize(min_height_diffs=min(diff(height_cm))) %>%
  filter (min_height_diffs < -1)

#count n of obs that goes down.
down %>%
  distinct(person_id) %>%
  count() #NO obs goes down more than 1cm., so keep all

#12. Check how many height goes up more than 1cm.
up <- long %>%
group_by(person_id) %>%
  arrange(age_estimated) %>%
  mutate(time_elap = as.numeric (age_estimated - first(age_estimated)),
         obs_diff = height_cm - lag(height_cm)) %>%
  filter(person_id %in% unique(person_id[obs_diff > 1]))

#Check all obs. for each individual to change/remove obs based on general trajectory
# Ind. ACARI-04-02 has two observations, and one is 10 cm higher than the first (obs #1vrdv)
# seems to be a typo 163->173 but will delete entire row as second obs.#1vrdv doesn't have weight
dat <- dat %>%
  filter(obs_id != "1vrdv") #removed all data for obs_id #1vrdv

#keep repeated measures of the same day; ask PEDRODG about the order of the measurements 


#13. Check heights of individuals < 1 year old to see if there are any biologically implausible values
#heights/weights of small children look fine.

#14. Indicate how many NA where introduced to the data 
#No NA where introduced 

#write.csv(dat, "site_measurements.csv",row.names = FALSE)
