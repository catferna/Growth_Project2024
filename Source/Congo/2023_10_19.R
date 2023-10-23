############################################################
####### Cleaning Congo (BaYaka and Bandongo) data ########
###########################################################

#starting commit: 8eac5b5
#Author: catalina fernandez

# !!!! individuals have no dates of birth only years. !!!!!
# *** Talk to John and Caissa about how to create a range *********

setwd ("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Congo/")

library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv") # 516 observations loaded

#####  Checks to ensure consistency #### 
colnames (dat)

#1. remove observations that do not have BOTH height and weight. Keep if only one is missing
sum(is.na(dat[["height_cm"]])) # 40 NA height
sum(is.na(dat[["weight_kg"]])) # 0 NA height

dat <- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) #no rows where both height and weight are NA

#2. Check if there are duplicated rows
rep <- dat %>% select(-obs_id)
sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) # NO duplicated obs. 

# 3. Check how many people have X observations?
table(table(dat$person_id)) # Most individuals have 1 obs, and half have two.

# 4. Add column estimated age to make it easier to assess whether differences are likely or not 
### !!! discuss this with Caissa and John

#5. Check how many ind. don't have age, or don't have dob, or date of measure
sum(is.na(dat[["birth_year"]])) # 7
sum(is.na(dat[["date_of_measure"]])) # 117 observations don't have date_of measure
sum(is.na(dat[["observation_year"]])) # ALL observations have observation year, at least
nrow(filter(dat, is.na(birth_year) & is.na(date_of_measure))) # No rows have both date of birth and date of obs. missing

### !!! Discuss this with Caissa and John about what to do in these cases.

#6. Remove rows that don't have age, or age can't be calculated bc missing dates 
## Remove all these observations??!! ## !!!!!!!!!! ??????????
### discuss this with Caissa and John

#7. Count number of people in the dataframe

dat %>% distinct(person_id) %>%
  count()  # 392 individuals

#8. Plot heights to catch evident outliers 

# CAN''T PLOT WITHOUT AGES. 

library(ggplot2)
dev.off() 
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point() #

dev.off() 
ggplot(dat, aes(x = age_estimated, y = weight_kg , group = person_id))+
  geom_line() +
  geom_point() #


# check ind. younger than 1yo ???

#9.add count of obs. per ind. as new column 

#10. Create new df with ind. that have at least two obs. for additional checks
long <- dat %>%
  filter(obs_counts >= 2)

#11.Check obs. that go down more than 3 cm

down <- dat %>%
  arrange(person_id, age_estimated) %>%  # Arrange data by person_id and age
  group_by(person_id) %>%    # Group data by person_id
  mutate(obs_diff = height_cm - lag(height_cm, default = first(height_cm))) %>%
  ungroup() %>%
  filter (obs_diff <=-3)


#12. #count n of obs that goes down.
down %>%
  distinct(person_id) %>%
  count() # ???

i <- 1
print(long[long$person_id == down$person_id [i], ]) 
i <- i + 1


#13. Check how many height obs. goes up more than 10cm in a period of 1 year or less.

# ---update long first:
long <- dat %>%
  filter(obs_counts >= 2) #3488 obs.


###
up <- long %>% 
  arrange(person_id, age_estimated) %>%
  group_by(person_id) %>% 
  mutate(time_elap = as.numeric (age_estimated - first(age_estimated)),
         obs_diff = height_cm - lag(height_cm, default = first(height_cm))) %>%
  ungroup() %>%
  filter(person_id %in% unique(person_id[obs_diff >= 10 & time_elap < 1])) # ???

i <- 1
print(long[long$person_id == up$person_id [i], ]) 
i <- i + 1



#14. Indicate how many obs. were removed 
sum(is.na(dat[["height_cm"]])) 
sum(is.na(dat[["weight_kg"]])) 

# ?
# ?

#write.csv(dat, "site_measurements.csv",row.names = FALSE)




