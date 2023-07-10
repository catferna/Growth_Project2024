###################################################################
### Checking and cleaning Afrocolombians site_measurements.csv  ###
###################################################################

#author: catalina fernandez
#starting commit: 34b2863

rm(list = ls())

library (tidyverse)
library(lubridate)

setwd ("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Afrocolombians")

dat <- read.csv ("site_measurements.csv")

#1. remove observations that do not have BOTH height and weight.keep if only one is missing

sum(is.na(dat[["height_cm"]]))
sum(is.na(dat[["weight_kg"]]))

dat<- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) #this keep 13NA and 6NA heights and weights 

# check if there are duplicated rows.

rep <- dat %>% select(-obs_id)
sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) #no duplicated


# how many people have X observations?
table(table(dat$person_id)) #Most observations are single observations 


# flag observations whose heights go DOWN at least 5 cm
# add column estimated age to make it easier to assess whether differences are likely or not 
dat <- dat %>%
  mutate (estim_date_birth = as.Date(estim_date_birth),
         date_of_measure = as.Date(date_of_measure,format = "%d/%m/%Y"),
         date_of_measure = format(date_of_measure, "%Y-%m-%d"),
         age_estimated = as.numeric(interval(estim_date_birth, date_of_measure) / dyears(1)))

# individuals that have at least two observations
long <- dat[dat$person_id %in% names(obs_counts[obs_counts >= 2]), ]

library(ggplot2)
dev.off() 
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point()

# From individuals that have at least 2 obs(in "long"), check ind. whose weights go down
#more than 3 cm, and convert those observations to NA

down <- long %>%
  group_by (person_id) %>%
  arrange(date_of_measure) %>%
  mutate(obs_dif = height_cm - lag(height_cm))  %>%
  filter(any (obs_dif < -3))

# For individual B7D, obs. #gu14n is 6cm smaller than previous. Converted to NA
dat$height_cm [which(dat$obs_id=="gu14n")] <- NA

# For individual GEB, obs. #y64qx is 4cm smaller than previous. Converted to NA
dat$height_cm [which(dat$obs_id=="y64qx")] <- NA 

# For individual M82, obs. #0c3qi is 5cm smaller than previous. Converted to NA
dat$height_cm [which(dat$obs_id=="0c3qi")] <- NA 

# For individual R6N, obs. #1k0jd is 4 cm smaller than previous. Converted to NA
dat$height_cm [which(dat$obs_id=="1k0jd")] <- NA 

# For individual SYE, obs. #v0qzg is 3.5 cm smaller than previous. Converted to NA
dat$height_cm [which(dat$obs_id=="v0qzg")] <- NA 

# For individual XXM, obs. #r88lg is 6 cm smaller than previous. Converted to NA
dat$height_cm [which(dat$obs_id=="r88lg")] <- NA 

# For individual XY1, obs. #0bvkx is 4 cm smaller than previous. Converted to NA
dat$height_cm [which(dat$obs_id=="0bvkx")] <- NA 


#from long, check individuals whose heights go up 10 or more cm. If this much growth occurs
#in a year or less and they're toddlers children or adolescents, it's ok and possible, 
#otherwise check time elapsed bt measurements and age, and consider converting to NA


up <- long %>%
  mutate(date_of_measure = as.Date(date_of_measure)) %>%
  group_by(person_id) %>%
  arrange(date_of_measure) %>%
  mutate(time_elap = as.numeric (date_of_measure - first(date_of_measure)),
         obs_dif = height_cm - lag(height_cm)) %>%
  filter(person_id %in% unique(person_id[obs_dif >= 10 & time_elap < 365]))

# There are 3 ind. (B8W,MWZ,S53) that have measures increasing 10 or more cm in a year. 
# obs_ids: n865u,erfkx and p98wq (increase 10-12cm) Toddlers, so leave them as they are.

# Check whether any ind. having measures at >15yo  has height increases of>10cm
up15 <- long %>%
  mutate(date_of_measure = as.Date(date_of_measure)) %>%
  group_by(person_id) %>%
  arrange(date_of_measure) %>%
  mutate(time_elap = as.numeric (date_of_measure - first(date_of_measure)),
         obs_dif = height_cm - lag(height_cm)) %>%
filter(any (age_estimated >=15), any (obs_dif >= 10))

# Four >15 yo ind.(A5H, A5H,KJD, MRM) have pair of obs. with differences >10 cm (8sw9r,dflx6,o1jvm, c4c0e)
# Ind. are adolescents and time bt measurements is over a year (2-3yrs) so leave them as they are.

#Check heights of individuals < 1 year old to see if there are any biologically implausible values 

# 8 ind < 12 months and heights and weights seem OK.

# 20 heights are now  (from 13) Weight measurements remained unchanged. 

write.csv(dat, "site_measurements.csv",row.names = FALSE)
