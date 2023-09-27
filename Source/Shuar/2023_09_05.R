############################################################
##### Cleaning Shuar data - Morona-Santiago, Ecuador ######
###########################################################

#starting commit: 2af3942

####!!!!!#### FIX SEX MISTMATCH FROM IND 357782 !!!!!!!!!!

#Author: catalina fernandez
setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Shuar/")

library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv") #5140 obs.loaded

#####  checks to ensure consistency:
colnames (dat)

# ** FIX SEX MISTMATCH:S.Urlacher confirmed by e-mail that ind.357782 is a female across obs.

sex_mismatch <- dat %>%
  group_by(person_id) %>%
  summarise(same_sex = n_distinct(sex) == 1) %>%
  filter(!same_sex) %>%
  pull(person_id)
print (sex_mismatch) # 357782 has a sex mismatch

# obs_id #3diw1 and  #uw1z8 for ind. 357782 are male but should be female.

dat$sex [which(dat$obs_id=="3diw1")] <- "f"
dat$sex [which(dat$obs_id=="uw1z8")] <- "f"


#1. remove observations that do not have BOTH height and weight. Keep if only one is missing

sum(is.na(dat[["height_cm"]])) # 1536 obs. don't have height
sum(is.na(dat[["weight_kg"]])) # 26 obs. don't have weight

dat <- dat %>%
  filter(!(is.na(weight_kg) & is.na(height_cm))) #no rows where both have and weight are NA


#2. Check if there are duplicated rows.
rep <- dat %>% select(-obs_id)
sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) # 20 duplicated 

#check observations:
duplicated_rows <- rep[duplicated(rep) | duplicated(rep, fromLast = TRUE), ] # remove 10 obs.

#check observations:
duplicated_rows <- rep[duplicated(rep) | duplicated(rep, fromLast = TRUE), ] # remove 10 obs.
# person_id: 357545, 357568, 357951, 357957, 360361, 360366, 360741, 360742, 523012, 523013.

# ind.357545, obs. #1k3nb and #na227 have exactly same info, delete one (#na227).
dat <- dat %>%
  filter(obs_id != "na227")

# ind.357568, obs. #gjetc and #8rmhg have exactly same info, delete one (#8rmhg).
dat <- dat %>%
  filter(obs_id != "8rmhg")

# ind.357951, obs. #ik3az and #8io5t have exactly same info, delete one (#8io5t).
dat <- dat %>%
  filter(obs_id != "8io5t")

# ind.357957, obs. #iq3qv and #6rrzs have exactly same info, delete one (#6rrzs).
dat <- dat %>%
  filter(obs_id != "6rrzs")

# ind.360361 obs. #559mt and #sxdq5 have exactly same info, delete one (#sxdq5).
dat <- dat %>%
  filter(obs_id != "sxdq5")

# ind.360366 obs. #f1af9 and #j56pr have exactly same info, delete one (#j56pr).
dat <- dat %>%
  filter(obs_id != "j56pr")

# ind. 360741 obs. #l9hx1 and #sdhxj have exactly same info, delete one (#sdhxj).
dat <- dat %>%
  filter(obs_id != "sdhxj")

# ind. 360742 obs. #3ihh4 and #7q2fm have exactly same info, delete one (#7q2fm).
dat <- dat %>%
  filter(obs_id != "7q2fm")

# ind. 523012 obs. #w1kc9 and #w9u9m have exactly same info, delete one (#w9u9m).
dat <- dat %>%
  filter(obs_id != "w9u9m")

#ind. 523013 obs. #s3b6h and #58asb have exactly same info, delete one (#58asb). 
dat <- dat %>%
  filter(obs_id != "58asb")


# 3. Check how many people have X observations?
table(table(dat$person_id)) #Most ind. have more than one obs.,up to 38!


# 4. Add column estimated age to make it easier to assess whether differences are likely or not 
##### age is already in the dataset

#5. Check how many ind. don't have age, or don't have dob, or date of measure
sum(is.na(dat[["age_estimated"]])) #ALL ind. have age.

#6. Remove rows that don't have age, or age can't be calculated bc missing dates 
#ALL ind. have age.

#7. Count number of people in the dataframe
dat %>% distinct(person_id) %>%
  count()  # 2460 individual in the dataset

#8. Plot heights to catch evident outliers

library(ggplot2)
dev.off() 
ggplot(dat, aes(x =age_estimated, y = height_cm , group = person_id))+
  geom_line() +
  geom_point() #

dev.off() 
ggplot(dat, aes(x =age_estimated, y = weight_kg , group = person_id))+
  geom_line() +
  geom_point() #

# check ind. younger than 1yo as apparently there are some weird values

#inspect ind. 522711, obs. #djv87: is a single obs. so it's difficult to judge.
# Ind. is younger than 2mo. and height is 88, weight 10kg. UNLIKELY
#remove obs. djv87
dat <- dat %>%
  filter(obs_id != "djv87")

# inspect ind.357951, obs. #55mv7: obs.#55mv7 correspond to ind. when younger than 2mo
# and height is too high and same than same ind. when older than one yo. Remove height but not weight.
dat$height_cm [which(dat$obs_id=="55mv7")] <- NA

# inspect ind.357345, obs. #xm8so: Height of 87cm for a 9mo is very unlikely, but more unlikely is the
#weight of 6kg for that height (and for that age). Probably error in data entry. Delete entire row.
dat <- dat %>%
  filter(obs_id != "xm8so")

# inspect ind.357537, obs. #l76mj: 88cm and 6kg is very unlikely for an ind. older than 1yo., especially
#since later obs. ind. has a height of 70-ish cm. Delete entire row. 
dat <- dat %>%
  filter(obs_id != "l76mj")

#9.add count of obs. per ind. as new column 
dat <- dat %>%
  add_count(person_id, name = "obs_counts")

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
  count() # 24 obs.

i <- 1
print(long[long$person_id == down$person_id [i], ]) 
i <- i + 1

# a1. ind. 357220; obs. #gzdwe goes down 5cm. Only 2 obs, so hard to know which is right. Remove ind:
dat <- dat %>%
  filter(person_id != "357220") #remove indivual (2 rows)

#a2. ind.357228; obs.#strwd goes down 3 cm but previous obs. #4ktlt height probably has a typo given trajectory 
dat$height_cm [which(dat$obs_id=="strwd")] <- NA  #remove only height as weight seems concordant

#a3. ind.357307 ; obs.#dkk2f goes down 4cm. Probably a typo of 123cm, but hard to know so remove height.  
dat$height_cm [which(dat$obs_id=="dkk2f")] <- NA # NA only height as weight seems OK.

#a4. ind.357343 ; obs.#tiz76 and obs.#obr6o goes down 4cm; remove both heights. Weight looks concordant.
dat$height_cm [which(dat$obs_id=="tiz76")] <- NA
dat$height_cm [which(dat$obs_id=="obr6o")] <- NA

#a5. ind.357468 ; obs.#q64dr goes down 3 cm. Remove height only as weight looks concordant.
dat$height_cm [which(dat$obs_id=="q64dr")] <- NA

#a6. ind.357474; obs. #6tkwn goes down more than 4cm. Remove height only as weight looks concordant
dat$height_cm [which(dat$obs_id=="6tkwn")] <- NA

#a7. ind.357537; height obs.#2vlq9 and height obs. #gkkfp goes down more than 3 cm 
# and are off the trajectory. Weights follows trajectory.
dat$height_cm [which(dat$obs_id=="2vlq9")] <- NA
dat$height_cm [which(dat$obs_id=="gkkfp")] <- NA 

#a8. ind.357539 ; obs.#jj2hm goes down over 4 cm, probably typo given trajectory. Keep weight
dat$height_cm [which(dat$obs_id=="jj2hm")] <- NA 

#a9. ind.357549 ; obs.#wyh2i goes down more than 3 cm; probably typo given trajectory. Keep weight
dat$height_cm [which(dat$obs_id=="wyh2i")] <- NA 

#a10. ind.357550  ; obs.#5mvr1 goes down more than 6cm; probably typo given trajectory. Keep weight
dat$height_cm [which(dat$obs_id=="5mvr1")] <- NA 

#a11. ind.357612  ; obs.#dcpg5. apparently goes down but probably typos given trajectory
dat$height_cm [which(dat$obs_id=="okvda")] <- 91 # was 81cm but probably typo given rest of trajectory
dat$height_cm [which(dat$obs_id=="dcpg5")] <- 93 # was 83cm but probably typo given rest of trajectory

#a12. ind.357626  ; obs.#ui49z and obs. #3005d are a few months apart and height diff is >4cm. 
#Remove both heights bc it's not possible to know which is correct as there're no more height obs. 
dat$height_cm [which(dat$obs_id=="ui49z")] <- NA 
dat$height_cm [which(dat$obs_id=="3005d")] <- NA 

#a13. ind.357656  ; obs.#xtf6p height is 98 but the obs. before and after are 94.
dat$height_cm [which(dat$obs_id=="xtf6p")] <- 94  # changed to 94 from 98

#a14. ind.357694 ; obs.#6regp  is 113.99 cm but inconsistent with trajectory.
#113.99 was most likely a typo of 119 cm
dat$height_cm [which(dat$obs_id=="6regp")] <-  119 #changed to 119 from 113.99 

#a15. ind.357713  ; obs.#khuco height decreases 4 cm. Convert height to NA; weight seems ok.
dat$height_cm [which(dat$obs_id=="khuco")] <- NA

#a16. ind.357781 ; obs.#xp6eo and #j55p4 ; only two height obs. and more than 5 cm difference. Rm heights, keep weights
dat$height_cm [which(dat$obs_id=="xp6eo")] <- NA
dat$height_cm [which(dat$obs_id=="j55p4")] <- NA

#a17. ind.357834  ; obs.#o6fuk height decreases more tan 5cm. Following height is NA so can't guess if typo.
dat$height_cm [which(dat$obs_id=="o6fuk")] <- NA  #convert to NA 

#a18. ind.357908  ; obs.#ryzxg height decreases more than 4cm. Probably a typo but can't guess probable number.
#height obs.# zvqj2 also seems supicious but shouldn't remove without additional info ; keep #zvqj2 as it is for now.
dat$height_cm [which(dat$obs_id=="ryzxg")] <- NA  #convert to NA 

#a19. ind.357921  ; obs.#8utw4 and obs. #2xqaj ; there're 3cm bt obs. Impossible to know which is right
dat$height_cm [which(dat$obs_id=="8utw4")] <- NA  #convert to NA
dat$height_cm [which(dat$obs_id=="2xqaj")] <- NA  #convert to NA

#a20. ind.357923  ; obs.#473sm height decreases 3 cm from obs.# pu70k and impossible to know which is right
dat$height_cm [which(dat$obs_id=="pu70k")] <- NA  #convert to NA. 
dat$height_cm [which(dat$obs_id=="473sm")] <- NA  #convert to NA.  

#a21. ind. 357957 ; obs.#pwo1m. This height (89cm) is quite off the trajectory
dat$height_cm [which(dat$obs_id=="pwo1m")] <- NA  #convert to NA

#a22. ind.522864  ; obs.#48d3h and obs.#1bn1u have 4cm of difference. Remove both as we can't guess which is right
dat$height_cm [which(dat$obs_id=="48d3h")] <- NA  #convert to NA
dat$height_cm [which(dat$obs_id=="1bn1u")] <- NA  #convert to NA

#a23. ind.522865 ; obs.#1h44x and obs.#4xx6h are 6cm apart. only 2 obs., and on adult ind. Convert both to NA
dat$height_cm [which(dat$obs_id=="1h44x")] <- NA  #convert to NA
dat$height_cm [which(dat$obs_id=="4xx6h")] <- NA  #convert to NA

#a24. ind.523013  ; obs.#gdc9e and #z3apa are 86 ccm but previous is 80cm and next is 81, so most likely typo
dat$height_cm [which(dat$obs_id=="gdc9e")] <- 80 #changed from 86 as would be consistent with trajectory
dat$height_cm [which(dat$obs_id=="z3apa")] <- 80 #changed from 86 as would be consistent with trajectory


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
  filter(person_id %in% unique(person_id[obs_diff >= 10 & time_elap < 1])) # 5 ind have a height measure that rises more than 10cm/year 

i <- 1
print(long[long$person_id == up$person_id [i], ]) 
i <- i + 1

# From the 5 ind. that have height obs that rise more than 10cm over a year or less:

#b1. ind. 357357; obs. #5b7qf and #d1byu were taken less than a month apart and are 10cm apart
#remove both height obs. since at least one of them is wrong; weight seems ok
dat$height_cm [which(dat$obs_id=="5b7qf")] <- NA  #convert to NA
dat$height_cm [which(dat$obs_id=="d1byu")] <- NA  #convert to NA

#b2. ind, 523086 has only 2 obs., taken 1.56 month apart and 16 cm difference between them. At least one of them is wrong 
#remove both heights;  weight seems ok.
dat$height_cm [which(dat$obs_id=="hg07z")] <- NA  #convert to NA
dat$height_cm [which(dat$obs_id=="qeekn")] <- NA  #convert to NA

#b3. ind. 523090 #only 2 obs. taken about 6mo apart during first 6 months of life
# This much growth may be possible; left it as it is. 

#b4. ind. 523091. Obs. taken at 1 and 3 month are 9 cm apart. This is unlikely but within measurement error in both.
#left as it is.
 
#b5. ind. 523096. Obs. are taken 6 month and are 13 cm apart. This is possible first 6 mo.
# Left as it is.  


#14. Indicate how many obs. were removed 
sum(is.na(dat[["height_cm"]])) 
sum(is.na(dat[["weight_kg"]])) 

# #5140 obs.loaded; KEPT 5125 (15 obs. removed entirely) 
# 1536 NA height loaded; INTRODUCED 26 NA; now 1564 
# 25 weight loaded; kept 26 weight NA; zero added

write.csv(dat, "site_measurements.csv",row.names = FALSE)
