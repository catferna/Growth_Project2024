############################################################
### Cleaning Xavante data. Mato Grosso, Brazil ####
###########################################################

#starting commit: 906a51e
#Author: catalina fernandez

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Xavante/")


library(dplyr)
library(lubridate)

rm(list = ls())
dat <- read.csv("site_measurements.csv")

#####  checks to ensure consistency:

#1. remove observations that do not have BOTH height and weight.keep if only one is missing

sum(is.na(dat[["height_cm"]])) #Zero NA, all rows have height
sum(is.na(dat[["weight_kg"]])) # Zero NA, all rows have weight


# check if there are duplicated rows.
rep <- dat %>% select(-obs_id)
sum(duplicated(rep) | duplicated(rep, fromLast = TRUE)) #no duplicated


# how many people have X observations?
table(table(dat$person_id)) # Most individuals have 6 observations
# 1   2   3   4   5   6   7 
# 55  33  31  40  55 176   1 

#count number of people in the dataframe
dat %>%
  distinct(person_id) %>%
  count() #391 individuals


# plot heights to catch evident outliers

glimpse(dat)
#plot heights
library(ggplot2) 

dev.off() 
ggplot(dat, aes(x =age_years, y = height_cm , group = person_id))+
  geom_line() +
  geom_point()

# two height obs. for different individuals are 999 cm:ind 486, obs # 6wd8r and ind. 413, obs #f574s
# ind. 111, obs #3s4hp has a height of 13.6. 

#Check all obs. for each individual to change/remove obs based on general trajectory

print(dat[dat$person_id == 486, ]) 
print(dat[dat$person_id == 413, ]) 
print(dat[dat$person_id == 111, ]) 

# 999 is off the general growth trajectory for the individual 413 (it's not a typo of 99.9 cm)
# 999 is the only height available for ind. 486, and would be too small for age if it was 99 cm
# 999 is usually used for missing values in SPSS so convert both obs #f574s and #6wd8r to NA
#13.6 cm in 111 most likely a typo of 113.6 according to trajectory (n=6 obs)

dat$height_cm [which(dat$obs_id=="f574s")] <- NA 
dat$height_cm [which(dat$obs_id=="6wd8r")] <- NA 
dat$height_cm [which(dat$obs_id=="3s4hp")] <- 113.6 #13.6 probably typo; 113.6 likely given trajectory

## Flag obs, that are off the trajectory (for repeated measures)

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
  arrange(age_years) %>%
  summarize(min_height_diffs=min(diff(height_cm))) %>%
  filter (min_height_diffs <=-3)

#count n of obs that goes down.
down %>%
  distinct(person_id) %>%
  count() # 14 individuals whose heights go down 3 or more cm

#i <- 1
#print(long[long$person_id == down$person_id [i], ]) 
#i <- i + 1


#-1 #ind 90; obs #6nnnu is off the trajectory and male . Not the same ind (90). Remove entire row
dat <- dat %>%
  filter(obs_id != "6nnnu") #remove entire row for obs.#6nnnu 

#-2 #ind.150; obs #uj6lg:
dat$height_cm [which(dat$obs_id=="uj6lg")] <- 116.6 #changed 106.6 to 116.6; probably a typo given trajectory

#-3 #ind.263; obs. #lc3ij:
dat$height_cm [which(dat$obs_id=="lc3ij")] <- 113.2 #changed 103.2 to 113.2; probably a typo given trajectory

#-4 #ind.400; obs #hh4a2 
dat$height_cm [which(dat$obs_id=="hh4a2")] <- 109.9 #changed 119.9 to 109.9; probably a typo given trajectory

#-5 #ind.545; obs #dkcs1
dat$height_cm [which(dat$obs_id=="dkcs1")] <- 111.6  #changed 101.6  to 111.6 ; probably a typo given trajectory 

#-6 #ind.550; obs.#qsyrs
dat$height_cm [which(dat$obs_id=="qsyrs")] <- NA  #changed 108.2to NA; OFF the trajectory and no evident fix

#-7 #ind.553; obs #e1n9p. Both height and weight are off the trajectory. Delete entire row.
dat <- dat %>%
  filter(obs_id != "e1n9p") #remove entire row for obs.#e1n9p 

#-8 ind.579 ; obs #iuhbt
dat$height_cm [which(dat$obs_id=="iuhbt")] <-  113.9  #changed 103.9 to 113.9; likely a typo given trajectory

#-9 ind.582; obs #tzyeg  
dat$height_cm [which(dat$obs_id=="tzyeg")] <-  118.9 #changed 108.9 to 118.9; likely a typo given trajectory

#-10 ind. 724; obs #2wf55 
dat$height_cm [which(dat$obs_id=="2wf55")] <-  70.7 #changed 80.7 to 70.1; likely a typo given trajectory

#-11 ind.732; obs.#tkyxw 
dat$height_cm [which(dat$obs_id=="tkyxw")] <- 83.3 #changed 73.8 to 83.8; likely a typo given trajectory

#-12 ind.733; obs. #t24uw  
dat$height_cm [which(dat$obs_id=="t24uw")] <- 74.3 #changed 64.3 to 74.3; likely a typo given trajectory

#-13 ind.748; obs #smigr
dat$height_cm [which(dat$obs_id=="smigr")] <- 74.4 #changed 74.4 to 84.4; likely a typo given trajectory

#-14 ind.767 obs #5ud2j
dat$height_cm [which(dat$obs_id=="5ud2j")] <- 63.5 #changed 73.5 to 63.5; likely a typo given trajectory


#Most ind. in this ds were measured biannualy.From long, check inds whose heights go up >=10cm. 
long <- dat %>% #update "long"
  filter(obs_counts >= 2)

up <- long %>%
  group_by(person_id) %>%
  arrange(age_years) %>%
  mutate(time_elap = as.numeric (age_years - first(age_years)),
         obs_diff = height_cm - lag(height_cm)) %>%
  filter(person_id %in% unique(person_id[obs_diff >= 10 & time_elap < 1]))


#up <- long %>%
#  group_by(person_id) %>%
#  arrange(age_years) %>%
#  summarize(min_height_diffs=min(diff(height_cm))) %>%
 # filter(min_height_diffs >=10)

#n of obs that goes up +10
up %>%
  distinct(person_id) %>%
  count() #40 inds.

i <- 1
print(up[up$person_id == up$person_id [i], ]) 
i <- i + 1

# -1 ind.125 increase 13 cm in 6 months while 6 yo is a lot but keep it as it is.

# -2 ind.644 increase 10 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -3 ind.660 increase 11 cm in 6 months (toddler age).left it as it is

# -4 ind.665 increase 22cm in 6 months (57cm at 13mo to 79 at 19mo). Probably typo on 13mo measure (#9vwxg)
dat$height_cm [which(dat$obs_id=="9vwxg")] <- 66.9 #changed 56.9 to 66.9; probably typo given trajectory

#-5 ind.677 increase 17 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -6 ind. 689 increase 12 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -7 ind 699 increase 15 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -8 ind.706 increase 14 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -9 ind.707 increase 13 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -10 ind.738 increase 11 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -11 ind. 749; obs #xo7ej
dat$height_cm [which(dat$obs_id=="xo7ej")] <- 73.7 #changed 63.7 to 73.7; probably typo given trajectory and age

# -12 ind.751 increase 11 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -13 ind 753 increase of 12cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is.

# -14 ind. 757 increase 16 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -15 ind.760 increase 14 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -16 ind.761 increase 10 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -17 ind.763 increase 12 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -18 ind. 776 increase 16 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -19 ind.778 increase 12 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -20 ind.779 increase 13 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -21 ind.782 increase 11 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is
 
# -22 ind 783 increase 17 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -23 ind.784 increase 11 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -24 ind.788 increase 19 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is
 
# -25 ind.793 increase 10 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -26 ind.794 increase 14 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -27 ind. 806 increase 10 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -28 ind.807 increase 13 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -29 ind.810  increase 13 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -30 ind.812 increase 12 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -31 ind. 813 increase 14 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -32 ind. 816 increase 12 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -33 ind. 819 increase 13 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is   

# -34 ind.820 increase 18 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is   

# -35 ind.821 increase 11 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is 

# -36 ind. 822 increase 12 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -37 ind 823 increase 16 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -38 ind.824 increase 16 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -39 ind.825 increase 15 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is

# -40 ind.827 increase 12 cm in 6 months (±from birth to 6mo). ±15cm are expected first 6mo.
#left it as it is 


#Check heights of individuals < 1 year old to see if there are any biologically implausible values
# measures of newborn look ok although many heights and weights are quite low. No changes made.  

#introduced 3 NA for heights, zero for weight.


#write.csv(dat, "site_measurements.csv",row.names = FALSE)

####


