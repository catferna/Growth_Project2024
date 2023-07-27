
####### !!!Check that all obs for a male/female are male/female, respectively

#starting commit 2850caa
#author: catalina fernandez.

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source")

afroc <- read.csv("Afrocolombians/site_measurements.csv" )
sex_mismatch <- afroc %>%
  group_by(person_id) %>%
  summarise(same_sex = n_distinct(sex) == 1) %>%
  filter(!same_sex) 

print (sex_mismatch) #NO indiv. w sex mismatches. NO CHANGES MADE to last commit.

##



