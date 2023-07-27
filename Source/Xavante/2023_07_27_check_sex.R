
####### !!!Check that all obs for a male/female are male/female, respectively

#starting commit: c3d3ca3
#author: catalina fernandez.

library(dplyr)
rm(list = ls())
setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source")

xavante<- read.csv("Xavante/site_measurements.csv" )

sex_mismatch <- xavante %>%
  group_by(person_id) %>%
  summarise(same_sex = n_distinct(sex) == 1) %>%
  filter(!same_sex)

print (sex_mismatch) # 5 individuals have sex mismatches across observations.

# -1. ind.252, obs #3p6s5 is M and remaining 5 are female. 
print(xavante[xavante$person_id == 252, ]) #Obs #3p6s5 follow ind. trajectory so change it to F
xavante$sex [which(xavante$obs_id=="3p6s5")] <- "F"

# -2. ind.523 obs #84dxx is M and remaining 5 are female. 
print(xavante[xavante$person_id == 523, ])
xavante$sex [which(xavante$obs_id=="84dxx")] <- "F"

# -3. ind.703 obs #6lcye is M and remaining 5 are female
print(xavante[xavante$person_id == 703, ])
xavante$sex [which(xavante$obs_id=="6lcye")] <- "F"

# -4. ind.782. 2 obs are F and 2 are M. No way to know which is the real sex or 2 diff ind.
print(xavante[xavante$person_id == 782, ]) # REMOVE person 782
xavante <- xavante %>%
  filter(person_id != "782")

# -5. ind.825. only 2 obs, 1 F and 1 M. No way to know which is the real sex or 2 diff ind.
print(xavante[xavante$person_id == 825, ]) # REMOVE person 825
xavante <- xavante %>%
  filter(person_id != "825")

#write.csv(xavante, "Xavante/site_measurements.csv", row.names = FALSE)
