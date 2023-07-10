###########################################################################
### From Anthropometrics_Colombia.csv, create dataset in long format  ###
###########################################################################

# Script modified from "Data_Maker.R" sent by Cody Ross, and also in this folder. 
# Commented out the lines that add "Cluster", as we haven't decided yet whether we will use this that way or not.
# I didn't create Cluster because number of observations are reduced 

library (tidyverse)
library (lubridate)
setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Afrocolombians/")

rm (list = ls())

d = read.csv("raw/Anthropometrics_Colombia.csv") #reading wide format of the data 

vars_1 = c("PID", "HHID", "Sex", "Ethnicity", "Location",  "Sub_Location", "Site", "Ethnicity_Plot", "Birth_Month", "Birth_Year",
           "Day_Wave_1", "Month_Wave_1", "Year_Wave_1", "Height_Wave_1", "Weight_Wave_1", "BMI_Wave_1", "Age_Wave_1")

vars_2 = c("PID", "HHID", "Sex", "Ethnicity", "Location",  "Sub_Location", "Site", "Ethnicity_Plot", "Birth_Month", "Birth_Year",
           "Day_Wave_2", "Month_Wave_2", "Year_Wave_2", "Height_Wave_2", "Weight_Wave_2", "BMI_Wave_2", "Age_Wave_2")

vars_3 = c("PID", "HHID", "Sex", "Ethnicity", "Location",  "Sub_Location", "Site", "Ethnicity_Plot", "Birth_Month", "Birth_Year",
           "Day_Wave_3", "Month_Wave_3", "Year_Wave_3", "Height_Wave_3", "Weight_Wave_3", "BMI_Wave_3", "Age_Wave_3")

colnames_all = c("PID", "HHID", "Sex", "Ethnicity", "Location",  "Sub_Location", "Site", "Ethnicity_Plot", "Birth_Month", "Birth_Year",
                 "Day", "Month", "Year", "Height", "Weight", "BMI", "Age_At_Measurement", "Wave") #,"Cluster")


d_w1 = d[,vars_1]
d_w2 = d[,vars_2]
d_w3 = d[,vars_3]

d_w1$Wave = "Wave_1"
d_w2$Wave = "Wave_2"
d_w3$Wave = "Wave_3"

#d_w1$Cluster = NA
#d_w2$Cluster = NA
#d_w3$Cluster = NA

colnames(d_w1) = colnames_all
colnames(d_w2) = colnames_all
colnames(d_w3) = colnames_all

d_long = rbind(d_w1, d_w2, d_w3)

#1. change name of column "Site" to "Site_Code"
dat <- d_long %>%
  rename (Site_Code = Site)

#2. Check date format.For birth date, there's no day, and month and year are in separate columns
# Remove observations that don't have birth year or measurement date
#Create date dd/mm/yyyy, and arbitrarily add 15 as the day for all dates of birth

sum(is.na (dat$Birth_Year))
sum (is.na (dat$Birth_Month))

# Remove obs that don't have birth year (105 removed)
dat <- dat  %>%
  filter (!is.na (Birth_Year))
 
#Create date columns as mm/yyyy or dd/mm/yyyy 
#Create additional birth date columns for minimum , estimated and maximum dates
#will consider min_date_birth as the first day of the month if month is available
#if month is NA, min_date will be the first day of the year.
#same logic  apply for the estim_date and maximum_date, depending on Birth_Month 

dat <- dat  %>%
  mutate ( date_of_measure = ifelse(is.na(Day) | is.na(Month) | is.na(Year), NA, 
                                    sprintf("%02d/%02d/%04d", Day, Month, Year)),
    min_date_birth = as.Date(ifelse(is.na(Birth_Month), paste0("01/01/", Birth_Year), paste0("01/", Birth_Month, "/", Birth_Year)), format = "%d/%m/%Y"),
         estim_date_birth = as.Date (ifelse (is.na (Birth_Month),paste0("15/06/", Birth_Year), paste0 ("15/",Birth_Month,"/", Birth_Year)), format = "%d/%m/%Y"),
          max_date_birth =  as.Date (ifelse (is.na(Birth_Month),
                                            as.Date(paste(Birth_Year, "-12-31", sep = "")), 
                                            as.Date(paste(Birth_Year, "-", Birth_Month, "-01", sep = "")))) %>%
           ceiling_date("month") - days(1)) # to create max.date, next day o ceiling date minus 1 day

# 3. Generate random ID for each observation 

generateRandomString <- function(length) {
  chars <- c(letters, 0:9)  # All possible characters
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

dat <- dat %>% 
  mutate(obs_id = sapply(1:nrow(dat), function(i) generateRandomString(5)))


# 3.rename columns to be included
colnames(dat)
dat <- dat %>%
  rename (person_id = PID,
          sex =Sex,
          height_cm = Height,
          weight_kg = Weight,
          ethnicity = Ethnicity,
          location = Location)


#4. select only the columns that we need

dat <- dat %>%
  select (obs_id, person_id, sex, ethnicity,location, height_cm, weight_kg,date_of_measure,
          min_date_birth, estim_date_birth, max_date_birth)

#5. add site name column

dat$site <- "colombia" 


#write.csv(dat, "site_measurements.csv", row.names = FALSE)







####################################################################################################################### This drops some people at each site, including those of mixed ethnic identity
#d_long$Cluster = ifelse(d_long$Ethnicity=="MESTIZO" & d_long$Location=="SUESCA", "IH_M", d_long$Cluster)
#d_long$Cluster = ifelse(d_long$Ethnicity=="MESTIZO" & d_long$Location=="TAIBA",  "WH_M", d_long$Cluster)
#d_long$Cluster = ifelse(d_long$Ethnicity=="MESTIZO" & d_long$Location=="BAHIA SOLANO", "PC_M", d_long$Cluster)
#d_long$Cluster = ifelse(d_long$Ethnicity=="MESTIZO" & d_long$Location=="SANTA CECILIA", "WL_M", d_long$Cluster)

#d_long$Cluster = ifelse(d_long$Ethnicity=="EMBERA" & d_long$Location=="BAHIA SOLANO", "PC_E", d_long$Cluster)
#d_long$Cluster = ifelse(d_long$Ethnicity=="EMBERA" & d_long$Location=="SANTA CECILIA", "WL_E", d_long$Cluster)

#d_long$Cluster = ifelse(d_long$Ethnicity=="AFROCOLOMBIAN" & d_long$Location=="BAHIA SOLANO", "PC_A", d_long$Cluster)
#d_long$Cluster = ifelse(d_long$Ethnicity=="AFROCOLOMBIAN" & d_long$Location=="SANTA CECILIA", "WL_A", d_long$Cluster)


#d_full = d_long[which(!is.na(d_long$Cluster)),]  


############## By Sex
# dev.new()
# ggplot(data = d_full, aes(x = Age_At_Measurement, y = Weight, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
# dev.new()
# ggplot(data = d_full, aes(x = Age_At_Measurement, y = Height, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
# dev.new()
# ggplot(data = d_full, aes(x = Age_At_Measurement, y = BMI, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
# 
# 
# dev.new()
# ggplot(data = d_full, aes(x = Weight, y = BMI, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
# dev.new()
# ggplot(data = d_full, aes(x = Height, y = BMI, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
# 
# dev.new()
# ggplot(data = d_full, aes(x = Height, y = Weight, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
# 
# 
# ################### By Group
# dev.new()
# ggplot(data = d_full, aes(x = Age_At_Measurement, y = Weight, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
# dev.new()
# ggplot(data = d_full, aes(x = Age_At_Measurement, y = Height, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
# dev.new()
# ggplot(data = d_full, aes(x = Age_At_Measurement, y = BMI, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
# 
# 
# dev.new()
# ggplot(data = d_full, aes(x = Weight, y = BMI, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
# dev.new()
# ggplot(data = d_full, aes(x = Height, y = BMI, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
# 
# dev.new()
# ggplot(data = d_full, aes(x = Height, y = Weight, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
# 
# 
# 


