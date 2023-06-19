#### Checking Piperata's data ####
### Author: Catalina Fernández ###

rm(list = ls())

library(dplyr)
library(tidyverse)
library(readxl)
library(anytime)
library(lubridate)
setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Data/")

# 1. Check if data is on csv format and can be read, otherwise convert to csv
# 2. Check if there are one or more sheets in case there is a xls file
# 3. Check if N of column names match columns of data
# 4. Check if periods are used for decimals 
# 5. Check that dates are in dd/mm/yyyy format
# 6. Check if  dataframe is structured  according to the guidelines; if not fix
# 7. Check if data is in the right unit of measurements (cm, kg)

# 1. Check if data is on csv format and can be read, otherwise convert to csv



csvpip <- read.csv("Caxiuana_longitudinal_dataset_2002_2009_formatted_csv.csv", sep = ";")

# Check 4 and change.
csvpip <- csvpip %>% 
  mutate (Heightcm_2002 = gsub (",",".", Heightcm_2002),
          Heightcm_2009 = gsub (",",".", Heightcm_2009),
          Weightkg_2002 = gsub (",",".", Weightkg_2002), 
          Weightkg_2009 = gsub (",",".", Weightkg_2009),
          Birthday = gsub ("[[:punct:]]", "/", Birthday))
