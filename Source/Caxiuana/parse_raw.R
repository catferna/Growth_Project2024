#####################################################################
### Check B.Piperata and M.Inglez data on Ribeirinhos -Caixuana ####
#####################################################################

#Author: catalina fernandez ###
#starting commit: 2af3942

setwd("/Users/catalinafernandez/Library/CloudStorage/OneDrive-Personal/Growth_Project/Source/Caxiuana/")

library(dplyr)
library(readxl)
library(tidyxl)
library (lubridate)

dat_cross <- read.csv("raw/Caxiuana_cross-sectional-dataset_2002_2009_csv.csv")
dat_cross_inglez <- read_excel("raw/Caxiuana_cross-sectional-dataset_Piperata_Inglez_2002-2009-2021clean.xlsx")
dat_long <- read.csv("raw/Caxiuana_longitudinal_dataset_2002_2009_csv_v2.csv")


# -1. Check if data is on csv format and can be read, otherwise convert to csv
# -2. Check if there are one or more sheets in case it's a .xls file
# -3. Check if N of column names match columns of data
# -4. Check if periods are used for decimals 
# -5. Check that dates are in dd/mm/yyyy format
# -6. Check if data is in the right units of measurements (cm, kg)
# -7. Check if dataframe is structured according to the guidelines (e.g.is sex f/m?)
# -8. Check sex mismatches
# -9. Modify column names to standard lowercase (person_id, sex, height_m, weight_kg)
# -10. Create observation id variable (obs_id) 
# -11. Save dataframe as "site_measurements.csv"

glimpse (dat)  #1 ??



