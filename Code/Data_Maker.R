

setwd("C:\\Users\\cody_ross\\Dropbox\\Open Papers\\Anthropometrics")

d = read.csv("Anthropometrics_Colombia.csv")

vars_1 = c("PID", "HHID", "Sex", "Ethnicity", "Location",  "Sub_Location", "Site", "Ethnicity_Plot", "Birth_Month", "Birth_Year",
           "Day_Wave_1", "Month_Wave_1", "Year_Wave_1", "Height_Wave_1", "Weight_Wave_1", "BMI_Wave_1", "Age_Wave_1")

vars_2 = c("PID", "HHID", "Sex", "Ethnicity", "Location",  "Sub_Location", "Site", "Ethnicity_Plot", "Birth_Month", "Birth_Year",
           "Day_Wave_2", "Month_Wave_2", "Year_Wave_2", "Height_Wave_2", "Weight_Wave_2", "BMI_Wave_2", "Age_Wave_2")

vars_3 = c("PID", "HHID", "Sex", "Ethnicity", "Location",  "Sub_Location", "Site", "Ethnicity_Plot", "Birth_Month", "Birth_Year",
           "Day_Wave_3", "Month_Wave_3", "Year_Wave_3", "Height_Wave_3", "Weight_Wave_3", "BMI_Wave_3", "Age_Wave_3")

colnames_all = c("PID", "HHID", "Sex", "Ethnicity", "Location",  "Sub_Location", "Site", "Ethnicity_Plot", "Birth_Month", "Birth_Year",
                 "Day", "Month", "Year", "Height", "Weight", "BMI", "Age_At_Measurement", "Wave", "Cluster")


d_w1 = d[,vars_1]
d_w2 = d[,vars_2]
d_w3 = d[,vars_3]

d_w1$Wave = "Wave_1"
d_w2$Wave = "Wave_2"
d_w3$Wave = "Wave_3"

d_w1$Cluster = NA
d_w2$Cluster = NA
d_w3$Cluster = NA

colnames(d_w1) = colnames_all
colnames(d_w2) = colnames_all
colnames(d_w3) = colnames_all

d_long = rbind(d_w1, d_w2, d_w3)

######################################################################################################################## This drops some people at each site, including those of mixed ethnic identity
d_long$Cluster = ifelse(d_long$Ethnicity=="MESTIZO" & d_long$Location=="SUESCA", "IH_M", d_long$Cluster)
d_long$Cluster = ifelse(d_long$Ethnicity=="MESTIZO" & d_long$Location=="TAIBA",  "WH_M", d_long$Cluster)
d_long$Cluster = ifelse(d_long$Ethnicity=="MESTIZO" & d_long$Location=="BAHIA SOLANO", "PC_M", d_long$Cluster)
d_long$Cluster = ifelse(d_long$Ethnicity=="MESTIZO" & d_long$Location=="SANTA CECILIA", "WL_M", d_long$Cluster)

d_long$Cluster = ifelse(d_long$Ethnicity=="EMBERA" & d_long$Location=="BAHIA SOLANO", "PC_E", d_long$Cluster)
d_long$Cluster = ifelse(d_long$Ethnicity=="EMBERA" & d_long$Location=="SANTA CECILIA", "WL_E", d_long$Cluster)

d_long$Cluster = ifelse(d_long$Ethnicity=="AFROCOLOMBIAN" & d_long$Location=="BAHIA SOLANO", "PC_A", d_long$Cluster)
d_long$Cluster = ifelse(d_long$Ethnicity=="AFROCOLOMBIAN" & d_long$Location=="SANTA CECILIA", "WL_A", d_long$Cluster)


d_full = d_long[which(!is.na(d_long$Cluster)),]  


############## By Sex
dev.new()
ggplot(data = d_full, aes(x = Age_At_Measurement, y = Weight, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
dev.new()
ggplot(data = d_full, aes(x = Age_At_Measurement, y = Height, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
dev.new()
ggplot(data = d_full, aes(x = Age_At_Measurement, y = BMI, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")


dev.new()
ggplot(data = d_full, aes(x = Weight, y = BMI, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")
dev.new()
ggplot(data = d_full, aes(x = Height, y = BMI, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")

dev.new()
ggplot(data = d_full, aes(x = Height, y = Weight, color = Sex)) + geom_point() + facet_wrap(~Cluster, nrow=2) + scale_color_brewer(palette = "Dark2")


################### By Group
dev.new()
ggplot(data = d_full, aes(x = Age_At_Measurement, y = Weight, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
dev.new()
ggplot(data = d_full, aes(x = Age_At_Measurement, y = Height, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
dev.new()
ggplot(data = d_full, aes(x = Age_At_Measurement, y = BMI, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")


dev.new()
ggplot(data = d_full, aes(x = Weight, y = BMI, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")
dev.new()
ggplot(data = d_full, aes(x = Height, y = BMI, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")

dev.new()
ggplot(data = d_full, aes(x = Height, y = Weight, color = Cluster)) + geom_point() + facet_wrap(~Sex, nrow=1) + scale_color_brewer(palette = "Dark2")





