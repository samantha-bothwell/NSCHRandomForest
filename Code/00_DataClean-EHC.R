################################################
# Project: NSCHRandomForest (CoSIBS 2022)
# PI: Jill Kaar
# Analyst: Emily Cooper
# Program:  00_DataClean-EHC
# Date created: 07/05/2022
# Purpose: generate clean dataset with complete cases
################################################

library(haven); library(dplyr)

####2018 data####
# screener_2018 <- read_sas("nsch_2018_screener.sas7bdat")
topical_2018 <- read_sas("nsch_2018_topical.sas7bdat")

####2019 data####
# screener_2019 <- read_sas("nsch_2019_screener.sas7bdat")
topical_2019 <- read_sas("nsch_2019_topical.sas7bdat")

#Merging by HHID.
# screener_2018 <- rename(screener_2018, HHID = HHIDS)
# st_2018 <- merge(screener_2018, topical_2018, by="HHID", all=T)
# screener_2019 <- rename(screener_2019, HHID = HHIDS)
# st_2019 <- merge(screener_2019, topical_2019, by="HHID", all=T)

#Select variables of interest
t_2018 <- topical_2018 %>% select(YEAR, FIPSST, SC_AGE_YEARS, SC_HISPANIC_R, SC_RACE_R, SC_SEX, BMICLASS, OVERWEIGHT, PHYSACTIV, SCREENTIME, HOURSLEEP, K2Q32A, K2Q32B, K2Q32C, K2Q33A, K2Q33B, K2Q33C, ACE1, ACE3, ACE4, ACE5, ACE6, ACE7, ACE8, ACE9, ACE10)
t_2019 <- topical_2019 %>% select(YEAR, FIPSST, SC_AGE_YEARS, SC_HISPANIC_R, SC_RACE_R, SC_SEX, BMICLASS, OVERWEIGHT, PHYSACTIV, SCREENTIME, HOURSLEEP, K2Q32A, K2Q32B, K2Q32C, K2Q33A, K2Q33B, K2Q33C, ACE1, ACE3, ACE4, ACE5, ACE6, ACE7, ACE8, ACE9, ACE10)
dta <- rbind(t_2018, t_2018)

#Subset by age criteria
DataOfficial <- dta %>% filter(dta$SC_AGE_YEARS>=13 & dta$SC_AGE_YEARS<=18)
dim(DataOfficial)

#Complete cases for variables to be included in RF
DataOfficial2 <- DataOfficial[complete.cases(DataOfficial[ , c("SC_AGE_YEARS","SC_HISPANIC_R", "SC_RACE_R", "SC_SEX", "BMICLASS", "PHYSACTIV", "SCREENTIME", "HOURSLEEP", "K2Q33A", "K2Q32A","ACE1","ACE3","ACE4","ACE5","ACE6","ACE7","ACE8","ACE9","ACE10")]), ] 
dim(DataOfficial2)

#Create new BMICLASS variable with correct groupings
DataOfficial2$BMICLASS2 <- ifelse(DataOfficial2$BMICLASS %in% c(1,2), 1, ifelse(
  DataOfficial2$BMICLASS==3, 2, 3))

write.csv(DataOfficial2, "NSCHRandomForest-clean-070522.csv", row.names=F)
