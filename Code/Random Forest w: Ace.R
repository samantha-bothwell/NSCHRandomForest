#################################
# Random Forest W/ Ace Variable #
#################################

library(haven)
library(dplyr)
screener_2018 <- read_sas("/Users/carlyschwartz/Downloads/CoSIBS 2022 Summer/Project/nsch_2018_screener_SAS/nsch_2018_screener.sas7bdat")
screener_2019 <- read_sas("/Users/carlyschwartz/Downloads/CoSIBS 2022 Summer/Project/nsch_2019_screener_SAS/nsch_2019_screener.sas7bdat")
topical_2018 <- read_sas("/Users/carlyschwartz/Downloads/CoSIBS 2022 Summer/Project/nsch_2018_topical_SAS/nsch_2018_topical.sas7bdat")
topical_2019 <- read_sas("/Users/carlyschwartz/Downloads/CoSIBS 2022 Summer/Project/nsch_2019_topical_SAS/nsch_2019_topical.sas7bdat")

########################
# Select ACE Variables #
########################

screener_2018 <- rename(screener_2018, HHID = HHIDS)
st_2018 <- merge(screener_2018, topical_2018, by="HHID")
screener_2019 <- rename(screener_2019, HHID = HHIDS)
st_2019 <- merge(screener_2019, topical_2019, by="HHID")
st_2018$YEAR <- st_2018$YEAR.x
st_2019$YEAR <- st_2019$YEAR.x
st_2018$FIPS <- st_2018$FIPSST.x
st_2019$FIPS <- st_2019$FIPSST.x
st_2018_1 <- st_2018 %>% select(ACE1, ACE10, ACE12, ACE3, ACE4, ACE5, ACE6, ACE7, ACE8, ACE9, SC_AGE_YEARS)

st_2019_1 <- st_2019 %>% select(ACE1, ACE10, ACE12, ACE3, ACE4, ACE5, ACE6, ACE7, ACE8, ACE9, SC_AGE_YEARS)



data <- rbind(st_2018_1, st_2019_1)
DataOfficial <- data %>% filter(data$SC_AGE_YEARS>=13 & data$SC_AGE_YEARS<=18)
dim(DataOfficial)



st_2018_21 <- st_2018_1 %>% select(ACE1, ACE10, ACE12, ACE3, ACE4, ACE5, ACE6, ACE7, ACE8, ACE9, SC_AGE_YEARS)
st_2019_21 <- st_2019_1 %>% select(ACE1, ACE10, ACE12, ACE3, ACE4, ACE5, ACE6, ACE7, ACE8, ACE9, SC_AGE_YEARS)
st_2018_21c <- st_2018_21[complete.cases(st_2018_21),]
st_2019_21c <- st_2019_21[complete.cases(st_2019_21),]
st_1819_c <- rbind(st_2018_21c, st_2019_21c) #combined dataset
st_1819_c <- st_1819_c %>% filter(st_1819_c$SC_AGE_YEARS>=13 & st_1819_c$SC_AGE_YEARS<=18)

#################
# Random Forest #
#################

library(randomForest)
set.seed(2022)

# 80/20 ratio training set
Train <- sample(nrow(st_1819_c), 0.8*nrow(st_1819_c),replace=FALSE)
TrainSet <- st_1819_c[Train,]
TestSet <- st_1819_c[-Train,] # whatever is not part of trainset
summary(TrainSet)
summary(TestSet)

# Disregard warning messages; run anyways
options(warn=-1)

RF1 <- randomForest(formula = BMICLASS ~ ., data = TrainSet, importance=TRUE)
RF1 # Yields confusion matrix w factorized variables

predTrain <- predict(RF1, TrainSet, type = "class")
predTest <- predict(RF1, TestSet, type = "class")

table(round(predTrain),TrainSet$BMICLASS);
table(round(predTest),TestSet$BMICLASS);
importance(RF1)

varImpPlot(RF1,sort=TRUE,n.var=min(21, nrow(RF1$importance)),main="Variable Importance Plot for Obesity Predictions with Psychoecological Variables")





