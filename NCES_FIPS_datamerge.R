<<<<<<< HEAD

###############################################################
######Creating School Directory for Public schools#############
###############################################################
#KC 4/18/2021
library(dplyr)
library(tidyr)
library(forcats)
library(readxl)
library(ggplot2)
library(here)
library(stringr)

#Read in excel files with public school districts and public schools from NCES database
#https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&State=41

NCES <- read_excel('NCES Data/NCES_AL_GA_LA_MS_SC.xlsx')
FIPS <- read_excel("NCES Data/FIPS_Codes.xlsx")

FIPS <- FIPS %>% filter( State == "AL" | State == "GA" | State == "LA" | State == "MS" | State == "SC")

NCES_FIPS <- merge(NCES, FIPS, by.x = c("County Name","State"), by.y = c("Name","State"))


=======

###############################################################
######Creating School Directory for Public schools#############
###############################################################
#KC 4/18/2021
#SP 4/18/2021


#Read in excel files with public school districts and public schools from NCES database
#https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&State=41

#Sanjana setup
directory <- "C:/Users/spampat/Documents/EPI 760 - Causal inference"
setwd(directory)
NCES <- read_excel('NCES_AL_GA_LA_MS_SC.xlsx')
FIPS <- read_excel("FIPS_Codes.xlsx")
SCHOOL_POLICY <- read_excel("mch_schooldata.xlsx")
ccvi <- read.csv("ccvi.csv")


#NCES <- read_excel('NCES Data/NCES_AL_GA_LA_MS_SC.xlsx')
#FIPS <- read_excel("NCES Data/FIPS_Codes.xlsx")

FIPS <- FIPS %>% filter( State == "AL" | State == "GA" | State == "LA" | State == "MS" | State == "SC")
NCES_FIPS <- merge(NCES, FIPS, by.x = c("County Name","State"), by.y = c("Name","State"))
SCHOOL_POLICY <- SCHOOL_POLICY %>% filter(PhysicalState=="AL" | PhysicalState == "GA" | PhysicalState == "LA" | PhysicalState == "MS" | PhysicalState == "SC")
names(NCES_FIPS)[names(NCES_FIPS)=="NCES District ID"] <- "DistrictNCES"
POLICY_FIPS <- merge(SCHOOL_POLICY, NCES_FIPS, by="DistrictNCES") 

#teaching method

POLICY_FIPS$TeachingMethod[POLICY_FIPS$TeachingMethod=="Other" | POLICY_FIPS$TeachingMethod=="Pending" | POLICY_FIPS$TeachingMethod=="Unknown"] <- NA
POLICY_FIPS$school_inperson <- ifelse(POLICY_FIPS$TeachingMethod=="Hybrid" | POLICY_FIPS$TeachingMethod=="On Premises", 1, 0)
merged <- merge(POLICY_FIPS, ccvi, by="FIPS")

#pull in geography
us <- counties(cb = TRUE, resolution = '5m',
               class = 'sf',
               year = 2018) %>%
  st_transform(5070)



us <- us %>%
  dplyr::select(GEOID, STATEFP, COUNTYFP, NAME)


names(us)[names(us)=="COUNTYFP"] <- "FIPS"
us$FIPS <- as.numeric(us$FIPS)

merged2 <- us %>%
  right_join(merged, by = "FIPS")
>>>>>>> 4b2c8130ab9285b2afc64b6f92d60115893bf7b6
