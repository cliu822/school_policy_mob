
###############################################################
######Creating School Directory for Public schools#############
###############################################################
#KC 4/18/2021


#Read in excel files with public school districts and public schools from NCES database
#https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&State=41

NCES <- read_excel('NCES Data/NCES_AL_GA_LA_MS_SC.xlsx')
FIPS <- read_excel("NCES Data/FIPS_Codes.xlsx")

FIPS <- FIPS %>% filter( State == "AL" | State == "GA" | State == "LA" | State == "MS" | State == "SC")

NCES_FIPS <- merge(NCES, FIPS, by.x = c("County Name","State"), by.y = c("Name","State"))
