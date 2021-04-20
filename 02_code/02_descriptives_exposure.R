
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

POLICY_FIPS <- read_csv("04_final_data_se.txt")


#NCES <- read_excel('NCES Data/NCES_AL_GA_LA_MS_SC.xlsx')
#FIPS <- read_excel("NCES Data/FIPS_Codes.xlsx")


#teaching method

POLICY_FIPS$TeachingMethod[POLICY_FIPS$TeachingMethod=="Other" | POLICY_FIPS$TeachingMethod=="Pending" | POLICY_FIPS$TeachingMethod=="Unknown"] <- NA
POLICY_FIPS$school_inperson <- ifelse(POLICY_FIPS$TeachingMethod=="Hybrid" | POLICY_FIPS$TeachingMethod=="On Premises", 1, 0)

POLICY_FIPS <- filter(POLICY_FIPS, !is.na(school_inperson))

POLICY_FIPS2 <- POLICY_FIPS %>%
    group_by(FIPS) %>%
   summarize(total = sum(school_inperson, na.rm=TRUE))

POLICY_FIPS2$status_county <- ifelse(POLICY_FIPS2$total>=1, 1, 0)

final <- POLICY_FIPS %>%
  left_join(POLICY_FIPS2, by = "FIPS")

#group by county and then say if any district status equals open 
#one

#pull in geography
us <- counties(cb = TRUE, resolution = '5m',
               class = 'sf',
               year = 2018) %>%
  st_transform(5070)




us <- us %>%
  filter((STATEFP %in% c('01','13','22','28','45'))) %>%
  dplyr::select(GEOID, STATEFP, COUNTYFP, NAME)


names(us)[names(us)=="GEOID"] <- "FIPS"
us$FIPS <- as.numeric(us$FIPS)

merged2 <- us %>%
  left_join(final, by = "FIPS")

#state borders
states <- states(cb = TRUE, resolution = '5m',
                 class = 'sf',
                 year = 2018) %>%
  st_transform(5070)

states <- states %>%
  filter((STATEFP %in% c('01','13','22','28','45'))) %>% #We are pulling the Northeast states
  dplyr::select(GEOID, STATEFP, NAME)

plot(st_geometry(states))


#map of school reopening status
t1 <- tm_shape(merged2) + 
  tm_fill('status_county',
          style = 'cat',
          labels =c("Virtual only", "Any in-Person", "Missing"),
          palette = 'BuPu', 
          title = 'School Reopening Status')+ 
  tm_borders(alpha = 0.2) +
  tm_layout(main.title = 'School Reopening Status, Deep South, US, Fall 2020',
            legend.outside = T, bg.color='white', attr.outside='TRUE', main.title.size=1.4)+
  tm_shape(states)+
  tm_borders('Black')

t1


table(merged2$school_inperson, useNA = "always")

merged3 <- merged2 %>% drop_na(school_inperson)
CrossTable(merged3$FIPS, merged3$school_inperson)


write.csv(final,"final.csv")

final2 <- final

final2 <- final2 %>% distinct(FIPS, .keep_all = TRUE)


label(final2$status_county)     <- "School Re-opening Status"

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c(sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

final2$status_county <- 
  factor(final2$status_county, 
         levels=c(0,1),
         labels=c("Virtual only", 
                  "Any in-person"))

label(final2$status_county) <- "School Re-opening Status"
label(final2$V3_THEME1) <- "Socioeconomic Status"
label(final2$V3_THEME2) <- "Minority Status & Language"
label(final2$V3_THEME3) <- "Housing type, Transportation, Household Composition & Disability"
label(final2$V3_THEME4) <- "Epidemiological Factors"
label(final2$V3_THEME5) <- "Healthcare System Factors"
label(final2$V3_THEME6) <- "High Risk Environments"
label(final2$V3_THEME7) <- "Population Density"

table1(~ V3_THEME1 + V3_THEME2 + V3_THEME3 + V3_THEME4 + V3_THEME5 + V3_THEME6 + V3_THEME7  | status_county, data=final2, overall="Total", render.continuous=my.render.cont)
