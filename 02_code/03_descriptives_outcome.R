###############################################################
######     Creating Outcome Descriptive    ###################
###############################################################
#KC 4/21/2021

library(readr)
library(tidyverse)
library(tmap)
library(tidycensus)
library(sf)
library(tigris)

safegraph <- readRDS("~/GitHub/school_policy_mob/01_data/mobs_ne_county.RDS")
AL <- readRDS("~/GitHub/school_policy_mob/safegraph/data_cbg_naic/AL.RDS")
GA <- readRDS("~/GitHub/school_policy_mob/safegraph/data_cbg_naic/GA.RDS")
LA <- readRDS("~/GitHub/school_policy_mob/safegraph/data_cbg_naic/LA.RDS")
MS <- readRDS("~/GitHub/school_policy_mob/safegraph/data_cbg_naic/MS.RDS")
SC <- readRDS("~/GitHub/school_policy_mob/safegraph/data_cbg_naic/SC.RDS")


final <- read_csv("05_final2.csv")
head(final)
summary(final$status_county)

summary(safegraph$pre)
summary(safegraph$post)

final_cnty <- final %>%
  group_by(FIPS) %>%
  slice(1) %>% select(FIPS, status_county)

FIPS1 <- sprintf("%05d", final_cnty$FIPS)
fips <- cbind(final_cnty,  fips_code = FIPS1)


#Merge opening status with outcome data

sg_cnty <- merge(fips,safegraph,by.y = "FIPS", by.x = "fips_code")

mob_open_pre <- mean(sg_cnty$pre[sg_cnty$status_county == 1])
mob_closed_pre <- mean(sg_cnty$pre[sg_cnty$status_county == 0])
mob_open_post <- mean(sg_cnty$post[sg_cnty$status_county == 1])
mob_closed_post <- mean(sg_cnty$post[sg_cnty$status_county == 0])




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
sg_cnty$FIPS <- as.numeric(sg_cnty$FIPS)

sg_cnty_shp <- us %>%
  left_join(sg_cnty, by = "FIPS")

#state borders
states <- states(cb = TRUE, resolution = '5m',
                 class = 'sf',
                 year = 2018) %>%
  st_transform(5070)

states <- states %>%
  filter((STATEFP %in% c('01','13','22','28','45'))) %>% #We are pulling the southeast states
  dplyr::select(GEOID, STATEFP, NAME)

plot(st_geometry(states))


#map of school reopening status
t1 <- tm_shape(sg_cnty_shp) + 
  tm_fill('pre',
          style = 'fixed',
          breaks = c(0.02,0.105,0.150, 0.190, 0.240, 0.50, Inf),
          palette = 'OrRd', 
          title = 'Per capita visits')+ 
  tm_borders(alpha = 0.2) +
  tm_layout(main.title = 'Summer 2020',
            legend.outside = T, bg.color='white', attr.outside='TRUE', main.title.size=1.4)+
  tm_shape(states)+
  tm_borders('Black')

t1


t2 <- tm_shape(sg_cnty_shp) + 
  tm_fill('post',
          style = 'fixed',
          breaks = c(0.02,0.105,0.150, 0.190, 0.240, 0.50, Inf),
          palette = 'OrRd', 
          title = 'Per capita visits')+ 
  tm_borders(alpha = 0.2) +
  tm_layout(main.title = 'Fall 2020',
            legend.outside = T, bg.color='white', attr.outside='TRUE', main.title.size=1.4)+
  tm_shape(states)+
  tm_borders('Black')

t2

prepost <- tmap_arrange(t1, t2, ncol=1)

prepost







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