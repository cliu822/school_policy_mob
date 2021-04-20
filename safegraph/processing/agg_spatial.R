setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/Research/school_policy_mob/safegraph/data_cbg_naic")
school_stat <- read.csv("../../05_final2.csv")
naics <- read.csv("../naics_final.csv")
pop <- readRDS("../census_age.rds")

## Pop data wrangling
pop <- pop%>% arrange(census_block_group)
pop$census_block_group <- as.character(pop$census_block_group)
pop$census_block_group[1:3438]<- paste("0",pop$census_block_group[1:3438],sep="")

countypop <- pop%>%
              mutate(statefips = substr(census_block_group,1,2),
                     countyfips = substr(census_block_group,3, 5)) %>%
              group_by(statefips, countyfips) %>%
              summarize(totpop = sum(B01001e1),
                        num_above14 = sum(num_above14),
                        num_more25 = sum(num_more25))
## School stat wrangling
school_stat <- school_stat %>% arrange(FIPS)
school_stat$FIPS <- as.character(school_stat$FIPS)
school_stat$FIPS[1:135] <- paste("0",school_stat$FIPS[1:135],sep = "")

ss_county <- school_stat %>%
            mutate(
               OpenDate=as.Date(school_stat$OpenDate)
            ) %>%
            group_by(FIPS) %>% 
                mutate(
                  first = min(OpenDate),
                  last = max(OpenDate)
                ) %>% select(FIPS, first,last) %>%
            mutate(
              daydiff = last-first
            )

ss_county_first <- ss_county %>% select(FIPS, first) %>% unique() %>%
                  filter(first < as.Date("2020-09-11"))

### POI mobility data

filenames<- list.files(pattern = "RDS")
df<- list()

for (i in 1:length(filenames)){
  df[[i]] <- readRDS(filenames[i])
}

df<- do.call(rbind,df)
df <-df %>% mutate(statefips = substr(index,1,2),
             countyfips = substr(index,3, 5)) 

## Percent of dropped trips because missing naics code
dropped <- df %>% filter(is.na(naics_code)) %>%
          summarize(sum=sum(visitor_home_cbgs))
totaltrips <- df %>% summarize(sum=sum(visitor_home_cbgs))

dropped/totaltrips ## Dropped 2% of visits due to missing naics_code

## Some CBGS appear multiple times for different POIs, want to aggregate this
df1 <- df %>% filter(!is.na(naics_code)) %>% 
  left_join(naics, by = "naics_code", all=F)


df2 <- df1 %>% group_by(index, week, final_cat) %>% summarize(tot_trips = sum(visitor_home_cbgs))

df3 <- df2 %>% left_join(pop %>% select(-num_less25) %>%
                           mutate(census_block_group = as.character(census_block_group)),
                         by = c("index" = "census_block_group"), all=F) %>%
                          mutate(statefips = substr(index,1,2),
                                countyfips = substr(index,3, 5))

df99 <- df3 %>% filter(statefips %in% c("01","13","22","28","45"))

df99 <- df99 %>% mutate(tot_trip25 = tot_trips * prop25)

df99 <- df99 %>%
  group_by(statefips, countyfips, week, final_cat) %>%
  summarize(tot_trips = sum(tot_trips),
            tot_trip25 = sum(tot_trip25)) 

df99<- df99 %>% left_join(countypop, by= c("statefips"="statefips","countyfips"="countyfips")) %>%
            mutate(percap_trip25 = tot_trip25/num_more25,
                   percap_trip = tot_trips/totpop)

df99 <- df99 %>% mutate(FIPS = paste(statefips,countyfips,sep="")) %>%
                left_join(ss_county_first, by="FIPS")

df99 <- df99 %>% filter(!is.na(first))

df99 <- df99 %>% mutate(
        week = gsub("_","-",week),
        week = as.Date(week),
        index_time =  week -first
)

### spread to wide form
### Join in county school opening date

df99 <- df99 %>% mutate(
       index_week = ifelse(index_time < -28, NA,
                      ifelse(index_time < -21 & index_time >= -28, -4,
                         ifelse(index_time < -14 & index_time >= -21, -3,
                           ifelse(index_time < -7 & index_time >= -14, -2,
                              ifelse(index_time < 0 & index_time >= -7, -1,
                                ifelse(index_time <7 & index_time >= 0, 1,
                                    ifelse(index_time <14 & index_time >= 7, 2,
                                      ifelse(index_time <21 & index_time >= 14, 3,
                                        ifelse(index_time < 28 & index_time >= 21, 4, NA)))))) )))
)

head(df)
      
df %>% spread(week, visitor_home_cbgs)



library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

census_api_key("e43913e7c2ca1b09ec3bc84deeb202b66d5cbcfc")

vars10<-c("B01001e1") #dummy variable which is a repeat of summary_var

census <- get_decennial(geography = "block group", variables = vars10, year = 2010,
                    summary_var = "P013001", state = "GA", geometry = TRUE)


#summary_var = "P001001", state = "GA", geometry = TRUE) %>%
#mutate

ga$county<-str_remove_all(ga$NAME, " County, Georgia")
ga$county<-toupper(ga$county)

age10 <- get_decennial(geography = "state", 
                       variables = , 
                       year = 2010)


setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/Research/school_policy_mob/safegraph")

sp <- geojson_read("cbg.geojson", what = "sp")