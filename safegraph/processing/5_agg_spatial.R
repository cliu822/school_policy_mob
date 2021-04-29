library(viridis)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/Research/school_policy_mob/safegraph/data_cbg_naic")
school_stat <- read.csv("../../01_data/final_allUS.csv")
naics <- read.csv("../naics_add.csv")
pop <- readRDS("../census_age.rds")

## Pop data wrangling
pop <- pop%>% arrange(census_block_group)
pop$census_block_group <- as.character(pop$census_block_group)
#pop$fip_state <- gsub('.{10}$', '', pop$census_block_group)
pop <- pop %>% mutate(census_block_group = ifelse(nchar(census_block_group) ==11, paste("0",census_block_group,sep = ""),census_block_group))

## Calculating population by county and number above 25 by county
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
school_stat <- school_stat %>% mutate(FIPS = ifelse(nchar(FIPS)==4, paste("0", FIPS, sep =""),FIPS))

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
                  filter(first < as.Date("2020-10-10"))

### Code below takes the mobility data disaggregated by POI and CBG for each state and does the following:
## - recalculates the adjusted visit count (correctly)
## - reaggregates by "index" (CBG), week and naics category
## - Joins in CBG level age cat data to estimate number of visits made by adults aged >25
## - Aggregates to county level
## - Calculates estimated percap trips made by those above >25 per county per week (long listed by week/county)
## - Joins in school start date
## - Time between the week and school start date



setwd("C:/Users/cliu369/myLocalDirectory")

state <- read.csv("state.csv")
state <- state %>% mutate(FIPS = ifelse(nchar(FIPS)==1, paste("0",FIPS, sep = ""),FIPS))

filenames<- list.files(path = "1_res", pattern = "RDS")
filenames <- filenames[-which(filenames %in% c("GU.RDS","MP.RDS","PR.RDS","VI.RDS"))]


state_names <- substr(filenames, start = 1, stop =2)


df <- list()

for (i in 1:length(filenames)){
   state_fip <- state$FIPS[which(state$State == state_names[i])]
   
   df<- readRDS(paste("1_res/", filenames[i], sep = "")) %>%
    
  
             mutate(adj_visit_count = visitor_home_cbgs * (unweighted_pop/number_devices_residing)*(raw_visit_counts/raw_visitor_counts)) %>%

              filter(!is.na(naics_code)) %>% 
              left_join(naics, by = "naics_code", all=F) %>%   ## If want to recategorize NAICS, would need to rejoin NACIS here
                
              group_by(index, date_range_start, final_cat) %>% 
              summarize(tot_trips = sum(adj_visit_count))%>%  
    
    left_join(pop %>% select(-num_less25) %>%
                 mutate(census_block_group = as.character(census_block_group)),
                by = c("index" = "census_block_group"), all=F) %>%
                mutate(statefips = substr(index,1,2),
                       countyfips = substr(index,3, 5)) %>%
                
                filter(statefips == unlist(state_fip),
                       prop25 != "-Inf")  %>%
                       
                mutate(tot_trip25 = tot_trips * prop25)  %>%
    
          group_by(statefips, countyfips, date_range_start, final_cat) %>%
                summarize(tot_trips = sum(tot_trips),
                          tot_trip25 = sum(tot_trip25))  %>% 
    
    left_join(countypop, by= c("statefips"="statefips","countyfips"="countyfips")) %>%  ## Already calculated population by county
    mutate(percap_trip25 = tot_trip25/num_more25,                                      ## And pop over 25 by county
           percap_trip = tot_trips/totpop,
           FIPS = paste(statefips,countyfips,sep=""),
           week = as.Date(date_range_start)) %>%
    
            left_join(ss_county_first, by="FIPS") %>%   ## County data on when the first school district opened
      
      filter(!is.na(first)) %>% 
      
      mutate(                                          ## Time between week and school start date
            week = gsub("_","-",week),
            week = as.Date(week),
            index_time =  week -first)
  
  saveRDS(df, paste("2_res/",state_names[i],".RDS",sep=""))
  
}

### Read all the states processed data back in
df99 <- list()
filenames<- list.files(path = "2_res", pattern = "RDS")
for (i in 1:length(filenames)){
  df99[[i]] <- readRDS(paste("2_res/",filenames[i],sep=""))
}


df99<- do.call(rbind,df99)                
  

### spread to wide form
## Recategorize time days between week and school start date into weeks 

df99 <- df99 %>% mutate(
       index_week = ifelse(index_time < -28, NA,
                      ifelse(index_time < -21 & index_time >= -28, "bef4",
                         ifelse(index_time < -14 & index_time >= -21, "bef3",
                           ifelse(index_time < -7 & index_time >= -14, "bef2",
                              ifelse(index_time < 0 & index_time >= -7, "bef1",
                                ifelse(index_time <7 & index_time >= 0, "post1",
                                    ifelse(index_time <14 & index_time >= 7, "post2",
                                      ifelse(index_time <21 & index_time >= 14, "post3",
                                        ifelse(index_time < 28 & index_time >= 21, "post4", NA)))))) )))
)

## Filter on trips made to non-essential places and save to RDS

df_ne_wide <- df99 %>% filter(!is.na(index_week),final_cat ==0)%>%ungroup()%>%
  select(FIPS,index_week, percap_trip25)

df_ne_wide <- df_ne_wide %>% spread(index_week, percap_trip25)
df_ne_wide <- df_ne_wide %>% rowwise() %>% 
              mutate(pre = mean(c(bef1,bef2,bef3,bef4), na.rm=T),
                     post = mean(c(post1,post2,post3,post4), na.rm=T))

saveRDS(df_ne_wide, "../../01_data/05_mobs_ne_county_allUS.RDS")

### school

df_sch_wide <- df99 %>% filter(!is.na(index_week),final_cat ==2)%>%ungroup()%>%
  select(FIPS,index_week, percap_trip25) %>%
  spread(index_week, percap_trip25) %>%
  rowwise() %>% 
  mutate(pre = mean(c(bef1,bef2,bef3,bef4), na.rm=T),
         post = mean(c(post1,post2,post3,post4), na.rm=T))


### Plot####
setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/Research/school_policy_mob/safegraph/data_cbg_naic")
df99 <- df99 %>% left_join(school_stat %>% select(FIPS, status_county) %>% unique(),
                    by = "FIPS") 
df99 <- df99 %>% mutate(index_week = factor(index_week, levels=c("bef4","bef3","bef2","bef1",
                                                                 "post1","post2","post3","post4")))
png("../plots/percap_visits_nonessential_allUS.png", units="in", width=6, height=5, res=600)
df99 %>% filter(final_cat ==0) %>%group_by(status_county,index_week) %>%
  filter(!is.na(index_week)) %>%
  summarize(percap_trip25 = mean(percap_trip25,na.rm = T)) %>%
  ggplot(aes(x=index_week, y=percap_trip25, color= as.factor(status_county))) +
  geom_line(aes(color=as.factor(status_county),group = status_county), size=1) +
  geom_point(aes(color=as.factor(status_county), group = status_county), size=2)+
  scale_color_manual(name = "Policy",values = c("#9ECAE1","#084594"),labels = c("Virtual","Any in-person"))+
  theme_classic()+ ylim(0,6.5) +ylab("Per capita visits per week")+xlab("Week from school start")+
  scale_x_discrete(labels = c("-4","-3","-2","-1","1","2","3","4"))+
  ggtitle("Per capita visits per week to non-essential places")
dev.off()

png("../plots/percap_visits_essential_allUS.png",  units="in", width=6, height=5, res=600)
df99 %>% filter(final_cat ==1) %>%group_by(status_county,index_week) %>%
  filter(!is.na(index_week)) %>%
  summarize(percap_trip25 = mean(percap_trip25,na.rm = T)) %>%
  ggplot(aes(x=index_week, y=percap_trip25, color= as.factor(status_county))) +
  geom_line(aes(color=as.factor(status_county),group = status_county), size=1) +
  geom_point(aes(color=as.factor(status_county), group = status_county), size=2)+
  scale_color_manual(name = "Policy",values = c("#9ECAE1","#084594"),labels = c("Virtual","Any in-person"))+
  theme_classic() + ylim(0,6.5) + ylab("Per capita visits per week")+xlab("Week from school start")+
  scale_x_discrete(labels = c("-4","-3","-2","-1","1","2","3","4"))+
  ggtitle("Per capita visits per week to essential places")
dev.off()

png("../plots/percap_visits_school_allUS.png",  units="in", width=6, height=5, res=600)
df99 %>% filter(final_cat ==2) %>%group_by(status_county,index_week) %>%
  filter(!is.na(index_week)) %>%
  summarize(percap_trip25 = mean(percap_trip25,na.rm = T)) %>%
  ggplot(aes(x=index_week, y=percap_trip25, color= as.factor(status_county))) +
  geom_line(aes(color=as.factor(status_county),group = status_county), size=1) +
  geom_point(aes(color=as.factor(status_county), group = status_county), size=2)+
  scale_color_manual(name = "Policy",values = c("#9ECAE1","#084594"),labels = c("Virtual","Any in-person"))+
  theme_classic() + ylim(0,6.5) +ylab("Per capita visits per week")+xlab("Week from school start")+
  scale_x_discrete(labels = c("-4","-3","-2","-1","1","2","3","4"))+
  ggtitle("Per capita visits per week to schools")
dev.off()



#################################################
######################Junk code##################
################################################
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

df <-df 


df %>% 
  
  mutate(statefips = substr(index,1,2),
         countyfips = substr(index,3, 5)) %>%
  
  filter(!is.na(naics_code)) %>% 
  left_join(naics, by = "naics_code", all=F) %>%
  
  group_by(index, date_range_start, final_cat) %>% 
  summarize(tot_trips = sum(adj_visit_count))

df<- list()

for (i in 1:length(filenames)){
  df[[i]] <- readRDS(filenames[i])
}


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

df99 <- df3 %>% filter(statefips %in% c("01","13","22","28","45"),
                       prop25 != "-Inf")

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