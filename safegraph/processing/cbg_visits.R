library(SafeGraphR)
library(data.table)
library(dplyr)

setwd("C:/Users/cliu369/myLocalDirectory")

## Read in POI and naics data
poi <- readRDS("poi/poi_comb.RDS")
naics <- read.csv("poi/naics.csv")

## Merge poi data with naics categorizations
poi <- poi %>% filter(!is.na(naics_code)) %>%  ## removed 3611 or so without naics code
  
  left_join(naics %>% select(naics_code, Cat), by="naics_code", all=F) %>% # categories of business
  
  select(safegraph_place_id, top_category,sub_category,naics_code,latitude, longitude,Cat) %>% ## select columns, keep this line
  
  unique()



## Filter on relevant NAIAC codesss


##
filenames <- list.files(path = "patterns/splitdat/week", pattern = "RDS")
state <- c("AL","GA","LA","MS","SC")
weeks <- c("2020_06_24","2020_07_01","2020_07_08","2020_07_15","2020_07_22","2020_07_29",
           "2020_08_05","2020_08_12","2020_08_19","2020_08_26","2020_09_02","2020_09_09",
           "2020_09_16","2020_09_23","2020_09_30")

for (i in 1:length(state)){
    files <- filenames[which(grepl(paste(state[i],sep=""),filenames))]
    df99 <- list()
    for (n in 1: length(files)){
      dat <- readRDS(paste("patterns/splitdat/week/",files[n], sep=""))
      dat <- dat %>% left_join(poi, by="safegraph_place_id")
      df99[[n]] <- expand_cat_json(dat, 'visitor_home_cbgs', by="naics_code")
      df99[[n]]$week <- weeks[n]
      
    }
    
    df99 <- do.call(rbind,df99)
    saveRDS(df99,file =paste("0_res/",state[i],".RDS",sep=""))
    
}

## cenus data wrangling
census <- read.csv("census/data/cbg_b01.csv")
names<- c("B01001e27","B01001e28","B01001e29","B01001e30","B01001e31","B01001e32","B01001e33","B01001e34", ##females under 25
          "B01001e3","B01001e3","B01001e5","B01001e6","B01001e7","B01001e8","B01001e9","B01001m10") ## males under 25

census$num_less25 <- census[,which(colnames(census) %in% names)] %>% rowSums()
census_age <- census %>% mutate(num_above14 = B01001e1-B01001e27-B01001e28-B01001e29-B01001e3-B01001e4-B01001e5,
                                num_more25 = B01001e1 - num_less25,
                                prop25 = num_more25/num_above14) %>%
  select(census_block_group, B01001e1, num_above14, num_more25,num_less25, prop25)

saveRDS(census_age,"census_age.RDS")
