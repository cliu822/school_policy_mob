library(SafeGraphR)
library(data.table)
library(dplyr)

setwd("C:/Users/cliu369/myLocalDirectory")
## cenus data wrangling
census <- read.csv("census/data/cbg_b01.csv")
names<- c("B01001e27","B01001e28","B01001e29","B01001e30","B01001e31","B01001e32","B01001e33","B01001e34", ##females under 25
          "B01001e3","B01001e3","B01001e5","B01001e6","B01001e7","B01001e8","B01001e9","B01001m10") ## males under 25

census$num_less25 <- census[,which(colnames(census) %in% names)] %>% rowSums()
census_age <- census %>% mutate(num_above14 = B01001e1-B01001e27-B01001e28-B01001e29-B01001e3-B01001e4-B01001e5,
                                num_more25 = B01001e1 - num_less25,
                                prop25 = num_more25/num_above14) %>%
              select(census_block_group, B01001e1, num_above14, num_more25,num_less25, prop25)

## Read in Georgia data
df<- readRDS("patterns/splitdat/week/GA_2020_07_01.RDS")
poi <- readRDS("poi/GA.RDS")
naics <- read.csv("poi/naics.csv")

## Filter on relevant NAIAC codesss
poi <- poi %>% filter(!is.na(naics_code)) %>%  ## removed 2000 or so without naics code
  
  left_join(naics %>% select(sub_category, Cat), by="sub_category", all=F) %>% # categories of business
  
  select(safegraph_place_id, top_category,sub_category,naics_code,latitude, longitude,Cat) %>% ## select columns
  
  unique()

##
filenames <- list.files(path = "patterns/splitdat/week", pattern = "RDS")
state <- c("GA")
weeks <- c("2020_07_01","2020_07_08","2020_07_15")

for (i in 1:length(state)){
    files <- filenames[which(grepl(paste(state[i],sep=""),filenames))]
    df99 <- list()
    for (n in 1: length(files)){
      dat <- readRDS(paste("patterns/splitdat/week/",files[n], sep=""))
      dat <- dat %>% left_join(poi, by="safegraph_place_id") %>% filter(!is.na(Cat))
      df99[[n]] <- expand_cat_json(dat, 'visitor_home_cbgs', by="naics_code")
      df99[[n]]$week <- weeks[n]
      
    }
    
    df99 <- do.call(rbind,df99)
    saveRDS(df99,file =paste("0_res/",state[i],".RDS",sep=""))
    
}



## Join naics code and categories with the data
df1 <- df %>% left_join(poi, by="safegraph_place_id") %>% filter(!is.na(Cat))
## Expand the census block group

ex <- expand_cat_json(df1, 'visitor_home_cbgs', by="naics_code")






#ex1 <- expand_cat_json(df1, 'visitor_home_cbgs', by="Cat")


naics_code <- poi %>% select(naics_code, top_category, sub_category) %>% unique()
naics_code2 <- poi %>% select(naics_code, top_category, sub_category,category_tags) %>% unique()

write.csv(naics_code, "poi/naics.csv")
write.csv(naics_code2,"poi/naics2.csv")


