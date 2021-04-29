library(SafeGraphR)
library(data.table)
library(dplyr)
setwd("C:/Users/cliu369/myLocalDirectory")

## Read in all home panel data
#home_panel <- read_many_csvs(dir="home_panel_summary")
#home_panel <- home_panel %>% mutate(census_block_group = as.character(census_block_group),
#                                    census_block_group = ifelse(nchar(census_block_group) == 11, paste("0", census_block_group, sep=""), census_block_group))

#home_panel <- home_panel %>% group_by(census_block_group) %>%
#                            summarize(number_devices_residing = mean(number_devices_residing, na.rm=T))
#write.csv(home_panel, "home_panel_summary/home_panel_comb.csv")
home_panel <- read.csv("home_panel_comb.csv")

#home_panel[,census_block_group := as.character(as.numeric(census_block_group))]  ## Make cbg value as character
#home_panel[,c('state_fips','county_fips') := fips_from_cbg(census_block_group)]

#poi <- read_many_csvs(dir="2020/08")

data(cbg_pop)
cbg_pop[,c('state_fips','county_fips') := fips_from_cbg(poi_cbg)]
cbg_pop[,poi_cbg := as.character(as.numeric(poi_cbg))]
cbg_pop <- cbg_pop %>% mutate(poi_cbg = as.character(poi_cbg),
                              poi_cbg = ifelse(nchar(poi_cbg) == 11, paste("0", poi_cbg, sep=""), poi_cbg))



#patterns <- readRDS("patterns/splitdat/week/AK_2020_06_24.rds")
#poi <- readRDS(paste("poi/","AK.RDS", sep=""))

#Function to do block-groupwise normalization.
#param patterns: Safegraph patterns dataset.
#pre patterns must have the columns: 'raw_visits_counts'.
#param home_summary: Safegraph home_panel_summary dataset.
#pre home_summary must have columns 'census_block_group' and 'number_devices_residing'.
#param bay_blockgroups: list of blockgroup GEOIDs in the bay area for filtering of the Safegraph datasets.
#param ca_pop_blockgroup: dataframe. Censis population count for each blockgroup in CA.
#returns patterns dataset with column visit_counts that multiples raw visits based on ratio of Bay area population to safegraph population.


normBG <- function(patterns, poi,
                   home_summary = home_panel, 
                   pop_blockgroup = cbg_pop)
{
  
  #Expand and categorize visitors by origin_census_block_group.
  #Also join population and home summary data.

  
  visit_cbg <- expand_cat_json(patterns, 'visitor_home_cbgs', by="safegraph_place_id") %>%     ##Explode JSON cells
    
      left_join(pop_blockgroup, by = c('index' = 'poi_cbg')) %>%                                ## Join CBG census pop data
    
      left_join(home_summary, by = c("index"="census_block_group")) %>%  ##Join in home panel by CBG
    
      left_join(patterns %>% 
                  select(safegraph_place_id, raw_visit_counts, raw_visitor_counts), by="safegraph_place_id") %>%
    
      mutate(adj_visit_count = (unweighted_pop/number_devices_residing)*(raw_visit_counts))%>% 
      left_join(poi %>% select(safegraph_place_id, naics_code), 
                                        by="safegraph_place_id")
return(visit_cbg)

}
  
  

##
filenames <- list.files(path = "patterns/splitdat/week", pattern = "RDS")
poi_file <- list.files(path="poi", pattern = "RDS")
state <- substr(poi_file, start = 1, stop =2)
state <- state[-which(state %in% c("AS","AK", "AL", "AR","AZ","CA", "CO", "CT","FL","TX","NY","IL","GA","OH","MI","PA","NC","MO","IN"))]
#state <- state[which(state %in% c("TX","UT","VA","VI","VT","WA","WI","WV","WY"))]

## AS doesnt work well here?

#state <- c("AL","GA","LA","MS","SC")
weeks <- c("2020_06_24","2020_07_01","2020_07_08","2020_07_15","2020_07_22","2020_07_29",
           "2020_08_05","2020_08_12","2020_08_19","2020_08_26","2020_09_02","2020_09_09",
           "2020_09_16","2020_09_23","2020_09_30","2020_10_07","2020_10_14","2020_10_21",
           "2020_10_28","2020_11_04","2020_11_11")


for (i in 1:length(state)){
    files <- filenames[which(grepl(paste(state[i],sep=""),filenames))]
    poi1 <- poi_file[which(grepl(paste(state[i], sep=""), poi_file))]
    poi <- readRDS(paste("poi/",poi1, sep=""))
    
    df99 <- list()
    for (n in 1: length(files)){
      dat <- readRDS(paste("patterns/splitdat/week/",files[n], sep=""))
      
      date <- unique(dat$date_range_start)
      date <- as.Date(date[1])
      
      df99[[n]] <- normBG(dat, poi)
      df99[[n]]$week <- weeks[n]
      df99[[n]]$date_range_start <- date
      
    }
    
    df99 <- do.call(rbind,df99)
    saveRDS(df99,file =paste("1_res/",state[i],".RDS",sep=""))
    
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


## Below code weighs to the county level? not really what we want

#adjust <- sample_size_adjust(
#            data = home_panel,
#            from_id = "census_block_group",
#            sample_id = "number_devices_residing",
#            from_level = "cbg",
#            to_level = "county",
#            by = 'date_range_start',
#            pop_data = cbg_pop
#          )

## Read in POI and naics data
#poi <- readRDS("poi/AK.RDS")


## Merge poi data with naics categorizations
#poi <- poi %>% filter(!is.na(naics_code)) %>%  ## removed 3611 or so without naics code

#  left_join(naics %>% select(naics_code, Cat), by="naics_code", all=F) %>% # categories of business

#  select(safegraph_place_id, top_category,sub_category,naics_code,latitude, longitude,Cat) %>% ## select columns, keep this line

#  unique()



## Filter on relevant NAIAC codesss