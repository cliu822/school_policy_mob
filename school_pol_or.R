library(dplyr)
library(tidyr)
library(forcats)
library(readxl)
library(ggplot2)
library(here)
library(stringr)

## Read in all excel sheets from weekly school status data downloaded from: 
##https://www.oregon.gov/ode/students-and-family/healthsafety/Pages/2020-21-School-Status.aspx

x <- list()
folder <- here('oregon_status')
filenames <- list.files(folder,pattern = "\\.xlsx", full.names = T) ## list all excel files

for (n in 1:length(filenames))
{
  x[[n]] = read_excel(filenames[n]) #read SAS file
  x[[n]]$week = str_match(filenames[n], "COVID-19 SCHOOL STATUS REPORT \\s*(.*?)\\s* to ")[2] #add column indicating week
  
}

sch_stat <- do.call(plyr::rbind.fill,x)

colnames(sch_stat)[1:5] <- c("ID","school_name","district","week_sr","status")

sch_stat<- sch_stat %>% mutate(
          status_cat = status,
          status_cat = ifelse(status == "Fully On-Site"| status == "On-Site (Green)", "on_site",
                          ifelse(status == "Comprehensive Distance Learning (CDL)"| status == "Distance Learning (Red)", "distance",
                              ifelse(status == "Hybrid"| status == "On-Site and Distance Learning (Yellow)", "hybrid",
                                  ifelse(status == "Transition (Orange)", "transition",
                                     ifelse(is.na(status), NA, "noclass"))))),
          week = factor(week, levels = c("9-27", "10-4", unique(sch_stat$week)[1:3], "11-1", "11-8",unique(sch_stat$week)[6:8], 
                                         unique(sch_stat$week)[10:12])))

## Write out RDS file for fall
saveRDS(sch_stat, "oregon_status/school_stat_fall2020.RDS")

###############################################################
##########Exploring school opening situation####################
###############################################################
summary <- sch_stat %>% group_by(district, week, status_cat) %>% 
            summarize(sum=n()) %>% 
            left_join(sch_stat %>% group_by(district, week) %>% summarize(tot = n()), 
            by= c("district"="district", "week"="week"))

## Plot of estimated proportion of districts offering different instruction modes over weeks
## NOte that I did not recategorize this as one district one value, districts with different instruction modes are duplicated here
summary %>% group_by(week, status_cat) %>% summarize(n=n()) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=week, y = prop, color = status_cat)) + geom_col(aes(group = status_cat, fill = status_cat))+
  theme(axis.text.x = element_text(angle = 45)) + ggtitle("Estimated % school districts offering instructional models by week")

## Plot of estimated schools offering different instruction modes over weeks
sch_stat %>% group_by(week, status_cat) %>% 
  summarize(sum=n()) %>% 
  left_join(sch_stat %>% group_by(week) %>% summarize(tot = n()), 
            by= c("week"="week")) %>% group_by(week)%>% mutate(prop = sum/tot) %>%
  ggplot(aes(x=week, y = prop, color = status_cat)) + geom_col(aes(group = status_cat, fill = status_cat))+
  theme(axis.text.x = element_text(angle = 45)) + ggtitle("Estimated % schools offering instructional models by week")


## What percent of districts with some in-person have all schools in-person? A little over half I think 
summary %>% mutate(prop = sum/tot)  %>%  filter(status_cat == "on_site") %>% 
  mutate(all_onsite = ifelse(prop ==1, 1,0)) %>% group_by(week, all_onsite) %>% summarize(n=n()) %>% mutate(n/sum(n)) %>%
  filter(all_onsite ==1)

