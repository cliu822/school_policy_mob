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




###############################################################
######Creating School Directory for Public schools#############
###############################################################
#KC 4/8/2021

#Read in excel files with public school districts and public schools from NCES database
#https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&State=41

OR_PubSchDist <- read_excel("oregon_school_directory/Oregon_PubSchDist.xlsx")
OR_PubSchList <- read_excel("oregon_school_directory/Oregon_PubSchList.xlsx")

#Check distribution of schools per district
OR_PubSchDist$Schools <- as.numeric(OR_PubSchDist$Schools)
hist(OR_PubSchDist$Schools)


#Merge directories to include district information with school list
OR_totalsch <- merge(OR_PubSchList, OR_PubSchDist, by = "NCES District ID")

sch_stat <- readRDS("oregon_status/school_stat_fall2020.RDS")
summary <- sch_stat %>% group_by(district, week, status_cat) %>% 
  summarize(sum=n()) %>% 
  left_join(sch_stat %>% group_by(district, week) %>% summarize(tot = n()), 
            by= c("district"="district", "week"="week"))

#Check % of students in OR that are in districts that have some in-person

summary[is.na(summary)] = "unknown"
df1 <- summary %>% mutate(onsite = ifelse(status_cat == "on_site", 1,0)) %>% 
  group_by(district) %>% mutate(wk_onsite = sum(onsite)) %>% filter(row_number(district) == 1)

OR_totalsch <- merge(OR_totalsch, df1, by.x = "District", by.y = "district")

summary(OR_totalsch$onsite)
sum(OR_totalsch$onsite)


OR_totalsch$Students_sch <- as.numeric(OR_totalsch$`Students*.x`)
OR_totalsch$Students_sch[is.na(OR_totalsch$Students_sch)] = 0
total_students <- sum(OR_totalsch$Students_sch)
onsite_student <- sum(OR_totalsch$Students_sch[which(OR_totalsch$onsite == 1)])
perc_onsite <- (onsite_student/total_students)*100


#Check % of students within each district with in-person that are actually in-person
OR_totalsch$Students_dist <- as.numeric(OR_totalsch$`Students*.y`)

df2 <- OR_totalsch %>%  filter(onsite == 1) %>% group_by(District) %>%
  mutate(students_onsite = sum(Students_sch)) %>% mutate(perc_onsite_dist = students_onsite/Students_dist) %>% 
  filter(row_number(District) == 1)

summary(df2$perc_onsite_dist)















