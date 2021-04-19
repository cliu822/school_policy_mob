# NAICS Fit
# KC 4/19/2021

library(readxl)
library(tidyr)
library(dplyr)

naics <- read.csv("safegraph/naics.csv")
key <- read_excel("safegraph/naics_processing/NAICS_key.xlsx")

#create 4 digit code for naics to merge with the key
naicskey <- sapply(naics$naics_code, function(x) substring(x, first=1, last= 4))
new <- cbind(naics, naicskey)

new1 <-  merge(new, key, by.x = "naicskey", by.y = "NAICSCode")
new2 <- new1 %>% select(naicskey, naics_code, sub_category, NAICS_Subgroup, Essential, Cat)
new3 <- new2 %>% mutate(final_cat = ifelse(Essential == "Yes", 1, ifelse(Cat == "School", 2, 0)))

write.csv(new3, "safegraph/naics_processing/naics_final.csv")