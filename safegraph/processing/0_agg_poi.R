##POI disagg
##Taking the point of interest data from safegraph from August, rearranging by state and combining the different parts of the week
## ALso writing out to help categorize NAICs codes.

setwd("C:/Users/cliu369/myLocalDirectory/2020/08")
filenames <- list.files(pattern="csv")

for (i in 1:length(filenames)){
  df <- fread(filenames[i])
  part <- sub("core_poi- *(.+) *.csv.*","\\1", filenames[i])

  ls <- split(df, df$region)
  
  
  for (j in 1:length(ls)){
    saveRDS(ls[[j]], file = paste("../../poi/",names(ls[j]),"_08",part,".RDS", sep=""))
  }
  
}

##Reagg poi by state from the 4 different parts
setwd("C:/Users/cliu369/myLocalDirectory/poi")
filenames <- list.files(pattern="RDS")
state <- unique(sub("_08.*","",filenames))


for (i in 1:length(state)){
  files <- filenames[which(grepl(state[i],filenames))]
  df99 <- list()
  for (n in 1: length(files)){
    df99[[n]] <- readRDS(files[n])
  }
  
  df99 <- do.call(rbind,df99)
  saveRDS(df99,file =paste(state[i],".RDS",sep=""))
  
  
}


####Combine all the states###
list <- list()
for (i in 1:length(filenames)){
  list[[i]] <- readRDS(filenames[i])
}


poi<- do.call(rbind,list)

naics <- read.csv("naics.csv")

##Join different states
poi1 <- readRDS("GA.RDS")
poi2 <- readRDS("AL.RDS")
poi3 <- readRDS("LA.RDS")
poi4 <- readRDS("MS.RDS")
poi5 <- readRDS("SC.RDS")
# combine
poi <- rbind(poi1,poi2,poi3,poi4,poi5)
saveRDS(poi,"poi_comb.RDS")

## Recheck naics_codelist
naics_code <- poi %>% select(naics_code, top_category, sub_category) %>% unique()

setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/Research/school_policy_mob/safegraph/data_cbg_naic")
naics <- read.csv("../naics_final.csv")
naics_code1 <- naics_code %>% left_join(naics %>% select(naics_code, naicskey, NAICS_Subgroup, Essential, Cat, final_cat))
write.csv(naics_code1, "../naics_add.csv")





