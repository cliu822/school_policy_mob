memory.limit(size=1000000000)
library(data.table)
library(here)

#data.dir<- here('safegraph')
setwd("C:/Users/cliu369/myLocalDirectory/patterns/2020")
filenames <- list.files(pattern="csv")

for (i in 1:60){
    df<- fread(filenames[i])
    datepart <- sub(".patterns- *(.+) *.csv.*","\\1", filenames[i])
    date <- sub("_patterns.*","",filenames[i])
    date<- gsub("_","-",date)
    
    df$week <- as.Date(date)
    ls <- split(df, df$region)
    
    
    for (i in 1:length(ls)){
     saveRDS(ls[[i]], file = paste("../splitdat/",names(ls[i]),"_",datepart,".RDS", sep=""))
    }
   rm(df)
}







