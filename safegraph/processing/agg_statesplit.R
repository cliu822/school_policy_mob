memory.limit(size=1000000000)
library(data.table)
library(here)

#data.dir<- here('safegraph')
setwd("C:/Users/cliu369/myLocalDirectory/patterns/2020")
filenames <- list.files(pattern="csv")

#for (i in 16:16){
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


##POI disagg
setwd("C:/Users/cliu369/myLocalDirectory/2020/07")
filenames <- list.files(pattern="csv")

for (i in 3:length(filenames)){
    df <- fread(filenames[i])
    part <- sub("core_poi- *(.+) *.csv.*","\\1", filenames[i])
    df$month <- "July"
    
    ls <- split(df, df$region)
    
    
    for (j in 1:length(ls)){
        saveRDS(ls[[j]], file = paste("../../poi/",names(ls[j]),"_07",part,".RDS", sep=""))
    }
    
}

##Reagg poi by state
setwd("C:/Users/cliu369/myLocalDirectory/poi")
filenames <- list.files(pattern="RDS")
state <- unique(sub("_07.*","",filenames))


for (i in 1:length(state)){
    files <- filenames[which(grepl(state[i],filenames))]
    df99 <- list()
    for (n in 1: length(files)){
        df99[[n]] <- readRDS(files[n])
    }
    
    df99 <- do.call(rbind,df99)
    saveRDS(df99,file =paste(state[i],".RDS",sep=""))
    
        
}





