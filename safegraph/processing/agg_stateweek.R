setwd("C:/Users/cliu369/myLocalDirectory/patterns/splitdat")

filenames <- list.files(pattern="RDS")

state <- filenames[which(grepl("2020_07_15",filenames))]

weeks <- c("2020_07_01","2020_07_08","2020_07_15")
state <- unique(sub("_2020.*","",filenames))

                
for (i in 1:length(state)){
  for (j in 1:length(weeks)){
    files <- filenames[which(grepl(paste(state[i],"_",weeks[j],sep=""),filenames))]
    df99 <- list()
     for (n in 1: length(files)){
       df99[[n]] <- readRDS(files[n])
     }
    
    df99 <- do.call(rbind,df99)
    saveRDS(df99,file =paste("week/",state[i],"_",weeks[j],".RDS",sep=""))
    
  }
}

