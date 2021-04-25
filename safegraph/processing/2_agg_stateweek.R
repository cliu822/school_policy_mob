setwd("C:/Users/cliu369/myLocalDirectory/patterns/splitdat")

filenames <- list.files(pattern="RDS")

#weeks <- c("2020_07_22","2020_07_29","2020_08_05","2020_08_12","2020_08_19","2020_08_26","2020_09_02")
weeks <- c("2020_11_11", "2020_11_18","2020_11_25")
state <- unique(sub("_2020.*","",filenames))

#state1<- state[1:28]

#state<-state1


for (i in 1:length(state)){
  for (j in 1:length(weeks)){
    files <- filenames[which(grepl(paste(state[i],"_",weeks[j],sep=""),filenames))]
    df99 <- list()
     for (n in 1: length(files)){
       df99[[n]] <- readRDS(files[n])
     }
    
    df99 <- do.call(rbind,df99)
    saveRDS(df99,file =paste("week2/",state[i],"_",weeks[j],".RDS",sep=""))
    
  }
}

