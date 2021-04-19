## Safegraph data wrangle
memory.limit(size=1000000000)
library(data.table)
setwd("C:/Users/cliu369/myLocalDirectory/patterns/2020")
df <- fread("C:/Users/cliu369/myLocalDirectory/patterns/2020/06/24/16/patterns-part1.csv.gz")
df2 <- fread("C:/Users/cliu369/myLocalDirectory/patterns/2020/06/24/16/patterns-part2.csv.gz")
df3 <- fread("C:/Users/cliu369/myLocalDirectory/patterns/2020/06/24/16/patterns-part3.csv.gz")
df4 <- fread("C:/Users/cliu369/myLocalDirectory/patterns/2020/2020_06_24_patterns-part4.csv.gz")

df1 <- df[sample(nrow(df), size =1000),]

df4$week <-as.Date("2020-06-24")

ls <- split(df4, df4$region)

for (i in 1:length(ls)){
  saveRDS(ls[i], file = paste(names(ls[i]),"_2020-06-24",".csv.gz", sep=""))
}

rm(df3)

lapply(ls, function(x) saveRDS(x, file = paste(names(x),".rds")))

saveRDS(ls[1], file = paste(names(ls[1]),".RDS"))

