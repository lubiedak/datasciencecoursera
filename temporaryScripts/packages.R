
dat <-read.XML("getdata-data-restaurants.xml",)

DT<-fread("data/getdata-data-ss06pid.csv")

a1 <- system.time(replicate(1000,tapply(DT$pwgtp15,DT$SEX,mean)))

a2 <- system.time(replicate(1000,rowMeans(DT)[DT$SEX==1]))
a2 <- a2 + system.time(replicate(1000,rowMeans(DT)[DT$SEX==2]))

a3 <- system.time(replicate(1000,DT[,mean(pwgtp15),by=SEX]))
a4 <- system.time(replicate(1000,sapply(split(DT$pwgtp15,DT$SEX),mean)))
a5 <- system.time(replicate(1000,mean(DT$pwgtp15,by=DT$SEX)))


a6 <- system.time(replicate(1000,mean(DT[DT$SEX==1,]$pwgtp15)))
a6 <- a6+system.time(replicate(1000,mean(DT[DT$SEX==2,]$pwgtp15)))