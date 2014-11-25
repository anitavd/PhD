#Endring av returnivå ved Blindern etter rekordhendelse 26.06.14

#Daily, klokkedøgn
a <- read.table("~/PhD//Rekordhendelse//18700_AM_daily.txt",header=F,dec=",")
b<-fevd(a[,1],type="GEV",method="MLE")
ci <- ci.fevd(b,type="return.level",return.period=c(5,10,20,50,100,200))
c<-fevd(c(a[,1],72.8),type="GEV",method="MLE")
plot(log(c(5,10,20,50,100,200)),ci[,2],pch=19,ylim=c(40,75),ylab="Return level [mm]",xlab="Return period [years]",axes=F)
axis(1,at=c(log(5),log(10),log(20),log(50),log(100),log(200)),labels=c(5,10,20,50,100,200))
axis(2)
lines(log(c(5,10,20,50,100,200)),ci[,1])
lines(log(c(5,10,20,50,100,200)),ci[,3])
points(log(c(5,10,20,50,100,200)),return.level(c,return.period=c(5,10,20,50,100,200)),pch=19,col="red")

#Hourly, klokketime
a <- read.table("~/PhD//Sub-daily//data//tipping//18701_hourly_AM.txt",header=F,skip=3)
a <- c(a[,7],10.3)
b<-fevd(a,type="GEV",method="MLE")
ci<-ci.fevd(b,type="return.level",return.period=c(5,10,20,50,100,200))
#c<-fevd(c(a,37.7),type="GEV",method="MLE")  #pluvio, haggel?
c<-fevd(c(a,44.5),type="GEV",method="MLE")  #geonor
plot(log(c(5,10,20,50,100,200)),ci[,2],pch=19,ylim=c(10,65),ylab="Return level [mm]",xlab="Return period [years]",axes=F)
axis(1,at=c(log(5),log(10),log(20),log(50),log(100),log(200)),labels=c(5,10,20,50,100,200))
axis(2)
lines(log(c(5,10,20,50,100,200)),ci[,1])
lines(log(c(5,10,20,50,100,200)),ci[,3])
points(log(c(5,10,20,50,100,200)),return.level(c,return.period=c(5,10,20,50,100,200)),pch=19,col="red")
