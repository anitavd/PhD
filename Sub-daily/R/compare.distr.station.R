library(evd)
library(extRemes)

without <- 18701

file <- paste("/home/anitavd/PhD/Sub-daily/data/tipping/",without,"_hourly_AM.txt",sep="")

station <- read.table(file,header=F,skip=3)
colnames(station) <- c("STNR","EAST","NORTH","LON","LAT","YEAR","RR_1")
station$RR_1[which(station$RR_1 >= 50)] = NA
data <- station$RR_1[!is.na(station$RR_1)]

loc.obs <- fgev(data,shape=0.15)$estimate[1]
scale.obs <- fgev(data,shape=0.15)$estimate[2]

file <- paste("/home/anitavd/PhD/Sub-daily/Results/Alex_map/loc.mod6_",without,".RData",sep="")
load(file)
loc.mod <- mean(Z[,1],na.rm=T)
rm(Z)
file <- paste("/home/anitavd/PhD/Sub-daily/Results/Alex_map/scale.mod6_",without,".RData",sep="")
load(file)
scale.mod <- mean(Z[,1],na.rm=T)
rm(Z)

obs.med <- loc.obs + (scale.obs/0.15)*((log(2))^(-0.15)-1)
mod.med <- loc.mod + (scale.mod/0.15)*((log(2))^(-0.15)-1)
obs.q90 <- loc.obs + (scale.obs/0.15)*((log(1/0.9))^(-0.15)-1)
mod.q90 <- loc.mod + (scale.mod/0.15)*((log(1/0.9))^(-0.15)-1)

mod <- gen.gev(p=c(loc.mod,scale.mod,0.15),n=1000000)
obs <- gen.gev(p=c(loc.obs,scale.obs,0.15),n=1000000)
ymax <- max(max(density(obs)$y),max(density(mod)$y))
xmax <- max(obs.q90,mod.q90)

pdf(paste("~/PhD/Sub-daily/Results/Figs/PDF_",without,".pdf",sep=""))
plot(density(obs),ylim=c(0,ymax+0.01),xlim=c(0,xmax+20),xlab="[mm]",main="",ylab="Probability density",col="gray60",lwd=3,axes=F)
lines(density(mod),lwd=3)
axis(1,at=c(0,10,20,30,40))
axis(2)

abline(v=obs.med,col="gray60",lwd=2,lty="dashed")
abline(v=obs.q90,col="gray60",lwd=2,lty="dashed")
abline(v=mod.med,lwd=2,lty="dashed")
abline(v=mod.q90,lwd=2,lty="dashed")

legend(xmax+10,ymax+0.005,legend=c("Fit to obs","Mod"),col=c("gray60","black"),lty="solid",lwd=3,bty="n")
text(xmax+2,ymax,"q90")
text(max(mod.med,obs.med)+2,ymax,"q50")
text(0,ymax+0.01,"b)",font=2)

dev.off()

#############################################################################
#The following is dependent on some variables from the previous

without <- 18701

retper <- c(5,10,20,50,100,200)
p <- c(0.2,0.1,0.05,0.02,0.01,0.005)

file <- paste("/home/anitavd/PhD/Sub-daily/Results/Alex_map/MTmod6_",without,".RData",sep="")
load(file)
MT.mean.mod <- colMeans(Z)
MT.q5.mod <- c()
MT.q95.mod <- c()
MT.mean.obs <- c()

for (i in 1:ncol(Z)) {
	MT.q5.mod <- c(MT.q5.mod,quantile(Z[,i],probs=0.05))
	MT.q95.mod <- c(MT.q95.mod,quantile(Z[,i],probs=0.95))
	MT.mean.obs <- c(MT.mean.obs,loc.obs - scale.obs/0.15 * (1 - (-log(1 - p[i]))^(-0.15)))
}

ymax <- max(MT.q95.mod)+5
ymin <- min(MT.q5.mod)-5

pdf(paste("~/PhD/Sub-daily/Results/Figs/MT_",without,".pdf",sep=""))
plot(log(retper),MT.mean.obs,col="gray60",pch=17,xlab="Return period [years]",ylab="[mm]",axes=F,main="",ylim=c(0,80))
points(log(retper),MT.mean.mod,pch=19)
lines(log(retper),MT.q5.mod,lty="dashed")
lines(log(retper),MT.q95.mod,lty="dashed")

axis(1,at=log(retper),labels=retper)
axis(2)
#axis(2,at=c(0,20,40,60,80))

legend(log(retper)[1],70,legend=c("Fit to obs","Mod"),pch=c(17,19),col=c("gray60","black"),bty="n")

text(log(retper)[3]+0.4,80,paste("Station ",without,sep=""),font=2)
text(log(retper)[1],80,"a)",font=2)

dev.off()

