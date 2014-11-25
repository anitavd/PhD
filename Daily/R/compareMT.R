################################################################################################
#
# Plots areal MT using NERC, GEV and GPD methods
#
#
#Used extRemes to find best threshold (mrl.plot and gpd.fitrange): Arendal: 45 mm, Aursjøen: 37 mm, Aursunda: 41 mm, Barduelva: 29 mm, Båtsvatn: 30 mm, #Jolstra: 39 mm, Kongsvinger: 35 mm, Lauvsnes: 42 mm, Namsvatn: 37 mm, Roskreppfjord: 31 mm, Rossåga: 41 mm, Siljan: 42 mm, Sira: 33 mm, Soneren: 39 mm, #Svartevatn: 33 mm, Teksdal, 35 mm, Vekteren: 41 mm, Virdnejavrre: 26 mm
################################################################################################
source("/home/anitavd/PhD/R/computeArealExtremes.R")
library(evir)
source("/home/anitavd/PhD/R/rlevel.gpd.R")

compareMX <- function(field.name,threshold) {

retper <- c(5,10,20,50,100,200,500,1000)

annualMax <- read.table(paste("/p/PhD/Research/Comparison_methods/Output_GB/",field.name,"/",field.name,"_annualMaxima.txt",sep=""),header=T)
annualMax <- as.vector(annualMax$x[!is.na(annualMax$x)])
daily <- read.table(paste("/p/PhD/Research/Comparison_methods/Output_GB/",field.name,"/",field.name,"_areal_precipitation_grid.txt",sep=""),header=T,skip=11)

MT_NERC <- c()
MT_GEV_mean <- c()
MT_GEV_low <- c()
MT_GEV_high <- c()
MT_GPD <- c()
b <- 1

for (i in 1:length(retper)) {
	T=retper[i]

	if(T>5) b <- 2
	MT_NERC <- cbind(MT_NERC,computeArealMT(annualMax,T=T,nDay=1)[b])  

	MT_GEV_low <- cbind(MT_GEV_low,computeMT_GEV(annualMax,T=T)[1])
	MT_GEV_mean <- cbind(MT_GEV_mean,computeMT_GEV(annualMax,T=T)[2])
	MT_GEV_high <- cbind(MT_GEV_high,computeMT_GEV(annualMax,T=T)[3])
	
	MT_GPD <- cbind(MT_GPD,rlevel.gpd(gpd(daily$MAP,threshold),threshold,k.blocks=T)[1]) 
}
ymin <- round(((min(MT_GEV_low,MT_GPD,MT_NERC)-5)/5)*5,-1)
ymax <- round(((max(MT_GEV_high,MT_GPD,MT_NERC)+5)/5)*5,-1)

pdf(paste("/home/anitavd/PhD/Results/compareMT_",field.name,".pdf",sep=""),height=7,width=8)

plot(log(retper),MT_GEV_mean,xlab="MT",ylab="mm",ylim=range(ymin,ymax),axes=F,pch=19)
axis(1,log(retper),retper)
axis(2,at=seq(ymin,ymax,20))
lines(log(retper),MT_GEV_low,lty="dashed")
lines(log(retper),MT_GEV_high,lty="dashed")
points(log(retper),MT_NERC,col="red",pch=19)
points(log(retper),MT_GPD,col="blue",pch=19)
mtext(paste(field.name),3)

legend(2,ymax-20,c("NERC","GEV","GPD"),pch=19,col=c("red","black","blue"))

dev.off()
}



###############################################################################################
#-----------------------------------------------------------
# Compute MT using GEV (evir package)
#-----------------------------------------------------------
# area.max	vector of annual maxima
# T		vector of return period(s)
#
###############################################################################################

computeMT_GEV <- function(area.max,T=5) {

shape <- gev(area.max)$par.ests[1]
scale <- gev(area.max)$par.ests[2]
loc <- gev(area.max)$par.ests[3]
gevp <- exp(-(1+(shape*((area.max-loc)/scale)))^-(1/shape))

rlevel.gev(gev(area.max,block=1),k.blocks=T)

}	
