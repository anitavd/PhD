############################################################################
#
# pathtoMT: "/p/PhD/Research/Comparison_methods/MTanalysis/MTfields/"
# nHours: # or vector
# 
#
#
############################################################################


plotChangeMT <- function(pathtoMT, nHours, field.name) {

MT_19611990 <- read.table(paste(pathtoMT,field.name,"/",field.name,"_estimated_extreme_precipitation_grid_19611990.txt",sep=""))

MT_19812010 <- read.table(paste(pathtoMT,field.name,"/",field.name,"_estimated_extreme_precipitation_grid_19812010.txt",sep=""))

MT_19572010 <- read.table(paste(pathtoMT,field.name,"/",field.name,"_estimated_extreme_precipitation_grid_19572010.txt",sep=""))

noMT <- ncol(MT_19572010)

color<-c("deepskyblue4","skyblue","palegreen3","red3","sienna1")

for (i in 1:length(nHours)) {

if(nHours[i]==24) k=1
if(nHours[i]==48) k=2
if(nHours[i]==72) k=3

outfile = paste(pathtoMT,field.name,"/",field.name,"_MT_",nHours[i],"hrs.pdf",sep="")
pdf(file=outfile,width=11,height=11)

pch <- c(21,25,22,23,24)

ymax <- max(MT_19812010[k,],MT_19812010[k+3,],MT_19812010[k+6,],MT_19812010[k+9,],MT_19812010[k+12,])
ymin <- round((min(MT_19611990[k,],MT_19611990[k+3,],MT_19611990[k+6,],MT_19611990[k+9,],MT_19611990[k+12,])-20)/20)*20

n <- 0
diff.all <- c()

for (i in seq(k,k+12,by=3)) {

diff <- round((MT_19812010[i,]-MT_19611990[i,])*100/MT_19611990[i,],digits=1)
diff.all<-rbind(diff.all,diff)

if(i==k) {	
	plot(seq(1+(0.2*n),noMT+(0.2*n),1),MT_19572010[i,],pch=pch[n+1],xlim=c(1,9),ylim=c(max(0,ymin-20),ymax+30),bg="black",axes=F,xlab="",ylab="mm")
	points(seq(1+(0.2*n),noMT+(0.2*n),1),MT_19611990[i,],col="dodgerblue3",pch=pch[n+1],lwd=2)
	points(seq(1+(0.2*n),noMT+(0.2*n),1),MT_19812010[i,],col="firebrick",pch=pch[n+1],lwd=2)
} else {
	points(seq(1+(0.2*n),noMT+(0.2*n),1),MT_19572010[i,],pch=pch[n+1],bg="black",xlim=c(1,noMT),ylim=c(0,ymax))
	points(seq(1+(0.2*n),noMT+(0.2*n),1),MT_19611990[i,],col="dodgerblue3",pch=pch[n+1],lwd=2)
	points(seq(1+(0.2*n),noMT+(0.2*n),1),MT_19812010[i,],col="firebrick",pch=pch[n+1],lwd=2)
}

axis(1,at=c(1.4,2.4,3.4,4.4,5.4,6.4,7.4,8.4),labels=c("M5","M10","M50","M100","M200","M500","M1000","PMP"),tick=F,pos=max(0,ymin-20))
axis(2,at=seq(max(0,ymin-20),ymax+20,by=20))

for (r in 1:noMT) lines(c(r+(0.2*n),r+(0.2*n)),c(ymax+20,ymax+20+diff[r]),lwd=3,col=color[n+1])

n <- n + 1

} 

abline(h=ymax+20,lwd=3)
lines(c(noMT+1.32,noMT+1.32),c(ymax,ymax+40),lwd=2)
mtext(3,at=noMT+1.3,text="% change",line=1)
idx <- which(diff.all == max(diff.all,na.rm=T))
mtext(3,at=0.9+(0.2*idx[1]),text=paste(as.matrix(diff.all)[idx[1]],"%",sep=""),line=0.1,cex=0.9)

for (j in 2:8) abline(v=j-0.1,lty=2)

legend(1.1,ymax-10,c("Year","Winter","Spring","Summer","Fall"),text.col=color,pch=c(21,25,22,23,24),pt.bg ="black",bg="gray90")
text(2.4,ymax-20,"1957-2010")
text(2.4,ymax-30,"1961-1990",col="dodgerblue3")
text(2.4,ymax-40,"1981-2010",col="firebrick")

dev.off()

}

}




