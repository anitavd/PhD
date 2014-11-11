rm(list=ls())
plot=T

institute <- "SMHI"
model <- "ICHEC"
scen <- "rcp45"
if(institute=="SMHI") {
  wf.mod <- read.table(paste("~/PhD/Future/Data/Eur-11/",institute,"/",model,"/WF-loc-EUR11pr3h_",model,"_",institute,"_",scen,"_1970-2013.txt",sep=""),header=T)
  wi.mod <- read.table(paste("~/PhD/Future/Data/Eur-11/",institute,"/",model,"/WI-loc-EUR11pr3h_",model,"_",institute,"_",scen,"_1970-2013.txt",sep=""),header=T)
}
if(institute=="KNMI") {
  wf.mod <- read.table(paste("~/PhD/Future/Data/Eur-11/",institute,"/WF-loc-EUR11pr3h_ICHEC_KNMI_",scen,"_1966-2015.txt",sep=""),header=T)
  wi.mod <- read.table(paste("~/PhD/Future/Data/Eur-11/",institute,"/WI-loc-EUR11pr3h_ICHEC_KNMI_",scen,"_1966-2015.txt",sep=""),header=T)
}
wf.obs <- read.table("~/PhD/Future/Data/Obs/WF-Obs_pr3h_1970-2013.txt",header=T)
wi.obs <- read.table("~/PhD/Future/Data/Obs/WI-Obs_pr3h_1970-2013.txt",header=T)

wt.mod <- wf.mod*wi.mod
wt.obs <- wf.obs*wi.obs

#Fill in NA in the years with no observations
wf.mod.ny <- wf.mod
wi.mod.ny <- wi.mod
for (i in 2:20) {
  a <- which(is.na(wf.obs[,i]))
  b <- which(is.na(wi.obs[,i]))
  if(institute=="KNMI") {
    a <- c(1:5,a,48:50)
    b <- c(1:5,b,48:50)
  }
  wf.mod.ny[a,i] = NA
  wi.mod.ny[b,i] = NA
}

if(plot) {
  ymax <- max(max(wf.obs[,2:20],wf.mod.ny[,2:20],na.rm=T))
  ymin <- min(min(wf.obs[,2:20],wf.mod.ny[,2:20],na.rm=T))
  if(institute=="KNMI") png(paste("~/PhD/Future/Figs/KNMI/",scen,"/WF_comparison_",scen,".png",sep=""),width=600,height=540)
  if(institute=="SMHI") png(paste("~/PhD/Future/Figs/SMHI/",model,"/",scen,"/WF_comparison_",scen,".png",sep=""),width=600,height=540)
  boxplot(wf.obs[,2:20],col="blue",ylim=c(ymin,ymax),names=seq(1,19),ylab="Number of wet 3-hr events",xlab="Site")
  boxplot(wf.mod.ny[,2:20],add=T,col="red",names=seq(1,19))
  dev.off()
  ymax <- max(max(wi.obs[,2:20],wi.mod.ny[,2:20],na.rm=T))
  ymin <- min(min(wi.obs[,2:20],wi.mod.ny[,2:20],na.rm=T))
  if(institute=="KNMI") png(paste("~/PhD/Future/Figs/KNMI/",scen,"/WI_comparison_",scen,".png",sep=""),width=600,height=540)
  if(institute=="SMHI") png(paste("~/PhD/Future/Figs/SMHI/",model,"/",scen,"/WI_comparison_",scen,".png",sep=""),width=600,height=540)
  boxplot(wi.obs[,2:20],col="blue",ylim=c(ymin,ymax),names=seq(1,19),ylab="Mean intensity on wet 3-hr events [mm]",xlab="Site")
  boxplot(wi.mod.ny[,2:20],add=T,col="red",names=seq(1,19))
  dev.off()
  ymax <- max(max(wf.obs[,2:20]*wi.obs[,2:20],wf.mod.ny[,2:20]*wi.mod.ny[,2:20],na.rm=T))
  ymin <- min(min(wf.obs[,2:20]*wi.obs[,2:20],wf.mod.ny[,2:20]*wi.mod.ny[,2:20],na.rm=T))
  if(institute=="KNMI") png(paste("~/PhD/Future/Figs/KNMI/",scen,"/WT_comparison_",scen,".png",sep=""),width=600,height=540)
  if(institute=="SMHI") png(paste("~/PhD/Future/Figs/SMHI/",model,"/",scen,"/WT_comparison_",scen,".png",sep=""),width=600,height=540)
  boxplot(wf.obs[,2:20]*wi.obs[,2:20],col="blue",ylim=c(0,800),names=seq(1,19),ylab="Mean summer precipitation [mm]",xlab="Site")
  boxplot(wf.mod.ny[,2:20]*wi.mod.ny[,2:20],add=T,col="red",names=seq(1,19))
  dev.off()
}