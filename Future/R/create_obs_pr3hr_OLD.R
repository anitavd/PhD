##########################################################
# Reads hourly precipitation from KDVH and computes
# annual max 3h precipitation for GEV analysis
# AVD, MET, Jun-2014
##########################################################

#stations <- c(3030,3810,4781,12290,17870,18020,18320,18701,19490,19510,19710,20300,26890,30310,39150,44730,47240,60940,64300)
stations <- c(18020,18320,18701,19490,19510,19710)   #Osloarea
pluvio <- TRUE

#For pluviostations (RR_1)
if(pluvio) {

AM <- seq(1970,2014)  
WF <- seq(1970,2014) 
WI <- seq(1970,2014) 
  
for(n in 1:length(stations)) {
  station <- stations[n]
  print(station)
  am3h <- list(Year=c(),pr=c(),wf=c(),wi=c())   #3-hr pr, wet event frequency, mean wet event intensity
  sm3h <- list(Year=c(),pr=c())
  fm3h <- list(Year=c(),pr=c())
  for (year in 1970:2014) {
  
    print(year)
    am3h$Year <- c(am3h$Year,year)
    sm3h$Year <- c(sm3h$Year,year)
    fm3h$Year <- c(fm3h$Year,year)
    #msp$Year <- c(msp$Year,year)
    #wetf$Year <- c(wetf$Year,year)
  
    data <- "http://klapp/metnopub/production/metno?re=17&nmt=0&p=RR_1&h=0&h=1&h=2&h=3&h=4&h=5&h=6&h=7&h=8&h=9&h=10&h=11&h=12&h=13&h=14&h=15&h=16&h=17&h=18&h=19&h=20&h=21&h=22&h=23&ddel=dot&del=;&ct=text/plain&split=1&nod=NA"
  
    TD = paste("31.12.",year,sep="")
    FD = paste("01.01.",year,sep="")
  
    data <- paste(data,"&fd=",FD,"&td=",TD,"&s=",station,sep="")
  
    pr1h <- try(read.table(data, header = TRUE, na.strings = "NA"),silent=T)
  
    if(class(pr1h)=="try-error") {
      am3h$pr = c(am3h$pr,NA)
      am3h$wf = c(am3h$wf,NA)
      am3h$wi = c(am3h$wi,NA)
      sm3h$pr = c(sm3h$pr,NA)
      fm3h$pr = c(fm3h$pr,NA)
      #msp$pr = c(msp$pr,NA)
      #wetf$pr = c(wetf$pr,NA)
      next()
    }
    #if(length(which(is.na(pr1h$RR_1[which(pr1h$Month>=5 & pr1h$Month<=10)])))>4000) {
    #  am3h$pr = c(am3h$pr,NA)
    #  next()
    #}
  
    if(is.factor(pr1h$RR_1)) {
      pr <- as.numeric(levels(pr1h$RR_1))[pr1h$RR_1]
    }else {
      pr <- pr1h$RR_1
    }
  
    if(length(pr[which(pr1h$Month>4 & pr1h$Month<10)]) < 720) {
      am3h$pr = c(am3h$pr,NA)
      am3h$wf = c(am3h$wf,NA)
      am3h$wi = c(am3h$wi,NA)
      sm3h$pr = c(sm3h$pr,NA)
      fm3h$pr = c(fm3h$pr,NA)
      #msp$pr = c(msp$pr,NA)
      #wetf$pr = c(wetf$pr,NA)
      next()
    }
  
    pr[which(is.na(pr))] = 0
    
    if(length(pr[which(is.na(pr) | pr==0)]) == length(pr)) {
      am3h$pr = c(am3h$pr,NA)
      am3h$wf = c(am3h$wf,NA)
      am3h$wi = c(am3h$wi,NA)
      sm3h$pr = c(sm3h$pr,NA)
      fm3h$pr = c(fm3h$pr,NA)
      #msp$pr = c(msp$pr,NA)
      #wetf$pr = c(wetf$pr,NA)
      next()
    }
    
    am <- 0
    sm <- 0
    fm <- 0
    ssp <- 0   #sum of summer precipitation
    wf <- 0 #number of wet events
    
    for (i in seq(1,length(pr)-2,by=3)) {
    
      pr3h <- pr[i]+pr[i+1]+pr[i+2]
      am <- max(pr3h,am,na.rm=T)
      if(am<2) am <- NA
      if(pr1h$Month[i+1]>4 & pr1h$Month[i+1]<9) sm <- max(pr3h,sm,na.rm=T,na.rm=T)
      if(pr1h$Month[i+1]>8 & pr1h$Month[i+1]<12) fm <- max(pr3h,fm,na.rm=T,na.rm=T)
      if(pr1h$Month[i]>4 & pr1h$Month[i+2]<10) { #May through September
        if(pr3h > 0.5) {
          ssp <- ssp + pr3h
          wf <- wf + 1
        }
      }
     
    }
    am3h$pr <- c(am3h$pr,am)
    am3h$wf <- c(am3h$wf,wf)
    am3h$wi <- c(am3h$wi,round(ssp/wf,digits=2))
    sm3h$pr <- c(sm3h$pr,sm)
    fm3h$pr <- c(fm3h$pr,fm)
    #msp$pr <- c(msp$pr,round(ssp/wf,digits=2))  #mean summer 3-h precipitation, only counting wet events
    #wetf$pr <- c(wetf$pr,wf)     #wet event frequency

    }

  source("~/PhD//Future//R/correct_obs.R")

  AM <- cbind(AM,am3h$pr)
  WF <- cbind(WF,am3h$wf)
  WI <- cbind(WI,am3h$wi)
}
}

#################################################################################
#For geonor stations (RA)
if(!pluvio) {
for (year in 1970:2014) {
  
  print(year)
  
  data <- "http://klapp/metnopub/production/metno?re=17&p=RA&h=0&h=1&h=2&h=3&h=4&h=5&h=6&h=7&h=8&h=9&h=10&h=11&h=12&h=13&h=14&h=15&h=16&h=17&h=18&h=19&h=20&h=21&h=22&h=23&ddel=dot&del=;&ct=text/plain&split=1&nod=NA"
  
  TD = paste("31.12.",year,sep="")
  FD = paste("01.01.",year,sep="")

  data <- paste(data,"&fd=",FD,"&td=",TD,"&s=",station,sep="")

  pr1h <- read.table(data, header = TRUE, na.strings = "NA")
  pr1hcor <- pr1h
  
  for (j in 1:(length(pr1h$RA-1))) {
    if(is.na(pr1h$RA[j+1]-pr1h$RA[j])) next()
    if((pr1h$RA[j+1]-pr1h$RA[j] <= -1) & (pr1h$RA[j+1]-pr1h$RA[j] > -100)) pr1hcor$RA[j+1] = NA
  }

  rm(pr1h)
  am <- 0

  for (i in seq(1,length(pr1hcor$RA)-2,by=1)) {
  
    pr3h <- pr1hcor$RA[i+2]-pr1hcor$RA[i]
    #if(!is.na(pr3h) & !is.na(pr1hcor$RA[i+1]) & ((pr1hcor$RA[i+2]-pr1hcor$RA[i+1]) <= -1 )) pr3h = NA
    #if(!is.na(pr3h) & !is.na(pr1hcor$RA[i+1]) & ((pr1hcor$RA[i+1]-pr1hcor$RA[i]) <= -1)) pr3h = NA
    #if(!is.na(pr3h) & !is.na(pr1hcor$RA[i+1]) & ((pr1hcor$RA[i+2]-pr1hcor$RA[i]) <= -1)) pr3h = NA
    if(!is.na(pr3h) & pr3h > 100) pr3h = NA
    if(!is.na(pr3h) & is.na(pr1hcor$RA[i+1])) pr3h = NA
    am <- max(pr3h,am,na.rm=T)
    
  }
  am3h$Year <- c(am3h$Year,year)
  am3h$pr <- c(am3h$pr,am)
}
}

#am3h$pr[which(am3h$Year==2007)] = NA   #18950