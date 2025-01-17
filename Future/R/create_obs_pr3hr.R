##########################################################
# Reads hourly precipitation from KDVH and computes
# annual max 3h precipitation for GEV analysis
# AVD, MET, Jun-2014
##########################################################

#stations <- c(3030,3810,4781,12290,17870,18020,18320,18701,19490,19510,19710,20300,26890,30310,39150,44730,47240,60940,64300)
stations <- c(1220,2840,3030,3120,3810,4220,4781,11620,12290,12670,15720,17260,17870,17980,18020,18210,18269,
18270,18320, 18420, 18640, 18701,18702,18815,18920,19020,19490,19510,19710,20300,26890,27270,27450,27470,27580,30310,32100,36060,38130,39040,39150,40140,44190,44300,44660,44730,47240,47890,50480,52290,58700,
60940,62290,64300,66830,68170,68230,69150,73490,81620,82260,89350,90400,97250,99370)  # >= 10 years

#stations <- c(18020,18320,18701,19490,19510,19710)   #Osloarea
#stations <- c(4220,17980,18020,18210,18269,18270,18320,18420,18640,18701,18702,18815,18920,18980,19020,19490,19510,19710)   #Osloarea ++
#stations <- c(44190,44300,44660,44730)  #Stavanger
pluvio <- TRUE

#For pluviostations (RR_1)
if(pluvio) {

SM <- seq(1971,2014)  
WF <- seq(1971,2014) 
WI <- seq(1971,2014) 
  
for(n in 1:length(stations)) {
  station <- stations[n]
  print(station)
  am3h <- list(Year=c(),pr=c(),wf=c(),wi=c())   #3-hr pr, wet event frequency, mean wet event intensity

  for (year in 1971:2014) {
  
    print(year)
    am3h$Year <- c(am3h$Year,year)
    
    data <- "http://klapp/metnopub/production/metno?re=17&nmt=0&p=RR_1&h=0&h=1&h=2&h=3&h=4&h=5&h=6&h=7&h=8&h=9&h=10&h=11&h=12&h=13&h=14&h=15&h=16&h=17&h=18&h=19&h=20&h=21&h=22&h=23&ddel=dot&del=;&ct=text/plain&split=1&nod=NA"
    FD = paste("01.06.",year,sep="")
    TD = paste("31.08.",year,sep="")
    data <- paste(data,"&fd=",FD,"&td=",TD,"&s=",station,sep="")
  
    pr1h <- try(read.table(data, header = TRUE, na.strings = "NA"),silent=T)

    if(class(pr1h)=="try-error") {
      am3h$pr = c(am3h$pr,NA)
      am3h$wf = c(am3h$wf,NA)
      am3h$wi = c(am3h$wi,NA)
      next()
    }
    
    if(is.factor(pr1h$RR_1)) {
      pr <- as.numeric(levels(pr1h$RR_1))[pr1h$RR_1]
    }else {
      pr <- pr1h$RR_1
    }
    if(length(pr) < 2100) {
      am3h$pr = c(am3h$pr,NA)
      am3h$wf = c(am3h$wf,NA)
      am3h$wi = c(am3h$wi,NA)
      next()
    }

    pr[which(pr>50)] = NA
    pr[which(is.na(pr))] = 0
    
    if(length(pr[which(is.na(pr) | pr==0)]) == length(pr)) {
      am3h$pr = c(am3h$pr,NA)
      am3h$wf = c(am3h$wf,NA)
      am3h$wi = c(am3h$wi,NA)
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
      if(pr3h > 0.5) {
        ssp <- ssp + pr3h
        wf <- wf + 1
      }
    }
    if(am<2) {
      am <- NA
      wf <- NA
    }
    am3h$pr <- c(am3h$pr,am)
    am3h$wf <- c(am3h$wf,wf)
    am3h$wi <- c(am3h$wi,round(ssp/wf,digits=2))
  }
  source("~/PhD//Future//R/correct_obs.R")

  SM <- cbind(SM,am3h$pr)
  WF <- cbind(WF,am3h$wf)
  WI <- cbind(WI,am3h$wi)

}
}
#colnames(AM) <- c("Year","4220","17980","18020","18210","18269","18270","18320","18420","18640","18701","18702","18815","18920","18980","19020","19490","19510","19710")
#write.table(AM,"~/PhD//Future//Data//Obs//Osloarea_AM_pr3h_19712014.txt",row.names=F,quote=F)
#write.table(WF,"~/PhD//Future//Data//Obs//Osloarea_WF_pr3h_19712014.txt",row.names=F,quote=F)
#write.table(WI,"~/PhD//Future//Data//Obs//Osloarea_WI_pr3h_19712014.txt",row.names=F,quote=F)

#################################################################################
#For geonor stations (RA)
if(!pluvio) {
for (year in 1971:2014) {
  
  print(year)
  
  data <- "http://klapp/metnopub/production/metno?re=17&p=RA&h=0&h=1&h=2&h=3&h=4&h=5&h=6&h=7&h=8&h=9&h=10&h=11&h=12&h=13&h=14&h=15&h=16&h=17&h=18&h=19&h=20&h=21&h=22&h=23&ddel=dot&del=;&ct=text/plain&split=1&nod=NA"
  
  TD = paste("31.08.",year,sep="")
  FD = paste("01.06.",year,sep="")

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