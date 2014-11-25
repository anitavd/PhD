######################################################################
#
#  calc_pmp_nday.R
#
#  Reads a file of daily area precipitation values and calculate 
#  return values for n-hours precipitation for year and season.
#
#  Input file: 16 bits binary files containing 
#              <first year> <last year> value 1, value 2, .... ,value N
#	       named rr_<area_number>.bil stored in <srcdir>
#              Use timeseries_bin to generate rr_<area_number>.bil files
#              (extract whole years of data, ex 1961-01-01 - 2006-12-31)
#
#  Output: Return values printed on files in <trgdir>
#
#  Eli Alfnes, met.no, 20050912
#
######################################################################

PrintTomrad <- function()
{
   tomrad <- ""
   names(tomrad) = ""
   print(tomrad,quote=FALSE)
}

calcPMP <- function(area_number,ses1=c(12,1,2),ses2=c(3,4,5),ses3=c(6,7,8),ses4=c(9,10,11))
{
  require(date)

  # Declaration of global variables and functions
  appldir <- "/klimadata/applikasjon/gridding/src"
  source(paste(appldir,"/iobin.R",sep=""))
  source(paste(appldir,"/met_stat.R",sep=""))
  area_file <- "/klimadata/elia/R_code/rrfelt.pixel"
  srcdir <- "/klimadata/elia/pmp/tseries_felt/"   # her skal rr_<area_number>.bil filene ligge
  trgdir <- "/klimadata/elia/pmp/resultater/"     # her lagres resultatene på filer med navn "<nummer>.pmp" 

  param <- "rr"
  meta <- ParamMeta(param)

  # The normal period
  snormyr <- 1961
  enormyr <- 1990

  # Reads area_file
  areas <- read.csv(area_file,sep="\t")
  names <- unlist(strsplit(substr((as.character(areas[,3])),2,100),"'"))
  area_name <- names[which(areas[,1] == area_number)]
  if(length(area_name)==0)
  {
     area_name = area_number
  }

  # Seasons
  mthnames <- c('jan','feb','mar','apr','mai','jun','jul','aug','sep','okt','nov','des')
  sesgrphdr <- vector("character",length(4))
  seshdr1<-seshdr2<-seshdr3<-seshdr4 <- NULL
  for (i in 1:length(ses1))
    seshdr1 <- paste(seshdr1,mthnames[ses1[i]],sep=",")
  for (i in 1:length(ses2))
    seshdr2 <- paste(seshdr2,mthnames[ses2[i]],sep=",")
  for (i in 1:length(ses3))
    seshdr3 <- paste(seshdr3,mthnames[ses3[i]],sep=",")
  for (i in 1:length(ses4))
    seshdr4 <- paste(seshdr4,mthnames[ses4[i]],sep=",")

  #------------------------------------
  # read daily area values from file
  #------------------------------------
  #fil med døgnarealverdier 
  filename <- paste(srcdir,"rr_",area_number,".bil",sep="")
print(filename)

  # read from and to year to determine the expected number of 
  # records in the file
  fileinp <- readbinfile(2,filename,na=10000,size=2)
  syear <- fileinp[1]
  eyear <- fileinp[2]
  numOfRec <- (mdy.date(12,31,eyear)-mdy.date(01,01,syear)+1)

  # read area values from file
  fileinp <- readbinfile(numOfRec+2,filename,na=10000,size=2) #incl. the years

  ##########################
  ## NB!! fjerner rr-verdi 1/1-61 da denne er helt vill
  fileinp[3]<-0  
  ##########################

  area_val <- (fileinp[3:length(fileinp)]-meta$convadd)*10^(-meta$convmult)

  fileout <- paste(trgdir, unlist(strsplit(area_name," "))[1],".pmp",sep="")

  sink( fileout )
# ----------------------------
# calculate M5,MT and PMP
# ----------------------------
    tab_title <- paste("Påregnelig Ekstremnedbør beregnet fra døgnnedbørsgrid",
          "i perioden",syear,"-",eyear)
    names(tab_title) <- ""
    print(tab_title,quote=FALSE)

    tab_title <- paste(area_name,"   (løpenr ",area_number,")",sep="")
    names(tab_title) <- ""
    print(tab_title,quote=FALSE)
  
  # Hent ut normal årsnedbør for området
    # Middel av årsnormalen for gridcellene
    anNormPrec <- avrund(anNormal(area_val,snormyr,enormyr))  
    
    tab_title <- paste("Normal årsnedbør:",anNormPrec,"mm  (",snormyr,"-",enormyr,")")
    names(tab_title) <- ""
    print(tab_title,quote=FALSE)

  # -----------------------------------------------
  # Påregnelig 24 timers nedbør for år og sesonger
  # -----------------------------------------------

  # n-days periods
    nDay <- c(1,2,3,4,5,6,10,20)   # bør kunne gis som input argument
    area_nDay <- array(NA,dim=c(length(area_val),length(nDay)))
#    area_nDay[,1] <- area_val
    for (i in 1:length(nDay))
      area_nDay[,i] <- nDaySum(area_val,nDay[i],sides=1)

  # Generate array of area max
    # sesonginndelingen gis som input argument
    area_max <- array(NA,c(5,eyear-syear+1,length(nDay)))
    for (i in 1:length(nDay))
    { 
       area_max[1,,i] <- AreaYearMax(area_nDay[,i],syear,eyear) 
       area_max[2,,i] <- AreaSeasonMax(area_nDay[,i],syear,eyear,ses1)
       area_max[3,,i] <- AreaSeasonMax(area_nDay[,i],syear,eyear,ses2)
       area_max[4,,i] <- AreaSeasonMax(area_nDay[,i],syear,eyear,ses3)
       area_max[5,,i] <- AreaSeasonMax(area_nDay[,i],syear,eyear,ses4)
   }


  # return periods (Inf = PMP)
    T <- array(c(5,10,50,100,500,1000,Inf)) 

  # calculate return values and pmp
    pmp_area <- array(NA,c(nrow(area_max),length(T),length(nDay)))
    for (i in 1:length(nDay))
       pmp_area[,,i] <- AreaPMP(area_max[,,i],T,nday=as.integer(nDay[i]) )

#print(max(area_val))
#print(area_max)
#print(pmp_area)
    
  # write results on stdout
    # array of column names based on T (return periods)
    cname <- vector(mode="character",length=length(T))
    for (i in 1:length(T))
       cname[i] <- paste(strwrap(toString(T[i]),prefix="M"),"(mm)")
    cname[!is.finite(T)] <- "PMP (mm)       "
    colnames(pmp_area) <- cname
    # row names
    season_lable <- 
        t(c("Årsverdi",seshdr1,seshdr2,seshdr3,seshdr4))
    names(season_lable) <- ""
    rownames(pmp_area) <- season_lable
 
    tab_title <- "Påregnelig 24 timers nedbørsverdier"
    names(tab_title) <- ""
    print(tab_title,quote=FALSE)

    m5ssn_m5yr <- array(avrund(pmp_area[,1,1]/pmp_area[1,1,1],2),
                     dim=c(dim(pmp_area)[1],1))
    colnames(m5ssn_m5yr) <- "M5(årst)/M5(år)" 
    rownames(m5ssn_m5yr) <- season_lable
    print(t(m5ssn_m5yr))
    print(t(pmp_area[,,1]))
    


  # -----------------------------------------------
  # Påregnelig n-timers nedbør år og sesonger
  # -----------------------------------------------

  rTimes <- c(1,2,6,12,24,48,72,96,120,144,240,480)
  names(rTimes) <- rTimes
  RateArray <- NA*rTimes           # same size as rTimes

  # getting rate factors n hours / 24 hours
  # beregner for alle tidssteg for å kunne sammenligne med n-days pmp
  for (i in 1:length(rTimes))
#  for (i in 1:4)  # bare de under 1 døgn
     RateArray[i] <- M5TM524(anNormPrec,rTimes[i])

  # printing header
  tab_title <- "Påregnelig n-timers nedbørsverdier"
  names(tab_title) <- ""
  print(tab_title,quote=FALSE)

  tab_title <- "Rate n timer / 24 timer"
  names(tab_title) <- ""
  print(tab_title,quote=FALSE)
  print(avrund(RateArray[1:4],2))

  # printing n-hours precipitation for year and seasons 
  for (season in 1:nrow(pmp_area)) {
    pmp_ntime <- avrund(pmp_area[season,,1]%o%RateArray)
 #   pmp_ntime[,6:10] <- NA
 #   pmp_ntime[,5:(5-1+length(nDay))] <- pmp_area[season,,2:length(nDay)]
    pmp_ntime[,5:(5-1+length(nDay))] <- pmp_area[season,,]

    sub_title <- season_lable[,season]
    names(sub_title) <- ""
    print(sub_title,quote=FALSE)

    print(pmp_ntime)

   # print pmp beregnet mha M5_nday/M5_1day ratio
    M5ndM5d <- pmp_ntime[1,5:dim(pmp_ntime)[2]]/pmp_ntime[1,5]
 #   PMPnday <- pmp_ntime[7,5:length(nDay)]
    PMPnday <- pmp_ntime[7,5]*M5ndM5d
    PMPprint <- array(NA,dim=c(3,length(PMPnday)))
    colnames(PMPprint) <- c(nDay*24)
    rownames(PMPprint) <- c("  M5TM524","  Rate","  PMP (mm)")
    PMPprint[1,] <- avrund(RateArray[5:length(rTimes)],2)
    PMPprint[2,] <- avrund(M5ndM5d,2)
    PMPprint[3,] <- avrund(PMPnday,0)
    print(PMPprint)
  }

  # maximum observed n-days area precipitation
  tab_title <- "Maksimal observert aralnedbør"
  names(tab_title) <- ""
  print(tab_title,quote=FALSE)
#  print(nDay)
  nDaymax <- vector(mode="numeric",length=length(nDay))
  stdavvik <- vector(mode="numeric",length=length(nDay))
  cv <- vector(mode="numeric",length=length(nDay))
  max10mean <- vector(mode="numeric",length=length(nDay))
  max10ratio <- vector(mode="numeric",length=length(nDay))
  maxdates <- vector(mode="integer",length=length(nDay))
  datestr <- vector(mode="character",length=length(nDay)) 
  for (i in 1:length(nDay))
  {
    nDaymax[i] <- max(area_nDay[,i],na.rm=TRUE)
 #   datestr[i]  = sprintf("%02i.%02i.%04i",
 #                 date.mdy(mdy.date(1,1,syear) - 1 + which.max(area_nDay[,i]))
    maxdates[i] <- mdy.date(1,1,syear) - 1 + which.max(area_nDay[,i])
    datestr[i]  <- sprintf("%02i.%02i.%04i",date.mdy(maxdates[i])$day,
                   date.mdy(maxdates[i])$month,date.mdy(maxdates[i])$year)
    stdavvik[i] <- avrund(sd(area_nDay[,i],na.rm=TRUE),2)
    cv[i] <- avrund(mean(area_nDay[,i],na.rm=TRUE)/stdavvik[i],3)
    max10mean[i] <- avrund(mean(nMax(area_nDay[,i],10)),2)
    max10ratio[i] <- avrund(mean(nMax(area_nDay[,i],10))/mean(nMax(area_nDay[,1],10)),2)
  }
#  print(nDaymax)
#  print(datestr)
  parr <- array(c(nDay,nDaymax,datestr,stdavvik,cv,max10mean,max10ratio),dim=c(length(nDay),6))
  colnames(parr) <- c("Ant.dag","Maks.obs.RR","Dato (siste)","Std.avvik","CV","Max10mean","Max10ratio")
  rownames(parr) <- vector("character",length=length(nDay))
  print(parr,quote=FALSE)

  sink()  # skrur av output til fil
}
