#DAGRUN
# http://rfunction.com/archives/2487
#http://www.molecularecologist.com/2012/09/making-maps-with-r/

# install.packages("maps")
# install.packages("maptools")
# install.packages("mapdata")

# install.packages("mapproj")
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(mapproj)

map(database= "world", ylim=c(45,90), xlim=c(-160,-50), col="grey80", fill=TRUE, projection="gilbert", orientation= c(90,0,225))
lon <- c(-72, -66, -107, -154)  #fake longitude vector
lat <- c(81.7, 64.6, 68.3, 60)  #fake latitude vector
coord <- mapproject(lon, lat, proj="gilbert", orientation=c(90, 0, 225))  #convert points to projected lat/long
points(coord, pch=20, cex=1.2, col="red")  #plot converted points

add.pie(z=c(east, west), x=lon, y=lat, radius=sqrt(tot), col=c(alpha(“orange”, 0.6), alpha(“blue”, 0.6)), labels=””)

pcontorta <- readShapePoly("pinucont.shp")   #layer of data for species range

############################################################################################
#ANITA- Map irregular grid

library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(mapproj)
library(fields)

#lon, lat and z are matrices of dimentions x*y
coltab<-rev(tim.colors(n = 64, alpha=1.0))
nc <-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/DMI/Norway-pr_EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_3hr_1970.nc")
lonold <- ncvar_get(nc,"lon")   
latold <- ncvar_get(nc,"lat")
nc_close(nc)
map("worldHires",ylim=c(56,74), xlim=c(-10,45), col="black", fill=F,add=F) #can be wrongly projected without this
quilt.plot(as.vector(lonold),as.vector(latold),as.vector(z),nx=143,ny=143,col=coltab,add=T,axes=F)
map("worldHires",ylim=c(56,74), xlim=c(-10,45), col="black", fill=F,add=T)
