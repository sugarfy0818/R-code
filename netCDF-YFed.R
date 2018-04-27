
#install.packages("gstat")
#install.packages("lattice")
#install.packages("sp")
#install.packages("maptools")
#install.packages("ncdf4")
library(gstat)
library(lattice)
library(sp)
library(maptools)
library(ncdf4)

# Colorbar
rgb.palette<-colorRampPalette(c("white",rgb(121/255,181/255,213/255),rgb(64/255,145/255,52/255),rgb(249/255,239/255,29/255),rgb(182/255,56/255,24/255)),space="rgb")

# read precipitation dataset
ptcsv_R<-read.table(file.choose(),header=TRUE,sep=",")
ptcsv_R$Z<-ptcsv_R$Z*25.4*24
coordinates(ptcsv_R)<-~X+Y
# read domain grid
domainUS<-read.table(file.choose(),header=TRUE,sep=",")
coordinates(domainUS)<-~x+y

# Krige, parameter can be adjusted
v<-variogram(ptcsv_R$Z~1,ptcsv_R)
plot(v)
fit.variogram(v,vgm(700,"Sph",6,1150))
m<-vgm(934.1494,"Sph",7.163325,983.8332)
precip_mmd_R<-krige(ptcsv_R$Z~1,ptcsv_R,domainUS,model=m)

# check the values interpolated
levelplot(precip_mmd_R$var1.pred~domainUS$x+domainUS$y,col.regions=rgb.palette(170),at=seq(from=0,to=170,by=1),xlab="longitude",ylab="latitude",main="precip_obs_interpolate_R")

# open and read the nc file
precipnc<-nc_open("C:/Users/yfang.local/Desktop/BenMAP/CDF/PRISM_ppt_stable_4kmM3_200501.nc",write=TRUE)
lon <- ncvar_get(precipnc,"lon")
lat <- ncvar_get(precipnc,"lat")
precip_slice <- ncvar_get(precipnc,"precip")

# check the spatial distribution of nc file
lonlat <- as.matrix(expand.grid(lon,lat))
dataf <- data.frame(cbind(lonlat, as.vector(precip_slice)))

#rename the variable of dataf Var1,Var2, v3 to lat lon precip
colnames(dataf)[1:3] <- c("lon","lat","precip")

#YF new edit below levelplot(dataf$precip ~ dataf$lon * dataf$lat, data=grid, pretty=T, col.regions=rgb.palette(100))
levelplot(precip ~  lon* lat, data=dataf, pretty=T, col.regions=rgb.palette(100))


# creat an matrix which has the same size as the domain, to store the U.S. extraction
temp_array_r <- array(dim=c(1405,621))
temp_array_r[temp_array_r==-9999]<-NA

# put the values in a dataframe
data_fr <- data.frame(precip_mmd_R$x,precip_mmd_R$y,precip_mmd_R$var1.pred)

# extract the domain of U.S.
j <- sapply(data_fr$precip_mmd_R.x, function(x) which.min(abs(lon-x)))
k <- sapply(data_fr$precip_mmd_R.y, function(x) which.min(abs(lat-x)))


# extract the precipitation of U.S.
temp_array_r[cbind(unlist(j),unlist(k))]<-as.matrix(data_fr$precip_mmd_R.var1.pred)
#tmp.precip<-as.matrix(data_fr$precip_mmd_R.var1.pred)
#temp.array<-data.frame(cbind(j,k,temp.precip))

# override the precipitation in the orginal nc file
ncvar_put(precipnc, "precip", temp_array_r)
precipnc
nc_close(precipnc)


# save
#save.image()
#savehistory("~/Documents/R_workspace/history.Rhistory")

