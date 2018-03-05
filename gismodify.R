library(ggplot2)
library(ggpubr)
library(foreign)
library(dplyr)
library(foreign)
library(plyr)
library(reshape)
library(gdata)
library(tidyverse)
#Read data files, total 8
data1=read.dbf("C:/Users/yfang.local/Desktop/R/2040 lclu/r4_2040.dbf")
data2=read.dbf("C:/Users/yfang.local/Desktop/R/2040 lclu/r10_2040.dbf")
data3=read.dbf("C:/Users/yfang.local/Desktop/R/2040 lclu/r17_2040.dbf")
data4=read.dbf("C:/Users/yfang.local/Desktop/R/2040 lclu/r79_2040.dbf")
data5=read.dbf("C:/Users/yfang.local/Desktop/R/2040 lclu/r125_2040.dbf")
data6=read.dbf("C:/Users/yfang.local/Desktop/R/2040 lclu/r368_2040.dbf")
data7=read.dbf("C:/Users/yfang.local/Desktop/R/2040 lclu/r111213_2040.dbf")
data8=read.dbf("C:/Users/yfang.local/Desktop/R/2040 lclu/r14151618_2040.dbf")

#combine all the data set into one file
combine=rbind(data1,data2,data3,data4,data5,data6,data7,data8)
combine$Count<-NULL
combine$Value<-NULL
combine$OBJECTID<-NULL
combine$sum<-NULL

#new=setNames(data.frame(t(combine[,-1])),combine[,5])
data=spread(combine,key="LCLU",value="percent")

#set all NA to 0
data[is.na(data)]<-0

#Rename the column
datare<-rename.vars(data,from=c("10","20","30","40","50","60","70"),to=c("Water","Wetlands","Forest","Grassland",
                                                                        "cropland","barren","Urban"))

#creat new columns forest/3 to deciduous evergreen mixedforest
datare$deciduous<-datare$Forest/3
datare$evergreen<-datare$Forest/3
datare$mixedforest<-datare$Forest/3
datare$shrubland<-datare$Grassland/2
datare$Grassland2<-datare$Grassland/2
datare$Grassland<-NULL
colnames(datare)[12]<-"Grassland"  
  

#Merge it to the HUC key
#1. import key files
key<-read.table("F:/yfang_project_folder/YuanFang work/data/HUC12 ID KEY/keyhuc12id_nodups.csv",
                   header=TRUE,sep=",")
colnames(key)[4]<-"HUC"
#2. Do the merge
total<-merge(datare,key, by="HUC",all=TRUE)


#3. import the LAT/LON information
input<-read.table("F:/WaSSI_HUC12/INPUTS/CELLINFO_US_HUC12.txt",header=TRUE,sep=",")
#keep the lat long columns
a<-c("HUC_12","lat","long")
inputa<-input[a]

#Merge the data files

final<-merge(inputa,total,by="HUC_12",all=TRUE)

final$HUC<-NULL
final$HUC_8<-NULL
final$HUC_10<-NULL
final$N.Rows<-NULL
final$Forest<-NULL

#Reorder the columns
final<-final[c(1,2,3,6,9,10,11,13,12,5,4,8,7)]


#set all NA to 0
final[is.na(final)]<-0

#calculate the sum

attach(final)
final$sum<-cropland+deciduous+evergreen+mixedforest+Grassland+shrubland+Wetlands+Water+Urban+barren

#-----****
#Subset the hucs which has no data
emptyhuc<-subset(final,final$sum=="0")
epthucno<-emptyhuc[1]

write.table(final,'C:/Users/yfang.local/Desktop/R/WASSI input/inputSSP5_US_HUC12.txt',sep=',',row.names=F)

write.dbf(final,file="C:/Users/yfang.local/Desktop/R/WASSI input/lc2040final.dbf",factor2char=TRUE)











