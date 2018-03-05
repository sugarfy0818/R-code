library(ggplot2)
library(ggpubr)

library(dplyr)
library(foreign)

#---------------------wateryld by WASSI-----------------------------

#Read the WASSI results by 2006 landcover data
wateryld06<-read.table("F:/WaSSI_HUC12/OUTPUTS_landcover2006/A_WATERYLD.txt",
                       header=TRUE,sep=",")
#wateryld06<-subset(wateryld06,HUCNO==HUCselect)

#Read the WASSI results by 2011 landcover data
wateryld11<-read.table("F:/WaSSI_HUC12/OUTPUTS_landcover2011/A_WATERYLD.txt",
                       header=TRUE,sep=",")
#wateryld11<-subset(wateryld11,HUCNO==HUCselect)




###create data of difference of each hydrology variables between 2006 and 2011


#creat a new dataframe a and caluclate the difference
water<-cbind(wateryld06)

#of different land cover type from 2006 to 2011
water$AN_PAET=wateryld11$AN_PAET-wateryld06$AN_PAET
water$AN_AET=wateryld11$AN_AET-wateryld06$AN_AET
water$AN_YLD=wateryld11$AN_YLD-wateryld06$AN_YLD
keeps<-c("HUCNO","YEAR","AN_PAET", "AN_AET","AN_YLD")
wateryld=water[keeps]  




#--------------------landcover change data------------------------------

#library(shapefiles)

#Read the Urbanlization area from 2006 to 2011
urbantotal<-read.table("C:/Users/yfang.local/Desktop/gis work/landcoverchange.csv",header=TRUE,sep=",")

#---combine outlier data and landcover 2006 and 2011 dataset------------
#data outlier
names(outlier)[1]<-"HUC_12"

check<-merge(urban1,outlier,by="HUC_12")


#------------------------merge LAI data to check C:\WaSSi\WaSSI_HUC12\INPUTS
#input data
lai11<-read.table("C:/WaSSi/WaSSI_HUC12/INPUTS_when_running_prism2015_lulc2011/LANDLAI_US_HUC12.txt",
                       header=TRUE,sep=",")#year 2011 lai
lai06<-read.table("C:/WaSSi/WaSSI_HUC12/INPUTS_when_running_prism2012_lulc2006/LANDLAI_US_HUC12.txt",
                  header=TRUE,sep=",")#year 2006 lai
#average month to year
LAIavg11=aggregate(lai11[,3:12], list(lai$HUC_12), mean)
LAIavg06=aggregate(lai06[,3:12], list(lai$HUC_12), mean)


###create data of difference of each hydrology variables between 2006 and 2011


#creat a new dataframe a and caluclate the difference
LAI<-cbind(LAIavg11)

#of different land cover type from 2006 to 2011
LAI$crop=LAIavg11$crop_lai-LAIavg06$CROP_LAI
LAI$decid=LAIavg11$decid_lai-LAIavg06$DECID_LAI
LAI$evergreen=LAIavg11$evergreen_lai-LAIavg06$EVERGREEN_LAI
LAI$mixed=LAIavg11$mixed_lai-LAIavg06$MIXED_LAI
LAI$grassland=LAIavg11$grassland_lai-LAIavg06$GRASSLAND_LAI
LAI$shrubland=LAIavg11$shrubland_lai-LAIavg06$SHRUBLAND_LAI
LAI$wetland=LAIavg11$wetland_lai-LAIavg06$WETLAND_LAI
LAI$water=LAIavg11$water_lai-LAIavg06$WATER_LAI
LAI$urban=LAIavg11$urban_lai-LAIavg06$URBAN_LAI
LAI$barren=LAIavg11$barren_lai-LAIavg06$BARREN_LAI

LAIselect<-subset(LAI,Group.1==20301030606|Group.1==20301030702|Group.1==20301040404|
                     Group.1==20402050505|Group.1==20802080303|Group.1==30102050108|
                       Group.1==30102051601|Group.1==30102051704|Group.1==30202040702|
                       Group.1==30502020207,select=Group.1:barren)


print(LAIselect)



mixed<-c("HUCNO","YEAR","AN_PAET", "AN_AET","AN_YLD")
wateryld=water[keeps]  




#just keep the columns we want

keeps<-c("HUC_12","lat","long","urban1","deciduous1","evergreen1","wetlands1","shrubland1")
urban1=urbantotal[keeps]
#print(urban1)

#--------do the merge-------------

#Change the column name HUCNO to HUC_12 for the later combination
colnames(wateryld)[1]<-c("HUC_12")


total<-merge(wateryld,urban1,by="HUC_12")
#print(total)

#write.dbf(total,file="C:/Users/yfang.local/Desktop/R/landhydro.dbf",factor2char=TRUE)





#print(b)
#select data urban>0.05
c<-subset(total,deciduous1>=0.05 | deciduous1<=-0.05)
#print(total)
#d=aggregate(c[3], list(c$HUC_12), mean)



theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48
scale_x_continuous(limits = c(0.5,100))#set the x-axis limits

pl1<-ggplot(c, aes(x = deciduous1, y = AN_PAET))+ geom_point()+
  geom_smooth(method="loess", se=F)+labs(subtitle=("Annual ET of HUC"), 
                                         y="PAET changes",x="deciduous changes in percentage", title="Scatterplot", caption = "YEAR2006-2011")+
  coord_cartesian(ylim=c(-200,200))#set the x-axis limits
pl1
#method can be "lm"


#check out data |changee|>100

e<-subset(b,urban1>=0.05)





