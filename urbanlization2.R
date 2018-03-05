library(ggplot2)
library(ggpubr)
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


#just keep the columns we want

keeps<-c("HUC_12","lat","long","urban1")
urban1=urbantotal[keeps]
print(urban1)

#--------do the merge-------------

#Change the column name HUCNO to HUC_12 for the later combination
colnames(wateryld)[1]<-c("HUC_12")


total<-merge(wateryld,urban1,by="HUC_12")
print(total)


#print(b)
#select data urban>0.05
c<-subset(total,urban1>=0.05)
#print(total)
#d=aggregate(c[3], list(c$HUC_12), mean)


library(dplyr)
library(foreign)
library(ggplot2)

theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48
scale_x_continuous(limits = c(0.5,100))#set the x-axis limits

pl1<-ggplot(c, aes(x = urban1, y = AN_PAET))+ geom_point()+
  geom_smooth(method="loess", se=F)+labs(subtitle=("Annual ET of HUC"), 
                                         y="PAET changes",x="Urban changes in percentage", title="Scatterplot", caption = "YEAR2006-2011")+
  coord_cartesian(ylim=c(-200,200))#set the x-axis limits
pl1
#method can be "lm"


#check out data |changee|>100

e<-subset(b,urban1>=0.05)





