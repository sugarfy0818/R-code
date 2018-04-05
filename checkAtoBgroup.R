library(ggplot2)
library(ggpubr)
library(ggthemes)
library(scales)
library(dplyr)
library(foreign)
library(car)
library(data.table)
library(plotrix)
#.........................YEARLY.............................................
#urbantotal<-read.dbf("C:/Users/yfang.local/Desktop/R/landhydroavg40a.dbf")

#Read the WASSI results by 2006 landcover data
wateryld06<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_landcover2006/MM_WATERYLD.txt",
                       header=TRUE,sep=",")

#Read the WASSI results by 2040 landcover data
wateryld40<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_ssp5_2040_2006LAIimprevious/MM_WATERYLD.txt",
                       header=TRUE,sep=",")


#---------------------wateryld by WASSI-----------------------------

#Read the hucs of RTP
HUC<-read.dbf("C:/Users/yfang.local/Desktop/gis work/AtoB/shrubland.dbf")
keeps<-c("TEST","lat_x","long_x")
huc1=HUC[keeps]
#print(urban1)
colnames(huc1)[1]<-c("HUC12")
colnames(huc1)[2]<-c("lat")
colnames(huc1)[3]<-c("long")






#--------------------landcover change data------------------------------

water1<-merge(huc1,wateryld06,by="HUC12",all.y=TRUE)
water<-water1[!is.na(water1$lat),]
colnames(water)[6:11]<-c("PPT06","TEMP06","PET06","PAET06","AET06","YLD06")



water2<-merge(water,wateryld40,by=c("HUC12","MONTH"))

colnames(water2)[13:18]<-c("PPT40","TEMP40","PET40","PAET40","AET40","YLD40")
water2<-water2[order(water2[,1],water2[,2]),]


#Average by MONTH

d=water2
#Average by MONTH

e=aggregate(water2[,6:18], list(water2$MONTH), mean)

yld<-ggplot(e, aes(Group.1)) + 
  geom_line(aes(y = YLD06, colour = "YLD06")) + 
  geom_line(aes(y = YLD40, colour = "YLD40")) +
  geom_line(aes(y = PPT06, colour = "PPT")) +
  theme_bw()+scale_x_continuous(name="Month", limits=c(1, 12),oob = rescale_none) +
  scale_y_continuous(name="(mm)")+
  ggtitle("Shrubland to Urban YLD monthly ")
yld


yld <- data.table(d)

yld1 <- yld[, .("meanYLD06" = mean(YLD06), "meanYLD40" = mean(YLD40), "meanPPT" = mean(PPT06), 
                       "seYLD06" = std.error(YLD06),"seYLD40" = std.error(YLD40),"sePPT" = std.error(PPT06)), 
                   by = c("MONTH")]

write.table(yld1,file="C:/Users/yfang.local/Desktop/R/AtoBoutput/shrubland.txt")




