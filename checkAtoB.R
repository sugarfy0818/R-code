library(ggplot2)
library(ggpubr)

library(dplyr)
library(foreign)
library(car)
#.........................YEARLY.............................................
#urbantotal<-read.dbf("C:/Users/yfang.local/Desktop/R/landhydroavg40a.dbf")

#Read the WASSI results by 2006 landcover data
wateryld06<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_landcover2006/MM_WATERYLD.txt",
                       header=TRUE,sep=",")

#Read the WASSI results by 2040 landcover data
wateryld40<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_ssp5_2040_2006LAIimprevious/MM_WATERYLD.txt",
                       header=TRUE,sep=",")

#1
#HUCselect="110902020306"
#atob="Grass to Urban"

#2
#HUCselect="51202081101"
#atob="Forest to Urban"

#3
#HUCselect="51002050202"
#atob="Crop to Urban"

#4
HUCselect="130500031408"
atob="Barren to Urban"

huc1<-data.frame(HUC12=HUCselect)




#--------------------landcover change data------------------------------

water1<-merge(huc1,wateryld06,by="HUC12",all.y=TRUE)
water<-water1[!is.na(water1$HUC12),]
colnames(water)[4:9]<-c("PPT06","TEMP06","PET06","PAET06","AET06","YLD06")



water2<-merge(water,wateryld40,by=c("HUC12","MONTH"))

colnames(water2)[11:16]<-c("PPT40","TEMP40","PET40","PAET40","AET40","YLD40")
water2<-water2[order(water2[,2]),]


#Average by MONTH

d=water2


# final plot---done!! YLD
par(mar = c(5, 5, 3, 5))
#PAET06
plot(d$"MONTH", d$"YLD06",type ="l", ylab = "YLD (mm)",ylim=c(0,8),
     main = paste("YLD Mean",HUCselect,atob),xlab = "MONTH",
     col = "blue",lwd=1)
par(new = TRUE)

#PAET40
plot(d$"MONTH", d$"YLD40", type='l',ylab = " ",ylim=c(0,8),
     main = " ",xlab = "MONTH",
     col = "red",lwd=1)
par(new = TRUE)

#PPT
plot(d$"MONTH",d$"PPT06", type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "grey", lty = 2,lwd=2)
axis(side = 4)
mtext("PPT (mm)", side = 4, line = 3)
legend("topleft", c("YLD06", "PPT","YLD40"),
       col = c("blue", "grey","red"), lty = c(1, 2))





# final plot---done!! AET
par(mar = c(5, 5, 3, 5))
#PAET06
plot(d$"MONTH", d$"AET06",type ="l", ylab = "AET (mm)",ylim=c(0,60),
     main = paste("AET Mean",HUCselect,atob),xlab = "MONTH",
     col = "blue",lwd=1)
par(new = TRUE)

#PAET40
plot(d$"MONTH", d$"AET40", type='l',ylab = " ",ylim=c(0,60),
     main = " ",xlab = "MONTH",
     col = "red",lwd=1)
par(new = TRUE)

#PPT
plot(d$"MONTH",d$"PPT06", type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "grey", lty = 2,lwd=2)
axis(side = 4)
mtext("PPT (mm)", side = 4, line = 3)
legend("topleft", c("AET6", "PPT","AET40"),
       col = c("blue", "grey","red"), lty = c(1, 2))





#.........................ANNUALLY.............................................

#Read the WASSI results by 2006 landcover data
wateryld06<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_landcover2006/A_WATERYLD.txt",
                       header=TRUE,sep=",")

#Read the WASSI results by 2040 landcover data
wateryld40<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_ssp5_2040_2006LAIimprevious/A_WATERYLD.txt",
                       header=TRUE,sep=",")
#HUCselect="110902020306"
#atob="Grass to Urban"

#2
#HUCselect="51202081101"
#atob="Forest to Urban"

#3
#HUCselect="51002050202"
#atob="Crop to Urban"

#4
HUCselect="130500031408"
atob="Barren to Urban"
huc1<-data.frame(HUCNO=HUCselect)



#--------------------landcover change data------------------------------

water1<-merge(huc1,wateryld06,by="HUCNO",all.y=TRUE)
water<-water1[!is.na(water1$HUCNO),]
colnames(water)[3:8]<-c("PPT06","TEMP06","PET06","PAET06","AET06","YLD06")



water2<-merge(water,wateryld40,by=c("HUCNO","YEAR"))

colnames(water2)[9:14]<-c("PPT40","TEMP40","PET40","PAET40","AET40","YLD40")
colnames(water2)[1]<-c("HUC12")
water2<-water2[order(water2[,1]),]


#Average by YEAR

d=water2


# final plot---done!! YLD
par(mar = c(5, 5, 3, 5))
#PAET06
plot(d$"YEAR", d$"YLD06",type ="l", ylab = "YLD (mm)",ylim=c(0,50),
     main = paste("YLD Mean",HUCselect,atob),xlab = "YEAR",
     col = "blue",lwd=1)
par(new = TRUE)

#PAET40
plot(d$"YEAR", d$"YLD40", type='l',ylab = " ",ylim=c(0,50),
     main = " ",xlab = "YEAR",
     col = "red",lwd=1)
par(new = TRUE)

#PPT
plot(d$"YEAR",d$"PPT06", type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "grey", lty = 2,lwd=2)
axis(side = 4)
mtext("PPT (mm)", side = 4, line = 3)
legend("topleft", c("YLD06", "PPT","YLD40"),
       col = c("blue", "grey","red"), lty = c(1, 2))





# final plot---done!! AET
par(mar = c(5, 5, 3, 5))
#PAET06
plot(d$"YEAR", d$"AET06",type ="l", ylab = "AET (mm)",ylim=c(100,400),
     main = paste("AET Mean",HUCselect,atob),xlab = "YEAR",
     col = "blue",lwd=1)
par(new = TRUE)

#PAET40
plot(d$"YEAR", d$"AET40", type='l',ylab = " ",ylim=c(100,400),
     main = " ",xlab = "YEAR",
     col = "red",lwd=1)
par(new = TRUE)

#PPT
plot(d$"YEAR",d$"PPT06", type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "grey", lty = 2,lwd=2)
axis(side = 4)
mtext("PPT (mm)", side = 4, line = 3)
legend("topleft", c("AET6", "PPT","AET40"),
       col = c("blue", "grey","red"), lty = c(1, 2))


