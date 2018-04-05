library(ggplot2)
library(ggpubr)

library(dplyr)
library(foreign)
library(car)

#---------------------wateryld by WASSI-----------------------------

#Read the hucs of RTP
HUC<-read.dbf("C:/Users/yfang.local/Desktop/gis work/RTP/06.dbf")
keeps<-c("TEST","lat","long")
huc1=HUC[keeps]
#print(urban1)
colnames(huc1)[1]<-c("HUCNO")



#Read the WASSI results by 2006 landcover data
wateryld06<-read.table("F:/WaSSI_HUC12/OUTPUTS_landcover2006/A_WATERYLD.txt",
                       header=TRUE,sep=",")

#Read the WASSI results by 2040 landcover data
wateryld40<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_ssp5_2040_2006LAIimprevious/A_WATERYLD.txt",
                       header=TRUE,sep=",")

#--------------------landcover change data------------------------------

water1<-merge(huc1,wateryld06,by="HUCNO",all.y=TRUE)
water<-water1[!is.na(water1$lat),]
colnames(water)[5:10]<-c("PPT06","TEMP06","PET06","PAET06","AET06","YLD06")



water2<-merge(water,wateryld40,by=c("HUCNO","YEAR"))

colnames(water2)[11:16]<-c("PPT40","TEMP40","PET40","PAET40","AET40","YLD40")

#----select HUC 1. largest increase of urban
HUCselect="30300020606" 

#2. Raleigh slightly increase
#HUCselect="30202010803" 

#3. Large water AREA
#HUCselect="30300020610" 



singlehuc<-subset(water2,HUCNO==HUCselect)




# final plot YLD
par(mar = c(5, 5, 3, 5))
#PAET06
plot(singlehuc$"YEAR", singlehuc$"YLD06",type ="l", ylab = "YLD (mm)",ylim=c(200,800),
     main = "YLD HUC30300020610",xlab = "YEAR",
     col = "blue", lwd=1)
par(new = TRUE)

#PAET40
plot(singlehuc$"YEAR", singlehuc$"YLD40", type='l',ylab = " ",ylim=c(200,800),
     main = " ",xlab = "YEAR",
     col = "red",lwd=1)
par(new = TRUE)

#PPT
plot(singlehuc$"YEAR",singlehuc$"PPT06", type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "grey", lty = 2,lwd=2)
axis(side = 4)
mtext("PPT (mm)", side = 4, line = 3)
legend("topleft", c("YLD06", "PPT","YLD40"),
       col = c("blue", "grey","red"), lty = c(1, 2))




# final plot AET
par(mar = c(5, 5, 3, 5))
#PAET06
plot(singlehuc$"YEAR", singlehuc$"AET06",type ="l", ylab = "AET (mm)",ylim=c(550,800),
     main = "AET HUC30300020610",xlab = "YEAR",
     col = "blue",lwd=1)
par(new = TRUE)

#PAET40
plot(singlehuc$"YEAR", singlehuc$"AET40", type='l',ylab = " ",ylim=c(550,800),
     main = " ",xlab = "YEAR",
     col = "red",lwd=1)
par(new = TRUE)

#PPT
plot(singlehuc$"YEAR",singlehuc$"PPT06", type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "grey", lty = 2,lwd=2)
axis(side = 4)
mtext("PPT (mm)", side = 4, line = 3)
legend("topleft", c("AET06", "PPT","AET40"),
       col = c("blue", "grey","red"), lty = c(1, 2))























#Average by year

d=aggregate(water2[,5:16], list(water2$YEAR), mean)
#-------------------------plot the PPT, TEMP, AET06, AET40
#1. FOR [A_AET]

####################Plot two time series into one plot
# modify data


data06=d["AET06"]
data40=d["AET40"]

#precipitation
datap=d["PPT40"] #Year 06 and 40 precipitation are the same

# from 1961 to 2015 as a time series object
myts06 <- ts(data06, start=c(1961), end=c(2015), frequency=1)
myts40 <- ts(data40, start=c(1961), end=c(2015), frequency=1)
mytsp<-ts(datap, start=c(1961), end=c(2015), frequency=1)

# 2nd step make plot
library(astsa)

#----style2 PREFERRED!
#png(file='gtempsbase.png', width=600, height=320)

par(mar=c(2,3,2,0)+.5, mgp=c(1.6,.6,0)) #modify plot width and height
ts.plot(myts06, myts40,mytsp, ylab="mm", xlab="YEAR", main=c('HUC:',HUCselect),col=4, type='n')
grid(lty=1, col=gray(.9))

lines(myts06,  lwd=2, col = rgb(.9,  0, .7, .5) )
lines(myts40, lwd=2, col = rgb( 0, .7, .9, .5) )
lines(mytsp, lwd=3,lty=4, col = rgb( 0.1, 0, .1, .3) )


legend('topleft', col=c(rgb(.9, 0, .7),  rgb(0, .7, .9),rgb(.4,.4,.4)), lty=1, lwd=2, 
       legend=c("Landcover 2006 AET", "Landcover 2040 AET","PPT"), bg='white')  



#----average
ggplot(d,aes(Group.1))+geom_line(aes(y=AET06,colour="AET06"))+
  geom_line(aes(y=AET40,colour="AET40"))+
  scale_colour_manual(values=c("black","red"))


ggplot(d,aes(Group.1))+geom_line(aes(y=YLD06,colour="YLD06"))+
  geom_line(aes(y=YLD40,colour="YLD40"))+
  scale_colour_manual(values=c("black","red"))






# final plot---done!! YLD
par(mar = c(5, 5, 3, 5))
#PAET06
plot(d$"Group.1", d$"YLD06",type ="l", ylab = "YLD (mm)",ylim=c(200,720),
     main = "YLD Mean",xlab = "YEAR",
     col = "blue",lwd=1)
par(new = TRUE)

#PAET40
plot(d$"Group.1", d$"YLD40", type='l',ylab = " ",ylim=c(200,720),
     main = " ",xlab = "YEAR",
     col = "red",lwd=1)
par(new = TRUE)

#PPT
plot(d$"Group.1",d[,2], type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "grey", lty = 2,lwd=2)
axis(side = 4)
mtext("PPT (mm)", side = 4, line = 3)
legend("topleft", c("YLD06", "PPT","YLD40"),
       col = c("blue", "grey","red"), lty = c(1, 2))





# final plot---done!! AET
par(mar = c(5, 5, 3, 5))
#PAET06
plot(d$"Group.1", d$"AET06",type ="l", ylab = "AET (mm)",ylim=c(600,900),
     main = "AET Mean",xlab = "YEAR",
     col = "blue",lwd=1)
par(new = TRUE)

#PAET40
plot(d$"Group.1", d$"AET40", type='l',ylab = " ",ylim=c(600,900),
     main = " ",xlab = "YEAR",
     col = "red",lwd=1)
par(new = TRUE)

#PPT
plot(d$"Group.1",d[,2], type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "grey", lty = 2,lwd=2)
axis(side = 4)
mtext("PPT (mm)", side = 4, line = 3)
legend("topleft", c("AET6", "PPT","AET40"),
       col = c("blue", "grey","red"), lty = c(1, 2))



