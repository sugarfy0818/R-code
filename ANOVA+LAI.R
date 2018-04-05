library(ggplot2)
library(ggpubr)

library(dplyr)
library(foreign)
library(car)

#---------------------wateryld by WASSI-----------------------------

#Read the WASSI results by 2006 landcover data
wateryld06<-read.table("F:/WaSSI_HUC12/OUTPUTS_landcover2006/A_WATERYLD.txt",
                       header=TRUE,sep=",")
#wateryld06<-subset(wateryld06,HUCNO==HUCselect)

#Read the WASSI results by 2040 landcover data
wateryld40<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_ssp5_2040_2006LAIimprevious/A_WATERYLD.txt",
                       header=TRUE,sep=",")
#wateryld40<-subset(wateryld40,HUCNO==HUCselect)


#because wateryld40 has less number of rows than wateryld06, so modify wateryld06 to new06
format<-wateryld40[1:2]
new06<-merge(format,wateryld06,by=c("HUCNO","YEAR"),all.x=TRUE)
water06<-new06[order(new06[,1],new06[,2]),]
###create data of difference of each hydrology variables between 2006 and 2040


#creat a new dataframe a and caluclate the difference
water<-cbind(water06)

#of different land cover type from 2006 to 2040
water$AN_PAET=wateryld40$AN_PAET-water06$AN_PAET
water$AN_AET=wateryld40$AN_AET-water06$AN_AET
water$AN_YLD=wateryld40$AN_YLD-water06$AN_YLD
keeps<-c("HUCNO","YEAR","AN_PAET", "AN_AET","AN_YLD","AN_PPT","AN_TEMP")
wateryld=water[keeps]  
colnames(wateryld)[1]<-c("HUC_12")

#--------------------landcover change data------------------------------


urbantotal<-read.table("C:/Users/yfang.local/Desktop/gis work/landcoverchange40.csv",header=TRUE,sep=",")

#just keep the columns we want
colnames(urbantotal)[3]<-c("lat")
colnames(urbantotal)[4]<-c("long")

keeps<-c("HUC_12","lat","long","urban1","deciduous1","evergreen1","mixedforest1","wetlands1",
         "shrubland1","water1","crop1","barren1","grassland1")

urban1=urbantotal[keeps]
#print(urban1)

#--------do the merge-------------

#Change the column name HUCNO to HUC_12 for the later combination
colnames(wateryld)[1]<-c("HUC_12")


total<-merge(wateryld,urban1,by="HUC_12")
#print(total)

#write.dbf(total,file="C:/Users/yfang.local/Desktop/R/landhydro.dbf",factor2char=TRUE)
#----calculate the annual(average of many years)hydrology variable by HUC 

d=aggregate(total[,3:19], list(total$HUC_12), mean)
colnames(d)[1]<-c("HUC_12")

colnames(d)[9]<-c("urbanchange")
colnames(d)[10]<-c("decichange")
colnames(d)[11]<-c("everchange")
colnames(d)[12]<-c("mixedchange")
colnames(d)[13]<-c("wetlchange")
colnames(d)[14]<-c("shrubchange")
colnames(d)[15]<-c("waterchange")
colnames(d)[16]<-c("cropchange")
colnames(d)[17]<-c("barrenchange")
colnames(d)[18]<-c("grasschange")


#import LAI in year 2006
lai2006<-read.table("C:/WaSSi/WaSSI_HUC12/INPUTS_when_running_prism2012_lulc2006/LANDLAI_US_HUC12.txt",
                   header=TRUE,sep=",")
e=aggregate(lai2006[,3:12], list(lai2006$HUC_12), mean)
colnames(e)[1]<-c("HUC_12")



#Do the merge
final<-merge(d,e,by="HUC_12")


finalA<-final
attach(finalA)
finalA$crop<-CROP_LAI*cropchange
finalA$urban<-URBAN_LAI*urbanchange
finalA$barren<-BARREN_LAI*barrenchange
finalA$decid<-DECID_LAI*decichange
finalA$evergreen<-EVERGREEN_LAI*everchange
finalA$mixed<-MIXED_LAI*mixedchange
finalA$grass<-GRASSLAND_LAI*grasschange
finalA$shrubland<-SHRUBLAND_LAI*shrubchange
finalA$wetland<-WETLAND_LAI*wetlchange
finalA$water<-WATER_LAI*waterchange
detach(finalA)

finalB<-data.frame(finalA[1:38],sum1=rowSums(finalA[29:38]))
#--------------Finish data processing



finalB$climate[finalB$AN_PPT>1100]<-"Wet"
finalB$climate[finalB$AN_PPT<600]<-"Dry"
finalB$climate[finalB$AN_PPT>600 & finalB$AN_PPT<1100]<-"Mid"


finalB$laichange[finalB$sum1>0]<-"Increase"
finalB$laichange[finalB$sum1<=0]<-"Decrease"


#remove water area>90%
#finalB<-finalB[!(finalB$water>0.9),]

#remove urban decrease
finalB<-finalB[!(finalB$urbanchange<0),]
#remove LAI no change
finalB<-finalB[!(finalB$sum1==0),]

finalB<-finalB[!(finalB$AN_AET>(100)),]
finalB<-finalB[!(finalB$AN_YLD<(-100)),]

#plot scatterplot
#attach(finalB)
#plot(sum1,AN_YLD, main="Scatterplot",xlab="sum1",ylab="AN_YLD")
#detach(finalB)

#1. AN_YLD
head(finalB,6)
res<-cor.test(finalB$sum1,finalB$AN_YLD,method="pearson")
res
a1<-ggscatter(finalB,x="sum1",y="AN_YLD",add="reg.line",
              add.params=list(color="blue",fill="lightgray"),
              conf.int=TRUE,cor.coef=TRUE,cor.method="pearson", 
              xlab="Changes in LAI",ylab="Changes in YLD")
a1


#plot scatterplot
#attach(finalB)
#plot(sum1,AN_AET, main="Scatterplot",xlab="sum1",ylab="AN_AET")
#detach(finalB)

#1. AN_YLD
head(finalB,6)
res<-cor.test(finalB$sum1,finalB$AN_AET,method="pearson")
res
a2<-ggscatter(finalB,x="sum1",y="AN_AET",add="reg.line",
              add.params=list(color="blue",fill="lightgray"),
              conf.int=TRUE,cor.coef=TRUE,cor.method="pearson", 
              xlab="Changes in LAI",ylab="Changes in AET")
a2


#################### combine 2 figures into 1 page
library(ggpubr)
ggarrange(a1 , a2  , 
          labels = c("A.", "B."),
          ncol = 1, nrow = 2)


library(car)
###############model
model = lm(AN_YLD ~ climate + laichange +climate:laichange,
           data=finalB)


Anova(model, type="II")                    # Can use type="III"


boxplot(AN_YLD ~ climate:laichange,
        data = finalB,
        xlab = "Climate x laichange",
        ylab = "AN_YLD")
means<-aggregate(AN_YLD~climate:laichange, finalB,mean)
points(1:4, means$AN_YLD,col="red")
#text(1:4,means$AN_YLD+0.08,labels=means$AN_YLD)



##########1. use ggplotAET &climate and laichange

fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T)),digit=2))}
goo <- ggplot(finalB, aes(x=climate, y=AN_AET)) +
  geom_boxplot(aes(fill=climate)) +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1,hjust=-0.5)+
  theme_classic()

goo


fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T)),digit=2))}
goo1 <- ggplot(finalB, aes(x=laichange, y=AN_AET)) +
  geom_boxplot(aes(fill=laichange)) +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1,hjust=-0.5)+
  theme_classic()

goo1
#---in one
ggarrange(goo,goo1,labels=c("A","B"),ncol=2,nrow=1)





##########2. use ggplotAET &climate and laichange

fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T)),digit=2))}
goo2 <- ggplot(finalB, aes(x=climate, y=AN_YLD)) +
  geom_boxplot(aes(fill=climate)) +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1,hjust=-0.5)+
  theme_classic()

goo2


fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T)),digit=2))}
goo3 <- ggplot(finalB, aes(x=laichange, y=AN_YLD)) +
  geom_boxplot(aes(fill=laichange)) +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1,hjust=-0.5)+
  theme_classic()

goo3
#---in one
ggarrange(goo2,goo3,labels=c("A","B"),ncol=2,nrow=1)

