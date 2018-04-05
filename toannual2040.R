library(ggplot2)
library(ggpubr)

library(dplyr)
library(foreign)

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

#library(shapefiles)

#Read the Urbanlization area from 2006 to 2040
#urbantotal<-read.table("C:/Users/yfang.local/Desktop/R/landcoverchangenowater.txt",header=TRUE,sep="") exclude water>80%

urbantotal<-read.table("C:/Users/yfang.local/Desktop/gis work/landcoverchange40.csv",header=TRUE,sep=",")

#just keep the columns we want
colnames(urbantotal)[3]<-c("lat")
colnames(urbantotal)[4]<-c("long")

keeps<-c("HUC_12","lat","long","urban1","deciduous1","evergreen1","wetlands1","shrubland1","water1")
#keeps<-c("HUC_12","lat.x","long.x","urban1","deciduous1","evergreen1","wetlands1","shrubland1","water1")exclude water>80%

urban1=urbantotal[keeps]
#print(urban1)

#--------do the merge------------

total<-merge(wateryld,urban1,by="HUC_12")
#print(total)

#write.dbf(total,file="C:/Users/yfang.local/Desktop/R/landhydro.dbf",factor2char=TRUE)
#----calculate the annual(average of many years)hydrology variable by HUC 

d=aggregate(total[,3:15], list(total$HUC_12), mean)
#print(d)
#write.dbf(d,file="C:/Users/yfang.local/Desktop/R/landhydroavg40a.dbf",factor2char=TRUE)



#+++++data: subset data
#d<-d[!(d$AN_YLD>=50 | d$AN_YLD<= -50),]

d<-d[!(d$urban1<0.001&d$urban1>=0),]

#PAET------------
theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48
scale_x_continuous(limits = c(0.5,100))#set the x-axis limits

pl1<-ggplot(d, aes(x = urban1, y = AN_PAET))+ geom_point()+
  geom_smooth(span=0.3)+labs(subtitle=("Average annual PAET change vs.Urban change"), 
                             y="PAET changes",x=" Urban changes in percentage", title="Scatterplot", caption = "YEAR2006-2040")+
  coord_cartesian(ylim=c(-350,350))#set the x-axis limits
pl1
#method can be "lm"loess

#geom_smooth(method="loess", se=F)

#YLD-------------------
theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48
scale_x_continuous(limits = c(0.5,100))#set the x-axis limits

pl2<-ggplot(d, aes(x = urban1, y = AN_YLD))+ geom_point()+
  geom_smooth(span=0.3)+labs(subtitle=("Average annual YLD change vs.Urban change"), 
                             y="Anual YLD changes",x=" Urban changes in percentage", title="Scatterplot", caption = "YEAR2006-2040")+
  coord_cartesian(ylim=c(-350,350))#set the x-axis limits
pl2

#AET-------------------
theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48
scale_x_continuous(limits = c(0.5,100))#set the x-axis limits

pl3<-ggplot(d, aes(x = urban1, y = AN_AET))+ geom_point()+
  geom_smooth(span=0.3)+labs(subtitle=("Average annualAET change vs.Urban change"), 
                             y="Anual AET changes",x=" Urban changes in percentage", title="Scatterplot", caption = "YEAR2006-2040")+
  coord_cartesian(ylim=c(-350,350))#set the x-axis limits
pl3


#################### combine 2 figures into 1 page
library(ggpubr)
ggarrange(pl1 , pl2 ,pl3 , 
          labels = c("A.", "B.", "C."),
          ncol = 2, nrow = 2)


#######-----------------------------------------------------------------------------correlation analysis
#1. AN_AET
head(d,6)
res<-cor.test(d$AN_AET,d$urban1,method="pearson")
res
a1<-ggscatter(d,x="urban1",y="AN_AET",add="reg.line",
              add.params=list(color="blue",fill="lightgray"),
              conf.int=TRUE,cor.coef=TRUE,cor.method="pearson", 
              xlab="Urban changes in percentage",ylab="Annual AET")

#2. AN_YLD
res<-cor.test(d$AN_AET,d$urban1,method="pearson")
res
a2<-ggscatter(d,x="urban1",y="AN_YLD",add="reg.line",
              add.params=list(color="blue",fill="lightgray"),
              conf.int=TRUE,cor.coef=TRUE,cor.method="pearson", 
              xlab="Urban changes in percentage",ylab="Annual YLD")
#3. AN_PAET
res<-cor.test(d$AN_AET,d$urban1,method="pearson")
res
a3<-ggscatter(d,x="urban1",y="AN_PAET",add="reg.line",
              add.params=list(color="blue",fill="lightgray"),
              conf.int=TRUE,cor.coef=TRUE,cor.method="pearson", 
              xlab="Urban changes in percentage",ylab="Annual PAET")

#################### combine 2 figures into 1 page
library(ggpubr)
ggarrange(a1 , a2  , 
          labels = c("A.", "B."),
          ncol = 1, nrow = 2)
