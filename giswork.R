library(ggplot2)
library(ggpubr)

library(dplyr)
library(foreign)
library(plyr)


#[1]--------import selected huc dbf file R4
library("foreign")
data1=read.dbf("C:/Users/yfang.local/Desktop/R/export dbf reclass/R4.dbf")
#create a new cloumn "LCLU" which is equal to the right 2 digits of Value
data1$Value1<-data1$Value
data1$LCLU<-data1$Value%%100
#create a new cloumn "HUC" which is equal to the left 1-3 digits of value, seperate at the fouth right
data<-data1%>%separate(Value1,into=c('HUC','drop'),sep=-4,convert=TRUE)

#summrize the count by HUC
sumcount<-ddply(data,~HUC,summarise,sum=sum(Count))

#Outer join 
data2=merge(x=data,y=sumcount,by="HUC",all=TRUE)

#Creat a new column calculate the Count/Sum
data2$percent<-data2$Count/data2$sum
data2$drop<-NULL
#round(data1,digits=4)

write.dbf(data2,file="C:/Users/yfang.local/Desktop/R/2040 lclu/r4_2040.dbf",factor2char=TRUE)

#[2]--------import selected huc dbf file R10
library("foreign")
data1=read.dbf("C:/Users/yfang.local/Desktop/R/export dbf reclass/R10.dbf")
#create a new cloumn "LCLU" which is equal to the right 2 digits of Value
data1$Value1<-data1$Value
data1$LCLU<-data1$Value%%100
#create a new cloumn "HUC" which is equal to the left 1-3 digits of value, seperate at the fouth right
data<-data1%>%separate(Value1,into=c('HUC','drop'),sep=-4,convert=TRUE)

#summrize the count by HUC
sumcount<-ddply(data,~HUC,summarise,sum=sum(Count))

#Outer join 
data2=merge(x=data,y=sumcount,by="HUC",all=TRUE)

#Creat a new column calculate the Count/Sum
data2$percent<-data2$Count/data2$sum
data2$drop<-NULL
#round(data1,digits=4)

write.dbf(data2,file="C:/Users/yfang.local/Desktop/R/2040 lclu/r10_2040.dbf",factor2char=TRUE)



#[3]--------import selected huc dbf file R17
library("foreign")
data1=read.dbf("C:/Users/yfang.local/Desktop/R/export dbf reclass/R17.dbf")
#create a new cloumn "LCLU" which is equal to the right 2 digits of Value
data1$Value1<-data1$Value
data1$LCLU<-data1$Value%%100
#create a new cloumn "HUC" which is equal to the left 1-3 digits of value, seperate at the fouth right
data<-data1%>%separate(Value1,into=c('HUC','drop'),sep=-4,convert=TRUE)

#summrize the count by HUC
sumcount<-ddply(data,~HUC,summarise,sum=sum(Count))

#Outer join 
data2=merge(x=data,y=sumcount,by="HUC",all=TRUE)

#Creat a new column calculate the Count/Sum
data2$percent<-data2$Count/data2$sum
data2$drop<-NULL
#round(data1,digits=4)

write.dbf(data2,file="C:/Users/yfang.local/Desktop/R/2040 lclu/r17_2040.dbf",factor2char=TRUE)




#[4]--------import selected huc dbf file R79
library("foreign")
data1=read.dbf("C:/Users/yfang.local/Desktop/R/export dbf reclass/R79a.dbf")
#create a new cloumn "LCLU" which is equal to the right 2 digits of Value
data1$Value1<-data1$Value
data1$LCLU<-data1$Value%%100
#create a new cloumn "HUC" which is equal to the left 1-3 digits of value, seperate at the fouth right
data<-data1%>%separate(Value1,into=c('HUC','drop'),sep=-4,convert=TRUE)

#summrize the count by HUC
sumcount<-ddply(data,~HUC,summarise,sum=sum(Count))

#Outer join 
data2=merge(x=data,y=sumcount,by="HUC",all=TRUE)

#Creat a new column calculate the Count/Sum
data2$percent<-data2$Count/data2$sum
data2$drop<-NULL
#round(data1,digits=4)

write.dbf(data2,file="C:/Users/yfang.local/Desktop/R/2040 lclu/r79_2040.dbf",factor2char=TRUE)



#[5]--------import selected huc dbf file R125,*
library("foreign")
library("tidyr")
library("stringi")
data1=read.dbf("C:/Users/yfang.local/Desktop/R/export dbf reclass/R125.dbf")
#create a new cloumn "LCLU" which is equal to the right 2 digits of Value
data1$Value1<-data1$Value
data1$LCLU<-data1$Value%%100
#create a new cloumn "HUC" which is equal to the left 1-3 digits of value, seperate at the fouth right
data<-data1%>%separate(Value1,into=c('HUC','drop'),sep=-4,convert=TRUE)


#summrize the count by HUC
sumcount<-ddply(data,~HUC,summarise,sum=sum(Count))

#Outer join 
data1=merge(x=data,y=sumcount,by="HUC",all=TRUE)

#Creat a new column calculate the Count/Sum
data1$percent<-data1$Count/data1$sum
data1$drop<-NULL

write.dbf(data1,file="C:/Users/yfang.local/Desktop/R/2040 lclu/r125_2040.dbf",factor2char=TRUE)



#[6]--------import selected huc dbf file R368,*
library("foreign")
library("tidyr")
library("stringi")
data1=read.dbf("C:/Users/yfang.local/Desktop/R/export dbf reclass/R368.dbf")
#create a new cloumn "LCLU" which is equal to the right 2 digits of Value
data1$Value1<-data1$Value
data1$LCLU<-data1$Value%%100
#create a new cloumn "HUC" which is equal to the left 1-3 digits of value, seperate at the fouth right
data<-data1%>%separate(Value1,into=c('HUC','drop'),sep=-4,convert=TRUE)


#summrize the count by HUC
sumcount<-ddply(data,~HUC,summarise,sum=sum(Count))

#Outer join 
data1=merge(x=data,y=sumcount,by="HUC",all=TRUE)

#Creat a new column calculate the Count/Sum
data1$percent<-data1$Count/data1$sum
data1$drop<-NULL

write.dbf(data1,file="C:/Users/yfang.local/Desktop/R/2040 lclu/r368_2040.dbf",factor2char=TRUE)


#[7]--------import selected huc dbf file R111213,*
library("foreign")
library("tidyr")
library("stringi")
data1=read.dbf("C:/Users/yfang.local/Desktop/R/export dbf reclass/R111213.dbf")
#create a new cloumn "LCLU" which is equal to the right 2 digits of Value
data1$Value1<-data1$Value
data1$LCLU<-data1$Value%%100
#create a new cloumn "HUC" which is equal to the left 1-3 digits of value, seperate at the fouth right
data<-data1%>%separate(Value1,into=c('HUC','drop'),sep=-4,convert=TRUE)


#summrize the count by HUC
sumcount<-ddply(data,~HUC,summarise,sum=sum(Count))

#Outer join 
data1=merge(x=data,y=sumcount,by="HUC",all=TRUE)

#Creat a new column calculate the Count/Sum
data1$percent<-data1$Count/data1$sum
data1$drop<-NULL

write.dbf(data1,file="C:/Users/yfang.local/Desktop/R/2040 lclu/r111213_2040.dbf",factor2char=TRUE)


#[8]--------import selected huc dbf file R14151618,*
library("foreign")
library("tidyr")
library("stringi")
data1=read.dbf("C:/Users/yfang.local/Desktop/R/export dbf reclass/R14151618.dbf")
#create a new cloumn "LCLU" which is equal to the right 2 digits of Value
data1$Value1<-data1$Value
data1$LCLU<-data1$Value%%100
#create a new cloumn "HUC" which is equal to the left 1-3 digits of value, seperate at the fouth right
data<-data1%>%separate(Value1,into=c('HUC','drop'),sep=-4,convert=TRUE)


#summrize the count by HUC
sumcount<-ddply(data,~HUC,summarise,sum=sum(Count))

#Outer join 
data1=merge(x=data,y=sumcount,by="HUC",all=TRUE)

#Creat a new column calculate the Count/Sum
data1$percent<-data1$Count/data1$sum
data1$drop<-NULL

write.dbf(data1,file="C:/Users/yfang.local/Desktop/R/2040 lclu/R14151618_2040.dbf",factor2char=TRUE)

