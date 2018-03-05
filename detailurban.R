library(ggplot2)
library(ggpubr)

library(dplyr)
library(foreign)


#import selected huc dbf file
library("foreign")
data=read.dbf("C:/Users/yfang.local/Desktop/R/hydrolandcoverselected.dbf")




#######-----------------------------------------------------------------------------correlation analysis
#1. AN_AET
#head(d,6)
res<-cor.test(data()$AN_AET,data$urban1,method="pearson")
res
a1<-ggscatter(data,x="urban1",y="AN_AET",add="reg.line",
              add.params=list(color="blue",fill="lightgray"),
              conf.int=TRUE,cor.coef=TRUE,cor.method="pearson", 
              xlab="Urban changes in percentage",ylab="Annual AET")

#2. AN_YLD
res<-cor.test(data$AN_AET,data$urban1,method="pearson")
res
a2<-ggscatter(data,x="urban1",y="AN_YLD",add="reg.line",
              add.params=list(color="blue",fill="lightgray"),
              conf.int=TRUE,cor.coef=TRUE,cor.method="pearson", 
              xlab="Urban changes in percentage",ylab="Annual YLD")
#3. AN_PAET
res<-cor.test(data$AN_AET,data$urban1,method="pearson")
res
a3<-ggscatter(data,x="urban1",y="AN_PAET",add="reg.line",
              add.params=list(color="blue",fill="lightgray"),
              conf.int=TRUE,cor.coef=TRUE,cor.method="pearson", 
              xlab="Urban changes in percentage",ylab="Annual PAET")

#################### combine 2 figures into 1 page
library(ggpubr)
ggarrange(a1 , a2 ,a3 , 
          labels = c("A.", "B.", "C."),
          ncol = 2, nrow = 2)