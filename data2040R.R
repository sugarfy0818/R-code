library(ggplot2)
library(foreign)


lc2006<-read.table("F:/yfang_project_folder/gisdata/landuse/txt/CELLINFO_US_HUC12_2006.txt",
                   header=TRUE,sep=",")

lc2011<-read.table("F:/yfang_project_folder/gisdata/landuse/txt/CELLINFO_US_HUC12_2011.txt",
                   header=TRUE,sep=",")

lc2040<-read.dbf("C:/Users/yfang.local/Desktop/R/WASSI input/lc2040final.dbf")
#frequency analysis
hist(lc2040$Urban)
hist(lc2006$urban)
hist(lc2011$urban)


u06<-lc2006[(12)]
names(u06)[1]<-"urban"
u11<-lc2011[(12)]
names(u11)[1]<-"urban"
u40<-lc2040[(12)]
names(u40)[1]<-"urban"


u06$landuse<-'2006'
u40$landuse<-'2040'
u11$landuse<-'2011'

data2<-rbind(u06,u11,u40)


#density curves plot
u06$landuse<-'2040'
u40$landuse<-'2006'

data3<-rbind(u40,u11)
ggplot(data2, aes(urban, fill = landuse)) + geom_density(alpha = 0.5)+
  ggtitle("Kernel Density of urban estimates")+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12))

#Frequency
set.seed(42)
p1<-hist(u06$urban)
p2<-hist(u40$urban)
plot(0,0,main="Histogram")
plot(p1,col=rgb(0,0,1,1/4),xlim=c(0,1))
plot(p2,col=rgb(1,0,1,1/4),xlim=c(0,1),add=T)


#----------frequency count histagram

library(RColorBrewer)

p1 <- ggplot(data2, aes(x = urban, fill = landuse)) +
  geom_histogram(aes(y = ..count..), binwidth = 0.05,
                 position="identity", alpha=0.4) +
  #stat_function(fun=dnorm,args=list(mean=mean(data1$urban),sd=sd(data1$urban)))+
  scale_x_continuous(name = "Urban",
                     breaks = seq(-0.2, 1,0.2),
                     limits=c(0, 1)) +
  scale_y_continuous(name = "Count",limits=c(0,36000)) +

  ggtitle("Frequency histogram of Urban Area") +
  theme_bw() +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 11),
        axis.text.y=element_text(colour="black", size = 11)) +
  scale_fill_brewer(palette="Accent")
p1



































#------

n<-merge(lc2006,lc2040, by="HUC_12", all=TRUE)
n[is.na(n)]<-0
#creat a new dataframe a and caluclate the difference
#of different land cover type from 2006 to 2040
attach(n)
n$crop1<-cropland-crop
n$deciduous1=deciduous.y-deciduous.x
n$evergreen1=evergreen.y-evergreen.x
n$mixedforest1=mixedfores-mixed_forest
n$grassland1=lc2040$Grassland-lc2006$grassland
n$shrubland1=lc2040$shrubland-lc2006$shrubland
n$wetlands1=Wetlands-wetlands
n$water1=Water-open_water
n$urban1=Urban-urban
n$urban1p=(Urban-urban)/urban
n$barren1=barren.y-barren.x


#just keep the columns we want

keeps<-c("HUC_12","lat.x","long.x","crop1","deciduous1","evergreen1",
         "mixedforest1", "grassland1","shrubland1","wetlands1",
         "water1","urban1","barren1","urban1p")
b=n[keeps]


#frequency 
hist(n$urban1)


library(dplyr)
library(foreign)

write.dbf(b,"C:/Users/yfang.local/Desktop/gis work/landcoverchange40.dbf",factor2char=TRUE)
write.csv(b,"C:/Users/yfang.local/Desktop/gis work/landcoverchange40.csv")
plot
