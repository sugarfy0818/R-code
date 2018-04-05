library(ggplot2)
library(ggpubr)
library(foreign)
library(dplyr)
library(foreign)
library(plyr)
library(reshape)
library(gdata)
library(tidyverse)
######
#HUCselect<-30300020606 #NC
#HUCselect<-180600050603 #CA
#HUCselect<-31401020501 #FL 
HUCselect<-20401040103 #FL 

print(HUCselect)

#Read the WASSI results by 2006 landcover data
wateryld06<-read.table("F:/WaSSI_HUC12/OUTPUTS_landcover2006/A_WATERYLD.txt",
                       header=TRUE,sep=",")
wateryld06s<-subset(wateryld06,HUCNO==HUCselect)

#Read the WASSI results by 2040 ssp5 landcover data
wateryld40<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_ssp5_2040_2006LAIimprevious/A_WATERYLD.txt",
                       header=TRUE,sep=",")
wateryld40s<-subset(wateryld40,HUCNO==HUCselect)



###create data of difference of each hydrology variables between 2006 and 2040


#creat a new dataframe a and caluclate the difference
#water<-cbind(wateryld06)

#of different land cover type from 2006 to 2040
#water$AN_PAET=wateryld40$AN_PAET-wateryld06$AN_PAET
#water$AN_AET=wateryld40$AN_AET-wateryld06$AN_AET
#water$AN_YLD=wateryld40$AN_YLD-wateryld06$AN_YLD
#keeps<-c("HUCNO","YEAR","AN_PAET", "AN_AET","AN_YLD")
#wateryld=water[keeps]   #produce data file "wateryld" use for other program


#ggplot(wateryld,aes(x=HUCNO,y=AN_PAET))+geom_point(shape=1)


#####################################################
#Wateryld 2006
#1
theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48
scale_x_continuous(limits = c(0, 800))#set the x-axis limits

pl06_1<-ggplot(wateryld06s, aes(x = YEAR, y = AN_AET))+ geom_point()+
  geom_smooth(method="loess", se=F)+labs(subtitle=paste0("Annual ET of HUC", HUCselect), 
                                         y="AET",x="Year", title="Scatterplot", caption = paste0(HUCselect,"YEAR2006"))+
  coord_cartesian(ylim=c(600, 900))#set the x-axis limits
pl06_1
#method can be "lm"


#2 Time series plot 
library(ggplot2)
theme_set(theme_bw())

# Allow Default X Axis Labels
pl06_2<-ggplot(wateryld06s, aes(x=YEAR)) + 
  geom_line(aes(y=AN_AET)) + 
  labs(title="Time Series Chart", 
       subtitle=paste0("AET by WASSI, HUC: ", HUCselect), 
       caption=HUCselect, 
       y="Anual AET (mm)")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  coord_cartesian(ylim=c(600, 900))#set the x-axis limits
pl06_2


########################################################
#Wateryld 2040
#1
theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48

pl40_1<-ggplot(wateryld40s, aes(x = YEAR, y = AN_AET))+ geom_point()+
  geom_smooth(method="loess", se=F)+labs(subtitle=paste0("Annual ET of HUC", HUCselect),
                                         y="AET",x="Year", title="Scatterplot", caption = paste0(HUCselect,"YEAR2040"))+
  coord_cartesian(ylim=c(600, 900))#set the x-axis limits

pl40_1
#method can be "lm"


#2 Time series plot 
library(ggplot2)
theme_set(theme_bw())

# Allow Default X Axis Labels
# Allow Default X Axis Labels
pl40_2<-ggplot(wateryld40s, aes(x=YEAR)) + 
  geom_line(aes(y=AN_AET)) + 
  labs(title="Time Series Chart", 
       subtitle=paste0("AET by WASSI, HUC:", HUCselect), 
       caption=HUCselect, 
       y="Anual AET (mm)")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  coord_cartesian(ylim=c(600, 900))#set the x-axis limits
pl40_2




#################### combine 2 figures into 1 page
library(ggpubr)
ggarrange(pl06_1, pl40_1,pl06_2, pl40_2 + rremove("y.text"), 
          labels = c("06", "40", "06", "40"),
          ncol = 2, nrow = 2)

#-------------------------------------------------------------------------------
#1. FOR [A_AET]

####################Plot two time series into one plot
# modify data

keeps<-c("AN_AET")
keepsp<-c("AN_PPT")
data06=wateryld06s[keeps]
data40=wateryld40s[keeps]

#precipitation
datap=wateryld06s[keepsp] #Year 06 and 40 precipitation are the same

# from 1961 to 2015 as a time series object
myts06 <- ts(data06, start=c(1961), end=c(2015), frequency=1)
myts40 <- ts(data40, start=c(1961), end=c(2015), frequency=1)
mytsp<-ts(datap, start=c(1961), end=c(2015), frequency=1)

# 2nd step make plot
library(astsa)
#----style1
#png(file='gtemps2.png', width=600, height=320)
#theme_set(theme_bw()) # pre-set the bw theme.
gtemp.df= data.frame(Time=c(time(myts06)), gtemp=c(myts06), gtempl=c(myts40),gppt=c(mytsp))
ggplot(data = gtemp.df, aes(x=Time, y=value, color=variable)) +
  ylab('Anual AET (mm)') + xlab('YEAR')+
  geom_line(aes(y=gtemp , col='06'),  size=1, alpha=.5)  +
  geom_line(aes(y=gtempl, col='40'), size=1, alpha=.5) +
  geom_line(aes(y=gppt, col='4'), size=1, alpha=.5)
theme(legend.position=c(.1,.85))


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



#2. FOR [YLD]
# modify data

keeps<-c("AN_YLD")
data06=wateryld06s[keeps]
data40=wateryld40s[keeps]
#precipitation
datap=wateryld06s[keepsp] #Year 06 and 40 precipitation are the same

# from 1961 to 2015 as a time series object
myts06 <- ts(data06, start=c(1961), end=c(2015), frequency=1)
myts40 <- ts(data40, start=c(1961), end=c(2015), frequency=1)
mytsp<-ts(datap, start=c(1961), end=c(2015), frequency=1)


par(mar=c(2,3,2,0)+.5, mgp=c(1.6,.6,0)) #modify plot width and height
ts.plot(myts06, myts40,mytsp, ylab="mm", xlab="YEAR", main=c('HUC:',HUCselect),col=4, type='n')
grid(lty=1, col=gray(.9))
lines(myts06,  lwd=2, col = rgb(.9,  0, .7, .5) )
lines(myts40, lwd=2, col = rgb( 0, .7, .9, .5) )
lines(mytsp, lwd=3,lty=4, col = rgb( 0, 0, .1, .3))

legend('topleft', col=c(rgb(.9, 0, .7),  rgb(0, .7, .9),rgb(0,0,.1)), lty=1, lwd=2, 
       legend=c("Landcover 2006 AN_YLD", "Landcover 2040 AN_YLD","PPT"), bg='white')  



#3. FOR [AN_PAET]
# modify data

keeps<-c("AN_PAET")
data06=wateryld06s[keeps]
data40=wateryld40s[keeps]
#precipitation
datap=wateryld06s[keepsp] #Year 06 and 40 precipitation are the same

# from 1961 to 2015 as a time series object
myts06 <- ts(data06, start=c(1961), end=c(2015), frequency=1)
myts40 <- ts(data40, start=c(1961), end=c(2015), frequency=1)
mytsp<-ts(datap, start=c(1961), end=c(2015), frequency=1)


par(mar=c(2,3,2,0)+.5, mgp=c(1.6,.6,0)) #modify plot width and height
ts.plot(myts06, myts40,mytsp, ylab="mm", xlab="YEAR", main=c('HUC:',HUCselect),col=4, type='n')
grid(lty=1, col=gray(.9))
lines(myts06,  lwd=2, col = rgb(.9,  0, .7, .5) )
lines(myts40, lwd=2, col = rgb( 0, .7, .9, .5) )
lines(mytsp, lwd=3,lty=4, col = rgb( 0.1, 0, .1, .3) )

legend('topleft', col=c(rgb(.9, 0, .7),  rgb(0, .7, .9),rgb(.4,.4,.4)), lty=1, lwd=2, 
       legend=c("Landcover 2006 AN_PAET", "Landcover 2040 AN_PAET","PPT"), bg='white')   

