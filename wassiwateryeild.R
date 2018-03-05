library(ggplot2)

######
HUCselect<-120401040103
print(HUCselect)

#Read the WASSI results by 2006 landcover data
wateryld06<-read.table("F:/WaSSI_HUC12/OUTPUTS_landcover2006/A_WATERYLD.txt",
                       header=TRUE,sep=",")
wateryld06<-subset(wateryld06,HUCNO==HUCselect)

#Read the WASSI results by 2011 landcover data
wateryld11<-read.table("F:/WaSSI_HUC12/OUTPUTS_landcover2011/A_WATERYLD.txt",
                       header=TRUE,sep=",")
wateryld11<-subset(wateryld11,HUCNO==HUCselect)




#####################################################
#Wateryld 2006
#1
theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48
scale_x_continuous(limits = c(0, 800))#set the x-axis limits

pl06_1<-ggplot(wateryld06, aes(x = YEAR, y = AN_AET))+ geom_point()+
  geom_smooth(method="loess", se=F)+labs(subtitle=paste0("Annual ET of HUC", HUCselect), 
                                         y="AET",x="Year", title="Scatterplot", caption = paste0(HUCselect,"YEAR2006"))+
  coord_cartesian(ylim=c(430, 800))#set the x-axis limits
pl06_1
#method can be "lm"


#2 Time series plot 
library(ggplot2)
theme_set(theme_bw())

# Allow Default X Axis Labels
pl06_2<-ggplot(wateryld06, aes(x=YEAR)) + 
  geom_line(aes(y=AN_AET)) + 
  labs(title="Time Series Chart", 
       subtitle=paste0("AET by WASSI, HUC: ", HUCselect), 
       caption=HUCselect, 
       y="Anual AET (mm)")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  coord_cartesian(ylim=c(430, 800))#set the x-axis limits
pl06_2


########################################################
#Wateryld 2011
#1
theme_set(theme_bw()) # pre-set the bw theme.
options(scipen=999)  # turn-off scientific notation like 1e+48

pl11_1<-ggplot(wateryld11, aes(x = YEAR, y = AN_AET))+ geom_point()+
  geom_smooth(method="loess", se=F)+labs(subtitle=paste0("Annual ET of HUC", HUCselect),
                                         y="AET",x="Year", title="Scatterplot", caption = paste0(HUCselect,"YEAR2011"))+
  coord_cartesian(ylim=c(430, 800))#set the x-axis limits

pl11_1
#method can be "lm"


#2 Time series plot 
library(ggplot2)
theme_set(theme_bw())

# Allow Default X Axis Labels
# Allow Default X Axis Labels
pl11_2<-ggplot(wateryld11, aes(x=YEAR)) + 
  geom_line(aes(y=AN_AET)) + 
  labs(title="Time Series Chart", 
       subtitle=paste0("AET by WASSI, HUC:", HUCselect), 
       caption=HUCselect, 
       y="Anual AET (mm)")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  coord_cartesian(ylim=c(430, 800))#set the x-axis limits
pl11_2




#################### combine 2 figures into 1 page
library(ggpubr)
ggarrange(pl06_1, pl11_1,pl06_2, pl11_2 + rremove("y.text"), 
          labels = c("06", "11", "06", "11"),
          ncol = 2, nrow = 2)


####################Plot two time series into one plot
# modify data

keeps<-c("AN_YLD")
data06=wateryld06[keeps]
data11=wateryld11[keeps]

# from 1961 to 2015 as a time series object
myts06 <- ts(data06, start=c(1961), end=c(2015), frequency=1)
myts11 <- ts(data11, start=c(1961), end=c(2015), frequency=1)


plot(myts06)
plot(myts11)


# 2nd step make plot
library(astsa)
#----style1
#png(file='gtemps2.png', width=600, height=320)

gtemp.df= data.frame(Time=c(time(myts06)), gtemp=c(myts06), gtempl=c(myts11))
ggplot(data = gtemp.df, aes(x=Time, y=value, color=variable)) +
  ylab('Anual AET (mm)') + xlab('YEAR')+
  geom_line(aes(y=gtemp , col='06'),  size=1, alpha=.5)  +
  geom_line(aes(y=gtempl, col='11'), size=1, alpha=.5) +
  theme(legend.position=c(.1,.85))	

#dev.off()	
#----style2 PREFERRED!
#png(file='gtempsbase.png', width=600, height=320)

par(mar=c(2,3,2,0)+.5, mgp=c(1.6,.6,0)) #modify plot width and height
ts.plot(myts06, myts11, ylab="Anual Water YLD (mm)", xlab="YEAR", main=c('HUC:',HUCselect),col=4, type='n')
grid(lty=1, col=gray(.9))
lines(myts06,  lwd=2, col = rgb(.9,  0, .7, .5) )
lines(myts11, lwd=2, col = rgb( 0, .7, .9, .5) )
legend('topleft', col=c(rgb(.9, 0, .7),  rgb(0, .7, .9)), lty=1, lwd=2, 
       legend=c("Landcover 2006", "Landcover 2011"), bg='white')  
