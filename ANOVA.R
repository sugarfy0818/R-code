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
wateryld40<-read.table("C:/WaSSi/WaSSI_HUC12/OUTPUTS_landcover2011using2006impreviousLAIdata/A_WATERYLD.txt",
                       header=TRUE,sep=",")
#wateryld40<-subset(wateryld40,HUCNO==HUCselect)


###create data of difference of each hydrology variables between 2006 and 2040


#creat a new dataframe a and caluclate the difference
water<-cbind(wateryld06)

#of different land cover type from 2006 to 2040
water$AN_PAET=wateryld40$AN_PAET-wateryld06$AN_PAET
water$AN_AET=wateryld40$AN_AET-wateryld06$AN_AET
water$AN_YLD=wateryld40$AN_YLD-wateryld06$AN_YLD
keeps<-c("HUCNO","YEAR","AN_PAET", "AN_AET","AN_YLD","AN_PPT","AN_TEMP")
wateryld=water[keeps]  


#--------------------landcover change data------------------------------


urbantotal<-read.table("C:/Users/yfang.local/Desktop/gis work/landcoverchange40.csv",header=TRUE,sep=",")

#just keep the columns we want
colnames(urbantotal)[3]<-c("lat")
colnames(urbantotal)[4]<-c("long")

keeps<-c("HUC_12","lat","long","urban1","urban1p","deciduous1","evergreen1","mixedforest1","wetlands1","shrubland1","water1")
#keeps<-c("HUC_12","lat.x","long.x","urban1","deciduous1","evergreen1","wetlands1","shrubland1","water1")exclude water>80%

urban1=urbantotal[keeps]
#print(urban1)

#--------do the merge-------------

#Change the column name HUCNO to HUC_12 for the later combination
colnames(wateryld)[1]<-c("HUC_12")


total<-merge(wateryld,urban1,by="HUC_12")
#print(total)

#write.dbf(total,file="C:/Users/yfang.local/Desktop/R/landhydro.dbf",factor2char=TRUE)
#----calculate the annual(average of many years)hydrology variable by HUC 

d=aggregate(total[,3:17], list(total$HUC_12), mean)

colnames(d)[9]<-c("urbanchange")
colnames(d)[10]<-c("urbanchangep")
colnames(d)[11]<-c("decichange")
colnames(d)[12]<-c("everchange")
colnames(d)[13]<-c("mixedchange")

colnames(d)[1]<-c("HUC_12")

write.dbf(total,file="C:/Users/yfang.local/Desktop/R/urbanchange40withwater.dbf",factor2char=TRUE)

#import land cover in year 2006
lc2006<-read.table("F:/yfang_project_folder/gisdata/landuse/txt/CELLINFO_US_HUC12_2006.txt",
                   header=TRUE,sep=",")


#Do the merge
final<-merge(d,lc2006,by="HUC_12")
keep1<-c("HUC_12","lat.x","long.y","urbanchange","urbanchangep","decichange","everchange","mixedchange","deciduous","evergreen","mixed_forest","grassland","shrubland",
         "wetlands","open_water","barren","urban", "crop","AN_PAET","AN_AET","AN_YLD","AN_PPT","AN_TEMP")

final=final[keep1]
colnames(final)[2]<-c("lat")
colnames(final)[3]<-c("long")

#print(urban1)




#Modify the dataset for ANOVA analysis
#Original land cover---------
#Type1. Grassland+barren+shrubland+urban>0.5
#Type2. mixed forest+evergreen forest+deciduous forest>0.5
#DRY WET
#Type1. dry<400mm, Type2. mediate >400 and <1000mm, Type3. wet>1000mm

#final$climate<-cut(final$AN_PPT,breaks=c(-Inf, 400, 1000, Inf),
#                  labels=c("dry", "mediate" ,"wet"))
finalb=final
attach(finalb)
finalb$forestchange<-decichange+everchange+mixedchange
detach(finalb)
#remove urban decrease
finalb<-finalb[!(finalb$urbanchange<0),]
attach(finalb)
finalb$minus<-forestchange-urbanchange
detach(finalb)
#delete where forest increase greater than urban
final1<-finalb[!(finalb$minus>0),]

final1$climate[final1$AN_PPT>1100]<-"Wet"
final1$climate[final1$AN_PPT<500]<-"Dry"
attach(final1)
final1$sumlow<-grassland+barren+shrubland+urban
final1$sumhigh<-deciduous+mixed_forest+evergreen
detach(final1)

#remove water area>90%
final1<-final1[!(final1$open_water>0.9),]


final1$landcover06[final1$sumlow>0.5 & final1$sumhigh<0.4]<-"LowLAI"
#final1$landcover06[final1$sumhigh<0.5 & final1$sumhigh>0.3]<-"MidLAI"
final1$landcover06[final1$sumhigh>0.5 & final1$sumlow<0.4]<-"HighLAI"

#**********find out reason why urban increase but AET increase-
attach(final1)
odd<-final1[which(urbanchange>0 & AN_AET>0),]
detach(final1)

#plot scatterplot
attach(odd)
plot(urbanchange,AN_AET, main="Scatterplot",xlab="urbanchange ",ylab="AN_AET")

#---------------------



#plot scatterplot
attach(final1)
plot(climate,AN_AET, main="Scatterplot",xlab="climate ",ylab="AN_AET")

finala=final1
#final.df$climate=factor(final.df$climate,labels=c("wet","dry"))
final.df<-na.omit(finala)
#1.          AN_AET

group_by(final.df, climate) %>%
  summarise(
    count = n(),
    mean = mean(AN_AET, na.rm = TRUE),
    sd = sd(AN_AET, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(final.df, x = "climate", y = "AN_AET", 
          color = "climate", palette = c("#00AFBB", "#FC4E07"),
          order = c("Dry", "Wet"),
          ylab = "AN_AET", xlab = "climate")

# Compute the analysis of variance
res.aov <- aov(AN_AET ~ climate, data = final.df)
# Summary of the analysis
summary(res.aov)

#2.          AN_YLD
group_by(final.df, climate) %>%
  summarise(
    count = n(),
    mean = mean(AN_YLD, na.rm = TRUE),
    sd = sd(AN_YLD, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(final.df, x = "climate", y = "AN_YLD", 
          color = "climate", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("dry", "mediate", "wet"),
          ylab = "AN_YLD", xlab = "climate")

# Compute the analysis of variance
res.aov <- aov(AN_YLD ~ climate, data = final.df)
# Summary of the analysis
summary(res.aov)



library("ggpubr")
ggboxplot(final.df, x = "landcover06", y = "AN_YLD", 
          color = "landcover06", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("LowLAI", "MidLAI", "HighLAI"),
          ylab = "AN_YLD", xlab = "landcover06")

# Compute the analysis of variance
res.aov <- aov(AN_YLD ~ landcover06, data = final.df)
# Summary of the analysis
summary(res.aov)


library(car)
###############model
model = lm(AN_AET ~ climate + landcover06 +climate:landcover06,
           data=final.df)


Anova(model, type="II")                    # Can use type="III"


boxplot(AN_AET ~ climate:landcover06,
        data = final.df,
        xlab = "Climate x landcover06",
        ylab = "AN_AET")





###############model
model = lm(AN_YLD ~ climate + landcover06 + climate:landcover06,
           data=final.df)


Anova(model, type="II")                    # Can use type="III"


boxplot(AN_YLD ~ climate:landcover06,
        data = final.df,
        xlab = "Climate x landcover06",
        ylab = "AN_YLD")


######

