lc2006<-read.table("F:/yfang_project_folder/gisdata/landuse/txt/CELLINFO_US_HUC12_2006.txt",
                    header=TRUE,sep=",")
#lc2006=subset(lc2006s,open_water<0.8)
lc2011<-read.table("F:/yfang_project_folder/gisdata/landuse/txt/CELLINFO_US_HUC12_2011.txt",
                    header=TRUE,sep=",")
#lc2011=subset(lc2011s,water<0.8)


n<-merge(lc2006,lc2011, by="HUC_12")
#creat a new dataframe a and caluclate the difference
#of different land cover type from 2006 to 2011
n$crop1=n$crop.y-n$crop.x
n$deciduous1=n$deciduous.y-n$deciduous.x
n$evergreen1=n$evergreen.y-n$evergreen.x
n$mixedforest1=n$mixedforest-n$mixed_forest
n$grassland1=n$grassland.y-n$grassland.x
n$shrubland1=n$shrubland.y-n$shrubland.x
n$wetlands1=n$wetlands.y-n$wetlands.x
n$water1=n$water-n$open_water
n$urban1=n$urban.y-n$urban.x
n$barren1=n$barren.y-n$barren.x

#just keep the columns we want
a=subset(n,open_water<0.8 & water<0.8)

keeps<-c("HUC_12","lat.x","long.x","crop1","deciduous1","evergreen1",
         "mixedforest1", "grassland1","shrubland1","wetlands1",
         "water1","urban1","barren1")
b=a[keeps]
library(dplyr)
library(foreign)
#write.dbf(b,"landcoverchange%.dbf",factor2char=TRUE)
write.table(b,file="C:/Users/yfang.local/Desktop/R/landcoverchangenowater.txt")
