lc2006<-read.table("F:/yfang_project_folder/gisdata/landuse/txt/CELLINFO_US_HUC12_2006.txt",
                   header=TRUE,sep=",")
lc2011<-read.table("F:/yfang_project_folder/gisdata/landuse/txt/CELLINFO_US_HUC12_2011.txt",
                   header=TRUE,sep=",")
a<-cbind(lc2006)

#n<-merge(lc2006,lc2011, by="HUC_12")
#creat a new dataframe a and caluclate the difference
#of different land cover type from 2006 to 2011
a$crop1=lc2011$crop-lc2006$crop
a$deciduous1=lc2011$deciduous-lc2006$deciduous
a$evergreen1=lc2011$evergreen-lc2006$evergreen
a$mixedforest1=lc2011$mixedforest-lc2006$mixed_forest
a$grassland1=lc2011$grassland-lc2006$grassland
a$shrubland1=lc2011$shrubland-lc2006$shrubland
a$wetlands1=lc2011$wetlands-lc2006$wetlands
a$water1=lc2011$water-lc2006$open_water
a$urban1=lc2011$urban-lc2006$urban
a$barren1=lc2011$barren-lc2006$barren

#just keep the columns we want

keeps<-c("HUC_12","lat","long","crop1","deciduous1","evergreen1",
     "mixedforest1", "grassland1","shrubland1","wetlands1",
     "water1","urban1","barren1")
b=a[keeps]
library(dplyr)
library(foreign)

write.dbf(b,"landcoverchange.dbf",factor2char=TRUE)
write.csv(b,"landcoverchange.csv")
plot
