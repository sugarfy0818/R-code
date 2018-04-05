liu<-read.csv("C:/Users/yfang.local/Desktop/data.csv")
head(liu)

library(rworldmap)
newmap<-getMap(resolution="low")
plot(newmap,xlim=c(100,120),ylim=c(20,50),asp=1)
points(liu$Longitude,liu$Latitude,col="red",cex=1.2)