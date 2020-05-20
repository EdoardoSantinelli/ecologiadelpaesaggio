

setwd("/Users/edoardosantinelli/Desktop/lab")

install.packages("ncdf4")

library(ncdf4)
library(raster)

importare immagini : raster(un singolo livello) , brick , 


snowmay <- raster("snow")

 cl <- colorRampPalette(c("darkblue","blue","light blue"))(100))
 
plot(snowmay, col=cl) 

#### import snow1 data

setwd("/Users/edoardosantinelli/Desktop/lab/snow1")

