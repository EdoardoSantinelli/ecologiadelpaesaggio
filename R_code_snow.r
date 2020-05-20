

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

rlist <- list.files(pattern=".tif")
lapply(rlist, raster)
list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp,col=cl)

par(mfrow=c(1,2))

plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)

par(mfrow=c(1,2))

plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))


differenza nella neve difsnow

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r

snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff)

#### prediction 
scaricare il file nella cartella snow1 da IOL

source("prediction.r")

plot(prediction.snow.2025.norm, col=cl)

prediction.snow.2025.norm <- raster(view.php.tif)
plot(predicted.snow.2025.norm, col=cl)
