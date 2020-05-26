

setwd("/Users/edoardosantinelli/Desktop/lab")

library(raster)

d1c <- raster("d1c.tif)
d2c <- raster("d2c.tif")

par(mfrow=c(1,2))

cl <- colorRampPalette(c("green","black"))(100)

plot(d1c,col=cl)
plot(d2c,col=cl)


par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
 
 funzione CBIND : elimina alcuni valori es, agricoltura , si metonono come valori nulla ed estraiamo la foresta

# forest: class2 , agricolture class 1

d1c.for <- reclassify(d1c, cbind(1,NA))


cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for)

d2c.for <- reclassify(d2c, cbind(1,NA))

par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

# creating patchess

install.packages("igraph")
library(igraph) # for patches
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# d1c.for.patches <- raster("d1c.for.pacthes.tif")
# d2c.for.patches <- raster("d2c.for.pacthes.tif")

excercise plottare entrambe le mappe una accanto all'altra

par(mfrow=c(1,2))
plot(d1c.for.patches)
plot(d2c.for.patches)

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

# max patches d1 301
# max patches d2 1212

# plot results:
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)
