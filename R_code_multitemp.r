# R code analisi multitemporale di variazione della land cover

setwd("/Users/edoardosantinelli/Desktop/lab")
library(raster)

defor1 <- brick("defor1.png")
defor2 <- brick("defor2.png")

# names: defor1_.1 = NIR ,  defof1_.2= red , defor1_.3= green

plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")

# Excercise plot seconda data

plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

par(mfrow=c(2,1))
plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# classificazione non supervisionata (non diamo input al pc su quali zone sono foresta o meno)
library(RStoolbox)


d1c <- unsuperClass(defor1, nClasses=2)

plot(d1c$map)
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)

# classificazione di defor2
#excercise : classificare con due classsi l'immagine defor2
d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map, col=cl)
dev.off()

# plot delle 2 mappe ottenute

par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

freq(d1c$map)
# aree aperte =35213
# foresta = 306079

totd1 <- 306079 + 35213
totd1

percent1 <- freq(d1c$map) * 100 / 341292
percent1

# percentuali 
# foreste = 89,6
# aree aperte=10,4

freq(d2c$map)
totd2 <- 163094 + 179632
percent2 <- freq(d2c$map) * 100 / totd2
percent2
# percentuali
# foreste 52,42
# aree aperte 47,58

cover <- c("Agriculture","Forest")
before <- c(10.4, 89.6)
after <- c(47.58, 52.42)

output <- data.frame(cover, before, after)
library(ggplot2)

###### day 2
set
library(raster)
load("defor.RData")


par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

library(ggplot2)

ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

# excercise plot the histograms of the land cover after deforastation

ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")
####
install.packages("gridExtra")
library(gridExtra)

# grid.arrange(plot1, plo2, nrow=1)


# histograms of the % cover before deforstation
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

grid.arrange(grafico1, grafico2, nrow=1)

