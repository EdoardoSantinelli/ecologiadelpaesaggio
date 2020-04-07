# Codice R per analisi di immagini satellitari 

# packages: raster

install.packages("raster") o library(raster)

setwd("/Users/edoardosantinelli/Desktop/lab")

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

