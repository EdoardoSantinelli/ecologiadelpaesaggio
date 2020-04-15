# Codice R per analisi di immagini satellitari 

# packages: raster

install.packages("raster") o library(raster)

setwd("/Users/edoardosantinelli/Desktop/lab")

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

# day 2

setwd("/Users/edoardosantinelli/Desktop/lab")

load("teleril.RData")
 
ls()

plot(p224r63_2011)

cl <- colorRampPalette(c('black','grey','light grey'))(100) #

plot(p224r63_2011, col=cl)

# scala di grgi 
cllow <- colorRampPalette(c('black','grey','light grey'))(5)

plot(p224r63_2011, col=cllow)

# 

names(p224r63_2011)
# [1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
  # attach(dataframe) non funziona con il pacchetto raster
  #simbolo che lega la colonna (la banda) al dataset (immagine satellitare) : $
  
  # Exercise

clnir <- colorRampPalette(c('red','orange','yellow')
plot(p224r63_2011$B4_sre, col=clnir) 
  
# Multiframe
                          
 par(mfrow=c(2,2))
# Blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
# Green     
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre, col=clg)
# Red
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_2011$B3_sre, col=clr)
                          
# Infrarosso                
clnir <- colorRampPalette(c('red','orange','yellow')) (100)
plot(p224r63_2011$B4_sre, col=clnir) 

dev.off()           
                          
# natural colours
# 3 componenti: R G B
# 3 bands: R = banda del rosso, G = banda del verde , B = banda del blu                        
 plotRGB(p224r63_2011,r=3,g=2,b=1)
    

                          
                          
                          
 #rgb non viene letto ( maiuscolo)                       
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
                          
# nir
# false colours
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
 
 # Salvataggio                         
pdf("primografico.pdf") 
                          
                                                    
par(mfrow=c(1,2))                          
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
                          
# Excercise: Nir nella componente Green                        
plotRGB(p224r63_2011, r=1, g=4, b=3, stretch="Lin")
                       
# Excercise: Nir nella componente blue                         
plotRGB(p224r63_2011, r=1, g=4, b=3, stretch="Lin")

                          
####  day2                          
library(raster)                          
                          
setwd("/Users/edoardosantinelli/Desktop/lab")
                          
load(teleril.RData)
 
 # list 
ls()
                          
p224r63_1988 <- brick("p224r63_1988_masked.grd") 
                          
plot(p224r63_1988)
                          
par(mfrow=c(2,2))
                          #Blue
                          
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_1988$B1_sre, col=clb)
                      # Green    
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col=clg)
                           #Red
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_1988$B3_sre, col=clr)
                          #infrarosso
clnir <- colorRampPalette(c('red','orange','yellow')) (100)
plot(p224r63_1988$B4_sre, col=clnir) 
                          
                          
dev.off()   

plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")                          
                          
# excercise: plotta l'immmagine usando l'infrarosso nella componente R in RGB space                          
                      
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") 
              
 ## plot delle due immagini: 1988 e 2011:
                          
par(mfrow=c(2,1))
                          
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011") 
                          
                          
# spectral indices
#DVI1988 = nir1988-red1988                          
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$b3_sre
plot(dvi1988)
                       
  #excersie : calculate dvi for 20111                        
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
                          
cldvi <- colorRampPalette(c('light blue','light green','green'))(100)
plot(dvi2011, col=cldvi)  
                          
# multitemporal analysis (analisi multitemporale dei cambiamenti del paesaggio)
                          
difdvi <- dvi2011-dvi1988 
                          
plot(difdvi)
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) # 
plot(difdvi, col=cldifdvi)
# visualize the output
# multiframe 1988rgb, 2011rgb, difdiv                          
                          
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")
                        
plot(difdvi, col=cldifdvi)
                      
# Changing the grain (resolution)
                          
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)
                          

                              
par(mfrow=c(2,1))
                          
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
                          
 #lower resolution
                          
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
                          
                          
# original 30m -> resampled 1500m                         
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# dvi2011 low resolution
                          
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B4_sre
plot(dvi2011lr50)

# dvi1988 low resolution
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
plot(dvi1988lr50) 
# difdvi low resolution        (differenza dei due dvi a bassa risoluzione)               
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)
                          
# multiframe
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)

                          
