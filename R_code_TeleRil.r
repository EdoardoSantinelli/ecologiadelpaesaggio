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
                          
                          
                          
                          
                          
                          
                          
                           
                          
