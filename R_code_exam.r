
# R_code_exam.r

CODE 1

# potete scrivere qualsiasi cosa

# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO

install.packages("sp")                                 

"install.packages"
#ES il comando install.packages serve per installare i pacchetti che mi servono
#ES in R quando vado a prendere qualcosa di esterno ad R devo sempre metterlo tra virgolette

library("sp")                                                   
#ES per richiamare pacchetti installati in precedenza
#ES require(sp) è un altro comando per far partire le librerie o pacchetti

data(meuse)                                                         
#ES richiamare un gruppo di dati 
head(meuse)                                                       
#ES mostra solo il primo set di righe
names(meuse)                                                       
#ES mostra il nome delle variabili
summary(meuse)                                                  
#ES mostra min, max, media, mediana, 1°/3° quartile
pairs(meuse)                                                      
#ES utilizzato per correlazioni tra variabili all'interno di un dataset
#ES la tilde "~" significa uguale
pairs(~ cadmium + copper + lead , data = meuse)                         
#ES dico ad R quali variabili voglio rappresentare e da quale dataset (data=meuse)
 
# Excercise: cadium copper lead zinc 
                                                                       
 pairs(meuse[,3:6], col="red")                                      
 #col=cambio colore, pch= cambio simboli grafico
 pairs(meuse[,3:6], col="red", pch=19)                              
 #"[ ]" subset dei dati che metterò al suo interno 
 pairs(meuse[,3:6], col="red", pch=19 , cex=3, main="Primo pairs")    
#ES cex= utilizzato per cambiare la dimensione dei punti
#ES main=cambiamento titolo

 #prendiamo 3 pannelli gia fatti dall'esterno

 # panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)  

    usr <- par("usr"); on.exit(par(usr))                                                  
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))

 

    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)


 

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)

    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)


 


panel.histograms <- function(x, ...)

    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col=white", ...)

pairs(meuse [,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing)


pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)


plot(meuse$cadmium , meuse$copper)                                       
#ES $ collega un pezzo di codice ad un'altro es. colonna e dataframe
attach(meuse) 
#ES attach = 
plot(cadmium,copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2))
#ES xlab,ylab, cambiamento etichetta grafico rispettivamente delle ascisse e delle ordinate                                     
#ES cex.lab, utilizzato per esagerare i punti delle ascisse e delle ordinate
#ES dev.off, elimina il grafico corrente

#############################################################################
#############################################################################
CODE 2

# R spatial : Funzioni Spaziali 

install.packages("sp")
#ES install.packages permette di installare il pacchetto R che comprende le funzioni che mi servono
# library() 
#richiamo pacchetto
library(sp)

# dati
data(meuse)

head(meuse)

@ plot cadmium e lead

attach(meuse)

plot(cadmium,lead,col="red",pch=19,cex=2)

# exercise
#ES plot di copper e zinco con simbolo triangolo e colore verde

plot(copper,zinc,pch=17,col="green",cex=2)

# cambiare etichette

plot(copper,zinc,pch=17,col="green",cex=2,xlab="rame",ylab="zinco")

# multiframe o multipanel 

par(mfrow=c(1,2))                                                       
plot(cadmium,lead,col="red",pch=19,cex=2)                             
plot(copper,zinc,pch=17,col="green",cex=2)
#ES par = funzione che ti permette di visualizzare i grafici e confrontarli
#ES mfrow = indica come i grafici devono essere mostrati, n°righe e colonne

# invertiamo i grafici riga colonna in colonna riga 

par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,pch=17,col="green",cex=2)

#multiframe automatico

install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6])    
#ES attenzione alle maiuscole, R è cage sensitive

# spatial 

head(meuse)

coordinates(meuse)=~x+y
#ES coordinates= dico come si chiamano le variabili al software

plot(meuse)

spplot(meuse,"zinc")                                                
#ES spplot= funzione utilizzata per plottare spazialmente i dati

#####################################################################################
#####################################################################################
CODE 3

###### R spatial2

install.packages("sp")

# Libreria sp
library(sp)

# Dati da usare
data(meuse)
head(meuse)

# Coordinate del dataset                    
coordinates(meuse)=~x+y                                          

# Spplot dei dati di zinco
spplot(meuse,"zinc")                                                 
#ES sppplot dei dati con lo zinco 

# Exercise: spplot dei dati di rame
head(meuse)

# un'altra possibilità per vedere i nomi delle colonne è:
names(meuse)
spplot(meuse,"copper")                                                

# Bubble                                                                
bubble(meuse,"zinc")
#ES bubble= plotta i dati come delle bolle di dimensione che varia a seconda della loro concentrazione, utile se si hanno dati relativi ad abbondanze.
# Exercise: bubble del rame, colorato di rosso
bubble(meuse,"copper",col="red")
#ES bubble con pallini di colore rosso.

# foraminiferi (Sofia), carbon capture (Marco)
# array      
foram <- c(10, 20, 35, 55, 67, 80)    
#ES c = utilizziamo c quando dobbiamo elencare una serie di elementi
#ES associo con la freccia a sin il nome della variabile all'ogetto
carbon <- c(5, 15, 30, 70, 85, 99)
plot(foram, carbon, col="green", cex=2, pch=19)

# Dati dall'esterno sul covid-19

# MAC
setwd("/Users/nome_utente/Desktop/lab")                               
#ES setwd= set working direcotry, da inserire ogni volta che vogliamo utilizzare dati esterni
# leggere la tabella    
#read.table = mi permette di leggere la tabella che ho importato.

covid <- read.table("covid_agg.csv",head=TRUE)
# R mi mostrerà i primi dati della tabella 

#  cat     country cases       lat        lon
#1   1 Afghanistan    21  33.83890  66.026530
#2   2     Albania    51  41.14596  20.069178
#3   3     Algeria    49  28.16336   2.632366
#4   4     Andorra    14  42.54858   1.575688
#5   5     Antigua     1  17.28014 -61.791128
#6   6   Argentina    56 -35.37667 -65.167485


#####################################################################################
#####################################################################################
CODE 4

##### Codice per analisi dei Point Pattterns

install.packages("ggplot2")
install.packages("spatstat")
library(ggplot2)
libray("spatstat")


setwd("/Users/edoardosantinelli/Desktop/lab")

covid <- read.table("covid_agg.csv", head=T)                        

covid <- read.table("view.php.csv", head=T)








#ES las= funzione per cambiare le labels (etichette)                                                                            
#ES las=0 è di default e non cambia nulla
#ES las=1 è orizzontale
#ES las=2 è perpendicolare agli assi
#ES las=3 sono tutte verticali

plot(covid$country,covid$cases,las=0)               
# parallel

plot(covid$country,covid$cases,las=1)               
# horizontal labels

plot(covid$country,covid$cases,las=2)              
# perpendicular labels

plot(covid$country,covid$cases,las=3,cex.axis=0.5)  
# vertical labels

#ES cex.axis= dunzione per diminuire o aumentare la dimensione delle labels
                                                                 
# ggplot2
#ES ggplot gia installato quindi lo richiamo con library e carico i dati
library(ggplot2)
data(mpg)
head(mpg)

# Data
#ES aes= esthetics, tipo di geometria visualizzata, punti, linee o poligoni                    
# Tipo di geometria  (tipo di visualizzazione)
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()

attach(covid)
#ES il comando attach permette di richiamare direttamente un elemento del dataframe
# ggplot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# Density 
# Create dataset for spatstat
covid <- ppp (lon,lat, c(-180,180), c(-90,90))
#ES density= utlizzato per plottare un grafico di densità
d <- density(covid)   
#ES ho associato alla densità il nome d

plot(d)
points(covid)

# save Rdata 

setwd("/Users/edoardosantinelli/Desktop/lab")
load("point_pattern_Rdata")
ls()

library(spatstat)
#ES perchè gia installato in precedenza

plot(d)    
#ES colorRampPalette = utilizzato per cambiare i colori delle diverse densità.
# cambiare i colori delle densità (palette), creiamo l'oggetto cl che è una gamma di colori

cl <- colorRampPalette(c('yellow','orange','red')) (100)
#ES (100) = quanti livelli usiamo tra un colore e l'altro della palette.
#ES diamo un nome alla nostra colorRampPalette e poi la plottiamo con col=nome dato
plot(d,col=cl)

# excercise
plot dal verde al blu 
cl <- colorRampPalette(c('green','yellow','orange','red','blue')) (50)
plot(d,col=cl)

points(covids)
#ES aggiungiamo i confini degli stati

coastlines <- readOGR("ne_10m_coastline.shp")                

install.packages("rgdal")
library(rgdal)

coastlines <- readOGR("ne_10m_coastline.shp")

plot(coastlines, add=T)
#ES plotdella mappa di densita con nuovi colori e aggiunta delle coastlines

#### excercise: caricare il workspace point_pattern.Rdata e creare un grafico della mappa di densità

library(spatstat)
library(rgdal) # for the coastlines

 setwd("~/lab/")
load("point_pattern.RData")
ls()                                                       
#ES ls()= lista
 cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)


# interpolazione

head(covid)
marks(covids) <- covid$cases     
# marks = funzione utilizzata per associare un valore a una colonna

s <- Smooth(covids) 
#ES eliminiamo lo smoothing dei valori numerici
plot(s)

# Exercise: plot(s) with points and coastlines
#ES plot finale con colore titolo e coastlines
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

text(covids)                                                     per vedere il numero

####  mappa finale

par(mfrow=c(2,1))

# densità
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)      
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# interpolazione del numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

#ES altro esempio di grafici di densità e interpolazione

## San Marino

setwd se non è stata gia fatta  

load("Tesi.RData")

head(Tesi)
library (spatstat)
attach(Tesi)
summary(Tesi)
# x
# y
# point pattern: x, y c(min,max), c (ymin,ymax)

Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.9,43.95))    lascio un po di margine

dT <- density(Tesippp)

plot(dT)
points(Tesippp, col="green")

######### 

setwd("/Users/edoardosantinelli/Desktop/lab")

load("sanmarino.RData")

ls()

# dt= density map 
# Tesi = dataset originale
# tesippp = point pattern 

library(spatstat)

plot(dT)
points(Tesippp, col="green")

head(Tesi)

marks(Tesippp) <- Tesi$Species_richness

interpol <- Smooth(Tesippp)  

plot(interpol)
points(Tesippp, col="green")
 
 library(rgdal)
 
 sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T)

points(Tesippp, col="green")

# excercise plot multiframe di densità
plot(dT, main ="Density of points")
points(Tesippp, col="green")
plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")

atttach(Tesi)
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.9,43.95))
dT <- density(Tesippp)
marks(Tesippp) <- Tesi$Species_richness
par(mfrow=c(2,1))

par(mfrow=c(2,1))

 # scambio righe e colonne

par(mfrow=c(1,2))
plot(dT, main="Density of points")
points(Tesippp,col="green")
plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")

#####################################################################################
#####################################################################################
CODE 5

# Codice R per analisi di immagini satellitari 

# packages: raster
#ES installo il pacchetto raster o se gia installato lo richiamo con library

install.packages("raster") o library(raster)

setwd("/Users/edoardosantinelli/Desktop/lab")
#ES associamo la funzione brick e il nome del file mediante la freccia a sin per portare all'interno di R un'immagine satellitare
p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)
#ES la rinominiamo aggiungendo l'anno

# day 2

setwd("/Users/edoardosantinelli/Desktop/lab")

load("teleril.RData")
 
ls()
#ES con ls() vedo cosa contiene il mio file 

plot(p224r63_2011)
#ESsi vedono le sigle a quali bande corrispondono secondo la legenda standard di R
# B1: blue 
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

#ES riplotto con la colorRamp Palette scelta
cl <- colorRampPalette(c('black','grey','light grey'))(100) #

plot(p224r63_2011, col=cl)

# scala di grgi 
cllow <- colorRampPalette(c('black','grey','light grey'))(5)

plot(p224r63_2011, col=cllow)

names(p224r63_2011)
# [1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"
#ES vedo i vari nomi 

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
#ES attach(dataframe) non funziona con il pacchetto raster
#ES simbolo che lega la colonna (la banda) al dataset (immagine satellitare) : $
  
# Exercise

clnir <- colorRampPalette(c('red','orange','yellow')
plot(p224r63_2011$B4_sre, col=clnir) 
  
# Multiframe                                                     
#ES mettiamo in risalto il frame che ci niteressa: blu, verde, rosso e infrarosso                          
 
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
#ES chiudo tutti i grafici correnti                           

# natural colours
#ES plottiamo come vedrebbe l'occhio umano 
# 3 componenti: R G B
# 3 bands: R = banda del rosso, G = banda del verde , B = banda del blu                        
 plotRGB(p224r63_2011,r=3,g=2,b=1)
    
                         
 #rgb non viene letto ( maiuscolo)                       
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
#ES uso stretch per aumentare la rislouzione dell'immagine
# nir
# false colours
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
 
 # Salvataggio                                   
#ES pdf() = salvataggio grafico in pdf
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
#ES calcoliamo l indice DVI sottraendo la banda del red alla banda dell'infrarosso per ogni pixel
#DVI1988 = nir1988-red1988                          
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$b3_sre
plot(dvi1988)
                       
  #excersie : calculate dvi for 20111  
#ES stesso plot anche per il 2011

dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
                          clo
cldvi <- colorRampPalette(c('light blue','light green','green'))(100)
plot(dvi2011, col=cldvi)  
                          
# multitemporal analysis 
#ES analisi multitemporale dei cambiamenti del paesaggio)
                          
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
#ES FACT = aumento di valore per diminuire la risoluzione

par(mfrow=c(2,1))
                          
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
                          
 #lower resolution
 #ES aumento il fact, diminuisco la risoluzione                         
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
#ES differenza dei due anni con risoluzione 50

dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
plot(dvi1988lr50) 
# difdvi low resolution                                                        
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)
                          
# multiframe
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)

#####################################################################################
#####################################################################################
CODE 6

# Codice R per analisi di immagini satellitari 
#ES setto la WD, carico il pacchetto raster e se non l'ho gia installato installo e richiamo RStoolbox
# packages: raster

install.packages("raster") o library(raster)

setwd("/Users/edoardosantinelli/Desktop/lab")
#ES importo elementi da IOL medinate la funzione brick e lo metto tra virgolette perchè esterno

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
#plotto con le bande riferite ai colori RGB

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)
#ES suddivisione in 4 classi (nClasses=4) con la funzione unsuperclass 

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

plot(p224r63_2011c$map)
#ES seguo il plot che mi restituisce la mappa con le 4 classi
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100)
#ES associo una colorRampPalette
plot(p224r63_2011c$map, col=clclass)


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
                       
  #excersie : calculate dvi for 2011                        
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
# difdvi low resolution                                            
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)
                          
# multiframe
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)



#####################################################################################
#####################################################################################
CODE 7

# R code analisi multitemporale di variazione della land cover

setwd("/Users/edoardosantinelli/Desktop/lab")
library(raster)
#ES setto la WD richiamo il pacchetto raster e con brick carico le immagini (dataset)
#ES associo le immagini a due nomi
defor1 <- brick("defor1.png")
defor2 <- brick("defor2.png")

# names: defor1_.1 = NIR ,  defof1_.2= red , defor1_.3= green
#ES sono i nomi delle bande, tramite plotRGB  associo le tre bande di riferimento al colore rgb che voglio: mettiamo la nir con il red

plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")

# Excercise plot seconda data

plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

#ES ora classifichiamo in 2 classi per dividere la foresta dal resto tramite unsuperclass e per farlo carico la library giusta

par(mfrow=c(2,1))
plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

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
#ES freq = mi indica la frequenza dei vari dati inseriti nel grafico, da qui possiamo calcolare le percentuali.
# aree aperte =35213
# foresta = 306079

totd1 <- 306079 + 35213
#ES totale celle, ora calcolo le percentuali
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
#ES creo un output cioè un dataframe con il tipo di landcover, la percentuale prima e dopo il disboscamento
cover <- c("Agriculture","Forest")
before <- c(10.4, 89.6)
after <- c(47.58, 52.42)

output <- data.frame(cover, before, after)
library(ggplot2)

###### day 2
set
library(raster)
load("defor.RData")

#ES riplotto le immagini dei due anni della scorsa lezione
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

library(ggplot2)
#ES plotto con percentuali a confronto prima della deforestazione con ggplot

ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

# excercise plot the histograms of the land cover after deforastation
#ES e dopo la deforestazione
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
#ES grid.arrange = plotta i due grafici nella stessa schermata, con nrow decido se sulla stessa riga

library(ggplot2)
cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)

 

output <- data.frame(cover,before,after)
output

library(gridExtra)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

grid.arrange(grafico1, grafico2, nrow = 1)
 
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

grid.arrange(grafico1, grafico2, nrow = 1)

#####################################################################################
#####################################################################################
CODE 8

###### R code for analysin NO2 data from ESA, January to March 2020
set
#ES setto la WD, carico il paccchetto raster, rinomino gli elementi che importo

library(raster)

EN01 <- raster("EN_0001.png")
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

#ES plotto cambiano i colori
cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN01, col=cl)
plot(EN13, col=cl)

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

#ES differenza tra le due immagini
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)
#ES plotto tutte le immagini, o uso par ma piu lento soprattutto se ho molti elementi o creo uno stack
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

#### day 2

setwd("/Users/edoardosantinelli/Desktop/lab")

load("EN.RData")

setwd("/Users/edoardosantinelli/Desktop/lab/esa_no2")
#ES utilizzo il metodo tramite lo stack
EN <- stack(EN01,EN02,EN03,EN04,EN05,EN06,EN07,EN08,EN09,EN10,EN11,EN12,EN13) 
#ES stack mi ha imballato le 13 immagini in una sola

rlist <- list.files(pattern=".png")                       
#ES creo una cartella

lapply(rlist, raster)        
#ES lapply = faccio una lista dei files

listafinale <- lapply(rlist, raster)                       
#ES importo la lista

EN <- stack(listafinale)                                   
#ES faccio uno stack delle bande importate in una singola immagine su R

cl <- colorRampPalette(c('red','orange','yellow'))(100)
#ES aggiungo i coloti e plotto 

plot(EN, col=cl)

#### day 3
set
library (raster)
setwd("/Users/edoardosantinelli/Desktop/lab/esa_no2")

rlist <- list.files(pattern=".png")

listafinale <- lapply(rlist, raster)
EN <- stack(listafinale)
#ES voglio fare la differenza tra la prima e l'ultima
difEN <- EN$EN_0013 - EN$EN_0001

cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)

boxplot(EN)
#ES grafico dove si vede in un certo senso la distribuzione 
boxplot(EN, horizontal=T) 
#ES lo metto orrizontale
boxplot(EN, horizontal=T,outline=F) 
#ES piu visibile il calo dei valori massimi 
boxplot(EN, horizontal=T,outline=F,axes=T)
#ES aggiungo anche gli assi

#####################################################################################
#####################################################################################
CODE 9

setwd("/Users/edoardosantinelli/Desktop/lab")
#ES richiamo le librerie utili
install.packages("ncdf4")
library(ncdf4)
library(raster)
#ES porto i dati(con la funzione raster) da lab che ho scaricato da copernicus e messo nella cartella e rinomino
snowmay <- raster("snow")

 cl <- colorRampPalette(c("darkblue","blue","light blue"))(100))
 
plot(snowmay, col=cl) 

#### import snow1 data

setwd("/Users/edoardosantinelli/Desktop/lab/snow1")

rlist <- list.files(pattern=".tif")
lapply(rlist, raster)                                               
#ES importazione dei singoli file tramite la funzione r applicata a tutti i file contemporaneamete mediante la funzione lapply
list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp,col=cl)

par(mfrow=c(1,2))
#ES confronto tra anni 2000 w 2020

plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)

par(mfrow=c(1,2))

plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))
#ES uniamo il limite tra le due immagini tramite la funzione zlim=c con i limiti tra parentesi

#differenza nella neve difsnow

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r

snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff)

#### prediction 
#ES scaricare il file nella cartella snow1 da IOL

source("prediction.r")

plot(prediction.snow.2025.norm, col=cl)

prediction.snow.2025.norm <- raster(view.php.tif)
plot(predicted.snow.2025.norm, col=cl)

#####################################################################################
#####################################################################################
CODE 10

setwd("/Users/edoardosantinelli/Desktop/lab")

library(raster)
install.packages("igraph")
#ES install.packages("igraph"
library(igraph)
library(ggplot)
#ES mi serve ggplot per ilgrafico finale

#ES rinomino i dati
d1c <- raster("d1c.tif)
d2c <- raster("d2c.tif")
par(mfrow=c(1,2))
cl <- colorRampPalette(c("green","black"))(100)
#ES ho messo nero la non foreste e verdela foresta
plot(d1c,col=cl)
plot(d2c,col=cl)
#ES inverto i colori dell'array
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
 
                                                        
#ES funzione cbind: elimina alcuni valori es, agricoltura , si metonono come valori nulla ed estraiamo la foresta
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
#excercise plottare entrambe le mappe una accanto all'altra
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
#ES il numero di patches è dato dal max di una e il max dell'altra
output <- data.frame(time,npatches)                            
#ES ho correlato le patch pre e post deforestazione
attach(output)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

#####################################################################################
#####################################################################################
CODE 11
R code crop

setwd("/Users/edoardosantinelli/Desktop/lab/snow1")
library(raster)

##excercise : upload the whole set snow (raster o lapply)


rlist <- list.files(pattern=".tif")

lapply(rlist, raster)

listaimmagini <- lapply(rlist, raster)

SN <- stack(listaimmagini)                                  

cl <- colorRampPalette(c('blue','light blue','white'))(100)

plot(SN, col=cl)

# zoom 

plot(SN$snow2010r, col=cl)
#ES così con il $ lego la mia immagine al dataset che voglio plottare

ext <- c( -180, 180, -90, 90)                               
#ES estensione totale immagine

extension <- c(6, 18, 40, 50)                                 
#ES estensione a nostra discrezione zoomando una zona

zoom(SN$snow2010r, ext=extension)
#ES immettiamo le nostre coordinate

extension <- c(6, 20, 35, 50)
zoom(SN$snow2010r, ext=extension)

zoom(SN$snow2010r, ext=drawExtent())                         
#ES R aspetta un rettangolo disegnato da noi per zoommare
 
#crop                                                         
#ES senza ext= non è solo uno zoom ma un immagine nuova ritagliata daalla precedente

extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(SN$snow2010r, extension)
plot(snow2010r.italy, col=cl)
#ES uso crop e gli assoccio un nome, plotto


#excercise crop the italy extent on the whole stack of snow layers

extension <- c(6, 20, 35, 50)
snowitaly <- crop(SN, extension)
plot(snowitaly, col=cl)

plot(snowitaly, col=cl, zlim=(c(20, 200))

#ES facciamo un boxplot 
boxplot(snowitaly, horizontal= T, vertical= F)


 


1. R_code_first.r   
2. R_code_spatial.r   
3. R_code_spatial2.r
4. R_code_point_pattern   
5. R_code_teleril.r   
6. R_code_landcover.r   
7. R_code_multitemp.r   
8. R_code_multitemp_NO2.r   
9. R_code_snow.r   
10. R_code_patches.r 
11. R_code_crop.r

COPERNICUS DATA:
https://land.copernicus.vgt.vito.be/PDF/portal/Application.html



