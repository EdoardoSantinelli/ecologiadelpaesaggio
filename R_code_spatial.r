# R spatial : Funzioni Spaziali 

install.packages("sp")

# library() = richiamo pacchetto
library(sp)

# dati
data(meuse)

head(meuse)

@ plot cadmium e lead

# alleghiamo il database o dataframe 
attach(meuse)

plot(cadmium,lead,col="red",pch=19,cex=2)

# exercise
plot di copper e zinco con simbolo triangolo e colore verde

plot(copper,zinc,pch=17,col="green",cex=2)

# cambiare etichette

plot(copper,zinc,pch=17,col="green",cex=2,xlab="rame",ylab="zinco")

# multiframe o multipanel 

par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,pch=17,col="green",cex=2)

# invertiamo i grafici riga colonna in colonna riga

par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,pch=17,col="green",cex=2)

#multiframe automatico

install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6])

# spatial 

head(meuse)

coordinates(meuse)=~x+y

plot(meuse)
#funzione spplot per plottare i dati spazialmente

spplot(meuse,"zinc")

