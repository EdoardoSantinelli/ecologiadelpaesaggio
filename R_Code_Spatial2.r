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

# Exercise: spplot dei dati di rame
head(meuse)

# un'altra possibilità per vedere i nomi delle colonne è:
names(meuse)
spplot(meuse,"copper")

# Bubble
bubble(meuse,"zinc")

# Exercise: bubble del rame, colorato di rosso
bubble(meuse,"copper",col="red")

# foraminiferi (Sofia), carbon capture (Marco)
# array
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)
plot(foram, carbon, col="green", cex=2, pch=19)

# Dati dall'esterno sul covid-19

# MAC
setwd("/Users/nome_utente/Desktop/lab")

# leggere la tabella
covid <- read.table("covid_agg.csv",head=TRUE)
