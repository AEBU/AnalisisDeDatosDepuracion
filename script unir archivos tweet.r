
#script para unir los archivos de tweets en uno solo
#cargar los tweets para crear un archivo de base de datos de la metro
temp <- read.csv("tweets metro/tweets-metro-1.csv", header = TRUE, sep=",")
baseMetro <- temp
#cargamos los archivos de uno en uno para crear un solo archivo
for(i in 2:13) {
  nombreArchivo <- paste("tweets metro/tweets-metro-",i,".csv", sep = "")
  print(nombreArchivo)
  temp <- read.csv(nombreArchivo, header = TRUE, sep=",")
  baseMetro <- rbind(baseMetro, temp[1:NROW(temp),]) #aumento todas las filas de temp a la baseMetro
}
View(baseMetro)

#########################################################################
#cargar los tweets para crear un archivo de base de datos de la otra
temp <- read.csv("tweets laotra/tweets-laotra-1.csv", header = TRUE, sep=",")
baseLaOtra <- temp
#cargamos los archivos de uno en uno para crear un solo archivo
for(i in 2:13) {
  nombreArchivo <- paste("tweets laotra/tweets-laotra-",i,".csv", sep = "")
  print(nombreArchivo)
  temp <- read.csv(nombreArchivo, header = TRUE, sep=",")
  baseLaOtra <- rbind(baseLaOtra, temp[1:NROW(temp),]) #aumento todas las filas de temp a la baseLaOtra
}
View(baseLaOtra)
#########################################################################
