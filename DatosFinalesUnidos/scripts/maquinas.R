tweets <- as.data.frame(TweetsFinalUnido)

#separar los datos de muestra
#indices de posicion que son de muestra
muestraaleatoria <- sample(nrow(tweets), 367)
write.table(
  x = muestraaleatoria,
  file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/indices.csv",
  col.names = TRUE,
  sep = ";"
)


muestraentrenamiento <- tweets[muestraaleatoria,]
write.csv(x = muestraentrenamiento,
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/TweetsEntrenamiento.csv",
          row.names = FALSE)
muestraprueba <- tweets[-muestraaleatoria,]
write.csv(x = muestraprueba,
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/TweetsPrueba.csv",
          row.names = FALSE)

write.csv(x = muestraentrenamiento[1:92,28:30],
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/alexis.csv", row.names = FALSE)
write.csv(x = muestraentrenamiento[93:184,28:30],
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/bryan.csv", row.names = FALSE)
write.csv(x = muestraentrenamiento[185:275,28:30],
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/alejo.csv", row.names = FALSE)
write.csv(x = muestraentrenamiento[276:367,28:30],
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/david.csv", row.names = FALSE)





################################################################################################
library(readr)

#Ingreso de los tweets de entrenamiento
TweetsEntrenamiento <-
  read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/TweetsEntrenamiento.csv")

TweetsPrueba <-
  read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/TweetsPrueba.csv")

#TweetsEntrenamiento$depurado <- iconv(TweetsEntrenamiento$depurado,"latin1", "UTF-8")
#TweetsPrueba$depurado <- iconv(TweetsPrueba$depurado,"latin1", "UTF-8")

#############################################################################################

#MAQUINAS DE SOPORTE VECTORIAL

TweetsEntrenamiento$polaridadSVM <- c(rep(NA, NROW(TweetsEntrenamiento)))

TweetsPrueba$polaridadSVM <- c(rep(NA, NROW(TweetsPrueba)))

alexis <-
  read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/leer/alexis.csv")
bryan <-
  read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/leer/bryan.csv")
alejo <-
  read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/leer/alejo.csv")
david <-
  read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/leer/david.csv")

alexis$id <- c(rep(NA,NROW(alexis)))
bryan$id <- c(rep(NA,NROW(bryan)))
alejo$id <- c(rep(NA,NROW(alejo)))
david$id <- c(rep(NA,NROW(david)))

##Agregar id en alexis
for (i in 1:92) {
  alexis[i,4] <- TweetsEntrenamiento[i,8]
}
for (i in 1:92) {
  bryan[i,4] <- TweetsEntrenamiento[i+92,8]
}
for (i in 1:91) {
  alejo[i,4] <- TweetsEntrenamiento[i+184,8]
}
for (i in 1:92) {
  david[i,4] <- TweetsEntrenamiento[i+275,8]
}

TotalTweetsLeidos = rbind(alexis, bryan, alejo, david)

for (i in 1:367) {
  TweetsEntrenamiento[i,28]<-TotalTweetsLeidos[i,1]
}

TweetsFinal <- as.data.frame(rbind(TweetsEntrenamiento, TweetsPrueba))

library(RTextTools)

matriz <- create_matrix(TweetsFinal$text,
                                     language = "spanish",
                                     removeNumbers = TRUE,
                                     removePunctuation = TRUE,
                                     removeStopwords = TRUE)


contenedor <- create_container(matriz, TweetsFinal$polaridadSVM,
                                            trainSize = 1:367,
                                            testSize = 368:8066,
                                            virgin = FALSE)

model <- train_model(contenedor, "SVM")

clasificacion <- classify_model(contenedor, model)

TweetsFinal$polaridadSVM[368:8066] <- as.numeric(clasificacion$SVM_LABEL)-1

###########################################################################################
###TABLA DE CONFUSION

v <- sample(367, 293)
x<-c(1:367)
y<-c(1:367)
z<-data.frame(x,y)
i <- z[v,]
j <- z[-v,]

i1 <- i$x
j1 <- j$x

contenedor <- create_container(matriz, TweetsFinal$polaridadSVM,
                               trainSize = i1,
                               testSize = j1,
                               virgin = FALSE)

model <- train_model(contenedor, "SVM", kernel = "polynomial")

clasificacion <- classify_model(contenedor, model)

muestraVerificacion <- c(rep(0,73))

muestraVerificacion <- as.numeric(clasificacion$SVM_LABEL)-1


###########################################################################################
###DICCIONARIOS

diccionarioPositivo <-
  c(
    "positiva",
    "solicito",
    "quisiera",
    "necesito",
    "mejor",
    "complacer",
    "divina",
    "divino",
    "hermosa",
    "hermoso",
    "ponga",
    "pongan",
    "pongamen",
    "complazcamen",
    "uno",
    "mucho",
    "top",
    "deseo",
    "exito",
    "éxito",
    "alegría",
    "entusiasmo",
    "buenos",
    "momentos",
    "confianza",
    "excelente",
    "escuchar",
    "sonar",
    "poner",
    "tocar",
    "ponen",
    "sonar",
    "canción",
    "cancion",
    "complacen",
    "favorito",
    "quisiera",
    "quiero","puede","pueden","programen","podrian","podrían","queremos"
  )

TweetsFinal$polaridadDiccionarios <- c(rep(0, NROW(TweetsFinal)))

for (i in 1:NROW(TweetsFinal)) {
  texto <- TweetsFinal[i,17]
  v <- strsplit(texto," ")
  palabras <- data.frame(v)
  numPositivas <- 0
  numNegativas <- 0
  for (j in 1:NROW(palabras)) {
    for (k in 1:NROW(diccionarioPositivo)) {
      if (palabras[j,1]==diccionarioPositivo[k]) {
        numPositivas<-numPositivas+1
      }
    }
  }
  if (numPositivas >= 2) {
    TweetsFinal[i,29]<- 1 
  }
}


t <- TweetsFinal[,c(1,28)]
write.csv(x = t,
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/paraAlexis.csv", row.names = FALSE)
###########################################################################################