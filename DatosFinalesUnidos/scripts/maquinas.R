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

write.csv(x = TweetsFinal,
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/TweetsFinal.csv", row.names = FALSE)

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

TweetsFinal<-TweetsFinal[-3454,]

#t <- TweetsFinal[,c(1,28)]
#write.csv(x = t,
#          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/paraAlexis.csv", row.names = FALSE)
###########################################################################################
###########################################################################################

##FACTOR DE PONDERACION

#total de tweets por provincia
AZUAY <- 0
BOLIVAR <- 0
CAÑAR <- 0
CARCHI <- 0
COTOPAXI <- 0
CHIMBORAZO <- 0
EL_ORO <- 0
ESMERALDAS <- 0
GUAYAS <- 0
IMBABURA <- 0
LOJA <- 0
LOS_RIOS <- 0
MANABI <- 0
MORONA_SANTIAGO <- 0
NAPO <- 0
PASTAZA <- 0
PICHINCHA <- 0
TUNGURAHUA <- 0
ZAMORA_CHINCHIPE <- 0
GALAPAGOS <- 0
SUCUMBIOS <- 0
ORELLANA <- 0
SANTO_DOMINGO_TSACHILAS <- 0
SANTA_ELENA <- 0
OTROS <- 0
for (i in 1:NROW(TweetsFinal)) {
  if (TweetsFinal[i, 26] == "1") {
    AZUAY <- AZUAY + 1
  } else if (TweetsFinal[i, 26] == "2") {
    BOLIVAR <- BOLIVAR + 1
  } else if (TweetsFinal[i, 26] == "3") {
    CAÑAR <- CAÑAR + 1
  } else if (TweetsFinal[i, 26] == "4") {
    CARCHI <- CARCHI + 1
  } else if (TweetsFinal[i, 26] == "5") {
    CHIMBORAZO <- CHIMBORAZO + 1
  } else if (TweetsFinal[i, 26] == "6") {
    COTOPAXI <- COTOPAXI + 1
  } else if (TweetsFinal[i, 26] == "7") {
    EL_ORO <- EL_ORO + 1
  } else if (TweetsFinal[i, 26] == "8") {
    ESMERALDAS <- ESMERALDAS + 1
  } else if (TweetsFinal[i, 26] == "9") {
    GUAYAS <- GUAYAS + 1
  } else if (TweetsFinal[i, 26] == "10") {
    IMBABURA <- IMBABURA + 1
  } else if (TweetsFinal[i, 26] == "11") {
    LOJA <- LOJA + 1
  } else if (TweetsFinal[i, 26] == "12") {
    LOS_RIOS <- LOS_RIOS + 1
  } else if (TweetsFinal[i, 26] == "13") {
    MANABI <- MANABI + 1
  } else if (TweetsFinal[i, 26] == "14") {
    MORONA_SANTIAGO <- MORONA_SANTIAGO + 1
  } else if (TweetsFinal[i, 26] == "15") {
    NAPO <- NAPO + 1
  } else if (TweetsFinal[i, 26] == "16") {
    PASTAZA <- PASTAZA + 1
  } else if (TweetsFinal[i, 26] == "17") {
    PICHINCHA <- PICHINCHA + 1
  } else if (TweetsFinal[i, 26] == "18") {
    TUNGURAHUA <- TUNGURAHUA + 1
  } else if (TweetsFinal[i, 26] == "19") {
    ZAMORA_CHINCHIPE <- ZAMORA_CHINCHIPE + 1
  } else if (TweetsFinal[i, 26] == "20") {
    GALAPAGOS <- GALAPAGOS + 1
  } else if (TweetsFinal[i, 26] == "21") {
    SUCUMBIOS <- SUCUMBIOS + 1
  } else if (TweetsFinal[i, 26] == "22") {
    ORELLANA <- ORELLANA + 1
  } else if (TweetsFinal[i, 26] == "23") {
    SANTO_DOMINGO_TSACHILAS <- SANTO_DOMINGO_TSACHILAS + 1
  } else if (TweetsFinal[i, 26] == "24") {
    SANTA_ELENA <- SANTA_ELENA + 1
  } else if (TweetsFinal[i, 26] == "25") {
    OTROS <- OTROS + 1
  }
}

library(readxl)

codLocalidad <- as.character(c(1:25))
nombresLocalidad <-
  c(
    "AZUAY",
    "BOLIVAR",
    "CAÑAR",
    "CARCHI",
    "COTOPAXI",
    "CHIMBORAZO",
    "EL_ORO",
    "ESMERALDAS",
    "GUAYAS",
    "IMBABURA",
    "LOJA",
    "LOS_RIOS",
    "MANABI",
    "MORONA_SANTIAGO",
    "NAPO",
    "PASTAZA",
    "PICHINCHA",
    "TUNGURAHUA",
    "ZAMORA_CHINCHIPE",
    "GALAPAGOS",
    "SUCUMBIOS",
    "ORELLANA",
    "SANTO_DOMINGO_TSACHILAS",
    "SANTA_ELENA",
    "OTROS"
  )

#ingresamos los datos censales
datosCensales <-
  read_excel(
    "C:/Users/David/Documents/AnalisisDatos/Maquinas/Datos_David/datosCensales.xlsx"
  )
#seleccionamos la columna de porcentaje
porcenCenso <- datosCensales$porcentaje
#inicializamos el porcentaje de tweets
porcenTweets <- c(rep(0, 25))
#inicializamos el porcentaje de otros
porcenTweetsSinOtros <- c(rep(0, 25))
#inicializamos el factor de ponderacion
factorponderacion <- c(rep(0, 25))

#creamos un dataframe
porcentajeT <-
  data.frame(
    codLocalidad,
    nombresLocalidad,
    porcenCenso,
    porcenTweets,
    porcenTweetsSinOtros,
    factorponderacion
  )

#porcentaje por provincia
porcentajeT[1, 4] <- (AZUAY * 100) / NROW(TweetsFinal)
porcentajeT[2, 4] <- (BOLIVAR * 100) / NROW(TweetsFinal)
porcentajeT[3, 4] <- (CAÑAR * 100) / NROW(TweetsFinal)
porcentajeT[4, 4] <- (CARCHI * 100) / NROW(TweetsFinal)
porcentajeT[5, 4] <- (CHIMBORAZO * 100) / NROW(TweetsFinal)
porcentajeT[6, 4] <- (COTOPAXI * 100) / NROW(TweetsFinal)
porcentajeT[7, 4] <- (EL_ORO * 100) / NROW(TweetsFinal)
porcentajeT[8, 4] <- (ESMERALDAS * 100) / NROW(TweetsFinal)
porcentajeT[9, 4] <- (GUAYAS * 100) / NROW(TweetsFinal)
porcentajeT[10, 4] <- (IMBABURA * 100) / NROW(TweetsFinal)
porcentajeT[11, 4] <- (LOJA * 100) / NROW(TweetsFinal)
porcentajeT[12, 4] <- (LOS_RIOS * 100) / NROW(TweetsFinal)
porcentajeT[13, 4] <- (MANABI * 100) / NROW(TweetsFinal)
porcentajeT[14, 4] <- (MORONA_SANTIAGO * 100) / NROW(TweetsFinal)
porcentajeT[15, 4] <- (NAPO * 100) / NROW(TweetsFinal)
porcentajeT[16, 4] <- (PASTAZA * 100) / NROW(TweetsFinal)
porcentajeT[17, 4] <- (PICHINCHA * 100) / NROW(TweetsFinal)
porcentajeT[18, 4] <- (TUNGURAHUA * 100) / NROW(TweetsFinal)
porcentajeT[19, 4] <- (ZAMORA_CHINCHIPE * 100) / NROW(TweetsFinal)
porcentajeT[20, 4] <- (GALAPAGOS * 100) / NROW(TweetsFinal)
porcentajeT[21, 4] <- (SUCUMBIOS * 100) / NROW(TweetsFinal)
porcentajeT[22, 4] <- (ORELLANA * 100) / NROW(TweetsFinal)
porcentajeT[23, 4] <- (SANTO_DOMINGO_TSACHILAS * 100) / NROW(TweetsFinal)
porcentajeT[24, 4] <- (SANTA_ELENA * 100) / NROW(TweetsFinal)
porcentajeT[25, 4] <- (OTROS * 100) / NROW(TweetsFinal)


#calculo del porcentaje de tweets por localidad sin otros y calculo del factor de ponderacion
for (i in 1:NROW(porcentajeT)) {
  porcentajeT[i, 5] <-
    (porcentajeT[i, 4]) / (1 - (porcentajeT[25, 4] / 100))
  if (porcentajeT[i, 2] == "OTROS") {
    porcentajeT[i, 6] <- 1
  } else {
    fp <- porcentajeT[i, 3] / porcentajeT[i, 5]
    if (is.infinite(fp) == FALSE) {
      porcentajeT[i, 6] <- fp
    }
  }
}

#incluir factor de ponderacion en el dataframe
for (i in 1:NROW(TweetsFinal)) {
  codLocT <- TweetsFinal[i, 26]
  for (j in 1:NROW(porcentajeT)) {
    codLocP <- porcentajeT[j, 1]
    factPon <- porcentajeT[j, 6]
    if (codLocT == codLocP) {
      TweetsFinal[i, 27] <- factPon
    }
  }
}


write.csv(x = TweetsFinal, file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/TweetsFinalCompleto.csv", row.names = FALSE)
