#Añadimos las librerias necesarias
library(twitteR)
library(httr)
require('ROAuth')
require('RCurl')
library(base64enc)

#Almacenamos en variables las credenciales para la descarga de los tweets
consumer_key <-"RFHeP55qj0ejWv7YiSceWTphX"
consumer_secret<-"YOOUNzPvlJCAIHo23AWGU6hRm9VYxP6AY60H0n3u3dBM44aHZS"
access_token<-"366852754-QE8L7hZs1J0WbtDTImo4P3qxzabRZ6OEY0E9ckOs"
access_secret<-"zUZ3rnyPHCLQxso43A8PvXY1a3jWp81yS8Z2Rr0w96YzB"

#Autorizamos la descarga de los tweets
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Guardamos la fecha en la que descargamos los tweets
fecha <- Sys.Date()

#Seleccionamos la cuenta de la emisora a descargar los tweets
emisora <- "labrujaecuador"

#Descargamos los tweets
listaTweets <- searchTwitter(searchString = emisora, n=3700)
tweets <- twListToDF(listaTweets)
nombreArchivo <- paste("C:/Users/David/Documents/AnalisisDatos/Maquinas/scripts/Nuevos Scripts/.xlsx",emisora,"_",fecha,".csv", sep="")
write.csv(x=tweets, file=nombreArchivo, col.names = TRUE, row.names = FALSE)

#Como los tweets no fueron descargados con todas las variables necesarias, debemos
#de añadir dichas columnas, pero primero, realizamos la depuracion de los tweets
for (i in 1:NROW(tweets)) {
  texto <- tweets[i, 1]
  #eliminar simbolos de retweets
  sinRT <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", texto)
  #eliminar simbolo @ y #
  sinCuentas <- gsub("@|#", "", sinRT)
  #eliminar signos de puntuacion
  sinSimbolos <- gsub("[[:punct:]].", "", sinCuentas)
  #eliminar numeros
  sinNumeros <- gsub("[[:digit:]]", "", sinSimbolos)
  #eliminar enlaces
  sinEnlaces <- gsub("http\\w+", "", sinNumeros)
  tweets$depurado[i] <- sinEnlaces
  tweets$sinEspacios[i] <-
    gsub("[[:space:]]|", "", tweets$depurado[i])
}

#ordenamos al dataframe por el texto depurado sin espacios
tweets <- tweets[order(tweets$sinEspacios), ]

#eliminamos tweets que quedaron vacios
tweets <- tweets[!(tweets$sinEspacios == ""), ]

#eliminamos repetidos
tweets <- tweets[!duplicated(tweets$sinEspacios), ]

#añadimos las nuevas columas, las más importantes son: seguidores, seguidos y localidad
tweets$statusCount <- c(rep(NA, NROW(tweets)))
tweets$followers <- c(rep(NA, NROW(tweets)))
tweets$favorites <- c(rep(NA, NROW(tweets)))
tweets$friends <- c(rep(NA, NROW(tweets)))
tweets$location <- c(rep(NA, NROW(tweets)))

for (i in 1:NROW(tweets)) {
  usuario <- as.character(tweets[i, 11])
  tryCatch({
    informacionUsuario <- lookupUsers(usuario)
    usuarioDF <- twListToDF(informacionUsuario)
    tweets[i, 21] <- statusesCount(informacionUsuario)
    tweets[i, 22] <- followersCount(informacionUsuario)
    tweets[i, 23] <- favoritesCount(informacionUsuario)
    tweets[i, 24] <- friendsCount(informacionUsuario)
    tweets[i, 25] <- location(informacionUsuario)
  })
}


#RELEVANCIA

#calculamos la metrica de seguidores/seguidos
tweets$relevancia <- c(rep(0, NROW(tweets)))
for(i in 1:NROW(tweets)) {
  tweets[i,26]<-tweets[i,22]/tweets[i,24]
}

#CLASIFICACION DE LOS TWEETS

#Procemos a clasificar a los tweets, para ello realizamos dos tecnicas:
#Aprendizaje Supervisado y Aprendizaje No Supervisado

#APRENDIZAJE SUPERVISADO

#separamos los datos de entrenamiento, para ello hemos calculado primero el tamaño de muestra
#y aleatoriamente escogemos los tweets de entrenamiento
muestraaleatoria <- sample(nrow(tweets), 367)

TweetsEntrenamiento <- tweets[muestraaleatoria,]
TweetsPrueba <- tweets[-muestraaleatoria,]

#añadimos las columnas donde se guardará la polaridad
TweetsEntrenamiento$polaridadSVM <- c(rep(NA, NROW(TweetsEntrenamiento)))
TweetsPrueba$polaridadSVM <- c(rep(NA, NROW(TweetsPrueba)))

#Leemos los tweets de entrenamiento y añadimos su polaridad
fix(TweetsEntrenamiento)

#Unimos los tweets de entrenamiento y los de prueba para realizar el respectivo analisis
TweetsFinal <- as.data.frame(rbind(TweetsEntrenamiento, TweetsPrueba))

# Empleamos la libreria RTextTools, que sirve para la clasificación automática
# de textos usando Algoritmos de Aprendizaje Supervisado 
library(RTextTools)


# Usamos la funcion create_matrix del paquete RTextTools, la cual nos servirá para 
# los pasos siguientes.
# En esta función solo vamos a seleccionar la columna de los tweets, el lenguaje en
# en el que está escrito los tweets, y pedimos que elimine números, elimine signos
# de puntuación y elimine stopwords
matriz <- create_matrix(TweetsFinal$text,
                        language = "spanish",
                        removeNumbers = TRUE,
                        removePunctuation = TRUE,
                        removeStopwords = TRUE)

# Usamos la función create_container para separar de la matriz anterior, los tweets 
# de entrenamiento y los tweets de prueba y escojemos la variable dependiente 
# que en este caso es la polaridad
contenedor <- create_container(matriz, TweetsFinal$polaridadSVM,
                               trainSize = 1:367,
                               testSize = 368:8066,
                               virgin = FALSE)

# Con el comando train_model vamos a crear el modelo SVM
model <- train_model(contenedor, "SVM")

# Con el comando classify_model clasificaremos los tweets de prueba
clasificacion <- classify_model(contenedor, model)

# Añadimos la clasificación de los tweets de prueba en el dataframe
TweetsFinal$polaridadSVM[368:8066] <- as.numeric(clasificacion$SVM_LABEL)-1


#APRENDIZAJE NO SUPERVISADO

#Agregamos el diccionario con las palabras positivas, en total tenemos 44 palabras
diccionarioPositivo <-
  c("positiva","solicito","quisiera","necesito","mejor","complacer","divina","divino","hermosa",
    "hermoso","ponga","pongan","pongamen","complazcamen","uno","mucho","top","deseo","exito",
    "éxito","alegría","entusiasmo","buenos","momentos","confianza","excelente","escuchar",
    "sonar","poner","tocar","ponen","sonar","canción","cancion","complacen","favorito",
    "quisiera","quiero","puede","pueden","programen","podrian","podrían","queremos")

#Creamos la columna de polaridad con diccionarios
TweetsFinal$polaridadDiccionarios <- c(rep(0, NROW(TweetsFinal)))

#Recorremos todos los tweets, por cada tweet separamos en palabras al texto y las comparamos
#con las palabras del diccionario.
for (i in 1:NROW(TweetsFinal)) {
  texto <- TweetsFinal[i,17]
  v <- strsplit(texto," ")
  palabras <- data.frame(v)
  numPositivas <- 0
  numNegativas <- 0
  #Contamos el número de palabras postivias en cada texto del tweet
  for (j in 1:NROW(palabras)) {
    for (k in 1:NROW(diccionarioPositivo)) {
      if (palabras[j,1]==diccionarioPositivo[k]) {
        numPositivas<-numPositivas+1
      }
    }
  }
  #Realizamos la métrica, que seria que si encuentra mas de dos palabras positivas en el tweet,
  #ese tweet es postivo
  if (numPositivas >= 2) {
    TweetsFinal[i,29]<- 1 
  }
}

#FACTOR DE PONDERACION

#Procedos a encontrar el codigo de la localidad de cada tweet
tweets$localidadMayusculas <- c(rep("", NROW(tweets)))
tweets$codProv <- c(rep("", NROW(tweets)))

#convertimos la localidad en mayusculas
for (i in 1:NROW(tweets)) {
  tweets[i, 27] <- toupper(tweets[i, 25])
}

#Verificar la localidad con expresiones regulares y le asignamos su respectivo
#codigo de provincia, para otros hemos designado el codigo 25
for (i in 1:NROW(tweets)) {
  if (!is.na(tweets[i, 27])) {
    resultado <-
      grepl(
        "AZUAY|SIGSIG|SEVILLA DE ORO|SANTA ISABEL|SAN FERNANDO|PUCARA|PAUTE|OÑA|NABÓN|GUALACEO|GUACHAPALA|GIRÓN|EL PAN|CUENCA|CHORDELEG|CAMILO PONCE ENRÍQUEZ",
        tweets[i, 27]
      )
    if (resultado == TRUE) {
      tweets[i, 28] <- "1"
    } else {
      resultado <-
        grepl(
          "BOLIVAR|SAN MIGUEL|LAS NAVES|GUARANDA|ECHEANDÍA|CHIMBO|CHILLANES|CALUMA",
          tweets[i, 27]
        )
      if (resultado == TRUE) {
        tweets[i, 28] <- "2"
      } else {
        resultado <-
          grepl("CAÑAR|SUSCAL|LA TRONCAL|EL TAMBO|DÉLEG|CAÑAR|BIBLIÁN|AZOGUES",
                tweets[i, 27])
        if (resultado == TRUE) {
          tweets[i, 28] <- "3"
        } else {
          resultado <-
            grepl("CARCHI|TULCÁN|SAN PEDRO DE HUACA|MONTÚFAR|MIRA|ESPEJO|BOLÍVAR",
                  tweets[i, 27])
          if (resultado == TRUE) {
            tweets[i, 28] <- "4"
          } else {
            resultado <-
              grepl(
                "COTOPAXI|SIGCHOS|SAQUISILÍ|SALCEDO|PUJILI|PANGUA|LATACUNGA|LA MANÁ",
                tweets[i, 27]
              )
            if (resultado == TRUE) {
              tweets[i, 28] <- "5"
            } else {
              resultado <-
                grepl(
                  "CHIMBORAZO|RIOBAMBA|PENIPE|PALLATANGA|GUANO|GUAMOTE|CUMANDÁ|COLTA|CHUNCHI|CHAMBO|ALAUSI",
                  tweets[i, 27]
                )
              if (resultado == TRUE) {
                tweets[i, 28] <- "6"
              } else {
                resultado <-
                  grepl(
                    "EL ORO|ZARUMA|SANTA ROSA|PORTOVELO|PIÑAS|PASAJE|MARCABELÍ|MACHALA|LAS LAJAS|HUAQUILLAS|EL GUABO|CHILLA|BALSAS|ATAHUALPA|ARENILLAS",
                    tweets[i, 27]
                  )
                if (resultado == TRUE) {
                  tweets[i, 28] <- "7"
                } else {
                  resultado <-
                    grepl(
                      "ESMERALDAS|SAN LORENZO|RIOVERDE|QUININDÉ|MUISNE|LA CONCORDIA|ESMERALDAS|ELOY ALFARO|ATACAMES",
                      tweets[i, 27]
                    )
                  if (resultado == TRUE) {
                    tweets[i, 28] <- "8"
                  } else {
                    resultado <-
                      grepl(
                        "GYE|GUAYAS|SIMÓN BOLÍVAR|SANTA LUCÍA|SAN JACINTO DE YAGUACHI|SAMBORONDON|SAMBORONDÓN|SALITRE|SALITRE (URBINA JADO)|PLAYAS|PEDRO CARBO|PALESTINA|NOBOL|NARANJITO|NARANJAL|MILAGRO|LOMAS DE SARGENTILLO|ISIDRO AYORA|GUAYAQUIL|GENERAL ANTONIO ELIZALDE|EL TRIUNFO|EL EMPALME|DURÁN|DAULE|CORONEL MARCELINO MARIDUEÑA|COLIMES",
                        tweets[i, 27]
                      )
                    if (resultado == TRUE) {
                      tweets[i, 28] <- "9"
                    } else {
                      resultado <-
                        grepl(
                          "IMBABURA|SAN MIGUEL DE URCUQUÍ|PIMAMPIRO|OTAVALO|IBARRA|COTACACHI|ANTONIO ANTE",
                          tweets[i, 27]
                        )
                      if (resultado == TRUE) {
                        tweets[i, 28] <- "10"
                      } else {
                        resultado <-
                          grepl(
                            "LOJA|LOJA|CALVAS|CATAMAYO|CELICA|CHAGUARPAMBA|ESPÍNDOLA|GONZANAMÁ|MACARÁ|PALTAS|PUYANGO|SARAGURO|SOZORANGA|ZAPOTILLO|PINDAL|QUILANGA|OLMEDO",
                            tweets[i, 27]
                          )
                        if (resultado == TRUE) {
                          tweets[i, 28] <- "11"
                        } else {
                          resultado <-
                            grepl(
                              "LOS RIOS|BABAHOYO|BABA|MONTALVO|PUEBLOVIEJO|QUEVEDO|URDANETA|VENTANAS|VÍNCES|PALENQUE|BUENA FÉ|VALENCIA|MOCACHE|QUINSALOMA",
                              tweets[i, 27]
                            )
                          if (resultado == TRUE) {
                            tweets[i, 28] <- "12"
                          } else {
                            resultado <-
                              grepl(
                                "MANABI|PORTOVIEJO|BOLÍVAR|CHONE|EL CARMEN|FLAVIO ALFARO|JIPIJAPA|JUNÍN|MANTA|MONTECRISTI|PAJÁN|ROCAFUERTE|SANTA ANA|SUCRE|TOSAGUA|24 DE MAYO|PEDERNALES|OLMEDO|PUERTO LÓPEZ|JAMA|JARAMIJÓ|SAN VICENTE",
                                tweets[i, 27]
                              )
                            if (resultado == TRUE) {
                              tweets[i, 28] <- "13"
                            } else {
                              resultado <-
                                grepl(
                                  "MORONA SANTIAGO|MORONA|GUALAQUIZA|LIMÓN INDANZA|PALORA|SANTIAGO|SUCÚA|HUAMBOYA|SAN JUAN BOSCO|TAISHA|LOGROÑO|PABLO SEXTO|TIWINTZA",
                                  tweets[i, 27]
                                )
                              if (resultado == TRUE) {
                                tweets[i, 28] <- "14"
                              } else {
                                resultado <-
                                  grepl(
                                    "NAPO|TENA|ARCHIDONA|EL CHACO|QUIJOS|CARLOS JULIO AROSEMENA TOLA",
                                    tweets[i, 27]
                                  )
                                if (resultado == TRUE) {
                                  tweets[i, 28] <- "15"
                                } else {
                                  resultado <-
                                    grepl(
                                      "PASTAZA|PASTAZA|MERA|SANTA CLARA|ARAJUNO",
                                      tweets[i, 27]
                                    )
                                  if (resultado == TRUE) {
                                    tweets[i, 28] <- "16"
                                  } else {
                                    resultado <-
                                      grepl(
                                        "UIO|PICHINCHA|SAN MIGUEL DE LOS BANCOS|RUMIÑAHUI|QUITO|PUERTO QUITO|PEDRO VICENTE MALDONADO|PEDRO MONCAYO|MEJIA|CAYAMBE",
                                        tweets[i, 27]
                                      )
                                    if (resultado == TRUE) {
                                      tweets[i, 28] <- "17"
                                    } else {
                                      resultado <-
                                        grepl(
                                          "TUNGURAHUA|TISALEO|SANTIAGO DE PÍLLARO|SAN PEDRO DE PELILEO|QUERO|PATATE|MOCHA|CEVALLOS|BAÑOS DE AGUA SANTA|AMBATO",
                                          tweets[i, 27]
                                        )
                                      if (resultado == TRUE) {
                                        tweets[i, 28] <- "18"
                                      } else {
                                        resultado <-
                                          grepl(
                                            "ZAMORA CHINCHIPE|ZAMORA|YANTZAZA (YANZATZA)|YACUAMBI|PAQUISHA|PALANDA|NANGARITZA|EL PANGUI|CHINCHIPE|CENTINELA DEL CÓNDOR",
                                            tweets[i, 27]
                                          )
                                        if (resultado == TRUE) {
                                          tweets[i, 28] <- "23"
                                        } else {
                                          resultado <-
                                            grepl(
                                              "GALAPAGOS|SANTA CRUZ|SAN CRISTÓBAL|ISABELA",
                                              tweets[i, 27]
                                            )
                                          if (resultado == TRUE) {
                                            tweets[i, 28] <- "24"
                                          } else {
                                            resultado <-
                                              grepl(
                                                "SUCUMBIOS|SUCUMBÍOS|SHUSHUFINDI|PUTUMAYO|LAGO AGRIO|GONZALO PIZARRO|CUYABENO|CASCALES",
                                                tweets[i, 27]
                                              )
                                            if (resultado == TRUE) {
                                              tweets[i, 28] <- "21"
                                            } else {
                                              resultado <-
                                                grepl(
                                                  "ORELLANA|ORELLANA|LORETO|LA JOYA DE LOS SACHAS|AGUARICO",
                                                  tweets[i, 27]
                                                )
                                              if (resultado == TRUE) {
                                                tweets[i, 28] <- "22"
                                              } else {
                                                resultado <-
                                                  grepl(
                                                    "SANTO DOMINGO DE LOS TSACHILAS|SANTO DOMINGO",
                                                    tweets[i, 27]
                                                  )
                                                if (resultado == TRUE) {
                                                  tweets[i, 28] <- "23"
                                                } else {
                                                  resultado <-
                                                    grepl(
                                                      "SANTA ELENA|SANTA ELENA|SALINAS|LA LIBERTAD",
                                                      tweets[i, 27]
                                                    )
                                                  if (resultado == TRUE) {
                                                    tweets[i, 28] <- "24"
                                                  } else {
                                                    tweets[i, 28] <- "25"
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  } else {
    tweets[i, 28] <- "25"
  }
}

#calculamos el total de tweets por localidad
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


#creamos una nueva tabla donde calcularemos los porcentajes correspondientes y el factor de poneracion
library(readxl)

codLocalidad <- as.character(c(1:25))
nombresLocalidad <-
  c("AZUAY","BOLIVAR","CAÑAR","CARCHI","COTOPAXI","CHIMBORAZO","EL_ORO","ESMERALDAS","GUAYAS",
    "IMBABURA","LOJA","LOS_RIOS","MANABI","MORONA_SANTIAGO","NAPO","PASTAZA","PICHINCHA",
    "TUNGURAHUA","ZAMORA_CHINCHIPE","GALAPAGOS","SUCUMBIOS","ORELLANA","SANTO_DOMINGO_TSACHILAS",
    "SANTA_ELENA","OTROS")

#ingresamos los datos censales
datosCensales <-
  read_excel(
    "C:/Users/David/Documents/AnalisisDatos/Maquinas/scripts/Nuevos Scripts/datosCensales.xlsx"
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

#guardamos el dataframe en un archivo de texto csv
write.csv(x = TweetsFinal, file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/scripts/Nuevos Scripts/TweetsFinalCompleto.csv", row.names = FALSE)



