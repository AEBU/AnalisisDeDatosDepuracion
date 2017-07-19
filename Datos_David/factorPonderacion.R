library(readxl)
library(twitteR)
library(httr)
require('ROAuth')
require('RCurl')
library(base64enc)

setup_twitter_oauth("vIBC5JpbRD94S8glMGkm2PCqL",
                    "MvKhrzTOrRyoX8IoSUCmbkNTPTk0ivbe1fXPeZBS0mm2OopxMG",
                    "882236838601216003-ATD0ZPFsaxbEJfLpUkqinldVA7tyAGB",
                    "uIttaQpnA6F06c9oDirYFlPZrLTvt3aV7iaSc1IzLUQz5")

#Leemos el dataframe de tweets
tweets <-
  read.csv(
    "C:/Users/David/Documents/AnalisisDatos/Maquinas/Datos_David/labrujaecuador_2017-07-17.csv",
    header = TRUE,
    sep = ";"
  )


#ELIMINACIÓN DE DUPLICADOS

#Verificamos la cantidad de tweets
NROW(tweets)

#depuracion de los textos de los tweets
for (i in 1:NROW(tweets)) {
  texto <- tweets[i, 2]
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

#verificamos numero de tweets restantes
NROW(tweets)


#RELEVANCIA

#Ratio relevancia del usuario
tweets$relevancia1<-c(rep(0,NROW(tweets)))
userInfo <- lookupUsers(tweets$screenName)
userFrame <- twListToDF(userInfo)
userFrame$relevancia1 <- userFrame$followersCount/userFrame$friendsCount
for(i in 1:NROW(tweets)) {
  usuarioT <- tweets[i,12]
  for(j in 1:NROW(userFrame)) {
    usuarioF <- userFrame[j,11]
    rel <- userFrame[j,18]
    if (usuarioT == usuarioF) {
      tweets[i,21] <- rel
    }
  }
}


#FACTOR DE PONDERACION

#creamos unas columnas necesarias
tweets$localidadMayusculas <- c(rep("", NROW(tweets)))
tweets$codProv <- c(rep("", NROW(tweets)))
tweets$factorPonderacion <- c(rep(0, NROW(tweets)))

#Convertir a mayusculas
for (i in 1:NROW(tweets)) {
  tweets[i, 23] <- toupper(tweets[i, 18])
}

#Verificar la localidad con expresiones regulares
for (i in 1:NROW(tweets)) {
  if (!is.na(tweets[i, 23])) {
    resultado <-
      grepl(
        "AZUAY|SIGSIG|SEVILLA DE ORO|SANTA ISABEL|SAN FERNANDO|PUCARA|PAUTE|OÑA|NABÓN|GUALACEO|GUACHAPALA|GIRÓN|EL PAN|CUENCA|CHORDELEG|CAMILO PONCE ENRÍQUEZ",
        tweets[i, 23]
      )
    if (resultado == TRUE) {
      tweets[i, 24] <- "1"
    } else {
      resultado <-
        grepl(
          "BOLIVAR|SAN MIGUEL|LAS NAVES|GUARANDA|ECHEANDÍA|CHIMBO|CHILLANES|CALUMA",
          tweets[i, 23]
        )
      if (resultado == TRUE) {
        tweets[i, 24] <- "2"
      } else {
        resultado <-
          grepl("CAÑAR|SUSCAL|LA TRONCAL|EL TAMBO|DÉLEG|CAÑAR|BIBLIÁN|AZOGUES",
                tweets[i, 23])
        if (resultado == TRUE) {
          tweets[i, 24] <- "3"
        } else {
          resultado <-
            grepl("CARCHI|TULCÁN|SAN PEDRO DE HUACA|MONTÚFAR|MIRA|ESPEJO|BOLÍVAR",
                  tweets[i, 23])
          if (resultado == TRUE) {
            tweets[i, 24] <- "4"
          } else {
            resultado <-
              grepl(
                "COTOPAXI|SIGCHOS|SAQUISILÍ|SALCEDO|PUJILI|PANGUA|LATACUNGA|LA MANÁ",
                tweets[i, 23]
              )
            if (resultado == TRUE) {
              tweets[i, 24] <- "5"
            } else {
              resultado <-
                grepl(
                  "CHIMBORAZO|RIOBAMBA|PENIPE|PALLATANGA|GUANO|GUAMOTE|CUMANDÁ|COLTA|CHUNCHI|CHAMBO|ALAUSI",
                  tweets[i, 23]
                )
              if (resultado == TRUE) {
                tweets[i, 24] <- "6"
              } else {
                resultado <-
                  grepl(
                    "EL ORO|ZARUMA|SANTA ROSA|PORTOVELO|PIÑAS|PASAJE|MARCABELÍ|MACHALA|LAS LAJAS|HUAQUILLAS|EL GUABO|CHILLA|BALSAS|ATAHUALPA|ARENILLAS",
                    tweets[i, 23]
                  )
                if (resultado == TRUE) {
                  tweets[i, 24] <- "7"
                } else {
                  resultado <-
                    grepl(
                      "ESMERALDAS|SAN LORENZO|RIOVERDE|QUININDÉ|MUISNE|LA CONCORDIA|ESMERALDAS|ELOY ALFARO|ATACAMES",
                      tweets[i, 23]
                    )
                  if (resultado == TRUE) {
                    tweets[i, 24] <- "8"
                  } else {
                    resultado <-
                      grepl(
                        "GYE|GUAYAS|SIMÓN BOLÍVAR|SANTA LUCÍA|SAN JACINTO DE YAGUACHI|SAMBORONDON|SAMBORONDÓN|SALITRE|SALITRE (URBINA JADO)|PLAYAS|PEDRO CARBO|PALESTINA|NOBOL|NARANJITO|NARANJAL|MILAGRO|LOMAS DE SARGENTILLO|ISIDRO AYORA|GUAYAQUIL|GENERAL ANTONIO ELIZALDE|EL TRIUNFO|EL EMPALME|DURÁN|DAULE|CORONEL MARCELINO MARIDUEÑA|COLIMES",
                        tweets[i, 23]
                      )
                    if (resultado == TRUE) {
                      tweets[i, 24] <- "9"
                    } else {
                      resultado <-
                        grepl(
                          "IMBABURA|SAN MIGUEL DE URCUQUÍ|PIMAMPIRO|OTAVALO|IBARRA|COTACACHI|ANTONIO ANTE",
                          tweets[i, 23]
                        )
                      if (resultado == TRUE) {
                        tweets[i, 24] <- "10"
                      } else {
                        resultado <-
                          grepl(
                            "LOJA|LOJA|CALVAS|CATAMAYO|CELICA|CHAGUARPAMBA|ESPÍNDOLA|GONZANAMÁ|MACARÁ|PALTAS|PUYANGO|SARAGURO|SOZORANGA|ZAPOTILLO|PINDAL|QUILANGA|OLMEDO",
                            tweets[i, 23]
                          )
                        if (resultado == TRUE) {
                          tweets[i, 24] <- "11"
                        } else {
                          resultado <-
                            grepl(
                              "LOS RIOS|BABAHOYO|BABA|MONTALVO|PUEBLOVIEJO|QUEVEDO|URDANETA|VENTANAS|VÍNCES|PALENQUE|BUENA FÉ|VALENCIA|MOCACHE|QUINSALOMA",
                              tweets[i, 23]
                            )
                          if (resultado == TRUE) {
                            tweets[i, 24] <- "12"
                          } else {
                            resultado <-
                              grepl(
                                "MANABI|PORTOVIEJO|BOLÍVAR|CHONE|EL CARMEN|FLAVIO ALFARO|JIPIJAPA|JUNÍN|MANTA|MONTECRISTI|PAJÁN|ROCAFUERTE|SANTA ANA|SUCRE|TOSAGUA|24 DE MAYO|PEDERNALES|OLMEDO|PUERTO LÓPEZ|JAMA|JARAMIJÓ|SAN VICENTE",
                                tweets[i, 23]
                              )
                            if (resultado == TRUE) {
                              tweets[i, 24] <- "13"
                            } else {
                              resultado <-
                                grepl(
                                  "MORONA SANTIAGO|MORONA|GUALAQUIZA|LIMÓN INDANZA|PALORA|SANTIAGO|SUCÚA|HUAMBOYA|SAN JUAN BOSCO|TAISHA|LOGROÑO|PABLO SEXTO|TIWINTZA",
                                  tweets[i, 23]
                                )
                              if (resultado == TRUE) {
                                tweets[i, 24] <- "14"
                              } else {
                                resultado <-
                                  grepl(
                                    "NAPO|TENA|ARCHIDONA|EL CHACO|QUIJOS|CARLOS JULIO AROSEMENA TOLA",
                                    tweets[i, 23]
                                  )
                                if (resultado == TRUE) {
                                  tweets[i, 24] <- "15"
                                } else {
                                  resultado <-
                                    grepl(
                                      "PASTAZA|PASTAZA|MERA|SANTA CLARA|ARAJUNO",
                                      tweets[i, 23]
                                    )
                                  if (resultado == TRUE) {
                                    tweets[i, 24] <- "16"
                                  } else {
                                    resultado <-
                                      grepl(
                                        "UIO|PICHINCHA|SAN MIGUEL DE LOS BANCOS|RUMIÑAHUI|QUITO|PUERTO QUITO|PEDRO VICENTE MALDONADO|PEDRO MONCAYO|MEJIA|CAYAMBE",
                                        tweets[i, 23]
                                      )
                                    if (resultado == TRUE) {
                                      tweets[i, 24] <- "17"
                                    } else {
                                      resultado <-
                                        grepl(
                                          "TUNGURAHUA|TISALEO|SANTIAGO DE PÍLLARO|SAN PEDRO DE PELILEO|QUERO|PATATE|MOCHA|CEVALLOS|BAÑOS DE AGUA SANTA|AMBATO",
                                          tweets[i, 23]
                                        )
                                      if (resultado == TRUE) {
                                        tweets[i, 24] <- "18"
                                      } else {
                                        resultado <-
                                          grepl(
                                            "ZAMORA CHINCHIPE|ZAMORA|YANTZAZA (YANZATZA)|YACUAMBI|PAQUISHA|PALANDA|NANGARITZA|EL PANGUI|CHINCHIPE|CENTINELA DEL CÓNDOR",
                                            tweets[i, 23]
                                          )
                                        if (resultado == TRUE) {
                                          tweets[i, 24] <- "23"
                                        } else {
                                          resultado <-
                                            grepl(
                                              "GALAPAGOS|SANTA CRUZ|SAN CRISTÓBAL|ISABELA",
                                              tweets[i, 23]
                                            )
                                          if (resultado == TRUE) {
                                            tweets[i, 24] <- "24"
                                          } else {
                                            resultado <-
                                              grepl(
                                                "SUCUMBIOS|SUCUMBÍOS|SHUSHUFINDI|PUTUMAYO|LAGO AGRIO|GONZALO PIZARRO|CUYABENO|CASCALES",
                                                tweets[i, 23]
                                              )
                                            if (resultado == TRUE) {
                                              tweets[i, 24] <- "21"
                                            } else {
                                              resultado <-
                                                grepl(
                                                  "ORELLANA|ORELLANA|LORETO|LA JOYA DE LOS SACHAS|AGUARICO",
                                                  tweets[i, 23]
                                                )
                                              if (resultado == TRUE) {
                                                tweets[i, 24] <- "22"
                                              } else {
                                                resultado <-
                                                  grepl(
                                                    "SANTO DOMINGO DE LOS TSACHILAS|SANTO DOMINGO",
                                                    tweets[i, 23]
                                                  )
                                                if (resultado == TRUE) {
                                                  tweets[i, 24] <- "23"
                                                } else {
                                                  resultado <-
                                                    grepl(
                                                      "SANTA ELENA|SANTA ELENA|SALINAS|LA LIBERTAD",
                                                      tweets[i, 23]
                                                    )
                                                  if (resultado == TRUE) {
                                                    tweets[i, 24] <- "24"
                                                  } else {
                                                    tweets[i, 24] <- "25"
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
    tweets[i, 24] <- "25"
  }
}

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
for (i in 1:NROW(tweets)) {
  if (tweets[i, 24] == "1") {
    AZUAY <- AZUAY + 1
  } else if (tweets[i, 24] == "2") {
    BOLIVAR <- BOLIVAR + 1
  } else if (tweets[i, 24] == "3") {
    CAÑAR <- CAÑAR + 1
  } else if (tweets[i, 24] == "4") {
    CARCHI <- CARCHI + 1
  } else if (tweets[i, 24] == "5") {
    CHIMBORAZO <- CHIMBORAZO + 1
  } else if (tweets[i, 24] == "6") {
    COTOPAXI <- COTOPAXI + 1
  } else if (tweets[i, 24] == "7") {
    EL_ORO <- EL_ORO + 1
  } else if (tweets[i, 24] == "8") {
    ESMERALDAS <- ESMERALDAS + 1
  } else if (tweets[i, 24] == "9") {
    GUAYAS <- GUAYAS + 1
  } else if (tweets[i, 24] == "10") {
    IMBABURA <- IMBABURA + 1
  } else if (tweets[i, 24] == "11") {
    LOJA <- LOJA + 1
  } else if (tweets[i, 24] == "12") {
    LOS_RIOS <- LOS_RIOS + 1
  } else if (tweets[i, 24] == "13") {
    MANABI <- MANABI + 1
  } else if (tweets[i, 24] == "14") {
    MORONA_SANTIAGO <- MORONA_SANTIAGO + 1
  } else if (tweets[i, 24] == "15") {
    NAPO <- NAPO + 1
  } else if (tweets[i, 24] == "16") {
    PASTAZA <- PASTAZA + 1
  } else if (tweets[i, 24] == "17") {
    PICHINCHA <- PICHINCHA + 1
  } else if (tweets[i, 24] == "18") {
    TUNGURAHUA <- TUNGURAHUA + 1
  } else if (tweets[i, 24] == "19") {
    ZAMORA_CHINCHIPE <- ZAMORA_CHINCHIPE + 1
  } else if (tweets[i, 24] == "20") {
    GALAPAGOS <- GALAPAGOS + 1
  } else if (tweets[i, 24] == "21") {
    SUCUMBIOS <- SUCUMBIOS + 1
  } else if (tweets[i, 24] == "22") {
    ORELLANA <- ORELLANA + 1
  } else if (tweets[i, 24] == "23") {
    SANTO_DOMINGO_TSACHILAS <- SANTO_DOMINGO_TSACHILAS + 1
  } else if (tweets[i, 24] == "24") {
    SANTA_ELENA <- SANTA_ELENA + 1
  } else if (tweets[i, 24] == "25") {
    OTROS <- OTROS + 1
  }
}

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
porcentajeT[1, 4] <- (AZUAY * 100) / NROW(tweets)
porcentajeT[2, 4] <- (BOLIVAR * 100) / NROW(tweets)
porcentajeT[3, 4] <- (CAÑAR * 100) / NROW(tweets)
porcentajeT[4, 4] <- (CARCHI * 100) / NROW(tweets)
porcentajeT[5, 4] <- (CHIMBORAZO * 100) / NROW(tweets)
porcentajeT[6, 4] <- (COTOPAXI * 100) / NROW(tweets)
porcentajeT[7, 4] <- (EL_ORO * 100) / NROW(tweets)
porcentajeT[8, 4] <- (ESMERALDAS * 100) / NROW(tweets)
porcentajeT[9, 4] <- (GUAYAS * 100) / NROW(tweets)
porcentajeT[10, 4] <- (IMBABURA * 100) / NROW(tweets)
porcentajeT[11, 4] <- (LOJA * 100) / NROW(tweets)
porcentajeT[12, 4] <- (LOS_RIOS * 100) / NROW(tweets)
porcentajeT[13, 4] <- (MANABI * 100) / NROW(tweets)
porcentajeT[14, 4] <- (MORONA_SANTIAGO * 100) / NROW(tweets)
porcentajeT[15, 4] <- (NAPO * 100) / NROW(tweets)
porcentajeT[16, 4] <- (PASTAZA * 100) / NROW(tweets)
porcentajeT[17, 4] <- (PICHINCHA * 100) / NROW(tweets)
porcentajeT[18, 4] <- (TUNGURAHUA * 100) / NROW(tweets)
porcentajeT[19, 4] <- (ZAMORA_CHINCHIPE * 100) / NROW(tweets)
porcentajeT[20, 4] <- (GALAPAGOS * 100) / NROW(tweets)
porcentajeT[21, 4] <- (SUCUMBIOS * 100) / NROW(tweets)
porcentajeT[22, 4] <- (ORELLANA * 100) / NROW(tweets)
porcentajeT[23, 4] <- (SANTO_DOMINGO_TSACHILAS * 100) / NROW(tweets)
porcentajeT[24, 4] <- (SANTA_ELENA * 100) / NROW(tweets)
porcentajeT[25, 4] <- (OTROS * 100) / NROW(tweets)


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
for (i in 1:NROW(tweets)) {
  codLocT <- tweets[i, 24]
  for (j in 1:NROW(porcentajeT)) {
    codLocP <- porcentajeT[j, 1]
    factPon <- porcentajeT[j, 6]
    if (codLocT == codLocP) {
      tweets[i, 25] <- factPon
    }
  }
}
