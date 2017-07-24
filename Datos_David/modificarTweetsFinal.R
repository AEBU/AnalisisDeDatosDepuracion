library(twitteR)
library(httr)
require('ROAuth')
require('RCurl')
library(base64enc)

consumer_key <- "RFHeP55qj0ejWv7YiSceWTphX"
consumer_secret <-
  "YOOUNzPvlJCAIHo23AWGU6hRm9VYxP6AY60H0n3u3dBM44aHZS"
access_token <- "366852754-QE8L7hZs1J0WbtDTImo4P3qxzabRZ6OEY0E9ckOs"
access_secret <- "zUZ3rnyPHCLQxso43A8PvXY1a3jWp81yS8Z2Rr0w96YzB"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <-
  read.csv(
    "C:/Users/David/Desktop/tweets/TodosSinLocalidadFinal.csv",
    header = TRUE,
    sep = ";"
  )

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


#añadiendo otras columnas de los usuarios
tweets$statusCount <- c(rep(NA, NROW(tweets)))
tweets$followers <- c(rep(NA, NROW(tweets)))
tweets$favorites <- c(rep(NA, NROW(tweets)))
tweets$friends <- c(rep(NA, NROW(tweets)))
tweets$location <- c(rep(NA, NROW(tweets)))


for (i in 3126:NROW(tweets)) {
  usuario <- as.character(tweets[i, 12])
  tryCatch({
    informacionUsuario <- getUser(usuario)
    tweets[i, 21] <- statusesCount(informacionUsuario)
    tweets[i, 22] <- followersCount(informacionUsuario)
    tweets[i, 23] <- favoritesCount(informacionUsuario)
    tweets[i, 24] <- friendsCount(informacionUsuario)
    tweets[i, 25] <- location(informacionUsuario)
  })
}

write.table(
  x = tweets,
  file = "C:/Users/David/Desktop/tweets/TodosDavidFinal.csv",
  col.names = TRUE,
  sep = ";"
)

tweets <- tweets[,1:25]

tweets$relevancia <- c(rep(0, NROW(tweets)))
for(i in 1:NROW(tweets)) {
  tweets[i,26]<-tweets[i,22]/tweets[i,24]
}


tweets$localidadMayusculas <- c(rep("", NROW(tweets)))
tweets$codProv <- c(rep("", NROW(tweets)))
tweets$factorPonderacion <- c(rep(0, NROW(tweets)))

for (i in 1:NROW(tweets)) {
  tweets[i, 27] <- toupper(tweets[i, 25])
}

#Verificar la localidad con expresiones regulares
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

tweets2 <- tweets[,2:29]

write.csv(tweets2, file="C:/Users/David/Desktop/tweets/Final/TweetsDavidFinal.csv", row.names=F)

TweetsDavidFinal <- TweetsDavidFinal[,-17]
write.csv(TweetsDavidFinal, file="C:/Users/David/Desktop/tweets/Final/TweetsDavidCorrecto.csv", row.names=F)
