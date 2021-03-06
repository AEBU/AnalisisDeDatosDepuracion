library(readxl)

tweets <-
  read.csv(
    "C:/Users/David/Documents/AnalisisDatos/Maquinas/Datos_David/CanelaRadioEc_2017-07-17.csv",
    header = TRUE,
    sep = ";"
  )
tweets$localidadMayusculas <- c(rep("", NROW(tweets)))
tweets$codProv <- c(rep("", NROW(tweets)))
tweets$factorPonderacion <- c(rep(0, NROW(tweets)))

#Convertir a mayusculas
for (i in 1:NROW(tweets)) {
  tweets[i, 19] <- toupper(tweets[i, 18])
}

#Verificar la localidad con expresiones regulares
for (i in 1:NROW(tweets)) {
  if (!is.na(tweets[i, 19])) {
    resultado <-
      grepl(
        "AZUAY|SIGSIG|SEVILLA DE ORO|SANTA ISABEL|SAN FERNANDO|PUCARA|PAUTE|O�A|NAB�N|GUALACEO|GUACHAPALA|GIR�N|EL PAN|CUENCA|CHORDELEG|CAMILO PONCE ENR�QUEZ",
        tweets[i, 19]
      )
    pertenece <-
      resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
    if (pertenece == 1) {
      tweets[i, 20] <- "1"
    } else {
      resultado <-
        grepl(
          "BOLIVAR|SAN MIGUEL|LAS NAVES|GUARANDA|ECHEAND�A|CHIMBO|CHILLANES|CALUMA",
          tweets[i, 19]
        )
      pertenece <-
        resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
      if (pertenece == 1) {
        tweets[i, 20] <- "2"
      } else {
        resultado <-
          grepl("CA�AR|SUSCAL|LA TRONCAL|EL TAMBO|D�LEG|CA�AR|BIBLI�N|AZOGUES",
                   tweets[i, 19])
        pertenece <-
          resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
        if (pertenece == 1) {
          tweets[i, 20] <- "3"
        } else {
          resultado <-
            grepl("CARCHI|TULC�N|SAN PEDRO DE HUACA|MONT�FAR|MIRA|ESPEJO|BOL�VAR",
                     tweets[i, 19])
          pertenece <-
            resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
          if (pertenece == 1) {
            tweets[i, 20] <- "4"
          } else {
            resultado <-
              grepl(
                "COTOPAXI|SIGCHOS|SAQUISIL�|SALCEDO|PUJILI|PANGUA|LATACUNGA|LA MAN�",
                tweets[i, 19]
              )
            pertenece <-
              resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
            if (pertenece == 1) {
              tweets[i, 20] <- "5"
            } else {
              resultado <-
                grepl(
                  "CHIMBORAZO|RIOBAMBA|PENIPE|PALLATANGA|GUANO|GUAMOTE|CUMAND�|COLTA|CHUNCHI|CHAMBO|ALAUSI",
                  tweets[i, 19]
                )
              pertenece <-
                resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
              if (pertenece == 1) {
                tweets[i, 20] <- "6"
              } else {
                resultado <-
                  grepl(
                    "EL ORO|ZARUMA|SANTA ROSA|PORTOVELO|PI�AS|PASAJE|MARCABEL�|MACHALA|LAS LAJAS|HUAQUILLAS|EL GUABO|CHILLA|BALSAS|ATAHUALPA|ARENILLAS",
                    tweets[i, 19]
                  )
                pertenece <-
                  resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                if (pertenece == 1) {
                  tweets[i, 20] <- "7"
                } else {
                  resultado <-
                    grepl(
                      "ESMERALDAS|SAN LORENZO|RIOVERDE|QUININD�|MUISNE|LA CONCORDIA|ESMERALDAS|ELOY ALFARO|ATACAMES",
                      tweets[i, 19]
                    )
                  pertenece <-
                    resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                  if (pertenece == 1) {
                    tweets[i, 20] <- "8"
                  } else {
                    resultado <-
                      grepl(
                        "GYE|GUAYAS|SIM�N BOL�VAR|SANTA LUC�A|SAN JACINTO DE YAGUACHI|SAMBORONDON|SAMBOROND�N|SALITRE|SALITRE (URBINA JADO)|PLAYAS|PEDRO CARBO|PALESTINA|NOBOL|NARANJITO|NARANJAL|MILAGRO|LOMAS DE SARGENTILLO|ISIDRO AYORA|GUAYAQUIL|GENERAL ANTONIO ELIZALDE|EL TRIUNFO|EL EMPALME|DUR�N|DAULE|CORONEL MARCELINO MARIDUE�A|COLIMES",
                        tweets[i, 19]
                      )
                    pertenece <-
                      resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                    if (pertenece == 1) {
                      tweets[i, 20] <- "9"
                    } else {
                      resultado <-
                        grepl(
                          "IMBABURA|SAN MIGUEL DE URCUQU�|PIMAMPIRO|OTAVALO|IBARRA|COTACACHI|ANTONIO ANTE",
                          tweets[i, 19]
                        )
                      pertenece <-
                        resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                      if (pertenece == 1) {
                        tweets[i, 20] <- "10"
                      } else {
                        resultado <-
                          grepl(
                            "LOJA|LOJA|CALVAS|CATAMAYO|CELICA|CHAGUARPAMBA|ESP�NDOLA|GONZANAM�|MACAR�|PALTAS|PUYANGO|SARAGURO|SOZORANGA|ZAPOTILLO|PINDAL|QUILANGA|OLMEDO",
                            tweets[i, 19]
                          )
                        pertenece <-
                          resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                        if (pertenece == 1) {
                          tweets[i, 20] <- "11"
                        } else {
                          resultado <-
                            grepl(
                              "LOS RIOS|BABAHOYO|BABA|MONTALVO|PUEBLOVIEJO|QUEVEDO|URDANETA|VENTANAS|V�NCES|PALENQUE|BUENA F�|VALENCIA|MOCACHE|QUINSALOMA",
                              tweets[i, 19]
                            )
                          pertenece <-
                            resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                          if (pertenece == 1) {
                            tweets[i, 20] <- "12"
                          } else {
                            resultado <-
                              grepl(
                                "MANABI|PORTOVIEJO|BOL�VAR|CHONE|EL CARMEN|FLAVIO ALFARO|JIPIJAPA|JUN�N|MANTA|MONTECRISTI|PAJ�N|ROCAFUERTE|SANTA ANA|SUCRE|TOSAGUA|24 DE MAYO|PEDERNALES|OLMEDO|PUERTO L�PEZ|JAMA|JARAMIJ�|SAN VICENTE",
                                tweets[i, 19]
                              )
                            pertenece <-
                              resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                            if (pertenece == 1) {
                              tweets[i, 20] <- "13"
                            } else {
                              resultado <-
                                grepl(
                                  "MORONA SANTIAGO|MORONA|GUALAQUIZA|LIM�N INDANZA|PALORA|SANTIAGO|SUC�A|HUAMBOYA|SAN JUAN BOSCO|TAISHA|LOGRO�O|PABLO SEXTO|TIWINTZA",
                                  tweets[i, 19]
                                )
                              pertenece <-
                                resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                              if (pertenece == 1) {
                                tweets[i, 20] <- "14"
                              } else {
                                resultado <-
                                  grepl(
                                    "NAPO|TENA|ARCHIDONA|EL CHACO|QUIJOS|CARLOS JULIO AROSEMENA TOLA",
                                    tweets[i, 19]
                                  )
                                pertenece <-
                                  resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                if (pertenece == 1) {
                                  tweets[i, 20] <- "15"
                                } else {
                                  resultado <-
                                    grepl(
                                      "PASTAZA|PASTAZA|MERA|SANTA CLARA|ARAJUNO",
                                      tweets[i, 19]
                                    )
                                  pertenece <-
                                    resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                  if (pertenece == 1) {
                                    tweets[i, 20] <- "16"
                                  } else {
                                    resultado <-
                                      grepl(
                                        "UIO|PICHINCHA|SAN MIGUEL DE LOS BANCOS|RUMI�AHUI|QUITO|PUERTO QUITO|PEDRO VICENTE MALDONADO|PEDRO MONCAYO|MEJIA|CAYAMBE",
                                        tweets[i, 19]
                                      )
                                    pertenece <-
                                      resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                    if (pertenece == 1) {
                                      tweets[i, 20] <- "17"
                                    } else {
                                      resultado <-
                                        grepl(
                                          "TUNGURAHUA|TISALEO|SANTIAGO DE P�LLARO|SAN PEDRO DE PELILEO|QUERO|PATATE|MOCHA|CEVALLOS|BA�OS DE AGUA SANTA|AMBATO",
                                          tweets[i, 19]
                                        )
                                      pertenece <-
                                        resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                      if (pertenece == 1) {
                                        tweets[i, 20] <- "18"
                                      } else {
                                        resultado <-
                                          grepl(
                                            "ZAMORA CHINCHIPE|ZAMORA|YANTZAZA (YANZATZA)|YACUAMBI|PAQUISHA|PALANDA|NANGARITZA|EL PANGUI|CHINCHIPE|CENTINELA DEL C�NDOR",
                                            tweets[i, 19]
                                          )
                                        pertenece <-
                                          resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                        if (pertenece == 1) {
                                          tweets[i, 20] <- "19"
                                        } else {
                                          resultado <-
                                            grepl(
                                              "GALAPAGOS|SANTA CRUZ|SAN CRIST�BAL|ISABELA",
                                              tweets[i, 19]
                                            )
                                          pertenece <-
                                            resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                          if (pertenece == 1) {
                                            tweets[i, 20] <- "20"
                                          } else {
                                            resultado <-
                                              grepl(
                                                "SUCUMBIOS|SUCUMB�OS|SHUSHUFINDI|PUTUMAYO|LAGO AGRIO|GONZALO PIZARRO|CUYABENO|CASCALES",
                                                tweets[i, 19]
                                              )
                                            pertenece <-
                                              resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                            if (pertenece == 1) {
                                              tweets[i, 20] <- "21"
                                            } else {
                                              resultado <-
                                                grepl(
                                                  "ORELLANA|ORELLANA|LORETO|LA JOYA DE LOS SACHAS|AGUARICO",
                                                  tweets[i, 19]
                                                )
                                              pertenece <-
                                                resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                              if (pertenece == 1) {
                                                tweets[i, 20] <- "22"
                                              } else {
                                                resultado <-
                                                  grepl(
                                                    "SANTO DOMINGO DE LOS TSACHILAS|SANTO DOMINGO",
                                                    tweets[i, 19]
                                                  )
                                                pertenece <-
                                                  resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                                if (pertenece == 1) {
                                                  tweets[i, 20] <- "23"
                                                } else {
                                                  resultado <-
                                                    grepl(
                                                      "SANTA ELENA|SANTA ELENA|SALINAS|LA LIBERTAD",
                                                      tweets[i, 19]
                                                    )
                                                  pertenece <-
                                                    resultado[[1]][1] #devuelve 1 si texto pertenece a la expresion regular
                                                  if (pertenece == 1) {
                                                    tweets[i, 20] <- "24"
                                                  } else {
                                                    tweets[i, 20] <- "25"
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
    tweets[i, 20] <- "25"
  }
}

#total de tweets por provincia
AZUAY <- 0
BOLIVAR <- 0
CA�AR <- 0
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
  if (tweets[i, 20] == "1") {
    AZUAY <- AZUAY + 1
  } else if (tweets[i, 20] == "2") {
    BOLIVAR <- BOLIVAR + 1
  } else if (tweets[i, 20] == "3") {
    CA�AR <- CA�AR + 1
  } else if (tweets[i, 20] == "4") {
    CARCHI <- CARCHI + 1
  } else if (tweets[i, 20] == "5") {
    CHIMBORAZO <- CHIMBORAZO + 1
  } else if (tweets[i, 20] == "6") {
    COTOPAXI <- COTOPAXI + 1
  } else if (tweets[i, 20] == "7") {
    EL_ORO <- EL_ORO + 1
  } else if (tweets[i, 20] == "8") {
    ESMERALDAS <- ESMERALDAS + 1
  } else if (tweets[i, 20] == "9") {
    GUAYAS <- GUAYAS + 1
  } else if (tweets[i, 20] == "10") {
    IMBABURA <- IMBABURA + 1
  } else if (tweets[i, 20] == "11") {
    LOJA <- LOJA + 1
  } else if (tweets[i, 20] == "12") {
    LOS_RIOS <- LOS_RIOS + 1
  } else if (tweets[i, 20] == "13") {
    MANABI <- MANABI + 1
  } else if (tweets[i, 20] == "14") {
    MORONA_SANTIAGO <- MORONA_SANTIAGO + 1
  } else if (tweets[i, 20] == "15") {
    NAPO <- NAPO + 1
  } else if (tweets[i, 20] == "16") {
    PASTAZA <- PASTAZA + 1
  } else if (tweets[i, 20] == "17") {
    PICHINCHA <- PICHINCHA + 1
  } else if (tweets[i, 20] == "18") {
    TUNGURAHUA <- TUNGURAHUA + 1
  } else if (tweets[i, 20] == "19") {
    ZAMORA_CHINCHIPE <- ZAMORA_CHINCHIPE + 1
  } else if (tweets[i, 20] == "20") {
    GALAPAGOS <- GALAPAGOS + 1
  } else if (tweets[i, 20] == "21") {
    SUCUMBIOS <- SUCUMBIOS + 1
  } else if (tweets[i, 20] == "22") {
    ORELLANA <- ORELLANA + 1
  } else if (tweets[i, 20] == "23") {
    SANTO_DOMINGO_TSACHILAS <- SANTO_DOMINGO_TSACHILAS + 1
  } else if (tweets[i, 20] == "24") {
    SANTA_ELENA <- SANTA_ELENA + 1
  } else if (tweets[i, 20] == "25") {
    OTROS <- OTROS + 1
  }
}

codLocalidad <- as.character(c(1:25))
nombresLocalidad <-
  c(
    "AZUAY",
    "BOLIVAR",
    "CA�AR",
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
datosCensales <- read_excel("C:/Users/David/Documents/AnalisisDatos/Maquinas/Datos_David/datosCensales.xlsx")
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
  data.frame(codLocalidad, nombresLocalidad, porcenCenso, porcenTweets, porcenTweetsSinOtros,factorponderacion)

#porcentaje por provincia
porcentajeT[1, 4] <- (AZUAY * 100) / NROW(tweets)
porcentajeT[2, 4] <- (BOLIVAR * 100) / NROW(tweets)
porcentajeT[3, 4] <- (CA�AR * 100) / NROW(tweets)
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


#calculo del factor de ponderacion
for (i in 1:NROW(porcentajeT)) {
  porcentajeT[i, 5] <- (porcentajeT[i, 4])/(1-(porcentajeT[25, 4]/100))
  if (porcentajeT[i, 2]=="OTROS") {
    porcentajeT[i,6] <- 1
  } else {
    fp <- porcentajeT[i, 3]/porcentajeT[i, 5]
    if (is.infinite(fp)==FALSE) {
      porcentajeT[i,6] <- fp
    }
  }
}

#incluir factor de ponderacion en el dataframe
for(i in 1:NROW(tweets)) {
  codLocT <- tweets[i,20]
  for(j in 1:NROW(porcentajeT)) {
    codLocP <- porcentajeT[j,1]
    factPon <- porcentajeT[j,6]
    if (codLocT == codLocP) {
      tweets[i,21] <- factPon
    }
  }
}