

library(readr)
TweetsFinal <-
  read_csv(
    "C:/Users/David/Documents/AnalisisDatos/Maquinas/ParaCd/TweetsFinalCompleto.csv"
  )



#agregar columna de depurados sin radio
TweetsFinal$depuraSinRadios <- c(rep("", NROW(TweetsFinal)))

#quitar cuentas de radios
for (i in 1:NROW(TweetsFinal)) {
  texto <-
    as.character(tolower(iconv(TweetsFinal[i, 17], "latin1", "UTF-8")))
  texto <- gsub("canelaradioec", "", texto)
  texto <- gsub("labrujaecuador", "", texto)
  texto <- gsub("lOtraradioec", "", texto)
  texto <- gsub("metrostereo", "", texto)
  texto <- gsub("radio_sucre", "", texto)
  texto <- gsub("exafmecuador", "", texto)
  texto <- gsub("radioalfaec", "", texto)
  texto <- gsub("wqradio_ec", "", texto)
  texto <- gsub("oxigenowq", "", texto)
  texto <- gsub("wqradioc", "", texto)
  texto <- gsub("wqradio", "", texto)
  texto <- gsub("radiosucr", "", texto)
  texto <- gsub("wqnoticia", "", texto)
  texto <- gsub("pepetoala", "", texto)
  texto <- gsub("lorohomero", "", texto)
  texto <- gsub("laotraradioec", "", texto)
  texto <- gsub("calentamientoradialwq", "", texto)
  TweetsFinal[i, 30] <- texto
}


#filtramos solo polaridad 1

TweetsPositivos <- subset(TweetsFinal, TweetsFinal$polaridadSVM == 1)
library(tm)


corpus = Corpus(VectorSource(TweetsPositivos$depuraSinRadios))
corpus = tm_map(
  corpus,
  removeWords,
  c(
    stopwords("spanish"),
    "marcoscumba",
    "artistgrup",
    "squeayer",
    "xfa",
    "porfa",
    "acantaama",
    "podr",
    "sue",
    "sica",
    "duaubdedubu",
    "oxigeno",
    "soyfher",
    "canci",
    "gracias",
    "hola",
    "lanocheenwq",
    "promoecuadorjyb",
    "X0085",
    "0085",
    "angelochoar",
    "anibalsmith",
    "escuchar",
    "complacer"
  )
)
frequencies = DocumentTermMatrix(corpus)
length(corpus)
frequencies

#inspect(frequencies[800:805,505:515])

#Para tener un data frame de todas las palabras
tweetsDePeticiones = as.data.frame(as.matrix(frequencies))

#View(tweetsDePeticiones)

colnames(tweetsDePeticiones) = make.names(colnames(tweetsDePeticiones))

#View(tweetsDePeticiones)

#tweetsDePeticiones$sentiment = TweetsPositivos$polaridadSVM

#View(tweetsDePeticiones)

library(wordcloud)



positivas = as.data.frame(colSums(tweetsDePeticiones))
#View(positivas)
positivas$word = row.names(positivas)
colnames(positivas) = c("frecuencia", "palabra")
positivas = positivas[order(positivas$frecuencia, decreasing = T), ]

#View(positivas)
x11()
wordcloud(
  positivas$palabra,
  positivas$frecuencia,
  colors = brewer.pal(8, "Dark2"),
  max.words = 300,
  min.freq = 39
)





#----------------------Contar tweets de artistas
TweetsPositivos$codArtist = c(rep(NA, NROW(TweetsPositivos)))
tweets <- TweetsPositivos

#796+127+38+22+23+11+21+24+90+97+52+68+64+38+38
for (i in 1:NROW(tweets)) {
  if (!is.na(tweets[i, 30])) {
    resultado <-
      grepl("joelybrian",
            tweets[i, 30])
    if (resultado == TRUE) {
      tweets[i, 31] <- "1"
    } else {
      resultado <-
        grepl("pablojaraec",
              tweets[i, 30])
      if (resultado == TRUE) {
        tweets[i, 31] <- "2"
      } else {
        resultado <-
          grepl("maluma",
                tweets[i, 30])
        if (resultado == TRUE) {
          tweets[i, 31] <- "3"
        } else {
          resultado <-
            grepl("jesusmiranda",
                  tweets[i, 30])
          if (resultado == TRUE) {
            tweets[i, 31] <- "4"
          } else {
            resultado <-
              grepl("kenfficial",
                    tweets[i, 30])
            if (resultado == TRUE) {
              tweets[i, 31] <- "5"
            } else {
              resultado <-
                grepl("rkmoficial",
                      tweets[i, 30])
              if (resultado == TRUE) {
                tweets[i, 31] <- "5"
              } else {
                resultado <-
                  grepl("jbalvin|jbalvincuador",
                        tweets[i, 30])
                if (resultado == TRUE) {
                  tweets[i, 31] <- "7"
                } else {
                  resultado <-
                    grepl("johannvera",
                          tweets[i, 30])
                  if (resultado == TRUE) {
                    tweets[i, 31] <- "8"
                  } else {
                    resultado <-
                      grepl("jostinramirezp",
                            tweets[i, 30])
                    if (resultado == TRUE) {
                      tweets[i, 31] <- "9"
                    } else {
                      resultado <-
                        grepl("cncomusic|cncoecuadorvip|heydj",
                              tweets[i, 30])
                      if (resultado == TRUE) {
                        tweets[i, 31] <- "10"
                      } else {
                        resultado <-
                          grepl("chinomirandac",
                                tweets[i, 30])
                        if (resultado == TRUE) {
                          tweets[i, 31] <- "4"
                        } else {
                          resultado <-
                            grepl("soywilsonfranco",
                                  tweets[i, 30])
                          if (resultado == TRUE) {
                            tweets[i, 31] <- "12"
                          } else {
                            resultado <-
                              grepl("marquesmusica",
                                    tweets[i, 30])
                            if (resultado == TRUE) {
                              tweets[i, 31] <- "13"
                            } else {
                              resultado <-
                                grepl("melibeamusica",
                                      tweets[i, 30])
                              if (resultado == TRUE) {
                                tweets[i, 31] <- "14"
                              } else {
                                resultado <-
                                  grepl("danielpaezmusic",
                                        tweets[i, 30])
                                if (resultado == TRUE) {
                                  tweets[i, 31] <- "15"
                                } else {
                                  resultado <-
                                    grepl("arlosrivera",
                                          tweets[i, 30])
                                  if (resultado == TRUE) {
                                    tweets[i, 31] <- "16"
                                  }
                                  else{
                                    tweets[i, 31] <- "-1"
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
    tweets[i, 26] <- "25"
  }
}
table(tweets$codArtist)
View(tweets)


#Tweets de las artistas

#Joel Y brian
tweets1 = subset(tweets, tweets$codArtist == 1)
#Pablo jara ec
tweets2 = subset(tweets, tweets$codArtist == 2)
#Maluma
tweets3 = subset(tweets, tweets$codArtist == 3)
#Jesus miranda
tweets4 = subset(tweets, tweets$codArtist == 4)
#raking y Kenficcial
tweets5 = subset(tweets, tweets$codArtist == 5)
#johannvera
tweets7 = subset(tweets, tweets$codArtist == 7)
#Jbalvin|Jbalvein ecuador
tweets8 = subset(tweets, tweets$codArtist == 8)
#Jostin Ramirez p
tweets9 = subset(tweets, tweets$codArtist == 9)
#CNcoMusic|cncoEcuador VIP|Hwydj
tweets10 = subset(tweets, tweets$codArtist == 10)
#Soy wilson franco
tweets12 = subset(tweets, tweets$codArtist == 12)
#MarquezMusica
tweets13 = subset(tweets, tweets$codArtist == 13)
#MElibea musica
tweets14 = subset(tweets, tweets$codArtist == 14)
#Daniel Paez Music
tweets15 = subset(tweets, tweets$codArtist == 15)
#arlosRivera
tweets16 = subset(tweets, tweets$codArtist == 16)




##########RELEVANCIA################# total tweets positivos = 1509

totalTweets <- 1509

#Artista 1
tweets1$relevanciaFinal <- c(rep(0, NROW(tweets1)))
tweets1$relevanciaFinalFP <- c(rep(0, NROW(tweets1)))
#enncontramos la relevancia
tweets1$relevanciaFinal <-
  ((
    as.double(tweets1$favoriteCount) + as.double(tweets1$retweetCount)
  ) / totalTweets)
#sacamos la media ponderada, esto va para todos
mediaRelevancia1 <-
  weighted.mean(x = tweets1$relevanciaFinal,
                w = tweets1$factorPonderacion)

#encontramos la relevancia multiplicado por el factor de ponderacion
tweets1$relevanciaFinalFP <-
  ((
    as.double(tweets1$favoriteCount) + as.double(tweets1$retweetCount)
  ) / totalTweets) * (as.double((tweets1$factorPonderacion)))
#hallamos la media
total1 <- 0
for (i in 1:NROW(tweets1)) {
  total1 <- total1 + tweets1[i, 33]
}

mediaRelevancia1FP <- total1 / NROW(tweets1)


#Artista 2
tweets2$relevanciaFinal <- c(rep(0, NROW(tweets2)))
tweets2$relevanciaFinal <-
  ((
    as.double(tweets2$favoriteCount) + as.double(tweets2$retweetCount)
  ) / totalTweets)
mediaRelevancia2 <-
  weighted.mean(x = tweets2$relevanciaFinal,
                w = tweets2$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets2$relevanciaFinalFP <-
  ((
    as.double(tweets2$favoriteCount) + as.double(tweets2$retweetCount)
  ) / totalTweets) * (as.double((tweets2$factorPonderacion)))
#hallamos la media
total2 <- 0
for (i in 1:NROW(tweets2)) {
  total2 <- total2 + tweets2[i, 33]
}

mediaRelevancia2FP <- total2 / NROW(tweets2)

#Artista 3
tweets3$relevanciaFinal <- c(rep(0, NROW(tweets3)))
tweets3$relevanciaFinal <-
  ((
    as.double(tweets3$favoriteCount) + as.double(tweets3$retweetCount)
  ) / totalTweets)
mediaRelevancia3 <-
  weighted.mean(x = tweets3$relevanciaFinal,
                w = tweets3$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets3$relevanciaFinalFP <-
  ((
    as.double(tweets3$favoriteCount) + as.double(tweets3$retweetCount)
  ) / totalTweets) * (as.double((tweets3$factorPonderacion)))
#hallamos la media
total3 <- 0
for (i in 1:NROW(tweets3)) {
  total3 <- total3 + tweets3[i, 33]
}

mediaRelevancia3FP <- total3 / NROW(tweets3)

#Artista 4
tweets4$relevanciaFinal <- c(rep(0, NROW(tweets4)))
tweets4$relevanciaFinal <-
  ((
    as.double(tweets4$favoriteCount) + as.double(tweets4$retweetCount)
  ) / totalTweets)
mediaRelevancia4 <-
  weighted.mean(x = tweets4$relevanciaFinal,
                w = tweets4$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets4$relevanciaFinalFP <-
  ((
    as.double(tweets4$favoriteCount) + as.double(tweets4$retweetCount)
  ) / totalTweets) * (as.double((tweets4$factorPonderacion)))
#hallamos la media
total4 <- 0
for (i in 1:NROW(tweets4)) {
  total4 <- total4 + tweets4[i, 33]
}

mediaRelevancia4FP <- total4 / NROW(tweets4)

#Artista 5
tweets5$relevanciaFinal <- c(rep(0, NROW(tweets5)))
tweets5$relevanciaFinal <-
  ((
    as.double(tweets5$favoriteCount) + as.double(tweets5$retweetCount)
  ) / totalTweets)
mediaRelevancia5 <-
  weighted.mean(x = tweets5$relevanciaFinal,
                w = tweets5$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets5$relevanciaFinalFP <-
  ((
    as.double(tweets5$favoriteCount) + as.double(tweets5$retweetCount)
  ) / totalTweets) * (as.double((tweets5$factorPonderacion)))
#hallamos la media
total5 <- 0
for (i in 1:NROW(tweets5)) {
  total5 <- total5 + tweets5[i, 33]
}

mediaRelevancia5FP <- total5 / NROW(tweets5)

#Artista 7
tweets7$relevanciaFinal <- c(rep(0, NROW(tweets7)))
tweets7$relevanciaFinal <-
  ((
    as.double(tweets7$favoriteCount) + as.double(tweets7$retweetCount)
  ) / totalTweets)
mediaRelevancia7 <-
  weighted.mean(x = tweets7$relevanciaFinal,
                w = tweets7$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets7$relevanciaFinalFP <-
  ((
    as.double(tweets7$favoriteCount) + as.double(tweets7$retweetCount)
  ) / totalTweets) * (as.double((tweets7$factorPonderacion)))
#hallamos la media
total7 <- 0
for (i in 1:NROW(tweets7)) {
  total7 <- total7 + tweets7[i, 33]
}

mediaRelevancia7FP <- total7 / NROW(tweets7)

#Artista 8
tweets8$relevanciaFinal <- c(rep(0, NROW(tweets8)))
tweets8$relevanciaFinal <-
  ((
    as.double(tweets8$favoriteCount) + as.double(tweets8$retweetCount)
  ) / totalTweets)
mediaRelevancia8 <-
  weighted.mean(x = tweets8$relevanciaFinal,
                w = tweets8$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets8$relevanciaFinalFP <-
  ((
    as.double(tweets8$favoriteCount) + as.double(tweets8$retweetCount)
  ) / totalTweets) * (as.double((tweets8$factorPonderacion)))
#hallamos la media
total8 <- 0
for (i in 1:NROW(tweets8)) {
  total8 <- total8 + tweets8[i, 33]
}

mediaRelevancia8FP <- total8 / NROW(tweets8)

#Artista 9
tweets9$relevanciaFinal <- c(rep(0, NROW(tweets9)))
tweets9$relevanciaFinal <-
  ((
    as.double(tweets9$favoriteCount) + as.double(tweets9$retweetCount)
  ) / totalTweets)
mediaRelevancia9 <-
  weighted.mean(x = tweets9$relevanciaFinal,
                w = tweets9$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets9$relevanciaFinalFP <-
  ((
    as.double(tweets9$favoriteCount) + as.double(tweets9$retweetCount)
  ) / totalTweets) * (as.double((tweets9$factorPonderacion)))
#hallamos la media
total9 <- 0
for (i in 1:NROW(tweets9)) {
  total9 <- total9 + tweets9[i, 33]
}

mediaRelevancia9FP <- total9 / NROW(tweets9)

#Artista 10
tweets10$relevanciaFinal <- c(rep(0, NROW(tweets10)))
tweets10$relevanciaFinal <-
  ((
    as.double(tweets10$favoriteCount) + as.double(tweets10$retweetCount)
  ) / totalTweets)
mediaRelevancia10 <-
  weighted.mean(x = tweets10$relevanciaFinal,
                w = tweets10$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets10$relevanciaFinalFP <-
  ((
    as.double(tweets10$favoriteCount) + as.double(tweets10$retweetCount)
  ) / totalTweets) * (as.double((tweets10$factorPonderacion)))
#hallamos la media
total10 <- 0
for (i in 1:NROW(tweets10)) {
  total10 <- total10 + tweets10[i, 33]
}

mediaRelevancia10FP <- total10 / NROW(tweets10)

#Artista 12
tweets12$relevanciaFinal <- c(rep(0, NROW(tweets12)))
tweets12$relevanciaFinal <-
  ((
    as.double(tweets12$favoriteCount) + as.double(tweets12$retweetCount)
  ) / totalTweets)
mediaRelevancia12 <-
  weighted.mean(x = tweets12$relevanciaFinal,
                w = tweets12$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets12$relevanciaFinalFP <-
  ((
    as.double(tweets12$favoriteCount) + as.double(tweets12$retweetCount)
  ) / totalTweets) * (as.double((tweets12$factorPonderacion)))
#hallamos la media
total12 <- 0
for (i in 1:NROW(tweets12)) {
  total12 <- total12 + tweets12[i, 33]
}

mediaRelevancia12FP <- total12 / NROW(tweets12)

#Artista 13
tweets13$relevanciaFinal <- c(rep(0, NROW(tweets13)))
tweets13$relevanciaFinal <-
  ((
    as.double(tweets13$favoriteCount) + as.double(tweets13$retweetCount)
  ) / totalTweets)
mediaRelevancia13 <-
  weighted.mean(x = tweets13$relevanciaFinal,
                w = tweets13$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets13$relevanciaFinalFP <-
  ((
    as.double(tweets13$favoriteCount) + as.double(tweets13$retweetCount)
  ) / totalTweets) * (as.double((tweets13$factorPonderacion)))
#hallamos la media
total13 <- 0
for (i in 1:NROW(tweets13)) {
  total13 <- total13 + tweets13[i, 33]
}

mediaRelevancia13FP <- total13 / NROW(tweets13)

#Artista 14
tweets14$relevanciaFinal <- c(rep(0, NROW(tweets14)))
tweets14$relevanciaFinal <-
  ((
    as.double(tweets14$favoriteCount) + as.double(tweets14$retweetCount)
  ) / totalTweets)
mediaRelevancia14 <-
  weighted.mean(x = tweets14$relevanciaFinal,
                w = tweets14$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets14$relevanciaFinalFP <-
  ((
    as.double(tweets14$favoriteCount) + as.double(tweets14$retweetCount)
  ) / totalTweets) * (as.double((tweets14$factorPonderacion)))
#hallamos la media
total14 <- 0
for (i in 1:NROW(tweets14)) {
  total14 <- total14 + tweets14[i, 33]
}

mediaRelevancia14FP <- total14 / NROW(tweets14)

#Artista 15
tweets15$relevanciaFinal <- c(rep(0, NROW(tweets15)))
tweets15$relevanciaFinal <-
  ((
    as.double(tweets15$favoriteCount) + as.double(tweets15$retweetCount)
  ) / totalTweets)
mediaRelevancia15 <-
  weighted.mean(x = tweets15$relevanciaFinal,
                w = tweets15$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets15$relevanciaFinalFP <-
  ((
    as.double(tweets15$favoriteCount) + as.double(tweets15$retweetCount)
  ) / totalTweets) * (as.double((tweets15$factorPonderacion)))
#hallamos la media
total15 <- 0
for (i in 1:NROW(tweets15)) {
  total15 <- total15 + tweets15[i, 33]
}

mediaRelevancia15FP <- total15 / NROW(tweets15)

#Artista 16
tweets16$relevanciaFinal <- c(rep(0, NROW(tweets16)))
tweets16$relevanciaFinal <-
  ((
    as.double(tweets16$favoriteCount) + as.double(tweets16$retweetCount)
  ) / totalTweets)
mediaRelevancia16 <-
  weighted.mean(x = tweets16$relevanciaFinal,
                w = tweets16$factorPonderacion)


#encontramos la relevancia multiplicado por el factor de ponderacion
tweets16$relevanciaFinalFP <-
  ((
    as.double(tweets16$favoriteCount) + as.double(tweets16$retweetCount)
  ) / totalTweets) * (as.double((tweets16$factorPonderacion)))
#hallamos la media
total16 <- 0
for (i in 1:NROW(tweets16)) {
  total16 <- total16 + tweets16[i, 33]
}

mediaRelevancia16FP <- total16 / NROW(tweets16)












tablaRelevanciasMedia <-
  rbind(
    mediaRelevancia1,
    mediaRelevancia2,
    mediaRelevancia3,
    mediaRelevancia4,
    mediaRelevancia5,
    mediaRelevancia7,
    mediaRelevancia8,
    mediaRelevancia9,
    mediaRelevancia10,
    mediaRelevancia12,
    mediaRelevancia13,
    mediaRelevancia14,
    mediaRelevancia15,
    mediaRelevancia16
  )

tablaRelevanciasMediaFP <-
  rbind(
    mediaRelevancia1FP,
    mediaRelevancia2FP,
    mediaRelevancia3FP,
    mediaRelevancia4FP,
    mediaRelevancia5FP,
    mediaRelevancia7FP,
    mediaRelevancia8FP,
    mediaRelevancia9FP,
    mediaRelevancia10FP,
    mediaRelevancia12FP,
    mediaRelevancia13FP,
    mediaRelevancia14FP,
    mediaRelevancia15FP,
    mediaRelevancia16FP
  )

tablaRelevanciasSumaFP <-
  rbind(
    total1,
    total2,
    total3,
    total4,
    total5,
    total7,
    total8,
    total9,
    total10,
    total12,
    total13,
    total14,
    total15,
    total16
  )

View(tablaRelevanciasMedia)

View(tablaRelevanciasMediaFP)

View(tablaRelevanciasSumaFP)
