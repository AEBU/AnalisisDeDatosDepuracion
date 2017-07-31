#filtramos solo polaridad 1 con SVM

TweetsPositivos <-
  subset(TweetsFinal, TweetsFinal$polaridadSVM == 1)
library(tm)


corpus = Corpus(VectorSource(TweetsPositivos$depuraSinRadios))
corpus = tm_map(
  corpus,
  removeWords,
  c(
    stopwords("spanish"),
    "marcoscumba",
    "porfaa",
    "madelayne",
    "duaubdedubuceduaubcedubeubeduaubdedubu",
    "artistgrup",
    "lanocheen",
    "gomitwiin",
    "andradedirak",
    "auefuauefjbalvincuador",
    "genaromecachas",
    "squeayer",
    "mpdelaghetto",
    "arcangel",
    "lunes",
    "martes",
    "miercoles",
    "jueves",
    "viernes",
    "sabado",
    "domingo",
    "xfa",
    "lanochewq",
    "pueden",
    "quisiera",
    "sinton",
    "favor",
    "aqu",
    "porfavor",
    "devu",
    "lvemeelcoraz",
    "porfis",
    "porfa",
    "saludo",
    "mikebahia",
    "X00a0",
    "canela",
    "entrada",
    "duaubdedubud",
    "kiavenove",
    "giovag",
    "duaubdedubuamuchas",
    "acantaama",
    "rcoles",
    "duaubdedubudgracias",
    "graciasduaubdedubueduaubdedubuteampjarauio",
    "podr",
    "duaubdedubuf",
    "duaubcedubeubo",
    "papelduaubcedubeubsue",
    "duaubdedubueduaubdedubu",
    "duaubdedubuaeduaubdedubua",
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
    "complacer",
    "calentamientoradial",
    "pvillalobosm",
    "duaubdedubua",
    "universalmusice",
    "remix",
    "duaubdedubucquisiera"
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
positivas = positivas[order(positivas$frecuencia, decreasing = T),]

#View(positivas)
x11()
wordcloud(
  positivas$palabra,
  positivas$frecuencia,
  colors = brewer.pal(8, "Dark2"),
  max.words = 85,
  min.freq = 17
)





#----------------------Contar tweets de artistas
TweetsPositivos$CodArtista = c(rep(NA, NROW(TweetsPositivos)))
TweetsPositivos$NombreArtista = c(rep(NA, NROW(TweetsPositivos)))
tweets <- TweetsPositivos


for (i in 1:NROW(tweets)) {
  if (!is.na(tweets[i, 30])) {
    resultado <-
      grepl("joelybrian",
            tweets[i, 30])
    if (resultado == TRUE) {
      tweets[i, 31] <- "1"
      tweets[i, 32] <- "Joel y Brian"
    } else {
      resultado <-
        grepl("pablojaraec",
              tweets[i, 30])
      if (resultado == TRUE) {
        tweets[i, 31] <- "2"
        tweets[i, 32] <- "Pablo Jara"
      } else {
        resultado <-
          grepl("maluma",
                tweets[i, 30])
        if (resultado == TRUE) {
          tweets[i, 31] <- "3"
          tweets[i, 32] <- "Maluma"
        } else {
          resultado <-
            grepl("jesusmiranda",
                  tweets[i, 30])
          if (resultado == TRUE) {
            tweets[i, 31] <- "4"
            tweets[i, 32] <- "Jesús Miranda - Chyno"
          } else {
            resultado <-
              grepl("kenfficial",
                    tweets[i, 30])
            if (resultado == TRUE) {
              tweets[i, 31] <- "5"
              tweets[i, 32] <- "RKM & Ken-Y"
            } else {
              resultado <-
                grepl("rkmoficial",
                      tweets[i, 30])
              if (resultado == TRUE) {
                tweets[i, 31] <- "5"
                tweets[i, 32] <- "RKM & Ken-Y"
              } else {
                resultado <-
                  grepl("jbalvin|jbalvincuador",
                        tweets[i, 30])
                if (resultado == TRUE) {
                  tweets[i, 31] <- "6"
                  tweets[i, 32] <- "Jbalvin"
                } else {
                  resultado <-
                    grepl("johannvera",
                          tweets[i, 30])
                  if (resultado == TRUE) {
                    tweets[i, 31] <- "7"
                    tweets[i, 32] <- "Johann Vera"
                  } else {
                    resultado <-
                      grepl("jostinramirezp",
                            tweets[i, 30])
                    if (resultado == TRUE) {
                      tweets[i, 31] <- "8"
                      tweets[i, 32] <- "Jostin Ramirez"
                    } else {
                      resultado <-
                        grepl("cncomusic|cncoecuadorvip|heydj",
                              tweets[i, 30])
                      if (resultado == TRUE) {
                        tweets[i, 31] <- "9"
                        tweets[i, 32] <- "CNCO"
                      } else {
                        resultado <-
                          grepl("chinomirandac",
                                tweets[i, 30])
                        if (resultado == TRUE) {
                          tweets[i, 31] <- "4"
                          tweets[i, 32] <- "Jesús Miranda - Chyno"
                        } else {
                          resultado <-
                            grepl("soywilsonfranco",
                                  tweets[i, 30])
                          if (resultado == TRUE) {
                            tweets[i, 31] <- "10"
                            tweets[i, 32] <- "Wilson Franco"
                          } else {
                            resultado <-
                              grepl("marquesmusica",
                                    tweets[i, 30])
                            if (resultado == TRUE) {
                              tweets[i, 31] <- "11"
                              tweets[i, 32] <- "Marqués"
                            } else {
                              resultado <-
                                grepl("melibeamusica",
                                      tweets[i, 30])
                              if (resultado == TRUE) {
                                tweets[i, 31] <- "12"
                                tweets[i, 32] <- "Melibea"
                              } else {
                                resultado <-
                                  grepl("danielpaezmusic",
                                        tweets[i, 30])
                                if (resultado == TRUE) {
                                  tweets[i, 31] <- "13"
                                  tweets[i, 32] <- "Daniel Paez"
                                } else {
                                  resultado <-
                                    grepl("arlosrivera",
                                          tweets[i, 30])
                                  if (resultado == TRUE) {
                                    tweets[i, 31] <- "14"
                                    tweets[i, 32] <- "Carlos Rivera"
                                  }
                                  else{
                                    resultado <-
                                      grepl("sebastianyatra|sebastianyatrae",
                                            tweets[i, 30])
                                    if (resultado == TRUE) {
                                      tweets[i, 31] <- "15"
                                      tweets[i, 32] <-
                                        "Sebastián Yatra"
                                    }
                                    else{
                                      resultado <-
                                        grepl("reikmx",
                                              tweets[i, 30])
                                      if (resultado == TRUE) {
                                        tweets[i, 31] <- "16"
                                        tweets[i, 32] <- "Reik"
                                      }
                                      else{
                                        tweets[i, 31] <- "Otros"
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
    tweets[i, 26] <- "25"
  }
}
#table(tweets$CodArtista)
#table(tweets$NombreArtista)


#Tweets de las artistas

#Joel Y Brian
tweets1 = subset(tweets, tweets$CodArtista == "1")
#Pablo Jara
tweets2 = subset(tweets, tweets$CodArtista == "2")
#Maluma
tweets3 = subset(tweets, tweets$CodArtista == "3")
#Jesus Miranda - Chyno
tweets4 = subset(tweets, tweets$CodArtista == "4")
#RKM & Ken-Y
tweets5 = subset(tweets, tweets$CodArtista == "5")
#JBalvin
tweets6 = subset(tweets, tweets$CodArtista == "6")
#Johann Vera
tweets7 = subset(tweets, tweets$CodArtista == "7")
#Jostin Ramirez
tweets8 = subset(tweets, tweets$CodArtista == "8")
#CNCO
tweets9 = subset(tweets, tweets$CodArtista == "9")
#Wilson Franco
tweets10 = subset(tweets, tweets$CodArtista == "10")
#Marques
tweets11 = subset(tweets, tweets$CodArtista == "11")
#Melibea
tweets12 = subset(tweets, tweets$CodArtista == "12")
#Daniel Paez
tweets13 = subset(tweets, tweets$CodArtista == "13")
#Carlos Rivera
tweets14 = subset(tweets, tweets$CodArtista == "14")
#Sebastián Yatra
tweets15 = subset(tweets, tweets$CodArtista == "15")
#Reik
tweets16 = subset(tweets, tweets$CodArtista == "16")




##########RELEVANCIA################# total tweets positivos = 1509

totalTweets <- 1509

######################################################################################
######################################################################################
#Artista 1
#enncontramos la relevancia sin ponderacion
tweets1$RelevanciaArtista <-
  ((tweets1$favoriteCount + tweets1$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets1$RelevanciaArtistaFP <-
  tweets1$RelevanciaArtista * tweets1$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia1 <-
  weighted.mean(x = tweets1$RelevanciaArtista,
                w = tweets1$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia1 <- 0
for (i in 1:NROW(tweets1)) {
  SumaRelevancia1 <- SumaRelevancia1 + tweets1[i, 34]
}

mediaRelevancia1FP <- SumaRelevancia1 / NROW(tweets1)

df1 <-
  data.frame("Joel Y Brian",
             mediaRelevancia1,
             mediaRelevancia1FP,
             SumaRelevancia1)
names(df1) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")



######################################################################################
######################################################################################
#Artista 2
#enncontramos la relevancia sin ponderacion
tweets2$RelevanciaArtista <-
  ((tweets2$favoriteCount + tweets2$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets2$RelevanciaArtistaFP <-
  tweets2$RelevanciaArtista * tweets2$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia2 <-
  weighted.mean(x = tweets2$RelevanciaArtista,
                w = tweets2$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia2 <- 0
for (i in 1:NROW(tweets2)) {
  SumaRelevancia2 <- SumaRelevancia2 + tweets2[i, 34]
}

mediaRelevancia2FP <- SumaRelevancia2 / NROW(tweets2)

df2 <-
  data.frame("Pablo Jara",
             mediaRelevancia2,
             mediaRelevancia2FP,
             SumaRelevancia2)
names(df2) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 3
#enncontramos la relevancia sin ponderacion
tweets3$RelevanciaArtista <-
  ((tweets3$favoriteCount + tweets3$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets3$RelevanciaArtistaFP <-
  tweets3$RelevanciaArtista * tweets3$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia3 <-
  weighted.mean(x = tweets3$RelevanciaArtista,
                w = tweets3$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia3 <- 0
for (i in 1:NROW(tweets3)) {
  SumaRelevancia3 <- SumaRelevancia3 + tweets3[i, 34]
}

mediaRelevancia3FP <- SumaRelevancia3 / NROW(tweets3)

df3 <-
  data.frame("Maluma",
             mediaRelevancia3,
             mediaRelevancia3FP,
             SumaRelevancia3)
names(df3) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 4
#enncontramos la relevancia sin ponderacion
tweets4$RelevanciaArtista <-
  ((tweets4$favoriteCount + tweets4$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets4$RelevanciaArtistaFP <-
  tweets4$RelevanciaArtista * tweets4$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia4 <-
  weighted.mean(x = tweets4$RelevanciaArtista,
                w = tweets4$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia4 <- 0
for (i in 1:NROW(tweets4)) {
  SumaRelevancia4 <- SumaRelevancia4 + tweets4[i, 34]
}

mediaRelevancia4FP <- SumaRelevancia4 / NROW(tweets4)

df4 <-
  data.frame("Jesus Miranda - Chyno",
             mediaRelevancia4,
             mediaRelevancia4FP,
             SumaRelevancia4)
names(df4) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 5
#enncontramos la relevancia sin ponderacion
tweets5$RelevanciaArtista <-
  ((tweets5$favoriteCount + tweets5$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets5$RelevanciaArtistaFP <-
  tweets5$RelevanciaArtista * tweets5$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia5 <-
  weighted.mean(x = tweets5$RelevanciaArtista,
                w = tweets5$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia5 <- 0
for (i in 1:NROW(tweets5)) {
  SumaRelevancia5 <- SumaRelevancia5 + tweets5[i, 34]
}

mediaRelevancia5FP <- SumaRelevancia5 / NROW(tweets5)

df5 <-
  data.frame("RKM & Ken-Y",
             mediaRelevancia5,
             mediaRelevancia5FP,
             SumaRelevancia5)
names(df5) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 6
#enncontramos la relevancia sin ponderacion
tweets6$RelevanciaArtista <-
  ((tweets6$favoriteCount + tweets6$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets6$RelevanciaArtistaFP <-
  tweets6$RelevanciaArtista * tweets6$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia6 <-
  weighted.mean(x = tweets6$RelevanciaArtista,
                w = tweets6$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia6 <- 0
for (i in 1:NROW(tweets6)) {
  SumaRelevancia6 <- SumaRelevancia6 + tweets6[i, 34]
}

mediaRelevancia6FP <- SumaRelevancia6 / NROW(tweets6)

df6 <-
  data.frame("JBalvin",
             mediaRelevancia6,
             mediaRelevancia6FP,
             SumaRelevancia6)
names(df6) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 7
#enncontramos la relevancia sin ponderacion
tweets7$RelevanciaArtista <-
  ((tweets7$favoriteCount + tweets7$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets7$RelevanciaArtistaFP <-
  tweets7$RelevanciaArtista * tweets7$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia7 <-
  weighted.mean(x = tweets7$RelevanciaArtista,
                w = tweets7$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia7 <- 0
for (i in 1:NROW(tweets7)) {
  SumaRelevancia7 <- SumaRelevancia7 + tweets7[i, 34]
}

mediaRelevancia7FP <- SumaRelevancia7 / NROW(tweets7)

df7 <-
  data.frame("Johann Vera",
             mediaRelevancia7,
             mediaRelevancia7FP,
             SumaRelevancia7)
names(df7) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 8
#enncontramos la relevancia sin ponderacion
tweets8$RelevanciaArtista <-
  ((tweets8$favoriteCount + tweets8$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets8$RelevanciaArtistaFP <-
  tweets8$RelevanciaArtista * tweets8$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia8 <-
  weighted.mean(x = tweets8$RelevanciaArtista,
                w = tweets8$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia8 <- 0
for (i in 1:NROW(tweets8)) {
  SumaRelevancia8 <- SumaRelevancia8 + tweets8[i, 34]
}

mediaRelevancia8FP <- SumaRelevancia8 / NROW(tweets8)

df8 <-
  data.frame("Jostin Ramirez",
             mediaRelevancia8,
             mediaRelevancia8FP,
             SumaRelevancia8)
names(df8) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 9
#enncontramos la relevancia sin ponderacion
tweets9$RelevanciaArtista <-
  ((tweets9$favoriteCount + tweets9$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets9$RelevanciaArtistaFP <-
  tweets9$RelevanciaArtista * tweets9$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia9 <-
  weighted.mean(x = tweets9$RelevanciaArtista,
                w = tweets9$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia9 <- 0
for (i in 1:NROW(tweets9)) {
  SumaRelevancia9 <- SumaRelevancia9 + tweets9[i, 34]
}

mediaRelevancia9FP <- SumaRelevancia9 / NROW(tweets9)

df9 <-
  data.frame("CNCO", mediaRelevancia9, mediaRelevancia9FP, SumaRelevancia9)
names(df9) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 10
#enncontramos la relevancia sin ponderacion
tweets10$RelevanciaArtista <-
  ((tweets10$favoriteCount + tweets10$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets10$RelevanciaArtistaFP <-
  tweets10$RelevanciaArtista * tweets10$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia10 <-
  weighted.mean(x = tweets10$RelevanciaArtista,
                w = tweets10$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia10 <- 0
for (i in 1:NROW(tweets10)) {
  SumaRelevancia10 <- SumaRelevancia10 + tweets10[i, 34]
}

mediaRelevancia10FP <- SumaRelevancia10 / NROW(tweets10)

df10 <-
  data.frame("Wilson Franco",
             mediaRelevancia10,
             mediaRelevancia10FP,
             SumaRelevancia10)
names(df10) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 11
#enncontramos la relevancia sin ponderacion
tweets11$RelevanciaArtista <-
  ((tweets11$favoriteCount + tweets11$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets11$RelevanciaArtistaFP <-
  tweets11$RelevanciaArtista * tweets11$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia11 <-
  weighted.mean(x = tweets11$RelevanciaArtista,
                w = tweets11$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia11 <- 0
for (i in 1:NROW(tweets11)) {
  SumaRelevancia11 <- SumaRelevancia11 + tweets11[i, 34]
}

mediaRelevancia11FP <- SumaRelevancia11 / NROW(tweets11)

df11 <-
  data.frame("Marques",
             mediaRelevancia11,
             mediaRelevancia11FP,
             SumaRelevancia11)
names(df11) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")




######################################################################################
######################################################################################
#Artista 12
#enncontramos la relevancia sin ponderacion
tweets12$RelevanciaArtista <-
  ((tweets12$favoriteCount + tweets12$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets12$RelevanciaArtistaFP <-
  tweets12$RelevanciaArtista * tweets12$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia12 <-
  weighted.mean(x = tweets12$RelevanciaArtista,
                w = tweets12$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia12 <- 0
for (i in 1:NROW(tweets12)) {
  SumaRelevancia12 <- SumaRelevancia12 + tweets12[i, 34]
}

mediaRelevancia12FP <- SumaRelevancia12 / NROW(tweets12)

df12 <-
  data.frame("Melibea",
             mediaRelevancia12,
             mediaRelevancia12FP,
             SumaRelevancia12)
names(df12) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")





######################################################################################
######################################################################################
#Artista 13
#enncontramos la relevancia sin ponderacion
tweets13$RelevanciaArtista <-
  ((tweets13$favoriteCount + tweets13$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets13$RelevanciaArtistaFP <-
  tweets13$RelevanciaArtista * tweets13$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia13 <-
  weighted.mean(x = tweets13$RelevanciaArtista,
                w = tweets13$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia13 <- 0
for (i in 1:NROW(tweets13)) {
  SumaRelevancia13 <- SumaRelevancia13 + tweets13[i, 34]
}

mediaRelevancia13FP <- SumaRelevancia13 / NROW(tweets13)

df13 <-
  data.frame("Daniel Paez",
             mediaRelevancia13,
             mediaRelevancia13FP,
             SumaRelevancia13)
names(df13) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 14
#enncontramos la relevancia sin ponderacion
tweets14$RelevanciaArtista <-
  ((tweets14$favoriteCount + tweets14$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets14$RelevanciaArtistaFP <-
  tweets14$RelevanciaArtista * tweets14$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia14 <-
  weighted.mean(x = tweets14$RelevanciaArtista,
                w = tweets14$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia14 <- 0
for (i in 1:NROW(tweets14)) {
  SumaRelevancia14 <- SumaRelevancia14 + tweets14[i, 34]
}

mediaRelevancia14FP <- SumaRelevancia14 / NROW(tweets14)

df14 <-
  data.frame("Carlos Rivera",
             mediaRelevancia14,
             mediaRelevancia14FP,
             SumaRelevancia14)
names(df14) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")



######################################################################################
######################################################################################
#Artista 15
#enncontramos la relevancia sin ponderacion
tweets15$RelevanciaArtista <-
  ((tweets15$favoriteCount + tweets15$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets15$RelevanciaArtistaFP <-
  tweets15$RelevanciaArtista * tweets15$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia15 <-
  weighted.mean(x = tweets15$RelevanciaArtista,
                w = tweets15$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia15 <- 0
for (i in 1:NROW(tweets15)) {
  SumaRelevancia15 <- SumaRelevancia15 + tweets15[i, 34]
}

mediaRelevancia15FP <- SumaRelevancia15 / NROW(tweets15)

df15 <-
  data.frame("Sebastián Yatra",
             mediaRelevancia15,
             mediaRelevancia15FP,
             SumaRelevancia15)
names(df15) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


######################################################################################
######################################################################################
#Artista 16
#enncontramos la relevancia sin ponderacion
tweets16$RelevanciaArtista <-
  ((tweets16$favoriteCount + tweets16$retweetCount) / totalTweets)
#encontramos la relevancia con ponderacion
tweets16$RelevanciaArtistaFP <-
  tweets16$RelevanciaArtista * tweets16$factorPonderacion
#Encontramos la media ponderada con función de R
mediaRelevancia16 <-
  weighted.mean(x = tweets16$RelevanciaArtista,
                w = tweets16$factorPonderacion)
#Encontramos la media ponderada sin función de R
SumaRelevancia16 <- 0
for (i in 1:NROW(tweets16)) {
  SumaRelevancia16 <- SumaRelevancia16 + tweets16[i, 34]
}

mediaRelevancia16FP <- SumaRelevancia16 / NROW(tweets16)

df16 <-
  data.frame("Reik",
             mediaRelevancia16,
             mediaRelevancia16FP,
             SumaRelevancia16)
names(df16) <- c("Artista", "MediaConFuncion", "MediaSinFuncion", "Suma")


TablaRelevancias <-
  rbind(df1,
        df2,
        df3,
        df4,
        df5,
        df6,
        df7,
        df8,
        df9,
        df10,
        df11,
        df12,
        df13,
        df14,
        df15,
        df16)


#########################GRÁFICOS

## Media de Relevancia con función de R
#Diagrama de pastel
mf <- as.array(TablaRelevancias$MediaConFuncion)
names(mf) <- as.array(TablaRelevancias$Artista)
pie(mf, col = rainbow(16), main = "Relevancias")
#Diagrama de barras
barplot(
  height = TablaRelevancias$MediaConFuncion,
  names.arg = TablaRelevancias$Artista,
  col = rainbow(16)
)


## Media de Relevancia sin función de R
#Diagrama de pastel
mf <- as.array(TablaRelevancias$MediaSinFuncion)
names(mf) <- as.array(TablaRelevancias$CodArtista)
pie(mf, col = rainbow(16), main = "Relevancias")
#Diagrama de barras
barplot(
  height = TablaRelevancias$MediaSinFuncion,
  names.arg = TablaRelevancias$CodArtista,
  col = rainbow(16)
)


## Suma de Relevancia
#Diagrama de pastel
mf <- as.array(TablaRelevancias$Suma)
names(mf) <- as.array(TablaRelevancias$CodArtista)
pie(mf, col = rainbow(16), main = "Relevancias")
#Diagrama de barras
barplot(
  height = TablaRelevancias$Suma,
  names.arg = TablaRelevancias$CodArtista,
  col = rainbow(16)
)