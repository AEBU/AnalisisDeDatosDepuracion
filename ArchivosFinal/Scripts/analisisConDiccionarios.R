#filtramos solo polaridad 1 con SVM

TweetsPositivos <-
  subset(TweetsFinal, TweetsFinal$polaridadDiccionarios == 1)
library(tm)


corpus = Corpus(VectorSource(TweetsPositivos$depuraSinRadios))
corpus = tm_map(
  corpus,
  removeWords,
  c(
    stopwords("spanish"),
    "mejor","quiero","cancion","takashijakome","holaa","X00a0","entradas","cobijasquito","heydj",
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
    "duaubdedubucquisiera",
    "oscar",
    "ctor",
    "nombre",
    "manuel",
    "colon",
    "willie",
    "amigos",
    "leon",
    "faraon",
    "manuelle",
    "gitana",
    "melanie",
    "victor",
    "salsaviva",
    "boris",
    "quinde",
    "ddlovato",
    "willy","X00a0"
  )
)
frequencies = DocumentTermMatrix(corpus)
length(corpus)
frequencies

#Para tener un data frame de todas las palabras
tweetsDePeticiones = as.data.frame(as.matrix(frequencies))

colnames(tweetsDePeticiones) = make.names(colnames(tweetsDePeticiones))

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
  min.freq = 5
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
                                        tweets[i, 31] <- "99"
                                        tweets[i, 32] <- "Otros"
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
#table(tweets$CodArtista)
#table(tweets$NombreArtista)

#Filtramos por

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

tweetsArtistas <-
  rbind(
    tweets1,
    tweets2,
    tweets3,
    tweets4,
    tweets5,
    tweets6,
    tweets7,
    tweets8,
    tweets9,
    tweets10,
    tweets11,
    tweets12,
    tweets13,
    tweets14,
    tweets15,
    tweets16
  )



tweetsArtistas$RelevanciaArtista <-
  ((tweetsArtistas$favoriteCount + tweetsArtistas$retweetCount) / NROW(tweetsArtistas))
#encontramos la relevancia con ponderacion
tweetsArtistas$RelevanciaArtistaFP <-
  tweetsArtistas$RelevanciaArtista * tweetsArtistas$factorPonderacion

tweetsArtistas <- tweetsArtistas[order(tweetsArtistas$RelevanciaArtistaFP), ]

tweetsArtistas$Percentil <- c(rep(NA,NROW(tweetsArtistas)))

for (i in 1:NROW(tweetsArtistas)) {
  tweetsArtistas[i,35] <- ((i-(0.5))/NROW(tweetsArtistas))*100
}

tweetsArtistas$Votos <- c(rep(NA,NROW(tweetsArtistas)))

for (i in 1:NROW(tweetsArtistas)) {
  if (tweetsArtistas[i,35] <= 50) {
    tweetsArtistas[i,36] <- 1
  } else if (tweetsArtistas[i,35] > 50 & tweetsArtistas[i,35] <= 75) {
    tweetsArtistas[i,36] <- 2
  } else if (tweetsArtistas[i,35] > 75 & tweetsArtistas[i,35] <= 90) {
    tweetsArtistas[i,36] <- 3
  } else if (tweetsArtistas[i,35] > 90 & tweetsArtistas[i,35] <= 95) {
    tweetsArtistas[i,36] <- 4
  } else {
    tweetsArtistas[i,36] <- 5
  }
}


votos1 <- 0
votos2 <- 0
votos3 <- 0
votos4 <- 0
votos5 <- 0
votos6 <- 0
votos7 <- 0
votos8 <- 0
votos9 <- 0
votos10 <- 0
votos11 <- 0
votos12 <- 0
votos13 <- 0
votos14 <- 0
votos15 <- 0
votos16 <- 0

for (i in 1:NROW(tweetsArtistas)) {
  if (tweetsArtistas[i,31]=="1") {
    votos1 <- votos1 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="2") {
    votos2 <- votos2 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="3") {
    votos3 <- votos3 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="4") {
    votos4 <- votos4 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="5") {
    votos5 <- votos5 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="6") {
    votos6 <- votos6 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="7") {
    votos7 <- votos7 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="8") {
    votos8 <- votos8 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="9") {
    votos9 <- votos9 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="10") {
    votos10 <- votos10 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="11") {
    votos11 <- votos11 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="12") {
    votos12 <- votos12 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="13") {
    votos13 <- votos13 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="14") {
    votos14 <- votos14 + tweetsArtistas[i,36]
  } else if (tweetsArtistas[i,31]=="15") {
    votos15 <- votos15 + tweetsArtistas[i,36]
  } else {
    votos16 <- votos16 + tweetsArtistas[i,36]
  }
}

df1 <- data.frame("Joel Y Brian",votos1)
names(df1) <- c("Artista", "NumVotos")

df2 <- data.frame("Pablo Jara",votos2)
names(df2) <- c("Artista", "NumVotos")

df3 <- data.frame("Maluma",votos3)
names(df3) <- c("Artista", "NumVotos")

df4 <- data.frame("Jesus Miranda - Chyno",votos4)
names(df4) <- c("Artista", "NumVotos")

df5 <- data.frame("RKM & Ken-Y",votos5)
names(df5) <- c("Artista", "NumVotos")

df6 <- data.frame("JBalvin",votos6)
names(df6) <- c("Artista", "NumVotos")

df7 <- data.frame("Johann Vera",votos7)
names(df7) <- c("Artista", "NumVotos")

df8 <- data.frame("Jostin Ramirez",votos8)
names(df8) <- c("Artista", "NumVotos")

df9 <- data.frame("CNCO",votos9)
names(df9) <- c("Artista", "NumVotos")

df10 <- data.frame("Wilson Franco",votos10)
names(df10) <- c("Artista", "NumVotos")

df11 <- data.frame("Marques",votos11)
names(df11) <- c("Artista", "NumVotos")

df12 <- data.frame("Melibea",votos12)
names(df12) <- c("Artista", "NumVotos")

df13 <- data.frame("Daniel Paez",votos13)
names(df13) <- c("Artista", "NumVotos")

df14 <- data.frame("Carlos Rivera",votos14)
names(df14) <- c("Artista", "NumVotos")

df15 <- data.frame("Sebastián Yatra",votos15)
names(df15) <- c("Artista", "NumVotos")

df16 <- data.frame("Reik",votos16)
names(df16) <- c("Artista", "NumVotos")

TotalVotos <-
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


#Diagrama de pastel
mf <- as.array(TotalVotos$NumVotos)
names(mf) <- as.array(TotalVotos$Artista)
pie(mf, col = rainbow(16), main = "Numero Votos")


#Diagrama de barras
barplot(
  height = TotalVotos$NumVotos,
  names.arg = TotalVotos$Artista,
  col = rainbow(16)
)
