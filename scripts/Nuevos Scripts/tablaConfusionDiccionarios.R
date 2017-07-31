library(readr)

#como hemos leido 367 tweets y la polaridad esta en la columna  polaridadSVM, solo comparamos
#con la polaridad obtenido con diccionarios

TweetsFinalCompleto <- read_csv("~/AnalisisDatos/Maquinas/scripts/Nuevos Scripts/TweetsFinalCompleto.csv")
TweetsFinalCompleto <- TweetsFinalCompleto[1:367,]

table(TweetsFinalCompleto$polaridadSVM,TweetsFinalCompleto$polaridadDiccionarios)
