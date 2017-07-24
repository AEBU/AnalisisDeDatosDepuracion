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

tweets$statusCount <- c(rep(NA, NROW(tweets)))
tweets$followers <- c(rep(NA, NROW(tweets)))
tweets$favorites <- c(rep(NA, NROW(tweets)))
tweets$friends <- c(rep(NA, NROW(tweets)))
tweets$location <- c(rep(NA, NROW(tweets)))


for (i in 423:NROW(tweets)) {
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





#for (i in 1:NROW(tweets)) {
#  usuario <- as.character(tweets[i, 12])
#  tryCatch({
#    informacionUsuario <- getUser(usuario)
#    usuarioDF <- twListToDF(informacionUsuario)
#    tweets[i, 21] <- usuarioDF$statusesCount
#    tweets[i, 22] <- usuarioDF$followersCount
#    tweets[i, 23] <- usuarioDF$favoritesCount
#    tweets[i, 24] <- usuarioDF$friendsCount
#    tweets[i, 25] <- usuarioDF$location
#  })
#}
