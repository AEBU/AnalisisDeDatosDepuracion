library(twitteR)
library(httr)
require('ROAuth')
require('RCurl')
library(base64enc)

consumer_key <-"RFHeP55qj0ejWv7YiSceWTphX"
consumer_secret<-"YOOUNzPvlJCAIHo23AWGU6hRm9VYxP6AY60H0n3u3dBM44aHZS"
access_token<-"366852754-QE8L7hZs1J0WbtDTImo4P3qxzabRZ6OEY0E9ckOs"
access_secret<-"zUZ3rnyPHCLQxso43A8PvXY1a3jWp81yS8Z2Rr0w96YzB"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

fecha <- Sys.Date()

#Descargar tweets de la bruja
emisora <- "labrujaecuador"
listaTweets <- searchTwitter(searchString = emisora, n=3700)
tweets <- twListToDF(listaTweets)
userInfo <- lookupUsers(tweets$screenName)
userFrame <- twListToDF(userInfo)
columnaLocalidad <- c(rep(NA,NROW(tweets)))
tweets$localidad <- columnaLocalidad
for(i in 1:NROW(tweets)) {
  usuarioT <- tweets[i,11]
  for(j in 1:(NROW(userFrame)-1)) {
    usuarioF <- userFrame[j,11]
    localidad <- userFrame[j,12]
    if (usuarioT == usuarioF) {
      tweets[i,17] <- localidad
    }
  }
}
nombreArchivo <- paste("C:/Users/David/Desktop/tweets/",emisora,"_",fecha,".txt", sep="")
write.table(x=tweets, file=nombreArchivo, col.names = TRUE, sep = ";")


#Descargar tweets de radio canela
emisora <- "CanelaRadioEc"
listaTweets <- searchTwitter(searchString = emisora, n=3700)
tweets <- twListToDF(listaTweets)
userInfo <- lookupUsers(tweets$screenName)
userFrame <- twListToDF(userInfo)
columnaLocalidad <- c(rep(NA,NROW(tweets)))
tweets$localidad <- columnaLocalidad
for(i in 1:NROW(tweets)) {
  usuarioT <- tweets[i,11]
  for(j in 1:(NROW(userFrame)-1)) {
    usuarioF <- userFrame[j,11]
    localidad <- userFrame[j,12]
    if (usuarioT == usuarioF) {
      tweets[i,17] <- localidad
    }
  }
}
nombreArchivo <- paste("C:/Users/David/Desktop/tweets/",emisora,"_",fecha,".txt", sep="")
write.table(x=tweets, file=nombreArchivo, col.names = TRUE, sep = ";")
