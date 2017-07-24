# INTEGRANTES:
# Alexis Bautista
# Bryan Catucuamba
# David Jácome
# Alejandro Naranjo
# Richard Quimbiulco

# Tweets obtenidos de la emisora "La Bruja"

library(twitteR)
library(httr)
require('ROAuth')
require('RCurl')
library(base64enc)

palabrasPositivas <- c("bueno","bonitas","pongan","queremos","mejor")
palabrasNegativas <- c("malo","no","feo","otro","dañar","horrendo","cambien")

consumer_key <-"RFHeP55qj0ejWv7YiSceWTphX"
consumer_secret<-"YOOUNzPvlJCAIHo23AWGU6hRm9VYxP6AY60H0n3u3dBM44aHZS"
access_token<-"366852754-QE8L7hZs1J0WbtDTImo4P3qxzabRZ6OEY0E9ckOs"
access_secret<-"zUZ3rnyPHCLQxso43A8PvXY1a3jWp81yS8Z2Rr0w96YzB"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

listaTweets <- searchTwitter(searchString = "labrujaecuador", n=1000)

tweets <- twListToDF(listaTweets)

for (i in 1:NROW(tweets)) {
  
  texto <- tweets[i,1]
  
  #Remover retweets
  sinRT <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", texto)
  
  #Remover cuentas
  sinCuentas <- gsub("@\\w+", "", sinRT)
  
  #Remover simbolos de puntuacion
  sinSimbolos <- gsub("[[:punct:]]", "", sinCuentas)
  
  #Remover numeros
  sinNumeros <- gsub("[[:digit:]]", "", sinSimbolos)
  
  #Remover enlaces
  sinEnlaces <- gsub("http\\w+", "", sinNumeros)
  
  #Guarda en un vector las palabras del tweet
  v <- strsplit(sinEnlaces," ")
  
  palabras <- data.frame(v)
  
  numPositivas <- 0
  numNegativas <- 0
  for (j in 1:NROW(palabras)) {
    for (k in 1:NROW(palabrasPositivas)) {
      
      if (palabras[j,1]==palabrasPositivas[k]) {
        
        numPositivas<-numPositivas+1
        
      }
      
    }
    
    for (k in 1:NROW(palabrasNegativas)) {
      
      if (palabras[j,1]==palabrasNegativas[k]) {
        
        numNegativas<-numNegativas+1
        
      }
      
    }
    
  }
  
  metrica <- numPositivas - numNegativas
  
  if (metrica > 0) {
    
    resultado <- paste("Tweet", i , "positivo", sep = " ")
    print(resultado)
    
  } else if (metrica < 0) {
    
    resultado <- paste("Tweet", i , "negativo", sep = " ")
    print(resultado)
    
  } else {
    
    resultado <- paste("Tweet", i , "neutro", sep = " ")
    print(resultado)
    
  }
  
}
