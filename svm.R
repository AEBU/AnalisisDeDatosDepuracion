
tweets <- read.csv("C:/Users/David/Desktop/tweets/labrujaecuador_2017-07-11.csv", header = TRUE, sep=";")

for (i in 1:NROW(tweets)) {
  
  texto <- tweets[i,2]
  
  #Remover retweets
  sinRT <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", texto)
  
  #Remover cuentas
  sinCuentas <- gsub("@", "", sinRT)
  
  #Remover simbolos de puntuacion
  sinSimbolos <- gsub("[[:punct:]]", "", sinCuentas)
  
  #Remover numeros
  sinNumeros <- gsub("[[:digit:]]", "", sinSimbolos)
  
  #Remover enlaces
  sinEnlaces <- gsub("http\\w+", "", sinNumeros)
  
  tweets$depurado[i] <- sinEnlaces
  
}

library(e1071)

tweets_entrenamiento <- tweets[1:20,18]
te <- as.data.frame(tweets_entrenamiento)
polaridad <- c(0,0,0,0,1,0,0,0,0,1,1,1,1,0,1,0,1,1,1,1)
te$polaridad <- as.factor(polaridad)
te.svm <- svm(te$polaridad~.,data=te)
summary(te.svm)

tweets_entrenamiento <- tweets[21:100,18]
tp <- as.data.frame(tweets_entrenamiento)
tp$polaridad <- te$polaridad[1:20]
asignado <- predict(te.svm,new=tp)
