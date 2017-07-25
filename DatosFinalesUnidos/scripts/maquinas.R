tweets <- as.data.frame(TweetsFinalUnido)

#separar los datos de muestra
#indices de posicion que son de muestra
muestraaleatoria <- sample(nrow(tweets), 367)
write.table(
  x = muestraaleatoria,
  file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/indices.csv",
  col.names = TRUE,
  sep = ";"
)


muestraentrenamiento <- tweets[muestraaleatoria,]
write.csv(x = muestraentrenamiento,
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/TweetsEntrenamiento.csv",
          row.names = FALSE)
muestraprueba <- tweets[-muestraaleatoria,]
write.csv(x = muestraprueba,
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/TweetsPrueba.csv",
          row.names = FALSE)

muestraentrenamiento$polaridad<-c(rep(0,367))
muestraprueba$polaridad<-c(rep(0,7699))
muestraentrenamiento$indice=muestraaleatoria
muestraentrenamiento$textoLeer=muestraentrenamiento$text
muestraprueba$textoLeer=muestraentrenamiento$text

write.csv(x = muestraentrenamiento[1:92,28:30],
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/alexis.csv", row.names = FALSE)
write.csv(x = muestraentrenamiento[93:184,28:30],
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/bryan.csv", row.names = FALSE)
write.csv(x = muestraentrenamiento[185:275,28:30],
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/alejo.csv", row.names = FALSE)
write.csv(x = muestraentrenamiento[276:367,28:30],
          file = "C:/Users/David/Documents/AnalisisDatos/Maquinas/DatosFinalesUnidos/david.csv", row.names = FALSE)



library(readr)
alexis <- read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/leer/alexis.csv")
bryan <- read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/leer/bryan.csv")
alejo <- read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/leer/alejo.csv")
david <- read_csv("~/AnalisisDatos/Maquinas/DatosFinalesUnidos/leer/david.csv")


#maquinas de soporte

tweetsentrenamientofinal=rbind(alexis,bryan,alejo,david)
entrenamiento<-tweetsentrenamientofinal[,-2]

library(RTextTools)

textoT <- create_matrix(tweetsentrenamientofinal$textoLeer, language="spanish", removeNumbers = TRUE, removePunctuation = TRUE, removeStopwords = TRUE)
container <- create_container(textoT, tweetsentrenamientofinal$polaridad, trainSize=1:NROW(tweetsentrenamientofinal), virgin = FALSE)
model <- train_model(container, "SVM")
