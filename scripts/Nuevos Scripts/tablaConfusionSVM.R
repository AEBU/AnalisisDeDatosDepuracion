library(readr)
library(RTextTools)

#definimos los datos de entrenamiento

TweetsFinalCompleto <- read_csv("~/AnalisisDatos/Maquinas/scripts/Nuevos Scripts/TweetsFinalCompleto.csv")
TweetsFinalCompleto <- TweetsFinalCompleto[1:367,]


#definimos los datos de prueba

x <- subset(x = TweetsFinalCompleto, select = -polaridadSVM)
x$pol <- c(rep("",NROW(x)))
y <- TweetsFinalCompleto$polaridadSVM

#creamos el modelo svm

TweetsFinalCompleto$depurado <- iconv(TweetsFinalCompleto$depurado,"latin1", "UTF-8")
x$depurado <- iconv(x$depurado,"latin1", "UTF-8")

matriz <- create_matrix(TweetsFinalCompleto$depurado,
                        language = "spanish",
                        removeNumbers = TRUE,
                        removePunctuation = TRUE,
                        removeStopwords = TRUE)

contenedor <- create_container(matriz, TweetsFinalCompleto$polaridadSVM,
                               trainSize = 1:367,
                               virgin = TRUE)

model <- train_model(contenedor, "SVM", kernel="radial")

###realizamos la clasificacion con los datos de prueba basandonos en el modelo creado anteriormente

matriz1 <- create_matrix(x$depurado,
                        language = "spanish",
                        removeNumbers = TRUE,
                        removePunctuation = TRUE,
                        removeStopwords = TRUE)

contenedor1 <- create_container(matriz1,x$pol,
                               testSize = 1:367,
                               virgin = TRUE)

clasificacion <- classify_model(contenedor1, model)

###obtenemos la tabla de confusion

table(clasificacion$SVM_LABEL,y)