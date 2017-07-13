# Empleamos la libreria RTextTools, que sirve para la clasificación automática
# de textos usando Algoritmos de Aprendizaje Supervisado 
library(RTextTools)

# Leemos los tweets descargados
tweets <- read.csv("C:/Users/David/Desktop/tweets/labrujaecuador_2017-07-11.csv", header = TRUE, sep=";")

# Detectamos la polaridad de los primeros 20 tweets y los sobrantes ponemos vacio
# Polaridad en dos niveles: 1 - Si el tweet es una petición de canciones de un artista
#                           0 - Si el tweet no es petición de canciones de un artista
pol <- c(0,0,0,0,1,0,0,0,0,1,1,1,1,0,1,0,1,1,1,1,rep(NA,80))

# Añadimos una nueva columna Polaridad al dataframe de tweets
tweets$polaridad <- pol

# Usamos la funcion create_matrix del paquete RTextTools, la cual nos servirá para 
# los pasos siguientes.
# En esta función solo vamos a seleccionar la columna de los tweets, el lenguaje en
# en el que está escrito los tweets, y pedimos que elimine números, elimine signos
# de puntuación y elimine stopwords
textoT <- create_matrix(tweets$text, language="spanish", removeNumbers = TRUE, removePunctuation = TRUE, removeStopwords = TRUE)

# Usamos la función create_container para separar de la matriz anterior, los tweets 
# de entrenamiento y los tweets de prueba y escojemos la variable dependiente 
# que en este caso es la polaridad
container <- create_container(textoT, tweets$polaridad, trainSize=1:20, testSize=21:100, virgin = FALSE)

# Con el comando train_model vamos a crear el modelo SVM
model <- train_model(container, "SVM")

# Con el comando classify_model clasificaremos los tweets de prueba
clasificacion <- classify_model(container, model)

# Añadimos la nueva clasificación en el dataframe
tweets$polaridad[21:100] <- as.numeric(clasificacion$SVM_LABEL)-1

# Visualizamos los tweets y su respectiva polaridad
View(tweets[,c(2,18)])
