#Descargar datos

  radio.sucre <- searchTwitter("radio_sucre", n=128,since = "2017-07-18")
  df.sucre <- twListToDF(radio.sucre)
  df.sucre <- df.sucre[, order(names(df.sucre))]
  df.sucre.localidad=make.names("Localidad")
  for(i in 1:NROW(df.sucre$screenName)){
    print(df.sucre$screenName[i])
    usuario=df.sucre$screenName[i]
    get.user.info=getUser(usuario)
    df.sucre.localidad[i]=location(get.user.info)
    print(df.sucre.localidad[i])
  }
  df.sucre=cbind(df.sucre,df.sucre.localidad)
  if (file.exists(paste("radio_sucre", '_RadioSucre-18-07-2017.csv'))==FALSE) write.csv(df.sucre, file=paste("radio_sucre", '_RadioSucre-18-07-2017.csv'), row.names=F)
  
  
  
#Recuperamos el archivo
  library(readr)
  radio.sucre1 <- read_csv("~/radio_sucre _RadioSucre-18-07-2017.csv")
  View(radio.sucre1)
#Como elmiinar duplicados


  #Verificamos cuantas filas tiene la radio
  
  NROW(radio.sucre1)
  
  #Ordenamos por nombre
  
  radio.sucre1$text<- radio.sucre1$text[ order((radio.sucre1$text))]
  #Remover duplicados
  radio.sucre1=radio.sucre1[!duplicated(radio.sucre1[c("text")]),]
  #Verificamos
  NROW(radio.sucre1)
  