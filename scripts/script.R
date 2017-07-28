library(readr)
TweetsFinal <-
  read_csv("~/AnalisisDatos/Maquinas/ParaCd/TweetsFinalCompleto.csv")

#agregar columna de depurados sin radio
TweetsFinal$depuraSinRadios<-c(rep("",NROW(TweetsFinal)))

#quitar cuentas de radios
for (i in 1:NROW(TweetsFinal)) {
  texto <-
    as.character(tolower(iconv(TweetsFinal[i, 17], "latin1", "UTF-8")))
  texto <- gsub("canelaradioec", "", texto)
  texto <- gsub("labrujaecuador", "", texto)
  texto <- gsub("lOtraradioec", "", texto)
  texto <- gsub("metrostereo", "", texto)
  texto <- gsub("radio_sucre", "", texto)
  texto <- gsub("exafmecuador", "", texto)
  texto <- gsub("radioalfaec", "", texto)
  texto <- gsub("wqradio_ec", "", texto)
  texto <- gsub("radiosucr", "", texto)
  texto <- gsub("wqnoticia", "", texto)
  texto <- gsub("pepetoala", "", texto)
  texto <- gsub("lorohomero", "", texto)
  texto <- gsub("laotraradioec", "", texto)
  texto <- gsub("calentamientoradialwq", "", texto)
  TweetsFinal[i,30] <- texto
}


#filtramos solo polaridad 1
TweetsPositivos <- subset(TweetsFinal, TweetsFinal$polaridadSVM==1)

