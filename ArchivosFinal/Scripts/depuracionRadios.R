
library(readr)
TweetsFinal <-
  read_csv(
    "C:/Users/David/Documents/AnalisisDatos/Maquinas/scripts/Nuevos Scripts/TweetsFinalCompleto.csv"
  )


#agregar columna de depurados sin radio
TweetsFinal$depuraSinRadios <- c(rep("", NROW(TweetsFinal)))

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
  texto <- gsub("oxigenowq", "", texto)
  texto <- gsub("wqradioc", "", texto)
  texto <- gsub("wqradio", "", texto)
  texto <- gsub("radiosucr", "", texto)
  texto <- gsub("wqnoticia", "", texto)
  texto <- gsub("pepetoala", "", texto)
  texto <- gsub("lorohomero", "", texto)
  texto <- gsub("laotraradioec", "", texto)
  texto <- gsub("calentamientoradialwq", "", texto)
  TweetsFinal[i, 30] <- texto
}
