#download tweets with location

radio.sucre <- searchTwitter("radio_sucre", n=242,since = "2017-07-17")
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
if (file.exists(paste("radio_sucre", '_RadioSucre-17-07-2017.csv'))==FALSE) write.csv(df.sucre, file=paste("radio_sucre", '_RadioSucre-17-07-2017.csv'), row.names=F)

