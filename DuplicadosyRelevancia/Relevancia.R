#Relevancia

  #Obtenemos los datos
  library(readr)
  radio.sucre <- read_csv("~/radio_sucre _RadioSucre-18-07-2017.csv")
  #Solo tomamos las columnas de usuario y ubicacion
  df.sucre.users=radio.sucre[,c(2,11,13,17)]
  colnames(df.sucre.users)=make.names(c("favoritoCount","retweetCount","Usuario","Localidad"))
  
  #Le damos los nombres- pero al ultimo se borra, no tomemos en cuenta esta parte pero es necesario para definirlo
  #df.sucre.localidad=make.names("Localidad")
  df.sucre.seguidores=make.names("Seguidores")
  df.sucre.seguidos=make.names("Seguidos")
  df.sucre.ratio=make.names("MetricaUser")
  
  
  df.sucre.ratio.tweet=make.names("MetricaTweet")
  df.sucre.ratio.tweet.favourite=make.names("Favoritos")
  df.sucre.ratio.tweet.rt=make.names("Retuits")
  df.sucre.ratio.tweet.ntwets=make.names("Ntweets")

  #recogemos los datos de los usuarios y lo tomamos ya sea para sus seguidores , seguidos y la relacion que hay entre ellos 
  for(i in 1:NROW(df.sucre.users$Usuario)){
    print(df.sucre.users$Usuario[i])
    usuario=df.sucre.users$Usuario[i]
    get.user.info=getUser(usuario)
    #Para primera metrica
    df.sucre.seguidores[i]=followersCount(get.user.info)
    df.sucre.seguidos[i]=friendsCount(get.user.info)
    df.sucre.ratio[i]=as.integer(df.sucre.seguidores[i])/as.integer(df.sucre.seguidos[i])
    
    #Para segunda metrica
    df.sucre.ratio.tweet.favourite[i]=df.sucre.users$favoritoCount[i]
    df.sucre.ratio.tweet.rt[i]=df.sucre.users$retweetCount[i]
    df.sucre.ratio.tweet.ntwets[i]=statusesCount(get.user.info)
    
    
    df.sucre.ratio.tweet[i]=as.integer(df.sucre.ratio.tweet.favourite[i])+as.integer(df.sucre.ratio.tweet.rt[i])/as.integer(df.sucre.ratio.tweet.ntwets[i])
    print(df.sucre.localidad[i])
  }
  
  
  
  
  df.sucre=cbind(df.sucre.users,df.sucre.seguidores,df.sucre.seguidos,df.sucre.ratio,df.sucre.ratio.tweet)
  colnames(df.sucre)=make.names(c("favoritoCount","retweetCount","Usuario","Localidad","Seguidores","Seguidos","Ratio","Ratio2"))
  
