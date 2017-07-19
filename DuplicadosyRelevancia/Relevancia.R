#Relevancia

  #Obtenemos los datos
  library(readr)
  radio.sucre <- read_csv("~/radio_sucre _RadioSucre-18-07-2017.csv")
  #Solo tomamos las columnas de usuario y ubicacion
  df.sucre.users=radio.sucre[,c(13,17)]
  colnames(df.sucre.users)=make.names(c("Usuario","Localidad"))
  
  #Le damos los nombres- pero al ultimo se borra, no tomemos en cuenta esta parte
  #df.sucre.localidad=make.names("Localidad")
  df.sucre.seguidores=make.names("Seguidores")
  df.sucre.seguidos=make.names("Seguidos")
  df.sucre.ratio=make.names("MetricaUser")
  df.sucre.ratio=make.names("MetricaTweet")

  #recogemos los datos de los usuarios y lo tomamos ya sea para sus seguidores , seguidos y la relacion que hay entre ellos 
  for(i in 1:NROW(df.sucre.users$Usuario)){
    print(df.sucre.users$Usuario[i])
    usuario=df.sucre.users$Usuario[i]
    get.user.info=getUser(usuario)
    #df.sucre.localidad[i]=location(get.user.info)
    df.sucre.seguidores[i]=followersCount(get.user.info)
    df.sucre.seguidos[i]=friendsCount(get.user.info)
    df.sucre.ratio[i]=as.integer(df.sucre.seguidores[i])/as.integer(df.sucre.seguidos[i])
    print(df.sucre.localidad[i])
  }
  
  #En este apartado realizamos los nombres de las columnas
  df.sucre.seguidores=make.names("Seguidores")
  df.sucre.seguidos=make.names("Seguidos")
  df.sucre.ratio=make.names("MetricaRadio")
  
  
  df.sucre=cbind(df.sucre.users,df.sucre.seguidores,df.sucre.seguidos,df.sucre.ratio)
  colnames(df.sucre)=make.names(c("Usuario","Localidad","Seguidores","Seguidos","Ratio"))
  
