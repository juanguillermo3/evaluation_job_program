#
# este servicio dependen de una composicion de las contrataciones observadas y características de la
# demanda de trabajo
#
RUTA_HOME = "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/ruta de empleabilidad" 
#
DEMANDA_DE_TRABAJO_HOME=
  "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/demanda de trabajo"
#
INFO_BASICA_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/info basica"
#
OFERENTES_HOME= "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
#
CONTRATACIONES_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/contrataciones"
#
RESULTADOS_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes resultados"




#
db.cargar_contrataciones=function(
  
){
 
  last=function(a){a[length(a)]}
  Data_Frame.set_names=function(Data_Frame, new_names){
    names(Data_Frame)=new_names
    return(Data_Frame)
  }
  #
  Data_Frame.recast_as_strings=function(Data_Frame){
    Data_Frame=lapply(Data_Frame, function(some_col){
    as.character(some_col)
    }) %>% as.data.frame()
  }
  #
  setwd(CONTRATACIONES_HOME)
  list.files(pattern="contrataciones" ) %>%
    lapply(., function(some_file){
      read.csv(some_file, encoding = "UTF-8") %>%
        Data_Frame.recast_as_strings()
    }) %>%
    bind_rows() %>%
    distinct()  %>%
    transmute(
      doc_num=cedula,  
      proceso=proceso, 
      fecha_limite=fecha_limite,
      tipo_contrato=tipo_contrato,
      salario=rango_salario,
    )
}
#
db.cargar_contrataciones() %>% View()


#
library(dplyr)
db.oferentes_inscritos=function(
  file="oferentes_inscritos.xlsx"
){
  setwd(OFERENTES_HOME)
  readxl::read_excel(file)
}
#
db.oferentes_inscritos() %>% View()
#
people_with_ids_df.genderize=function(
  people_df,
  Oferentes_df=db.oferentes_inscritos()
){
  
  dplyr::left_join(
    people_df,
    Oferentes_df %>%
      dplyr::transmute(
        doc_num=documento,
        gender=ifelse(genero=="F", "mujer", "hombre")
      ),
    by="doc_num"
  )
}
#
people_with_ids_df.genderize(
  db.cargar_contrataciones()
) %>% View()


#
Tabla_de_contrataciones.ensamblar_colocaciones=function(
  Tabla_de_contrataciones=people_with_ids_df.genderize(
    db.cargar_contrataciones()
  ),
  salarios_altos=c("2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV"),
  mejores_contratos=c("Término Indefinido")
){
  
  
  Tabla_de_contrataciones %>% 
    split(.$doc_num) %>%
    lapply(., function(dfxdoc_num){
      dfxdoc_num[1,]
      
      data.frame(
        doc_num=dfxdoc_num$doc_num[1],
        gender=dfxdoc_num$gender[1],
        contrataciones=sum(dfxdoc_num$proceso!="None"),
        salarios_altos=sum(dfxdoc_num$salario %in% salarios_altos) ,
        mejores_contratos=sum(dfxdoc_num$tipo_contrato %in% mejores_contratos))
    
    }) %>% bind_rows()
    
}
#
Tabla_de_contrataciones.ensamblar_colocaciones() %>% View()
#
Tabla_de_contrataciones.ensamblar_colocaciones.resumen=function(
  Tabla_de_contrataciones=people_with_ids_df.genderize(
    db.cargar_contrataciones()
  ),
  salarios_altos=c("2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV"),
  mejores_contratos=c("Término Indefinido"),
  Oferentes_df=db.oferentes_inscritos()
  
){
  
  #
  W= length(unique(Oferentes_df$documento))/length(unique(db.cargar_contrataciones()$doc_num))

    
  #
  pretty_rate=function(x,X){
    round((x/X)*100,2)
  }
  
  Tabla_de_contrataciones.ensamblar_colocaciones(
    Tabla_de_contrataciones=Tabla_de_contrataciones,
    salarios_altos=salarios_altos,
    mejores_contratos=mejores_contratos) %>%
    {
    data.frame(
      pop_size=length(unique(.$doc_num))*W,
      contrataciones=as.integer( sum(.$contrataciones>0)*W),
      salarios_altos=as.integer(sum(.$salarios_altos>0)*W),
      mejores_contratos=as.integer(sum(.$mejores_contratos>0)*W)
    )
    } %>%
    dplyr::transmute(
      pop_size=as.integer(pop_size),
      #
      contrataciones=contrataciones,
      tasa_contrataciones= pretty_rate(contrataciones,pop_size),
      #
      salarios_altos=salarios_altos,
      tasa_salarios_altos= pretty_rate(salarios_altos,pop_size),
      #
      mejores_contratos=mejores_contratos,
      tasa_mejores_contratos= pretty_rate(mejores_contratos, pop_size)
    )
}
#
Tabla_de_contrataciones.ensamblar_colocaciones.resumen() 
#
Tabla_de_contrataciones.ensamblar_colocaciones.resumen(
  salarios_altos=c("2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV")
) 
#
Tabla_de_contrataciones.ensamblar_colocaciones.resumen(
  salarios_altos=c( "1 a 2 SMMLV", "2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV")
)
#
Tabla_de_contrataciones.ensamblar_colocaciones.resumen() 
#
people_with_ids_df.genderize(
  db.cargar_contrataciones()
) %>% 
  split(.$gender) %>% 
  lapply(., function(dfxgender){
    Tabla_de_contrataciones.ensamblar_colocaciones.resumen(
      Tabla_de_contrataciones=dfxgender,
      salarios_altos=c( "1 a 2 SMMLV", "2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV")
    )
  })


#
#
db.read_parametros_demanda_de_trabajo=function(){
  setwd(DEMANDA_DE_TRABAJO_HOME)
  list.files()
  read.csv("parametros vacantes.csv", encoding = "UTF-8")
}
#
db.read_parametros_demanda_de_trabajo() %>% View()

#
Tabla_de_contrataciones.persistir=function(
 Contrataciones_por_oferente=Tabla_de_contrataciones.ensamblar_colocaciones() 
){
  setwd(RESULTADOS_HOME)
  #
  write.csv(Tabla_de_contrataciones, "resultados por oferente.csv", fileEncoding ="UTF-8")
  
}
#
Tabla_de_contrataciones.persistir()

 



