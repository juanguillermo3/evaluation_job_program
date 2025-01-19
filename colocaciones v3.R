#
#
# tasas de colocacion
# --------

#
#
MUESTRA_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/muestra cmd"

#
#
library(dplyr)
db.muestra_cmd=function(){
  setwd(MUESTRA_HOME)
  openxlsx::read.xlsx("muestra cmd.xlsx")
}
#
db.muestra_cmd() %>% View()

#
#
muestra_cmd.modelo_demografico=function(
  muestra_cmd=db.muestra_cmd()
){
  muestra_cmd %>%
    #
    # (1)
    #
    dplyr::transmute(doc_num=doc_num,
                     genero=genero,
                     edad=edad,
                     estudios=estudios) %>%
    #
    # (2)
    #
    dplyr::mutate(
      sexo=ifelse(genero=="F", "mujer", "hombre")
    ) %>%
    dplyr::mutate(
      estudios=
        dplyr::case_when(
          estudios %in% c("Preescolar", "Básica Primaria(1-5)", "Básica Secundaria(6-9)")~"hasta basica secundaria",
          estudios %in% c("Media(10-13)")~"secundaria completa",
          estudios %in% c("Técnica Laboral", "Técnica Profesional", "Tecnológica")~"tecnica",
          estudios %in% c("Universitaria", "Especialización", "Maestría", "Doctorado")~"univ o posterior",
        ))
}
#
muestra_cmd.modelo_demografico() %>% View()
#
#
CONTRATACIONES_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/contrataciones"
#
Dataframe.vars_as_character=function(
  Dataframe=muestra_cmd.modelo_demografico()
){
  Dataframe %>% 
    lapply(function(data_col){
      as.character(data_col)
    }) %>%
    as.data.frame()
}
#
Dataframe.vars_as_character() %>% View()
#
#
db.contrataciones_normalizadas=function(){
  
  setwd(CONTRATACIONES_HOME)
  
  list.files(pattern="contrataciones") %>%
    lapply( function(data_file){
      read.csv(data_file, encoding="UTF-8") %>% 
        
        Dataframe.vars_as_character() 
      
    }) %>%  bind_rows() %>%
    
    distinct()  %>%
    
    transmute(
      en_contrataciones=TRUE,
      doc_num=cedula,  
      proceso=proceso, 
      fecha_limite=fecha_limite,
      tipo_contrato=tipo_contrato,
      salario=rango_salario) %>%
    dplyr::arrange(doc_num)
}
#
db.contrataciones_normalizadas() %>% View()


#
#
Dataframe.count_over_factor=function(
  Dataframe=db.contrataciones_normalizadas(),
  counted="tipo_contrato" 
){
  #
  # map Dataframe to a Dataframe that counts values for counted data column
  #
  
  table(Dataframe[[counted]]) %>%
    {
      data.frame(
      value=names(.) %>% as.character(),
      count=as.numeric(.))
    }  %>%
    tidyr::pivot_wider(
      names_from = value,
      values_from=count
    )
}
#
Dataframe.count_over_factor()
#
Dataframe.order=function(
  Dataframe=db.contrataciones_normalizadas(),
  order= c("tipo_contrato", "salario") 
){
  #
  # map Dataframe to a Dataframe that counts values for counted data column
  #
  
  Dataframe %>%
    {
      .[, c(order, setdiff(names(.), order) ) ]
    }
}
#
Dataframe.order() %>% View()



#
#
Contrataciones_normalizadas.ensamblar_colocaciones=function(
  Tabla_de_contrataciones=db.contrataciones_normalizadas()
){
  
  totals_df=
  bind_cols(
  data.frame(doc_num=999999999),
  Dataframe.count_over_factor(Tabla_de_contrataciones, counted="tipo_contrato"),
  Dataframe.count_over_factor(Tabla_de_contrataciones, counted="salario"))
  
  #
  #
  Tabla_de_contrataciones %>% 
    
    #
    #
    split(.$doc_num) %>%
    
    #
    #
    lapply(., function(dfxdoc_num){
      
      bind_cols(
      data.frame(doc_num=dfxdoc_num$doc_num[1]),
      Dataframe.count_over_factor(dfxdoc_num, counted="tipo_contrato"),
      Dataframe.count_over_factor(dfxdoc_num, counted="salario")
      )
      
    }) %>% 
    
    #
    #
    bind_rows() %>%
    
    dplyr::or
  
}
#
Contrataciones_normalizadas.ensamblar_colocaciones() %>% View()






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
