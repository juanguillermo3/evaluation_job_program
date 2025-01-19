#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# app services
# --------
#

setwd(CODE_HOME)
source("genderdize records.R")

#
# custom modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

#
#
# indicadores por oferente
# --------

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
db.contrataciones_normalizadas.provider2=function(){
  NA
}
#
#
# indicadores por oferente
# --------

#
#
Contrataciones_normalizadas.ensamblar_colocaciones=function(
  Tabla_de_contrataciones=db.contrataciones_normalizadas()
){
  
  #
  #
  col_order=
  bind_cols(
  data.frame(doc_num=999999999),
  Dataframe.count_over_factor(Tabla_de_contrataciones, counted="tipo_contrato"),
  Dataframe.count_over_factor(Tabla_de_contrataciones, counted="salario")) %>%
    names()
  
  #
  #
  Tabla_de_contrataciones %>% 
    
    #
    #
    split(.$doc_num) %>%
    
    #
    #
    lapply(., function(dfxdoc_num){
      
      suppressMessages(
      bind_cols(
      data.frame(doc_num=dfxdoc_num$doc_num[1]),
      Dataframe.count_over_factor(dfxdoc_num, counted="tipo_contrato"),
      Dataframe.count_over_factor(dfxdoc_num, counted="salario")
      ))
      
    }) %>% 
    
    #
    #
    bind_rows() %>%
    
    Dataframe.order(col_order) %>%
    
    Dataframe.map_NAS() %>%
    
    Dataframe.apply_valid_names_for_stata()
  
}
#
Contrataciones_normalizadas.ensamblar_colocaciones.cache=
  Contrataciones_normalizadas.ensamblar_colocaciones()


table(Contrataciones_normalizadas.ensamblar_colocaciones.cache$termino_indefinido)


#
#Contrataciones_normalizadas.ensamblar_colocaciones.cache %>% View()
#
#INDICADORES_POR_OFERENTE="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/indicadores por oferente"
#setwd(INDICADORES_POR_OFERENTE)
#openxlsx::write.xlsx(Contrataciones_normalizadas.ensamblar_colocaciones.cache, "indicadores por oferente.xlsx")

#
#
# servicios de analítica (aquí implementar comportamiento para la clase
# de indicadores por oferente)


# serivicio de caracterización demográfica (de muestra de estimacion cmd)
#
#
MUESTRA_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/muestra cmd"
#
library(dplyr)
db.muestra_cmd=function(){
  setwd(MUESTRA_HOME)
  openxlsx::read.xlsx("muestra cmd.xlsx")
}
#
db.muestra_cmd() %>% View()
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
INDICADORES_POR_OFERENTE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/indicadores por oferente"
setwd(INDICADORES_POR_OFERENTE_HOME)
#
library(dplyr)
db.indicadores_por_oferente=function(){
  setwd(INDICADORES_POR_OFERENTE_HOME)
  openxlsx::read.xlsx("indicadores por oferente.xlsx")
}
#
db.indicadores_por_oferente() %>% View()




#
#
Colocaciones_por_oferente.modelo_para_colocationes=function(
  
  Colocaciones_por_oferente=db.indicadores_por_oferente() ,
  
  modelo_demografico=muestra_cmd.modelo_demografico(),
  
  Mandatory_model=c("doc_num", "genero", "1_a_2_smmlv"),
  
  contratos=c(
    "aprendizaje",
    "obra",
    "otro",
    "prest_de_servicios",
    "temporal",
    "termino_fijo",
    "termino_indefinido"),
  #
  # tesitura de la contratacion
  #
  contratos_permanentes=c("termino_indefinido"),
  #
  salarios=c(
    "menos_de_1_smmlv",
    "1_smmlv",
    "1_a_2_smmlv",
    "2_a_4_smmlv",
    "4_a_6_smmlv",
    "6_a_9_smmlv",
    "a_convenir"),
  #
  # tesitura de la contratacion
  #
  salarios_altos=c(
    "2_a_4_smmlv",
    "4_a_6_smmlv",
    "6_a_9_smmlv")
  
){
  
  dplyr::left_join(modelo_demografico, Colocaciones_por_oferente) %>% 
    
    Dataframe.mandatory_model( Mandatory_model) %>%
    
    Dataframe.aggregate(
      aggregated=contratos,
      label="cualquier_contrato"
    ) %>%
    
    Dataframe.aggregate(
      aggregated=contratos_permanentes,
      label="contratacion_permanente"
    ) %>%
    
    Dataframe.aggregate(
      aggregated=salarios,
      label="cualquier_salario"
    ) %>%
    
    Dataframe.aggregate(
      aggregated=salarios_altos,
      label="salarios_altos"
    ) %>%
    
    mutate(
      alguna_colocacion=ifelse(.[["cualquier_salario"]]>0, 1, 0)
    )
  
}
#
Colocaciones_por_oferente.modelo_para_colocationes() %>% View()
#


colocaciones_df.modelo_colocaciones_normalizado() %>% names()
#
#
colocaciones_por_oferente.segun_nivel_eduativo=function(
  colocaciones_por_oferente=Colocaciones_por_oferente.modelo_para_colocationes(),
  estudios_levels=c(
    "hasta basica secundaria",
    "tecnica",
    "secundaria completa",
    "univ o posterior"
  )
){
  colocaciones_por_oferente %>% 
    split(paste(.$sexo,.$estudios)) %>%
      lapply(function(sub_df){
        data.frame(
          sexo=sub_df$sexo[1],
          estudios=sub_df$estudios[1],
          tamaño_submuestra=nrow(sub_df),
          num_colocaciones=sum(sub_df$alguna_colocacion)
        ) %>%
          dplyr::mutate(
            tasa_colocacion=round((num_colocaciones/nrow(sub_df))*100,3)
          )
      }) %>%
    bind_rows() %>%
    na.omit() %>%
    dplyr::mutate(
      estudios=factor(estudios, levels=estudios_levels)) %>%
    dplyr::arrange(sexo, estudios)
}
#
colocaciones_por_oferente.segun_nivel_eduativo()

#
colocaciones_por_oferente.segun_nivel_eduativo() %>%
   dplyr::select(-c("tamaño_submuestra","num_colocaciones")) %>%
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = tasa_colocacion
  ) %>%
  Dataframe.export_output(
    "por nivel educativo",
    output_home = INDICADORES_POR_OFERENTE_HOME
  ) 

#
colocaciones_por_oferente.segun_grupo_edad=function(
  colocaciones_por_oferente=Colocaciones_por_oferente.modelo_para_colocationes(),
  grupos_de_edad=c(15,20,30,40,50,60,70)
){
  colocaciones_por_oferente %>% 
    
    dplyr::mutate(
      grupos_de_edad=cut(edad, breaks=grupos_de_edad , lower=TRUE, rigth=TRUE )
    ) %>%
    
    split(paste(.$sexo,.$grupos_de_edad)) %>%
    lapply(function(sub_df){
      data.frame(
        sexo=sub_df$sexo[1],
        edad=sub_df$grupos_de_edad[1],
        tamaño_submuestra=nrow(sub_df),
        num_colocaciones=sum(sub_df$alguna_colocacion)
      ) %>%
        dplyr::mutate(
          tasa_colocacion=round((num_colocaciones/nrow(sub_df))*100,3)
        )
    }) %>%
    bind_rows() %>%
    na.omit() 
    # dplyr::mutate(
    #   estudios=factor(estudios, levels=estudios_levels)) %>%
    #dplyr::arrange(sexo, estudios)
}
#
colocaciones_por_oferente.segun_grupo_edad() 
#
colocaciones_por_oferente.segun_grupo_edad()  %>%
  dplyr::select(-c("tamaño_submuestra","num_colocaciones")) %>%
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = tasa_colocacion
  ) %>%
  Dataframe.export_output(
    "por grupo de edad",
    output_home = INDICADORES_POR_OFERENTE_HOME
  ) 


#
#
to_pop_size=
  function(x,
           sample_size= Colocaciones_por_oferente.modelo_para_colocationes() %>% nrow(),
           pop_size= informacion_de_control.modelo_de_control() %>% nrow()){
    
    w=pop_size/sample_size
    as.integer(w*x)
  }
#
#
Modelo_colocaciones.totales_por_genero=function(
  Modelo_colocaciones=Colocaciones_por_oferente.modelo_para_colocationes()
){
  Modelo_colocaciones %>% 
    split(.$sexo) %>%
    lapply(., function(gender_df){
      data.frame(sexo=gender_df$"sexo"[[1]], 
                 
                 
                 "tasa de colocacion"=
                   round((sum(gender_df$alguna_colocacion)/nrow(gender_df))*100,3),
                 
                 "tasa de colocacion (altos salarios)"=
                   round((sum(gender_df$"salarios_altos")/nrow(gender_df))*100,3),
                 
                 "tasa de colocacion (permanente)"=
                   round((sum(gender_df$contratacion_permanente)/nrow(gender_df))*100,3)
                 )
      
    }) %>%
    
    bind_rows()
}
#
Modelo_colocaciones.totales_por_genero()


#
Modelo_colocaciones.totales_por_genero() %>% 
  Dataframe.export_output("totales por genero",
                          output_home = INDICADORES_POR_OFERENTE_HOME)



# #
# Tabla_de_contrataciones.ensamblar_colocaciones.resumen=function(
#   Tabla_de_contrataciones=people_with_ids_df.genderize(
#     db.cargar_contrataciones()
#   ),
#   salarios_altos=c("2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV"),
#   mejores_contratos=c("Término Indefinido"),
#   Oferentes_df=db.oferentes_inscritos()
#   
# ){
#   
#   #
#   W= length(unique(Oferentes_df$documento))/length(unique(db.cargar_contrataciones()$doc_num))
#   
#   
#   #
#   pretty_rate=function(x,X){
#     round((x/X)*100,2)
#   }
#   
#   Tabla_de_contrataciones.ensamblar_colocaciones(
#     Tabla_de_contrataciones=Tabla_de_contrataciones,
#     salarios_altos=salarios_altos,
#     mejores_contratos=mejores_contratos) %>%
#     {
#       data.frame(
#         pop_size=length(unique(.$doc_num))*W,
#         contrataciones=as.integer( sum(.$contrataciones>0)*W),
#         salarios_altos=as.integer(sum(.$salarios_altos>0)*W),
#         mejores_contratos=as.integer(sum(.$mejores_contratos>0)*W)
#       )
#     } %>%
#     dplyr::transmute(
#       pop_size=as.integer(pop_size),
#       #
#       contrataciones=contrataciones,
#       tasa_contrataciones= pretty_rate(contrataciones,pop_size),
#       #
#       salarios_altos=salarios_altos,
#       tasa_salarios_altos= pretty_rate(salarios_altos,pop_size),
#       #
#       mejores_contratos=mejores_contratos,
#       tasa_mejores_contratos= pretty_rate(mejores_contratos, pop_size)
#     )
# }
# #
# Tabla_de_contrataciones.ensamblar_colocaciones.resumen() 
# #
# Tabla_de_contrataciones.ensamblar_colocaciones.resumen(
#   salarios_altos=c("2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV")
# ) 
# #
# Tabla_de_contrataciones.ensamblar_colocaciones.resumen(
#   salarios_altos=c( "1 a 2 SMMLV", "2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV")
# )
# #
# Tabla_de_contrataciones.ensamblar_colocaciones.resumen() 
# #
# people_with_ids_df.genderize(
#   db.cargar_contrataciones()
# ) %>% 
#   split(.$gender) %>% 
#   lapply(., function(dfxgender){
#     Tabla_de_contrataciones.ensamblar_colocaciones.resumen(
#       Tabla_de_contrataciones=dfxgender,
#       salarios_altos=c( "1 a 2 SMMLV", "2 a 4 SMMLV", "4 a 6 SMMLV","6 a 9 SMMLV")
#     )
#   })
# 
# 
# #
# #
# db.read_parametros_demanda_de_trabajo=function(){
#   setwd(DEMANDA_DE_TRABAJO_HOME)
#   list.files()
#   read.csv("parametros vacantes.csv", encoding = "UTF-8")
# }
# #
# db.read_parametros_demanda_de_trabajo() %>% View()
# 
# #
# Tabla_de_contrataciones.persistir=function(
#   Contrataciones_por_oferente=Tabla_de_contrataciones.ensamblar_colocaciones() 
# ){
#   setwd(RESULTADOS_HOME)
#   #
#   write.csv(Tabla_de_contrataciones, "resultados por oferente.csv", fileEncoding ="UTF-8")
#   
# }
# #
# Tabla_de_contrataciones.persistir()
