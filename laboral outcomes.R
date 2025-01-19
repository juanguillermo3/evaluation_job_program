#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# confg
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("constants.R")

#
# app services
# --------
#

#
setwd(CODE_HOME)
source("genderdize records.R")

#
setwd(CODE_HOME)
source("demographic model for cmd population.R")

#
# custom modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

setwd(CODE_HOME)
source("persistence utils.R")

setwd(CODE_HOME)
source("utensilios para fechas.R")

#
# laboral outcomes
# --------
#

#
db.contrataciones_normalizadas=function(){
  #
  setwd(CONTRATACIONES_ESTANDARIZADAS_HOME)
  #
  readxl::read_excel("colocaciones estandarizadas.xlsx") %>% 
    
    dplyr::mutate(fecha_colocacion=remediar_fechas(fecha_colocacion, sep="-")) %>% 
    
    dplyr::mutate(fecha_colocacion=as.Date(fecha_colocacion, format="%d-%m-%Y")) 
}
#
db.contrataciones_normalizadas() %>% View()

#
Contrataciones_normalizadas.ensamblar_colocaciones=function(
  Tabla_de_contrataciones=db.contrataciones_normalizadas(),
  Mandatory_vars=c("doc_num", "salario", "tipo_contrato")
){
  #
  # a split-apply-combine operation that changes the scope from placements
  # to people involved in these
  #
  
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
    dplyr::mutate(fecha_colocacion=as.character(fecha_colocacion)) %>%
    Dataframe.map_NAS(mapped_to = "fake") %>%
    Dataframe.complain_for_vars(
      mandatory_vars=Mandatory_vars
    ) %>%
    #
    #
    split(.$doc_num) %>%
    #
    #
    lapply(., function(dfxdoc_num){
      suppressMessages(
        bind_cols(
          data.frame(
            doc_num=dfxdoc_num$doc_num[1],
            num_contrataciones= sum(dfxdoc_num$salario!="fake", na.rm=TRUE)
            ) %>%
            dplyr::mutate(alguna_contratacion=ifelse(num_contrataciones>0,1,0)),
          Dataframe.count_over_factor(dfxdoc_num, counted="tipo_contrato"),
          Dataframe.count_over_factor(dfxdoc_num, counted="salario")
        )) 
    }) %>%
    bind_rows() %>% 
    dplyr::select(c(col_order, num_contrataciones,alguna_contratacion) ) %>%
    Dataframe.order(col_order) %>%
    Dataframe.map_NAS() %>%
    Dataframe.apply_valid_names_for_stata()
}
#

#
# ensamblar contrataciones durante todo el periodo de los datos
#
Contrataciones_normalizadas.ensamblar_colocaciones.cache=
  Contrataciones_normalizadas.ensamblar_colocaciones(
    Tabla_de_contrataciones=db.contrataciones_normalizadas() 
  )
#
Contrataciones_normalizadas.ensamblar_colocaciones.cache %>% View()

#
# ensamblar contrataciones antes de covid
#
Contrataciones_normalizadas.ensamblar_colocaciones_pre_covid.cache=
  Contrataciones_normalizadas.ensamblar_colocaciones(
    Tabla_de_contrataciones=
      db.contrataciones_normalizadas() %>%
      dplyr::filter(fecha_colocacion<FECHA_INICIO_COVID | is.na(fecha_colocacion) )
  )
#
Contrataciones_normalizadas.ensamblar_colocaciones_pre_covid.cache %>%
  View()

#
# ensamblar contrataciones desde despues del covid
#
Contrataciones_normalizadas.ensamblar_colocaciones_post_covid.cache=
  Contrataciones_normalizadas.ensamblar_colocaciones(
    Tabla_de_contrataciones=
      db.contrataciones_normalizadas() %>%
        dplyr::filter(fecha_colocacion>FECHA_INICIO_COVID | is.na(fecha_colocacion) )
  )
#
Contrataciones_normalizadas.ensamblar_colocaciones_post_covid.cache %>% View()

#
Colocaciones_por_oferente.modelo_para_colocationes=function(
  Colocaciones_por_oferente=Contrataciones_normalizadas.ensamblar_colocaciones.cache,
  Mandatory_model=c("doc_num","v_1_a_2_smmlv"),
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
    "v_1_smmlv",
    "v_1_a_2_smmlv",
    "v_2_a_4_smmlv",
    "v_4_a_6_smmlv",
    "v_6_a_9_smmlv",
    "a_convenir"),
  #
  # tesitura de la contratacion
  #
  salarios_altos=c(
    "v_1_a_2_smmlv",
    "v_2_a_4_smmlv",
    "v_4_a_6_smmlv",
    "v_6_a_9_smmlv"
    )
  
){
  Colocaciones_por_oferente %>% 
    Dataframe.mandatory_model( Mandatory_model) %>%
    Dataframe.aggregate(
      aggregated=contratos_permanentes,
      label="contratos_permanentes"
    ) %>%
    dplyr::mutate(
      contratos_permanentes=contratos_permanentes>0
    ) %>%
    Dataframe.aggregate(
      aggregated=salarios_altos,
      label="salarios_altos"
    ) %>%
    dplyr::mutate(
      salarios_altos=salarios_altos>0
    ) %>%
    Dataframe.order(c("doc_num", "alguna_colocacion","contratos_permanentes","salarios_altos"))
}
#
Contrataciones_normalizadas.ensamblar_colocaciones_post_covid.cache  %>%
Colocaciones_por_oferente.modelo_para_colocationes() %>% View()
#

#
colocaciones_por_oferente.persistir=function(
  #
  colocaciones_por_oferente,
  file_name="laboral outcomes.xlsx"
  #
){
  #
  setwd(LABORAL_OUTCOMES_HOME)
  #
  openxlsx::write.xlsx(colocaciones_por_oferente, file_name )
}
#
Contrataciones_normalizadas.ensamblar_colocaciones_pre_covid.cache %>%
  Colocaciones_por_oferente.modelo_para_colocationes() %>%
  Dataframe.prefix("pre_") %>%
  colocaciones_por_oferente.persistir(file_name="laboral outcomes pre covid.xlsx") 
#
Contrataciones_normalizadas.ensamblar_colocaciones_post_covid.cache  %>%
  Colocaciones_por_oferente.modelo_para_colocationes() %>%
  Dataframe.prefix("post_") %>%
  colocaciones_por_oferente.persistir(file_name="laboral outcomes post covid.xlsx") 

#
# servicios de analítica de los outcomes
# --------

#
db.colocaciones_por_oferente=function(){
  setwd(LABORAL_OUTCOMES_HOME)
  readxl::read_excel("laboral outcomes.xlsx" )
}
#
db.colocaciones_por_oferente() %>% View()


#
Modelo_colocaciones.totales_por_genero=function(
  Modelo_colocaciones=db.colocaciones_por_oferente(),
  dem_model=muestra_cmd.modelo_demografico(muestra_cmd=db.pop_cmd())
){
  Modelo_colocaciones %>% 
    dplyr::left_join(dem_model) %>% 
    split(.$sexo) %>%
    lapply(., function(gender_df){
      data.frame(sexo=gender_df$"sexo"[[1]], 
                 "tasa_colocacion"=
                   round((sum(gender_df$alguna_contratacion)/nrow(gender_df))*100,3),
                 "tasa_altos_salarios"=
                   round((sum(gender_df$"salarios_altos")/nrow(gender_df))*100,3),
                 "tasa_colocacion_permanente"=
                   round((sum(gender_df$contratos_permanentes)/nrow(gender_df))*100,3)
      )}) %>%
    bind_rows()
}
#
Modelo_colocaciones.totales_por_genero()
#
Modelo_colocaciones.totales_por_genero() %>% 
  Dataframe.export_output("totales por genero",
                          output_home = LABORAL_OUTCOMES_HOME,
                          new_output_array = TRUE)

#
colocaciones_por_oferente.segun_nivel_eduativo=function(
  colocaciones_por_oferente=db.colocaciones_por_oferente(),
  dem_model=muestra_cmd.modelo_demografico(),
  estudios_levels=c(
    "hasta basica secundaria",
    "tecnica",
    "secundaria completa",
    "univ o posterior"
  )
){
  colocaciones_por_oferente %>% 
    dplyr::left_join(dem_model) %>%
    split(paste(.$sexo,.$estudios)) %>%
      lapply(function(sub_df){
        data.frame(
          sexo=sub_df$sexo[1],
          estudios=sub_df$estudios[1],
          tamano_submuestra=nrow(sub_df),
          num_colocaciones=sum(sub_df$alguna_contratacion)
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
  # dplyr::select(-c("tamaño_submuestra","num_colocaciones")) %>%
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = c("tamano_submuestra", "num_colocaciones","tasa_colocacion")
  ) %>%
  Dataframe.export_output(
    "por nivel educativo",
    output_home = LABORAL_OUTCOMES_HOME
  ) 
#
#
colocaciones_por_oferente.segun_grupo_edad=function(
  colocaciones_por_oferente=db.colocaciones_por_oferente(),
  dem_model=muestra_cmd.modelo_demografico(),
  grupos_de_edad=c(15,20,30,40,50,60,70)
){
  colocaciones_por_oferente %>% 
    dplyr::left_join(dem_model) %>%
    dplyr::mutate(
      grupos_de_edad=cut(edad, breaks=grupos_de_edad , lower=TRUE, rigth=TRUE )
    ) %>%
    
    split(paste(.$sexo,.$grupos_de_edad)) %>%
    lapply(function(sub_df){
      data.frame(
        sexo=sub_df$sexo[1],
        edad=sub_df$grupos_de_edad[1],
        tamano_submuestra=nrow(sub_df),
        num_colocaciones=sum(sub_df$"alguna_contratacion")
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
  # dplyr::select(-c("tamaño_submuestra","num_colocaciones")) %>%
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = c("tamano_submuestra", "num_colocaciones","tasa_colocacion")
  ) %>% 
  Dataframe.export_output(
    "por grupo de edad",
    output_home = LABORAL_OUTCOMES_HOME
  ) 
