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
#
Contrataciones_normalizadas.primera_contratacion=function(
  Tabla_de_contrataciones=db.contrataciones_normalizadas(),
  Mandatory_vars=c("doc_num", "salario", "tipo_contrato")
){
  #
  # Tabla de la primera contratacion por cada cedula
  #
  Tabla_de_contrataciones %>%
    na.omit() %>%
    Dataframe.complain_for_vars(
      mandatory_vars=Mandatory_vars
    ) %>%
    
    split(.$doc_num) %>% 
    lapply(function(sub_df){
      sub_df %>% 
        dplyr::arrange(
          fecha_colocacion
        ) %>%
        head(., 1)
    }) %>%
    bind_rows()
}
Contrataciones_normalizadas.primera_contratacion() %>% View()


#
Dataframe.groupped_rate=function(
  Dataframe,
  rate,
  groupping=NULL,
  formatting=function(rate){
    round(rate*100,3) %>%
      return()
  }
){
  if (is.null(groupping)){
    mean(Dataframe[[rate]]) %>%
      formatting()
  } else {
    Dataframe %>%
      split(
        Dataframe %>%
          dplyr::select(groupping) %>%
          apply(MARGIN=1, function(row){
            paste(row, collapse="-")
            })
        ) %>% 
      lapply(
        function(subdf){
          new_df=
          data.frame(
            subsample_size=nrow(subdf),
            rate=mean(subdf[[rate]]) %>% formatting()
            )
          for (g in groupping){
            new_df[[g]]=subdf[[g]][1]
          }
          new_df
        }
      ) %>%
      bind_rows()
  }  
}

oferentes_df.aging_policy=function(
  dem_model=muestra_cmd.modelo_demografico(),
  age_limits=c(15,seq(20,55,10),Inf)
){
  dem_model %>% 
    dplyr::mutate(
      grupo_edad=cut(edad, age_limits )
    ) 
}
oferentes_df.aging_policy() %>% View()

#
dem_model=muestra_cmd.modelo_demografico() %>%
  oferentes_df.aging_policy() 
#
dem_model.covid_comparison_policy=
  function(
    dem_model=muestra_cmd.modelo_demografico(),
    comparison_half_size_in_days=365/2,
    covid_date=FECHA_INICIO_COVID
  ){
    dem_model %>%
      dplyr::mutate(
        registro_minus_covid=as.numeric(fecha_registro-FECHA_INICIO_COVID)
      ) %>%
      dplyr::mutate(
        covid_comparison_type=
        dplyr::case_when(
          abs(registro_minus_covid)>comparison_half_size_in_days~ NA_character_,
          0 < registro_minus_covid~ "pre-covid",
          0 >= registro_minus_covid~ "post-covid"
        )) %>%
      return()
  }
dem_model.covid_comparison_policy() %>% View()

#
primera_contratacion=
  Contrataciones_normalizadas.primera_contratacion() 
#
experimental_cases= db.experimental_cases()

#
#
dplyr::left_join(experimental_cases, dem_model ) %>% 
  dplyr::left_join(., primera_contratacion ) %>% 
  dem_model.covid_comparison_policy(
    comparison_half_size_in_days = 15
  ) %>% 
  oferentes_df.aging_policy() %>% 
  dplyr::mutate(
    some_placement=ifelse(!is.na(fecha_colocacion), 1, 0)
  ) %>% 
  Dataframe.groupped_rate(
    rate="some_placement",
    groupping = c("genero", "covid_comparison_type") )%>%
  na.omit() %>%
  Dataframe.go_wide_on_gender()

#
#
lapply(0:3, function(num_activities){
  dplyr::left_join(experimental_cases, dem_model ) %>% 
    dplyr::filter(activ_cualquiera>=num_activities) %>% 
    dplyr::left_join(., primera_contratacion ) %>% 
    oferentes_df.aging_policy() %>% 
    dplyr::mutate(
      some_placement=ifelse(!is.na(fecha_colocacion), 1, 0)
    ) %>% 
    Dataframe.groupped_rate(
      rate="some_placement",
      groupping = "genero" ) %>%
      dplyr::mutate(num_activities=num_activities)
}) %>% 
  
  bind_rows() %>%
  na.omit() %>%
  Dataframe.go_wide_on_gender() %>%
  
  Dataframe.export_output(
    "por cantidad de actividades",
    output_home = INSIGHTS_HOME,
    new_output_array = TRUE
  ) 


#
#
dplyr::left_join(experimental_cases, dem_model ) %>% 
  dplyr::filter(activ_cualquiera==0) %>% 
  dplyr::left_join(., primera_contratacion ) %>% 
  oferentes_df.aging_policy() %>% 
  dplyr::mutate(
    some_placement=ifelse(!is.na(fecha_colocacion), 1, 0)
  ) %>% 
  Dataframe.groupped_rate(
    rate="some_placement",
    groupping = c("genero", "estudios") )%>%
  na.omit() %>%
  Dataframe.go_wide_on_gender() %>%
  
  Dataframe.export_output(
    "según nivel educativo",
    output_home = INSIGHTS_HOME
  ) 

#
#
dplyr::left_join(experimental_cases, dem_model ) %>% 
  dplyr::filter(activ_cualquiera==0) %>% 
  dplyr::left_join(., primera_contratacion ) %>% 
  oferentes_df.aging_policy() %>% 
  dplyr::mutate(
    some_placement=ifelse(!is.na(fecha_colocacion), 1, 0)
  ) %>% 
  Dataframe.groupped_rate(
    rate="some_placement",
    groupping = c("genero", "grupo_edad") )%>%
  na.omit() %>%
  Dataframe.go_wide_on_gender() %>%
  
  Dataframe.export_output(
    "según grupo de edad",
    output_home = INSIGHTS_HOME
  ) 


#
Dataframe.go_wide_on_gender=function(
  Dataframe
){
  Dataframe %>% 
    tidyr::pivot_wider(
      names_from = "genero",
      values_from= c("rate", "subsample_size")
    )
}

#
#
dplyr::left_join(dem_model, primera_contratacion ) %>% 
  dplyr::mutate(
    some_placement=ifelse(!is.na(fecha_colocacion), 1, 0)
  ) %>% 
  Dataframe.groupped_rate(
    rate="some_placement",
    groupping = c("grupo_edad","genero") ) %>%
  Dataframe.go_wide_on_gender()
#
dplyr::left_join(dem_model, primera_contratacion ) %>% 
  dplyr::mutate(
    grupo_edad=cut(edad, c(seq(15,55,5),Inf) )
  ) %>% 
  dplyr::mutate(
    some_placement=ifelse(!is.na(fecha_colocacion), 1, 0)
  ) %>% 
  Dataframe.groupped_rate(
    rate="some_placement",
    groupping = c("grupo_edad","genero") ) %>%
  Dataframe.go_wide_on_gender()
#
dplyr::left_join(dem_model, primera_contratacion ) %>% 
  dplyr::mutate(
    grupo_edad=cut(edad, c(15, seq(20, 55, 10),Inf) )
  ) %>% 
  dplyr::filter(
    laboral_states=="desempleado"
  ) %>% 
  dplyr::mutate(
    some_placement=ifelse(!is.na(fecha_colocacion), 1, 0)
  ) %>% 
  Dataframe.groupped_rate(
    rate="some_placement",
    groupping = c("grupo_edad","genero") ) %>%
  Dataframe.go_wide_on_gender()

  split(.$employment_state) %>%
    lapply(function(sub_df){
      sub_df %>%
      Dataframe.groupped_rate(
        rate="some_placement",
        groupping = c("grupo_edad","genero") ) %>%
        Dataframe.go_wide_on_gender()
    }) 

  
  


#
# servicios de analÃ­tica de los outcomes
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
  # dplyr::select(-c("tamaÃ±o_submuestra","num_colocaciones")) %>%
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
  # dplyr::select(-c("tamaÃ±o_submuestra","num_colocaciones")) %>%
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = c("tamano_submuestra", "num_colocaciones","tasa_colocacion")
  ) %>% 
  Dataframe.export_output(
    "por grupo de edad",
    output_home = LABORAL_OUTCOMES_HOME
  ) 
