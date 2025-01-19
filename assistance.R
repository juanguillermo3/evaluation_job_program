#
# routes
# --------
#

rm(list=ls())
CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

# app services
# --------
#

setwd(CODE_HOME)
source("genderdize records.R")

setwd(CODE_HOME)
source("data quality control.R")
db.informacion_de_control() %>% View()


#
# app modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")
#

setwd(CODE_HOME)
source("persistence utils.R")

#
# people assisted
# --------
#


#
library(dplyr)
db.cargar_actividades=function(){
  setwd(ACTIVITIES_HOME)
  readxl::read_excel("actitivites_dataset.xlsx")
}
#
db.cargar_actividades() %>%
  View()
#

#
participaciones_df.filtrar_por_minuto_de_dios=function(
  participaciones_df=db.cargar_actividades()
){
  participaciones_df %>%
    dplyr::filter(prest_cmd==1 )
}
participaciones_df.filtrar_por_minuto_de_dios() %>% View()

#
participaciones_del_minuto_de_dios.last_participation=
  function(
    participaciones_del_minuto_de_dios=participaciones_df.filtrar_por_minuto_de_dios(),
    
    fechas_registro= 
      db.informacion_de_control() %>%
      dplyr::transmute(
        doc_num=doc_num,
        fecha_registro=
          fecha_actualizacion2 %>% stringr::str_extract("[^ ]+") %>% as.Date(format="%Y-%m-%d") %>% as.POSIXct())
  ){
    participaciones_del_minuto_de_dios %>%
      dplyr::arrange(
        fecha
      ) %>%
      dplyr::left_join(
        fechas_registro
      ) %>% 
      dplyr::transmute(
        doc_num=doc_num,
        fecha_ultima_actividad=ifelse(!is.na(fecha),
                                      fecha %>% as.character(), 
                                      fecha_registro %>% as.character()) 
      ) %>%
      
      split(.$doc_num) %>%
      lapply(function(doc_df){
        data.frame(
          doc_num=dplyr::first(doc_df$doc_num),
          registro=min(doc_df$fecha_ultima_actividad, na.rm=TRUE),
          ultima_actividad=max(doc_df$fecha_ultima_actividad,na.rm=TRUE)
        )
      }) %>%
      bind_rows()
  }
participaciones_del_minuto_de_dios.last_participation() %>% View()
#
df=participaciones_del_minuto_de_dios.last_participation()
df %>%
  Dataframe.share_with_app(
    label="fecha_ultima_asistencia",
    app_route = ASSISTANCE_HOME
  )
  


#
self_as_names=function(x){names(x)=x;x}
#
participaciones_df.principales_actividades=function(
  participaciones_df=
    participaciones_df.filtrar_por_minuto_de_dios(),
  standardizing_activities_names=function(
    name
  ){
    name %>% tolower() 
  }
){
  
  new_df=
  participaciones_df %>% 
    dplyr::mutate(
      nombre_actividad=standardizing_activities_names(nombre)
    ) %>%
    Dataframe.count_values("nombre_actividad") %>%
    Dataframe.new_names(c("actividad", "frecuencia")) %>%
    dplyr::arrange(frecuencia*(-1)) %>%
    return()
}
#
participaciones_df.principales_actividades() %>% View()
#
participaciones_df.mine_keywords=function(
  participaciones_df=
    participaciones_df.filtrar_por_minuto_de_dios(),
  standardizing_activities_names=function(
    name
  ){
    name %>% tolower() 
  },
  keywords=c("orientación","competencias", "búsqueda" ) %>% self_as_names()
){
  #
  new_df=
    participaciones_df %>% 
    dplyr::mutate(
      nombre_actividad=standardizing_activities_names(nombre)
    ) %>%
    Dataframe.count_values("nombre_actividad") %>%
    Dataframe.new_names(c("actividad", "frecuencia")) %>%
    dplyr::arrange(frecuencia*(-1)) 
  #
  lapply(keywords, function(keyword){
    new_df %>% 
      dplyr::filter( stringr::str_detect( actividad, keyword)) %>%
      Dataframe.insert(c( keyword, sum(.$frecuencia))) %>%
      dplyr::arrange(desc(frecuencia))
  })  %>%
    return()
}
#
participaciones_df.mine_keywords() %>% View()

#
participaciones_df.people_assisted_by_cmd=function(
  participaciones_df=db.cargar_participaciones()
){
  participaciones_df %>% 
    dplyr::filter(prest_cmd*prest_asistencia==1) %>% 
    split(.$doc_num) %>%
    lapply(function(doc_df){
      
      data.frame(
        doc_num=doc_df$doc_num[1],
        activ_orientacion=sum(doc_df$activ_orientacion, na.rm=TRUE), 
        activ_busqueda=sum(doc_df$activ_busqueda, na.rm=TRUE), 
        activ_transversales=sum(doc_df$activ_transversales, na.rm=TRUE),
        activ_totales=nrow(doc_df)-1)
    }) %>%
    bind_rows()
}
#
participaciones_df.people_assisted_by_cmd() %>% View()

#
#
people_assisted_by_cmd.genderized_overview=function(
  people_assisted_by_cmd=participaciones_df.people_assisted_by_cmd()
){
  people_assisted_by_cmd %>% 
   
    people_with_ids_df.genderdize() %>% 
    split(.$gender) %>%
      lapply(function(sub_df){
        data.frame(
          sexo=sub_df$"gender"[1],
          
          n_submuestra=nrow(sub_df),
          
          activ_cualquiera=
            sum(sub_df$activ_totales>0),
            
          activ_busqueda=
            sum(sub_df$activ_busqueda>0),
          
          activ_transversales=
            sum(sub_df$activ_transversales>0),
          
          activ_orientacion=
            sum(sub_df$ activ_orientacion>0)
          
        )
      }) %>%
      bind_rows() %>%
      Dataframe.order(c(
                      "sexo",
                      "n_submuestra",
                      "activ_totales",
                      "activ_orientacion",
                      "activ_transversales",
                      "activ_busqueda")
      )
}
#
people_assisted_by_cmd.genderized_overview() %>% View()
#
df=participaciones_df.people_assisted_by_cmd()
df %>%
  Dataframe.share_with_app(
    label="assistance_dataset",
    app_route =ASSISTANCE_HOME
  )

#
# experimental cases
# --------

#
assistance.summary=function(
  assistance=participaciones_df.people_assisted_by_cmd()
){
  assistance %>%
    people_with_ids_df.genderdize() %>% 
    split(.$gender) %>%
    lapply(function(sub_df){
      data.frame(
        sexo=sub_df$"gender"[1],
        n_submuestra=nrow(sub_df) ,
        grupo_control=
          sum(sub_df$activ_totales==0) ,
        trat_cualquiera=
          sum(sub_df$activ_totales>0) ,
        trat_busqueda=
          sum(sub_df$activ_busqueda>0) ,
        trat_transversales=
          sum(sub_df$activ_transversales>0) ,
        trat_orientacion=
          sum(sub_df$ activ_orientacion>0) 
      )
    }) %>%
    bind_rows()
}
#
assistance.summary()

#
#
assistance.summary() %>%
  #
  Dataframe.export_output(
    label="asistencia_de_la_intermediacion_laboral",
    output_home = ASSISTANCE_HOME
  )
#

