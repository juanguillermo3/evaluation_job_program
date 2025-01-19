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
setwd(CODE_HOME)
source("quality control.R")
setwd(CODE_HOME)
modelo_de_control=informacion_de_control.modelo_de_control() 
modelo_de_control %>% View()

#
# app modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

#
# people assisted
# --------
#

#
# (1) cargar participaciones en la ruta de empleabilidad
#
library(dplyr)
db.cargar_participaciones=function(){
  setwd(PARTICIPATION_RECORDS_HOME)
  readxl::read_excel("particacion actividades.xlsx")
}
#
db.cargar_participaciones() %>%
  View()
#

#
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
        activ_transversales=sum(doc_df$activ_transversales, na.rm=TRUE)
        )
      
    }) %>%
    
    bind_rows() %>%
    
    Dataframe.aggregate(
      aggregated = c("activ_orientacion", "activ_busqueda", "activ_transversales" ),
      label="activ_cualquiera"
    )
    
}
#
participaciones_df.people_assisted_by_cmd() %>% View()


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
          
          n_submuestra=nrow(sub_df) ,
          
          activ_cualquiera=
            sum(sub_df$activ_cualquiera>0),
            
          activ_busqueda=
            sum(sub_df$activ_busqueda>0) ,
          
          activ_transversales=
            sum(sub_df$activ_transversales>0) ,
          
          activ_orientacion=
            sum(sub_df$ activ_orientacion>0) 
          
        )
      }) %>%
      bind_rows() %>%
      Dataframe.order(c(
                      "sexo",
                      "n_submuestra",
                      "activ_cualquiera",
                      "activ_orientacion",
                      "activ_transversales",
                      "activ_busqueda")
      )
}
#
people_assisted_by_cmd.genderized_overview() %>% View()

#
#
# experimental cases
# --------


#
#
people_assisted_by_cmd.experimental_cases=function(
  people_assisted_by_cmd=participaciones_df.people_assisted_by_cmd()
){
  
  people_assisted_by_cmd %>% 
    
    people_with_ids_df.genderdize() %>% 
    
    dplyr::filter(
      (activ_orientacion > 0) + (activ_busqueda > 0) + (activ_transversales > 0)<2
    ) %>% 
    
    dplyr::mutate(
      activ_orientacion=ifelse(
        (activ_orientacion > 0) & (activ_busqueda==0) & (activ_transversales==0),
        1,
        0
      ))
}
#
people_assisted_by_cmd.experimental_cases() %>% View()

#
experimental_cases.persistir=function(
  experimental_cases=people_assisted_by_cmd.experimental_cases()
){
  setwd(EXPERIMENTAL_CASES_HOME)
  openxlsx::write.xlsx( experimental_cases,"experimental cases.xlsx")
}
#
experimental_cases.persistir()
#
db.experimental_cases=function(){
  setwd(EXPERIMENTAL_CASES_HOME)
  readxl::read_excel("experimental cases.xlsx")
}
db.experimental_cases() %>% View()

#
# analytics
# --------

#
#
setwd(CODE_HOME)
source("dataframe extention.R")
#
experimental_cases.genderized_overview=function(
  experimental_cases=db.experimental_cases(),
  tratamiento=c(
    "trat_cualquiera",
    "trat_orientacion",
    "trat_transversales",
    "trat_busqueda")
){
  experimental_cases %>%
    
    split(.$gender) %>%
    
    lapply(function(sub_df){
      data.frame(
        sexo=sub_df$"gender"[1],
        
        n_submuestra=nrow(sub_df),
        
        grupo_control=
          sum(sub_df$activ_cualquiera==0),
        
        trat_cualquiera=
          sum(sub_df$activ_cualquiera>0),
        
        trat_busqueda=
          sum(sub_df$activ_busqueda>0),
        
        trat_transversales=
          sum(sub_df$activ_transversales>0),
        
        trat_orientacion=
          sum(sub_df$ activ_orientacion>0) 
      )
    }) %>%
    bind_rows() %>%
    Dataframe.order(c("sexo", "n_submuestra", "grupo_control", tratamiento))
}
#
experimental_cases.genderized_overview()

#
#
experimental_cases.overview() %>%
  #
  Dataframe.export_output(
    label="casos experimentales resumen",
    output_home = EXPERIMENTAL_CASES_HOME
  )
#
