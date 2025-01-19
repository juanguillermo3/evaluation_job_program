#
# routes
# --------
#
CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# quality control
# --------
#

#
db.informacion_de_control=function(
  
){
  setwd(CONTROL_HOME)
  readxl:::read_excel("validacion_de_registros.xlsx") %>%
    dplyr::select(
      c(
        "doc_num",
        "fecha_actualizacion",
        "es_desempleado",
        "tiene_hoja_completa",
        "fecha_actualizacion2"
      ))
}
#
#db.informacion_de_control() %>% nrow()
#
#db.informacion_de_control() %>% View()
  
#  
informacion_de_control.modelo_de_control=function(
  informacion_de_control_df=db.informacion_de_control(),
  control_paramaters=c("es_desempleado","tiene_hoja_completa")
){
  informacion_de_control_df %>% {
    state_df=.
    for (c in control_paramaters){
      
      # sequantial application of control parameters
      #
      state_df= dplyr::filter(state_df, state_df[[c]] )  
    }
    state_df
  }
}
#
#informacion_de_control.modelo_de_control() %>% nrow()

#
#
Tabla_con_cedulas.aplicar_modelo_de_control=function(
  Tabla_de_oferentes,
  modelo_de_control=informacion_de_control.modelo_de_control()
){
  Tabla_de_oferentes %>%
    dplyr::filter(doc_num %in% modelo_de_control$doc_num )
}
