#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# tabla de vacantes
# --------
#

#
#
db.read_parametros_de_contratacion=function(
  
){
  setwd(DEMANDA_DE_TRABAJO_HOME)
  #
  read.csv("parametros vacantes.csv") %>%
    dplyr::mutate(
      proceso=codigo_vacante,
      tipo_contrato=tipo_de_contrato)
}
#
db.read_parametros_de_contratacion() %>% View()

#
#
Tabla_de_vacantes.incorporar_parametros=function(
  Tabla_de_vacantes,
  parametros_de_contratacion=db.read_parametros_de_contratacion()
){
    dplyr::left_join(
      Tabla_de_vacantes,
      parametros_de_contratacion %>%
      dplyr::select(c("proceso", "salario", "tipo_contrato"))
    )
}
#
#Tabla_de_vacantes.incorporar_salarios() %>% View()

