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
  read.csv("parametros vacantes.csv")
}
#
db.read_parametros_de_contratacion() %>% View()

#
#
Tabla_de_vacantes.incorporar_salarios=function(
  Tabla_de_vacantes,
  parametros_de_contratacion=db.read_parametros_de_contratacion()
){
  Tabla_de_vacantes
}
#
Tabla_de_vacantes.incorporar_salarios() %>% View()

