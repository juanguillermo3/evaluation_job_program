#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# data on placements
# --------
#

#
setwd(CODE_HOME)
source("utensilios para fechas.R", encoding="UTF-8") 
#
db.placements=function(){
  #
  setwd(CONTRATACIONES_ESTANDARIZADAS_HOME)
  #
  readxl::read_excel("colocaciones estandarizadas.xlsx") %>% 
    
    dplyr::mutate(fecha_colocacion=remediar_fechas(fecha_colocacion, sep="-")) %>% 
    
    dplyr::mutate(fecha_colocacion=as.Date(fecha_colocacion, format="%d-%m-%Y")) 
}
db.placements() %>% View()