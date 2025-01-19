#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# perfiles laborales
# --------
#
db.perfiles_laborales=function(){
  setwd(PERFIL_LABORAL_HOME)
  readxl::read_excel("perfiles laborales.xlsx") %>% 
    dplyr::mutate(doc_num=num_documento,
                  competencias_digitales=competencias_Digitales)
}