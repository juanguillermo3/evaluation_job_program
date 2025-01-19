#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# muestra etiquetada
# --------
#

db.labellled_sample=function(
  
){
  setwd(LABELLED_SAMPLE_HOME)
  #
  readxl::read_excel("labelled sample.xlsx")
}
#db.labellled_sample() %>% View()