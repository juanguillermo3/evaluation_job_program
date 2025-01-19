#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# fechas de registro
# --------
#

#
library(dplyr)
db.fechas_de_registro=function(
  file="oferentes_inscritos.xlsx"
){
  setwd(OFERENTES_HOME)
  readxl::read_excel(file) %>%
    dplyr::transmute(doc_num=documento, fecha_registro=mes_año_registro) %>%
    dplyr::mutate(fecha_registro=stringr::str_extract(fecha_registro, "[^ ]+")) %>%
    dplyr::mutate(
      fecha_registro=as.Date(fecha_registro, format="%d/%m/%Y")
    )
}
#
db.fechas_de_registro() %>% View()