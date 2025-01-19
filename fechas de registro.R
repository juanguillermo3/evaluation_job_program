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
setwd(OFERENTES_HOME)

db.oferentes_inscritos=function(
  file="oferentes_inscritos.xlsx"
){
  setwd(OFERENTES_HOME)
  readxl::read_excel(file)
}
#
Num_doc.fecha_registro=function(
  Num_doc="1233902272",
  Oferentes_df.fechas_registro= 
    db.oferentes_inscritos() %>% 
    dplyr::transmute(
      num_documento=documento,
      fecha_registro= lubridate::dmy( stringr::str_extract(mes_año_registro, "[0-9/]+"))
    ) 
){
  
  Oferentes_df.fechas_registro %>%
    dplyr::filter(num_documento==Num_doc) %>%
    .[["fecha_registro"]]
  
}
#
Num_doc.fecha_registro()