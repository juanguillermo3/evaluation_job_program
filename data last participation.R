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
db.last_participation=function(
  file="last participation.xlsx"
){
  setwd(PEOPLE_ASSISTED)
  readxl::read_excel(file) %>%
    return()
}
#
db.last_participation() %>% View()