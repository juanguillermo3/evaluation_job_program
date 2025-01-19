#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# data on postulations
# --------
#

library(dplyr)
db.candidates=function(){
  setwd(CANDIDATES_HOME) 
  readxl::read_excel("candidates_table.xlsx") %>%
     dplyr::mutate(fecha=as.Date(fecha))
}
db.candidates() %>% View()