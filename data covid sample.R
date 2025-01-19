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

db.covid_sample=function(){
  setwd(COVID_DESIGN_HOME) 
  readxl::read_excel("muestra covid.xlsx")
}
db.covid_sample()