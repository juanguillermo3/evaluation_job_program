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
db.experimental_cases=function(){
  setwd(EXPERIMENTAL_CASES_HOME)
  readxl::read_excel("experimental cases.xlsx")
}
#
db.experimental_cases()