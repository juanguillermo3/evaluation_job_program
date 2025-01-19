#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# app modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

UNBALANCED_SAMPLE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/unbalanced sample"
setwd(UNBALANCED_SAMPLE_HOME)

list.files()

library(dplyr)
setwd(UNBALANCED_SAMPLE_HOME)
openxlsx::read.xlsx("unbalanced sample.xlsx") %>% View()

CONTRATACIONES_ESTANDARIZADAS_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/contrataciones estandarizadas"
setwd(CONTRATACIONES_ESTANDARIZADAS_HOME)
list.files()

setwd(LABORAL_OUTCOMES_HOME)
readxl::read_excel("laboral outcomes.xlsx" ) %>% View()

lo=readxl::read_excel("laboral outcomes.xlsx" )
 
setwd(UNBALANCED_SAMPLE_HOME)
us=openxlsx::read.xlsx("unbalanced sample.xlsx")

ls=dplyr::left_join(us, lo) %>% 
  Dataframe.apply_valid_names_for_stata() 

#
setwd(LABELLED_SAMPLE)
haven::write_dta(ls, "labelled sample.dta")