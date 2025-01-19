#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# colcaciones por oferente
# --------
#
library(dplyr)
db.colocaciones_por_oferente=function(){
  setwd(LABORAL_OUTCOMES_HOME)
  readxl::read_excel("laboral outcomes.xlsx" ) %>%
    dplyr::inner_join(readxl::read_excel("laboral outcomes pre covid.xlsx") %>%
                        dplyr::mutate(doc_num=pre_doc_num)) %>%
    dplyr::inner_join(readxl::read_excel("laboral outcomes post covid.xlsx")%>%
                        dplyr::mutate(doc_num=post_doc_num))
}
#
db.colocaciones_por_oferente() %>% View()