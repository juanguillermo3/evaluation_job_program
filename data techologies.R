#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# data on technologies
# --------
#

library(dplyr)
db.technologies_in_curriculums=function(){
  setwd(TECNOLOGÍAS_HOME) 
  readxl::read_excel("tech_in_curricumulums.xlsx") 
}
#db.technologies_in_curriculums() %>% View()

technologies_in_curriculums.list_tech=function(
  technologies_in_curriculums=db.technologies_in_curriculums(),
  standardization=tolower
){
  technologies_in_curriculums %>%
    dplyr::select(grep(names(.), pattern="tech_", value = TRUE)) %>%
    lapply(function(tech_var){
      
      table( standardization(tech_var) %>% na.omit()) %>%
        sort(decreasing = TRUE) %>%
        as.data.frame() %>%
        Dataframe.new_names(c("tool", "frequency"))
    })
}
ToolsxType=technologies_in_curriculums.list_tech()
