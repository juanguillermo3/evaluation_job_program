#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# estimation matrix
# --------
#

#
db.read_labelled_sample=function(){
  setwd(LABELLED_SAMPLE_HOME)
  readxl::read_excel("labelled sample.xlsx")
}
#
db.read_labelled_sample() %>% View()

#
unbalanced_sample.estimation_task=function(
  unbalanced_sample=
    db.read_labelled_sample() %>%
      dplyr::left_join(covid_design())
){
  unbalanced_sample  %>% 
    
    mutate(es_participante=ifelse(activ_cualquiera>0,1,0 )) %>%
    
    dplyr::mutate(
      mujer=as.numeric(sexo=="mujer"),
    ) %>% 
    
    Dataframe.expand_regex_features(
      text_source="estudios",
      name_prefix="est_"
    ) %>% 
    
    Dataframe.expand_regex_features(
      text_source="laboral_states",
      name_prefix="lab_"
      
    ) }


#
unbalanced_sample.estimation_task.cache=unbalanced_sample.estimation_task()
#
setwd(MATRIZ_EVALUACION_HOME)
haven::write_dta(unbalanced_sample.estimation_task.cache %>%
                   Dataframe.apply_valid_names_for_stata(),
                 "matriz de evaluacion.dta")

