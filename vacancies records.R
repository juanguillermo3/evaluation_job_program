# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

# app data
# --------
#

# app services
# --------
#

setwd(CODE_HOME)
source("persistence utils.R", encoding="UTF-8")

setwd(CODE_HOME)
source("dataframe extention.R")

# conf
# --------

setwd(CODE_HOME)
list.files(pattern="sys")
source("sys constants.R")


# vacancies
# --------

#
library(dplyr)
db.vacantes_info_general=function(
  
){
  library(dplyr)
  setwd(REGISTROS_VACANTES_HOME)
  list.files(pattern="Detalle_Vacantes") %>%
    lapply(., function(file_path){
      readxl::read_excel(file_path) 
    }) %>% bind_rows() %>% 
    dplyr::transmute(
      codigo_vacante=.$"Código Proceso",
      cargo=.$"Cargo",
      num_vacantes=.$"Puestos de Trabajo",
      fecha_publicacion=as.Date(.$"Fecha Publicación", format="%d/%m/%Y") 
    ) 
}
#
db.vacantes_info_general() %>% View()


#
#  (1.1) data reader para el texto de las vacantes
#
basic_standardization=function(
  names
){
  #
  tolower(names) %>% iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
}
#
library(dplyr)
datasource.raw_text_from_vacancies=function(
  new_event=FALSE,
  cache_name="raw_vacancies_cache"
  ){
  
  #
  if ( cache_name %in% names(.GlobalEnv) & !new_event ){
    print(sprintf("%s fetched from cache", cache_name))
    return(.GlobalEnv[[cache_name]])
  }
  
  setwd(REGISTROS_VACANTES_HOME)
  list.files(pattern="((info vacantes)|(delivery.+)).csv$") %>%
  lapply( function(some_file){
    some_file %>%
      read.csv(encoding="UTF-8")%>% 
      {names(.)=c("n", "vacante");.} %>% 
      dplyr::select(vacante) %>%
      mutate(z_vacante=basic_standardization(vacante))
  }) %>% bind_rows() %>%
    
    {
      .GlobalEnv[[cache_name]]=.
    } %>%
    return()
}
#
datasource.raw_text_from_vacancies(new_event = TRUE) %>% View()
datasource.raw_text_from_vacancies() %>% View()

#
raw_vacancies.parse=function(
  
  Vacantes_df=datasource.raw_text_from_vacancies(),
  
  feature_markers=c("codigo_vacante"="[[0-9][-]]{5,15}",
                    "salario"=":[^:]*smmlv",
                    "tipo de contrato"="smmlv(.*)distribucion"
  ),
  
  new_event=FALSE,
  cache_name="raw_vacancies_parse_cache"
){
  
  #
  if ( cache_name %in% names(.GlobalEnv) & !new_event ){
    print(sprintf("%s fetched from cache", cache_name))
    return(.GlobalEnv[[cache_name]])
  }
  
    Vacantes_df %>% 
      Dataframe.extract_regex_fields(
        features=feature_markers,
        text_source="z_vacante",
        name_prefix = "",
        standardization=function(x){return(x)}
      ) %>%
    
    #
    #
    {
      print(sprintf("%s not found, but newly created in cache", cache_name))
      .GlobalEnv[[cache_name]]=.
      } %>%
      return()
}
#
raw_vacancies.parse() %>% View()


#
vacantes=
db.vacantes_info_general() %>% 
  #na.omit() %>%
  dplyr::left_join(
    datasource.raw_text_from_vacancies() %>%
      raw_vacancies.parse() %>%
      Dataframe.alter_table_Dataframe_add_primary_key("codigo_vacante")
  ) %>% 
  na.omit()

vacantes %>%
Dataframe.share_with_app(
  label= "vacantes",
  VACANTES_HOME
)
