#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# app services
# --------
#

setwd(CODE_HOME)
source("model employability parameters.R", encoding="UTF-8")

#
# app modules
# --------
#

#
# standardized curriculums
# --------
#


#
db.read_hojas_de_vida=function(){
  setwd(HOJAS_DE_VIDA_HOME)
  readxl::read_excel("hojas de vida.xlsx")
}
#
#db.read_hojas_de_vida() %>% View()

#
Virtual_profiles_df.employability_model=function(
  Virtual_profiles_df=db.read_hojas_de_vida()
){
  
  i=0
  Virtual_profiles_df %>%  
    .[["hoja_de_vida"]] %>%
    #head(., 1000) %>%
    lapply(., function(some_raw_record){
      i<<-i+1
      if(i%%500==0){print(i)}
      try({
      some_raw_record %>%
        Raw_virtual_record.parse() %>%
        Structured_virtual_record.model_employability()
      })
    }) 
}
#
Virtual_profiles_df.employability_model.cache=Virtual_profiles_df.employability_model()
Virtual_profiles_df.employability_model.cache %>% length()

#
Virtual_profiles_df.employability_model.cache2=Virtual_profiles_df.employability_model.cache[
  sapply(Virtual_profiles_df.employability_model.cache, 
         function(x){!("try-error" %in% class(x))}
  )
] %>% bind_rows()
#
Virtual_profiles_df.employability_model.cache2 %>% View()

#
Big_Data_df.model_big_data_covariates=function(
  Big_data_df=Virtual_profiles_df.employability_model.cache2
){
  

  Big_data_df %>% 
    
    dplyr::transmute(
      
      num_documento=num_documento,
      
      experiencia=as.numeric(experiencia),
      
      certificacion=as.numeric(tiempo_certificacion),
      
      idioma_extranjero=ifelse(is.na(Idioma_extranjero), 0, 1),
      
      competencias_Digitales=ifelse(Competencias_Digitales=="", 0, 1),
      
      software=grepl(Competencias_Digitales, pattern="Otros")*1,
      
      empleo_al_registro=ifelse(employed_at_register=="yes", 1, 0) %>%
        {.[is.na(.)]=0;.})
}
#
Big_Data_df.model_big_data_covariates.cache=Big_Data_df.model_big_data_covariates()
Big_Data_df.model_big_data_covariates.cache %>% View()

#
#
Big_Data_df.model_big_data_covariates.cache %>%
  Dataframe.share_with_app(
    label="perfiles laborales",
    app_route = PERFIL_LABORAL_HOME
  )
