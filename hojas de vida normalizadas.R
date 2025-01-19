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

setwd(CODE_HOME)
source("persistence utils.R")

#
# normalized virtual profiles
# --------
#

#
db.from_files_data_reader_virtual_profiles=function(
  sample_prefix="scraper delivery",
  encoding="UTF-8"
){
  #
  #
  library(dplyr)
  setwd(RUTA_HOME)
  list.files(pattern=sample_prefix) %>%
    lapply(., function(some_csv){
      read.csv(some_csv,) %>% 
        Dataframe.vars_as_character() 
    }) %>% bind_rows() %>%
    Dataframe.reencode()
}
#
db.from_files_data_reader_virtual_profiles_cache=db.from_files_data_reader_virtual_profiles() 
db.from_files_data_reader_virtual_profiles_cache %>% View()

#
Virtual_profiles_df.normalized_cv_model=function(
  Virtual_profiles_df=db.from_files_data_reader_virtual_profiles_cache
){
  Virtual_profiles_df %>% 
    transmute(
      num_documento=as.character(cedula),
      hoja_de_vida=cv
    ) %>%
    dplyr::filter(!duplicated(.$num_documento))
}
#
Virtual_profiles_df.normalized_cv_model() %>% View()

#
Virtual_profiles_df.normalized_cv_model() %>%
  Dataframe.share_with_app(label="hojas de vida",
                           app_route=HOJAS_DE_VIDA_HOME)










###
#
Virtual_profiles_df.model_virtual_cv.cache=Virtual_profiles_df.model_virtual_cv()
#
#
Textual_feature.basic_standardization=function(
  names
){
  #
  tolower(names) %>%
    iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
  #
}
#
Standardized_records_df.estandarizar_fechas=function(
  Standardized_records_df=ruta_de_empleabilidad.Session_records.Standardize.cache
){
  Standardized_records_df %>%
    mutate(
      z_fecha=sapply(fecha, function(some_fecha){
        
        if(!is.na(some_fecha)){
          
          paste(
            unlist(stringr::str_extract_all(some_fecha, "[0-9]{2}"))[1],
            unlist(stringr::str_extract_all(some_fecha, "[0-9]{2}"))[2],
            stringr::str_extract(some_fecha, "[0-9]{4}"), sep="-")
          
        } else {
          return(NA)
        }
      })
    ) %>% 
    
    mutate(z_fecha=lubridate::dmy(z_fecha)) 
}
#
Standardized_records_df.estandarizar_fechas() %>% View()
Standardized_records_df.estandarizar_fechas.cache=Standardized_records_df.estandarizar_fechas()
#
#
db.oferentes_inscritos=function(
  file="oferentes_inscritos.xlsx"
){
  setwd(OFERENTES_HOME)
  readxl::read_excel(file)
}
#
Num_doc.fecha_registro=function(
  Num_doc="1233902272",
  Oferentes_df.fechas_registro= 
    db.oferentes_inscritos() %>% 
    dplyr::transmute(
      num_documento=documento,
      fecha_registro= lubridate::dmy( stringr::str_extract(mes_año_registro, "[0-9/]+"))
    ) 
){
  
  Oferentes_df.fechas_registro %>%
    dplyr::filter(num_documento==Num_doc) %>%
    .[["fecha_registro"]]
  
}
#
Num_doc.fecha_registro()


#
setwd(CODE_HOME)
source("virtual profiles.R", 
       encoding = "UTF-8",
       local=FALSE)



#
#
Big_Data_df.model_big_data_covariates=function(
  Big_data_df=Virtual_profiles_df.employability_model.cache2
){
  
  #
  Education_model=list(
    "no_informado"=c("No Informado"),
    "sin bachillerato"=c("Preescolar", "Básica Primaria" , "Básica Secundaria" ),
    "bachillerato o técnico"= c("Media", "Técnica Laboral", "Técnica Profesional", "Tecnológica" ),
    "universitario o superior"= c("Universitaria", "Especialización", "Maestría","Doctorado"  ))
  #
  unlist(Education_model) %in% df$nivel_estudio
  #
  Big_data_df %>% 
    dplyr::transmute(
      num_documento=num_documento,
      
      nivel_estudio,
      aspiracion_salarial= aspiracion_salarial %>%
        {.[is.na(.)]="A convenir";.},
      
      fecha_registro=fecha_participacion_programa,
      
      experiencia=as.numeric(experiencia),
      
      certificacion=as.numeric(tiempo_certificacion),
      
      idioma_extranjero=ifelse(is.na(Idioma_extranjero), 0, 1),
      
      competencias_Digitales=ifelse(Competencias_Digitales=="", 0, 1),
      
      software=grepl(Competencias_Digitales, pattern="Otros")*1,
      
      empleo_al_registro=ifelse(employed_at_register=="yes", 1, 0) %>%
        {.[is.na(.)]=0;.}
      
    ) %>% 
    
    dplyr::mutate(
      nivel_estudio=stringr::str_replace(.$nivel_estudio, pattern="^[^:]+", replacement="") %>%
        stringr::str_replace(., pattern="[:]", replacement="") %>% trimws(),
      aspiracion_salarial=stringr::str_replace(.$aspiracion_salarial, pattern="^[^:]+", replacement="")%>%
        stringr::str_replace(., pattern="[:]", replacement="") %>% trimws(),
    ) %>%  {
      for (feature in names(Education_model)){
        .[[feature]]=(.[["nivel_estudio"]] %in% Education_model[[feature]])*1
      };.} 
  
}
#
Big_Data_df.model_big_data_covariates.cache=Big_Data_df.model_big_data_covariates()
Big_Data_df.model_big_data_covariates.cache %>% View()


#
#
Virtual_profiles.Virtual_profiles_df.Ensemble_features=function(
  Records=Virtual_profiles_df.model_virtual_cv.cache,
  Information_channel="hoja_de_vida",
  feature_markers=c("Edad", 
                    "Estado civil",
                    "Nivel educativo",
                    "Núcleo básico de conocimiento (NBC):",
                    "Estado",
                    "Años de experiencia",
                    "Aspiración salarial")
){
  #
  #
  list(
    num_documento=as.character(Records$num_documento),
    
    Records[[Information_channel]] %>%
      lapply(., function(some_record){
        sapply(feature_markers, function(some_marker){
          an=stringr::str_extract(some_record, sprintf("%s: [^[:punct:]]*", some_marker) )
          an=stringr::str_replace(an, sprintf("%s: ", some_marker), "" )
        })
      }) %>% bind_rows() ) %>% bind_cols() 
}
#
Virtual_profiles.Virtual_profiles_df.Ensemble_features() %>% View()


