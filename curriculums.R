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

# curriculums
# --------


#
from_datasource.raw_curriculums=function(
  sample_prefix="scraper delivery",
  
  new_event=FALSE,
  cache_name="raw_curriculums"
){
  
  #
  if ( cache_name %in% names(.GlobalEnv) & !new_event ){
    print(sprintf("%s fetched from cache", cache_name))
    return(.GlobalEnv[[cache_name]])
  }
  
  #
  library(dplyr)
  setwd(REGISTROS_RUTA_HOME)
  list.files(pattern=sample_prefix) %>%
    #head(500) %>%
    lapply(., function(some_csv){
      read.csv(some_csv) %>%
      Dataframe.vars_as_character()
    }) %>% bind_rows()  %>%
    Dataframe.reencode() %>%
    
    {
      .GlobalEnv[[cache_name]]=.
    } %>%
    return()
}
#
from_datasource.raw_curriculums(new_event = TRUE) %>% View()
from_datasource.raw_curriculums() %>% View()

#
raw_curriculums.curation=function(
  raw_curriculums=from_datasource.raw_curriculums()
){
  raw_curriculums %>% 
    transmute(
      doc_num=as.character(cedula),
      hoja_de_vida=cv
    ) %>%
    Dataframe.alter_table_Dataframe_add_primary_key("doc_num")
}
#
raw_curriculums.curation() %>% View()

#
Dataframe.fetch=function(
  Dataframe,
  fetched
){
  Dataframe %>% 
    {
      Dataframe[[fetched]]
    }
}
#
raw_curriculums.curation() %>% 
  Dataframe.fetch("hoja_de_vida") %>%
  head(12)
#
curriculums.information_technologies_features=function(
  curriculums=raw_curriculums.curation(),
  features_markers_for_information_technologies=
    list(
        "procesador_de_texto"="(?<=Word(.){1,4}Herramienta:)[a-zA-Z ]+",
        "hojas_de_calculo"="(?<=Excel(.){1,4}Herramienta:)[a-zA-Z ]+",
        "multimedia"="(?<=PowerPoint(.){1,4}Herramienta:)[a-zA-Z ]+",
        "otros"="(?<=Otros(.){1,4}Herramienta:)[a-zA-Z ]+"
        ),
  
  new_event=FALSE,
  cache_name="raw_technologies"
){
  
  #
  if ( cache_name %in% names(.GlobalEnv) & !new_event ){
    print(sprintf("%s fetched from cache", cache_name))
    return(.GlobalEnv[[cache_name]])
  }
  curriculums %>%
    Dataframe.extract_regex_fields(
      text_source="hoja_de_vida",
      features = features_markers_for_information_technologies,
      name_prefix="tech_",
      standardization = function(x){x}
    )%>%
    {
      .GlobalEnv[[cache_name]]=.
    } %>%
    return()
}
#
curriculums.information_technologies_features(
  curriculums=raw_curriculums.curation() %>% head(30)
) %>% View()
#
information_technologies=
  curriculums.information_technologies_features(
    curriculums=raw_curriculums.curation(),
    new_event=TRUE) 
information_technologies %>% View()
#
information_technologies  %>%
Dataframe.share_with_app(
  label="tech_in_curricumulums",
  app_route = TECNOLOGÍAS_HOME
)




#
Virtual_profiles.Records_of_virtual_profiles.sample=function(
  Records=Virtual_profiles_df.model_virtual_cv.cache,
  k=6
  
){
  sapply(sample(Records$hoja_de_vida, k ), function(some_example){
    print( trimws(some_example))
  })
}
#
Virtual_profiles.Records_of_virtual_profiles.sample()



#
#
library(dplyr)
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


# #
# #
# Digital_skills_df=
# Virtual_profiles_df.Ensemble_features(
#   Records=Virtual_profiles_df.model_virtual_cv(),
#   Information_channel = "hoja_de_vida",
#   feature_markers = c(
#     "Procesador de texto (Ejemplo: Word)\\nHerramienta",
#     "Hoja de cálculo (Ejemplo: Excel)\\nHerramienta:",
#     "Otros\\nHerramienta")) %
# #
# Digital_skills_df %>% View()
# #
# #
# Virtual_profiles.Virtual_profiles_df.Ensemble_features(
#   Records=Virtual_profiles_df.model_virtual_cv(),
#   Information_channel = "hoja_de_vida",
#   feature_markers = c(
#   "Procesador de texto (Ejemplo: Word)\\nHerramienta",
#   "Hoja de cálculo (Ejemplo: Excel)\\nHerramienta:",
#   "Otros\\nHerramienta")
# ) %>% View()
# 
# 
# 
# 
# #
# #
# library(dplyr)
# Ensemble_features=function(
#   Records= dplyr::sample_n(db.read_virtual_profiles(), 5),
#   Information_channel="curriculum_vitae",
#   feature_markers=c("Edad", 
#                     "Estado civil",
#                     "Nivel educativo",
#                     "Estado",
#                     "Años de experiencia",
#                     "Aspiración salarial")
# ){
#   #
#   #
#   
#   list(
#   doc_num=as.character(Records$x),
#   
#   Records[[Information_channel]] %>%
#     lapply(., function(some_record){
#       sapply(feature_markers, function(some_marker){
#         an=stringr::str_extract(some_record, sprintf("%s: [^[:punct:]]*", some_marker) )
#         an=stringr::str_replace(an, sprintf("%s: ", some_marker), "" )
#       })
#     }) %>% bind_rows() ) %>% bind_cols() 
# }
# #
# Ensemble_features() %>% View()
# 
# 
# 
# 
# 
# #
# #
# Virtual_profiles_df.infomation_completeness=function(
#   Virtual_profiles_df=db.read_virtual_profiles() %>% Ensemble_features()
# ){
#   
#   sapply(Virtual_profiles_df, function(data_col){
#     round(mean(is.na(data_col)*100), 3)
#   })
#   
# }
# #
# Virtual_profiles_df.infomation_completeness(
#   
#   db.read_virtual_profiles() %>% 
#     
#     Ensemble_features(
#       feature_markers=c("Edad", 
#                         "Estado civil",
#                         "Nivel de estudio",
#                         "Estado",
#                         "Años de experiencia",
#                         "Aspiración salarial")
#     ) 
# )
# 
# 
# df=
# db.read_virtual_profiles() %>% 
#   
#   Ensemble_features(
#     feature_markers=c("Edad", 
#                       "Estado civil",
#                       "Nivel de estudio",
#                       "Estado",
#                       "Años de experiencia",
#                       "Aspiración salarial")
#   ) 
# 
# round(prop.table(table(df$`Nivel de estudio`))*100, 3)
# 
# db.read_virtual_profiles() %>% Ensemble_features() %>% View()
# 
# 
# #
# #
# OFERENTES_HOME= "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
# #
# library(dplyr)
# db.oferentes_inscritos=function(
#   file="oferentes_inscritos.xlsx"
# ){
#   setwd(OFERENTES_HOME)
#   readxl::read_excel(file)
# }
# #
# db.oferentes_inscritos() %>% View()
# 
# db.read_virtual_profiles() %>% View()
# #
# #
# Virtual_profiles_df.age_model=function(
#   Virtual_profiles_df=db.read_virtual_profiles() %>% Ensemble_features(),
#   Oferentes_df=db.oferentes_inscritos(),
#   Age_models=list("sample wide"=c(0,1), "sample halves"=c(0,.5,1), "sample quarters"=c(0,.25, .5, .75, 1) )
# ){
#   
#   #
#   #
#   Oferentes_df.gender=
#   Oferentes_df %>%
#     dplyr::transmute(
#       doc_num=documento,
#       sex=ifelse( genero=="M", "hombre", "mujer"))
#   
#   #
#   #
#   Virtual_profiles_df=dplyr::left_join(Virtual_profiles_df, Oferentes_df.gender, by="doc_num" )
#   
#   
#   #
#   #
#   curated_sample=
#   Virtual_profiles_df %>% 
#     lapply(., function(some_data_col){
#       some_data_col[is.na(some_data_col)]="unk"
#       some_data_col
#     }) %>% as.data.frame() %>% 
#     Dataframe.apply_valid_names_for_stata() %>% 
#     dplyr::mutate(
#       doc_num=as.character(doc_num),
#       age=as.numeric(stringr::str_extract_all(edad, "[0-9]{2}"))) 
#   
#   
#   #
#   median_or_technical=c("Media","Técnica Laboral", "Técnica Profesional", "Tecnológica")
#   #
#   college=c("Universitaria","Maestría Doctorado","Especialización")
#   #
#   W=(nrow(Oferentes_df)/nrow(curated_sample))
#   
#   
#   #
#   lapply(Age_models, function(some_age_model){
#     some_age_model %>% i_am_my_name()
#     
#     df=
#     curated_sample %>% 
#       mutate(age_group=cut(age,
#                            breaks = quantile(.$age, some_age_model, na.rm=TRUE),
#                            include.lowest = TRUE,
#                            ordered_result = TRUE,
#                            right=FALSE,
#                            
#                            ))  %>% 
#       split( paste(.$age_group, .$sex, sep="X") ) %>%
#       lapply(., function(age_group_df){
#         
#         nrow(age_group_df)
#         data.frame(age_group=age_group_df$age_group[1],
#                    sex=age_group_df$sex[1],
#                    subsample_size=nrow(age_group_df),
#                    population_size=floor(W*nrow(age_group_df)),
#                    mean_age=round(mean(age_group_df$age),3),
#                    prop_soltero=round(mean(age_group_df$estado_civil=="Soltero")*100,3),
#                    prop_casado=round(mean(age_group_df$estado_civil=="Casado")*100,3),
#                    prop_unión=round(mean(age_group_df$estado_civil=="Unión Libre")*100,3)
#                    )}) %>% bind_rows() %>%
#       dplyr::filter(!is.na(age_group)) %>%
#       dplyr::filter(sex!="unk")
#   }) 
#   
# }
# #
# i_am_my_name=function(x){names(x)=x;x}
# Virtual_profiles_df.age_model() %>% View()



