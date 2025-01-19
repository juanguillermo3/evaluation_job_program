#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")


#
#
basic_standardization=function(
  names
){
  #
  tolower(names) %>%
    #
    iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
  #
}
#
stata_valid_names=function(names){
  text2vec::word_tokenizer(names) %>% 
    sapply(., function(some_name_units){
      s=paste(some_name_units, collapse="_")
      s=stringr::str_replace_all(s, "[.]", "_")
      substr(s, 0, 25) 
    }) %>% basic_standardization()
}
#
stata_valid_names(c("Variable_invalida1",
                    "Variable//.,invalida2",
                    "Variableinvalida3_con_nombre_super_largo",
                    "Variable_con_acentuaciÃ³n",
                    "nomnbre.invalido"
))
#
Dataframe.apply_valid_names_for_stata=function(
  Dataframe
){
  names(Dataframe)=stata_valid_names(names(Dataframe))
  Dataframe
}
#
library(dplyr)
db.read_virtual_profiles=function(
  file= "muestra ruta y oferentes 750.csv"
){
  setwd(PROFILES_HOME)
  read.csv(file, fileEncoding = "UTF-8") %>%
    Dataframe.apply_valid_names_for_stata() 
}
#
db.read_virtual_profiles() %>% View()



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
        lapply(., function(data_col){
          as.character(data_col)
        }) %>%
        as.data.frame()
    }) %>% bind_rows()  %>% {
      write.table(paste(.$cv, "<END OF RECORD>"), "tempfile.txt")
      cbind(
        dplyr::select(.,-cv) ,cv=read.table("tempfile.txt", encoding="UTF-8")[,1]
      )
    }
}
#
db.from_files_data_reader_virtual_profiles_cache=db.from_files_data_reader_virtual_profiles() 
db.from_files_data_reader_virtual_profiles_cache %>% View()
#
Virtual_profiles_df.model_virtual_cv=function(
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
Virtual_profiles_df.model_virtual_cv() %>% View()
#
Virtual_profiles_df.model_virtual_cv.cache=Virtual_profiles_df.model_virtual_cv()


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



