#
#
APP_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos"
INFO_BASICA_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/info basica"
MARCO_MUESTRAL_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/marco muestral"
HOJAS_DE_VIDA="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/hojas de vida virtuales"


#
#
library(dplyr)
db.oferentes_info_basica=function(){
  #
  setwd(INFO_BASICA_HOME)
  list.files(pattern=".csv") %>%
    lapply(function(data_file){
      read.csv(data_file, encoding = "UTF-8")
    }) %>%
    bind_rows() %>%
    dplyr::transmute(
      info_basica=X0
    ) %>%
    bind_rows()
}
#
db.oferentes_info_basica() %>% View()

#
#
library(dplyr)
oferentes_info_basica_df.normalize=function(
  oferentes_info_basica_df=db.oferentes_info_basica()
){
  #
  oferentes_info_basica_df %>% 
    dplyr::filter(
      !duplicated(info_basica)
    )
}
#
oferentes_info_basica_df.normalize.cache=oferentes_info_basica_df.normalize()
oferentes_info_basica_df.normalize.cache %>% View()


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
oferentes_info_basica_df.features_from_regex=function(
  oferentes_info_basica_df=oferentes_info_basica_df.normalize(),
  regex_features=list(
    "doc_num"="(?<=numero documento[:]) [0-9]+",
    "nombres_apellidos"="(?<=nombres y apellidos[:]) [a-zñ ]+",
    "contacto"="(?<=telefono de contacto[:]) [0-9 ]+",
    "fecha_actualizacion"="(?<=fecha ultima actualizacion[:]) .+[0-9]{4}"
  ),
  text_standardization=basic_standardization
){
  #
  df=oferentes_info_basica_df %>%
    dplyr::mutate(
      z_info_basica=text_standardization(info_basica)
    )
  # 
  for (new_feature in names(regex_features)){
    df =
      df %>% dplyr::mutate(
        new_var=
          trimws(stringr::str_extract(
          z_info_basica, 
          regex_features[[new_feature]]
        ))) %>%
      {names(.)[length(names(.))]=new_feature;.}

  }
  #
  df
}
#
oferentes_info_basica_df.features_from_regex() %>% View()

#
#
HOJAS_DE_VIDA="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/hojas de vida virtuales"
setwd(HOJAS_DE_VIDA)

#
#
db.hojas_de_vida_virtuales=function(
){
  #
  setwd(HOJAS_DE_VIDA)
  list.files(pattern="hojas de vida") %>%
    lapply(function(data_file){
      
      tryCatch(
        {
          
          read.csv(data_file, encoding = "UTF-8") %>%
            
            dplyr::transmute(
              doc_num=as.character(cedula),
              hoja_de_vida_virtual=as.character(hoja_de_vida_virtual))
    
        },
        error=function(cond) {
          
          data.frame()
          
        },
        
        finally={
        })
      
      
    }) %>%
    
    bind_rows() 
}
#
db.hojas_de_vida_virtuales() %>% View()


#
#
library(dplyr)
hojas_de_vida_df.normalize=function(
  hojas_de_vida_df=db.hojas_de_vida_virtuales()
){
  #
  hojas_de_vida_df %>% 
    dplyr::filter(
      !duplicated(hoja_de_vida_virtual)
    )
}
#
hojas_de_vida_df.normalize.cache=hojas_de_vida_df.normalize()
hojas_de_vida_df.normalize.cache %>% View()


#
#
hojas_de_vida_virtuales_df.features_from_regex=function(
  hojas_de_vida_virtuales_df=hojas_de_vida_df.normalize(),
  regex_features=list(
    "edad"="(?<=edad[:]) [0-9]+", 
    "estado_civil"="(?<=estado civil[:]) [a-z 0-9 ()]+",
    "nivel_educativo"="(?<=nivel educativo[:]) [a-z 0-9 ()]+",
    "estado"="(?<=estado[:]) [a-z ()]+",
    "herramienta"="(?<=herramienta[:]) [a-z 0-9 ()]+",
    "experiencia"="(?<=anos de experiencia[:]) [a-z 0-9 ()]+",
    "aspiracion_salarial"="(?<=aspiracion salarial[:]) [a-z 0-9 ()]+"
  ),
  text_standardization=basic_standardization
){
  #
  df= hojas_de_vida_virtuales_df %>%
    dplyr::mutate(
      z_hoja_de_vida_virtual=text_standardization(hoja_de_vida_virtual)
    )
  # 
  for (new_feature in names(regex_features)){
    df =
      df %>% dplyr::mutate(
        new_var=trimws(stringr::str_extract(
          z_hoja_de_vida_virtual, 
          regex_features[[new_feature]]
        ))) %>%
      {names(.)[length(names(.))]=new_feature;.}
    
  }
  #
  df
}
#
hojas_de_vida_virtuales_df.features_from_regex.cache=hojas_de_vida_virtuales_df.features_from_regex()
hojas_de_vida_virtuales_df.features_from_regex.cache %>% View()



#
#
#
parametros_de_hojas_de_vida_virtuales.modelo_demografico_especializado=function(
  parametros_de_hojas_de_vida_virtuales=hojas_de_vida_virtuales_df.features_from_regex.cache
){
  parametros_de_hojas_de_vida_virtuales %>%
    dplyr::transmute(
      doc_num=as.character(doc_num),
      hoja_de_vida_virtual=hoja_de_vida_virtual,
      experiencia=stringr::str_extract( experiencia, "[0-9]+"),
      competencias_digitales=!is.na(herramienta),
      edad=as.numeric(edad),
      estado_civil=estado_civil,
      nivel_educativo=nivel_educativo,
      aspiracion_salarial=aspiracion_salarial)
}
#
parametros_de_hojas_de_vida_virtuales.modelo_demografico_especializado() %>% View()


#
#
OFERENTES_HOME= "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
#
library(dplyr)
Oferentes_inscritos.db.oferentes_inscritos=function(
  file="oferentes_inscritos.xlsx"
){
  setwd(OFERENTES_HOME)
  readxl::read_excel(file)
}
#
Oferentes_inscritos.db.oferentes_inscritos() %>% View()
#
people_with_ids_df.genderize=function(
  people_df,
  Oferentes_df=
    Oferentes_inscritos.db.oferentes_inscritos() %>%
    dplyr::mutate(doc_num=documento)
){
  
  dplyr::left_join(
    people_df,
    Oferentes_df %>%
      dplyr::transmute(
        doc_num=documento,
        gender=ifelse(genero=="F", "mujer", "hombre")
      ),
    by="doc_num"
  )}
#
parametros_de_hojas_de_vida_virtuales.modelo_demografico_especializado() %>%
  people_with_ids_df.genderize() %>%
  View()


#
#
Dataframe.mandatory_model=function(
  df,
  mandatory_model
){
  df %>% 
    dplyr::filter(
      df %>%
        dplyr::select( mandatory_model) %>%
        apply( MARGIN=1, 
                    function(row_data){
        all(!is.na(row_data))
      })
    )
}
#
#
oferentes_info_basica_df.ensamblar_marco_muestral=function(
  info_basica_df=oferentes_info_basica_df.features_from_regex(),
  hojas_de_vida_virtuales_df=parametros_de_hojas_de_vida_virtuales.modelo_demografico_especializado() %>%
    people_with_ids_df.genderize(),
  Mandatory_model=c("doc_num",
                    "nombres_apellidos",
                    "contacto", 
                    "fecha_actualizacion", 
                    "edad",
                    "gender")
 
){
  
  dplyr::left_join(info_basica_df, hojas_de_vida_virtuales_df) %>% 
    Dataframe.mandatory_model(
      mandatory_model=Mandatory_model
    ) 

}
#
oferentes_info_basica_df.ensamblar_marco_muestral() %>% View()

MARCO_MUESTRAL_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/marco muestral"
#
marco_muestral.persistir=function(
  marco_muestral=oferentes_info_basica_df.ensamblar_marco_muestral()
){
  setwd(MARCO_MUESTRAL_HOME)
  write.csv2(marco_muestral, fileEncoding = "UTF-8", file="marco muestral.csv")
}
#
marco_muestral.persistir()