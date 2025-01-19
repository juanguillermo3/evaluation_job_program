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
source("genderdize records.R")

setwd(CODE_HOME)
source("demographic model for cmd population.R")

setwd(CODE_HOME)
source("quality control.R")
#modelo_de_control=informacion_de_control.modelo_de_control() 
#modelo_de_control %>% View()

setwd(CODE_HOME)
source("fechas de registro.R", encoding="UTF-8")

setwd(CODE_HOME)
source("virtual profiles.R", 
       encoding = "UTF-8",
       local=FALSE)


#
# app modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")


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
  Standardized_records_df
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
#Standardized_records_df.estandarizar_fechas() %>% View()
# Standardized_records_df.estandarizar_fechas.cache=Standardized_records_df.estandarizar_fechas()



#
Raw_virtual_record.standardize=function(
  Raw_record=Virtual_profiles_df.model_virtual_cv()$hoja_de_vida[[851]]
){
  
  f=stringr::str_extract_all(Raw_record, "'[A-Z ÁÉÍÓÚ:]{7,30}'") %>% unlist()
  s=stringr::str_split(Raw_record, "'[A-Z ÁÉÍÓÚ:]{7,30}'") %>% unlist()
  s=as.list(s[2:length(s)])
  names(s)=f
  return(s)
}
#
Raw_virtual_record.standardize() 
#
Structured_virtual_record.model_employability=function(
  Structured_virtual_record=Raw_virtual_record.standardize() 
){
  
  try({
    #
    num_doc=
      stringr::str_extract(Structured_virtual_record$"'PERFIL LABORAL:'", "No. documento: [^[:punct:]]*") %>%
      stringr::str_extract(.,"[0-9]+")
    #
    fecha_registro=Num_doc.fecha_registro(num_doc)
    fecha_actualizacion_hoja_de_vida=Sys.Date()
    #
    month_codes=list(
      "Enero"="01",
      "Febrero"="02",
      "Marzo"="03",
      "Abril"="04",
      "Mayo"="05",
      "Junio"="06",
      "Julio"="07",
      "Agosto"="08",
      "Septiembre"="09",
      "Octubre"="10",
      "Noviembre"="11",
      "Diciembre"="12"
    )
    #
    fechas_de_ingreso=
      stringr::str_extract_all(
        Structured_virtual_record$"'EXPERIENCIA LABORAL'",
        pattern="Fecha de (ingreso): [^[:punct:]]*") %>% unlist() %>%
      stringr::str_replace_all(., " de ", "-") %>%
      {
        for (some_month in names(month_codes)){
          .=stringr::str_replace_all(., some_month, month_codes[[some_month]]) %>% unlist()
        };.
      } %>% sapply(., function(some_date){
        paste(
          "01",
          stringr::str_extract(some_date, "[0-9]{2}"),
          stringr::str_extract(some_date, "[0-9]{4}"),
          sep= "-"
        ) 
      }) %>% lubridate::dmy()
    #
    fechas_de_retiro=
      stringr::str_extract_all(
        Structured_virtual_record$"'EXPERIENCIA LABORAL'",
        pattern="Fecha de (retiro): [^[:punct:]]*") %>% unlist() %>%
      stringr::str_replace_all(., " de ", "-") %>%
      {
        for (some_month in names(month_codes)){
          .=stringr::str_replace_all(., some_month, month_codes[[some_month]]) %>% unlist()
        };.
      } %>% sapply(., function(some_date){
        paste(
          "01",
          stringr::str_extract(some_date, "[0-9]{2}"),
          stringr::str_extract(some_date, "[0-9]{4}"),
          sep= "-"
        ) 
      }) %>% lubridate::dmy()
    #
    stopifnot(length(fechas_de_ingreso)==length(fechas_de_retiro))
    
    
    data.frame(ingreso=fechas_de_ingreso, retiro=fechas_de_retiro) %>%
      mutate()
    
    experiencia=
      data.frame(ingreso=fechas_de_ingreso, retiro=fechas_de_retiro) %>% {
        dplyr::case_when(
          (.$ingreso<fecha_registro & .$retiro<fecha_registro)~.$retiro-.$ingreso,
          (.$ingreso<fecha_registro & .$retiro>fecha_registro)~fecha_registro-.$ingreso,
          TRUE~0
        )
      } %>% sum() %>% as.numeric()
    
    
    #
    month_codes=list(
      "Enero"="01",
      "Febrero"="02",
      "Marzo"="03",
      "Abril"="04",
      "Mayo"="05",
      "Junio"="06",
      "Julio"="07",
      "Agosto"="08",
      "Septiembre"="09",
      "Octubre"="10",
      "Noviembre"="11",
      "Diciembre"="12"
    )
    #
    fechas_de_certificacion=
      stringr::str_extract_all(
        Structured_virtual_record$"'EDUCACIÓN INFORMAL'",
        pattern="(Fecha de (certificación): [^[:punct:]]*)|(No Certificado)") %>% unlist() %>%
      stringr::str_replace_all(., " de ", "-") %>%
      {
        for (some_month in names(month_codes)){
          .=stringr::str_replace_all(., some_month, month_codes[[some_month]]) %>% unlist()
        };.
      } %>% sapply(., function(some_date){
        an=
          paste(
            "01",
            stringr::str_extract(some_date, "[0-9]{2}"),
            stringr::str_extract(some_date, "[0-9]{4}"),
            sep= "-"
          ) 
        if(an=="01-NA-NA"){an="01-01-2050"}
        an
      }) %>% lubridate::dmy()
    #
    #
    horas_de_certificacion=
      stringr::str_extract_all(
        Structured_virtual_record$"'EDUCACIÓN INFORMAL'",
        pattern="(Duración en horas: [^[:punct:]]*)|(Ubicación: .{1,20}[A-Z]{2})|(Ubicación: .{1,20}['])") %>% unlist() %>%
      stringr::str_extract(., "[0-9]{1,3}") %>% as.numeric() %>%
      {.[is.na(.)]=0;.}
    #
    #
    #stopifnot(length(fechas_de_certificacion)==length( horas_de_certificacion))
    #
    tiempo_certificacion=
      data.frame(certificacion=fechas_de_certificacion, horas=horas_de_certificacion) %>% {
        dplyr::case_when(
          (.$certificacion<fecha_registro)~.$horas,
          TRUE~0
        )
      } %>% sum() %>% as.numeric()
    
    
    #
    Idioma_extranjero=stringr::str_extract(
      Structured_virtual_record$"'IDIOMAS Y OTROS CONOCIMIENTOS'",
      pattern="Inglés")
    #
    Competencias_Digitales=stringr::str_extract_all(
      Structured_virtual_record$"'IDIOMAS Y OTROS CONOCIMIENTOS'",
      pattern="Tipo: [^[:punct:]]*") %>% unlist() %>%
      paste(collapse=", ")
    #
    Software_especializado=stringr::str_extract_all(
      Structured_virtual_record$"'IDIOMAS Y OTROS CONOCIMIENTOS'",
      pattern="Tipo: Otros [^[:punct:]]*") %>% unlist() %>%
      paste(collapse=", ")
    
    #
    employed_at_register=
      ifelse(
        any(
          sapply(1:length(fechas_de_ingreso),
                 function(j){
                   
                   (fechas_de_ingreso[j] <=   fecha_registro &
                      fecha_registro <= fechas_de_retiro[j])
                   
                   
                 })), "yes", "no")
    
    
    
    #
    data.frame(num_documento=num_doc, 
               fecha_registro=fecha_registro,
               experiencia=experiencia,
               tiempo_certificacion=tiempo_certificacion,
               Idioma_extranjero=Idioma_extranjero,
               Competencias_Digitales=Competencias_Digitales,
               Software_especializado=Software_especializado,
               employed_at_register=employed_at_register)
  })
}
#
Structured_virtual_record.model_employability() %>% View()

#
#
Virtual_profiles_df.employability_model=function(
  Virtual_profiles_df=Virtual_profiles_df.model_virtual_cv()
){
  
  i=0
  Virtual_profiles_df %>%  
    .[["hoja_de_vida"]] %>%
    #head(., 1000) %>%
    lapply(., function(some_raw_record){
      i<<-i+1
      if(i%%50==0){print(i)}
      some_raw_record %>%
        Raw_virtual_record.standardize() %>%
        Structured_virtual_record.model_employability()
    }) 
}
#
Virtual_profiles_df.employability_model.cache=Virtual_profiles_df.employability_model()
Virtual_profiles_df.employability_model.cache %>% View()
#
Virtual_profiles_df.employability_model.cache2=Virtual_profiles_df.employability_model.cache[
  sapply(Virtual_profiles_df.employability_model.cache, 
         function(x){!("try-error" %in% class(x))}
  )
] %>% bind_rows()
#
Virtual_profiles_df.employability_model.cache2 %>% View()

#
#
Big_Data_df.model_big_data_covariates=function(
  Big_data_df=Virtual_profiles_df.employability_model.cache2
){
  
  #
  Big_data_df %>% 
    dplyr::transmute(
      
      doc_num=num_documento,
      
      fecha_registro=fecha_registro,
      
      experiencia=as.numeric(experiencia),
      
      certificacion=as.numeric(tiempo_certificacion),
      
      idioma_extranjero=ifelse(is.na(Idioma_extranjero), 0, 1),
      
      competencias_digitales=ifelse(Competencias_Digitales=="", 0, 1),
      
      software=grepl(Competencias_Digitales, pattern="Otros")*1,
      
      empleo_al_registro=ifelse(employed_at_register=="yes", 1, 0) %>%
        {.[is.na(.)]=0;.}
      
    ) 
  
}
#
Big_Data_df.model_big_data_covariates.cache=Big_Data_df.model_big_data_covariates()
Big_Data_df.model_big_data_covariates.cache %>% View()