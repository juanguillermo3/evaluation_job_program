#
# tabla de balance
#

setwd(EXPERIMENTAL_CASES_HOME)
experimental_cases.overview() %>%
  Dataframe.export_output(label="overview.xlsx",
                          output_home=EXPERIMENTAL_CASES_HOME)


#
# para mirar el balance de muestra, necesitamos hacer uso de un modelo demografico
#

#
#
library(dplyr)
db.muestra_cmd=function(){
  setwd(MUESTRA_HOME)
  openxlsx::read.xlsx("muestra cmd.xlsx")
}
#
db.muestra_cmd() %>% View()
#
muestra_cmd.modelo_demografico=function(
  muestra_cmd=db.muestra_cmd()
){
  muestra_cmd %>%
    #
    # (1)
    #
    dplyr::transmute(doc_num=doc_num,
                     genero=genero,
                     edad=edad,
                     estudios=estudios) %>%
    
    #
    # (2)
    #
    dplyr::mutate(
      sexo=ifelse(genero=="F", "mujer", "hombre")
    ) %>%
    dplyr::mutate(
      estudios=
        dplyr::case_when(
          estudios %in% c("Preescolar", "Básica Primaria(1-5)", "Básica Secundaria(6-9)")~"hasta basica secundaria",
          estudios %in% c("Media(10-13)")~"secundaria completa",
          estudios %in% c("Técnica Laboral", "Técnica Profesional", "Tecnológica")~"tecnica",
          estudios %in% c("Universitaria", "Especialización", "Maestría", "Doctorado")~"univ o posterior",
        ))
}
#
muestra_cmd.modelo_demografico() %>% View()


#
db.oferentes_inscritos=function(){
  setwd(OFERENTES_HOME)
  library(dplyr)
  list.files(
    pattern="entradas"
  ) %>% 
    
    lapply(., function(data_file){
      readxl::read_excel(data_file)
    }) %>%
    bind_rows()
}
#
db.oferentes_inscritos() %>% View()
#
entradas_df.normalized_oferentes_model=function(
  entradas_df=db.oferentes_inscritos()
){
  
  entradas_df %>% 
    
    dplyr::transmute(
      doc_num=.[["Número Documento"]],
      doc_type=.[["Tipo Documento"]],
      edad=.[["Edad"]],                           
      genero=.[["Género"]],
      estudios=.[["Nivel de Estudio"]],
      government_program=.[["Programa de Gobierno"]],
      condiciones=.[["Condiciones Especiales"]],         
      discapacidades=.[["Detalle Discapacidades"]],
      Ámbito=.[["Pertenece A"]]) %>%
    dplyr::filter(!duplicated(doc_num)) 
  
}
#
entradas_df.normalized_oferentes_model() %>% View()
#
muestra_cmd.modelo_demografico=function(
  muestra_cmd=entradas_df.normalized_oferentes_model()
){
  muestra_cmd %>%
    #
    # (1)
    #
    dplyr::transmute(doc_num=doc_num,
                     genero=genero,
                     edad=edad,
                     estudios=estudios,
                     doc_type=doc_type) %>%
    
    #
    # (2)
    #
    dplyr::mutate(
      sexo=ifelse(genero=="F", "mujer", "hombre")
    ) %>%
    dplyr::mutate(
      estudios=
        dplyr::case_when(
          estudios %in% c("Preescolar", "Básica Primaria(1-5)", "Básica Secundaria(6-9)")~"hasta basica secundaria",
          estudios %in% c("Media(10-13)")~"secundaria completa",
          estudios %in% c("Técnica Laboral", "Técnica Profesional", "Tecnológica")~"tecnica",
          estudios %in% c("Universitaria", "Especialización", "Maestría", "Doctorado")~"univ o posterior",
        )) %>%
    dplyr::mutate(
      es_migrante=ifelse(doc_type %in% c( "Cédula de Extranjeria",
                                          "Documento Nacional de Identificación",
                                          "Pasaporte",
                                          "Permiso Especial de Permanencia"
      ), "Sí", "No")
    )
}
#



#
#
muestra_cmd.modelo_demografico(entradas_df.normalized_oferentes_model()) %>% View()

#
# modelo de hoja de vida
# --------

#
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
  Standardized_records_df,
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
# Standardized_records_df.estandarizar_fechas() %>% View()
# Standardized_records_df.estandarizar_fechas.cache=Standardized_records_df.estandarizar_fechas()
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


#
# counterfactual outcomes
# --------

#
#
muestra_cmd.modelo_demografico(entradas_df.normalized_oferentes_model()) %>% View()
#
experimental_cases.counterfactual_outcomes=function(
  experimental_cases=people_assisted_by_cmd.experimental_cases(),
  modelo_demografico=muestra_cmd.modelo_demografico(entradas_df.normalized_oferentes_model()),
  modelo_hoja_de_vida=Big_Data_df.model_big_data_covariates()
){
  dplyr::left_join(
    experimental_cases,
    modelo_demografico) %>% 
    dplyr::left_join(
      .,
      modelo_hoja_de_vida) %>% 
    na.omit()
}
#
experimental_cases.counterfactual_outcomes() %>% View()
#
experimental_cases.counterfactual_outcomes.cache=
  experimental_cases.counterfactual_outcomes()
#
experimental_cases.counterfactual_outcomes.cache %>% View()


#
#
counterfactual_outcomes.demographic_gradient=function(
  counterfactual_outcomes_model=
    experimental_cases.counterfactual_outcomes.cache,
  logro_bachillerato=c("secundaria completa","univ o posterior"),
  logro_universitario=c("univ o posterior")
){
  counterfactual_outcomes_model %>% 
    mutate(ruta_empleabilidad=ifelse(activ_cualquiera>0, "participante", "no participante" )) %>%
    split(.$ruta_empleabilidad) %>%
    lapply(function(case_df){
      data.frame(
        ruta_empleabilidad=case_df$ruta_empleabilidad[1],
        tam_submuestra=nrow(case_df),
        
        #
        prop_mujeres=round(mean(case_df$sexo=="mujer")*100,3),
        
        #
        tasa_bachillerato_completo=round(mean(case_df$estudios %in% logro_bachillerato )*100,3),
        tasa_universitaria_o_superior=round(mean(case_df$estudios %in% logro_universitario )*100,3),
        
        #
        prom_edad=round(mean(case_df$edad  ),3),
        edad_menor_20=round(mean(case_df$edad <20  )*100,3),
        edad_20_a_30=round(mean(case_df$edad <=20 & case_df$edad <30  )*100,3),
        edad_mas_de_30=round(mean(case_df$edad>=30  )*100,3),
        
        #
        prop_migrante=round(mean(case_df$es_migrante=="Sí")*100,3)
      )
    }) %>%
    bind_rows()
}
#
counterfactual_outcomes.demographic_gradient()
#
UNBALANCED_SAMPLE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/unbalanced sample"
setwd(UNBALANCED_SAMPLE_HOME)
#
counterfactual_outcomes.demographic_gradient() %>%
  Dataframe.export_output(label="sample balance.xlsx",
                          output_home=UNBALANCED_SAMPLE_HOME)

#
#
counterfactual_outcomes.laboral_gradient=function(
  counterfactual_outcomes_model=
    experimental_cases.counterfactual_outcomes.cache
){
  counterfactual_outcomes_model %>% 
    mutate(ruta_empleabilidad=ifelse(activ_cualquiera>0, "participante", "no participante" )) %>%
    split(.$ruta_empleabilidad) %>%
    lapply(function(case_df){
      data.frame(
        ruta_empleabilidad=case_df$ruta_empleabilidad[1],
        tam_submuestra=nrow(case_df),
        
        #
        prom_experiencia=round(mean(case_df$experiencia),3),
        #
        prom_certificacion=round(mean(case_df$certificacion),3),
        #
        idioma_extranjero=round(mean(case_df$idioma_extranjero==1)*100,3),
        #
        competencias_digitales=round(mean(case_df$competencias_digitales==1)*100,3)
        
      )
    }) %>%
    bind_rows()
}
#
counterfactual_outcomes.laboral_gradient()

#
UNBALANCED_SAMPLE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/unbalanced sample"
setwd(UNBALANCED_SAMPLE_HOME)
#
counterfactual_outcomes.demographic_gradient() %>%
  Dataframe.export_output(label="sample balance dem.xlsx",
                          output_home=UNBALANCED_SAMPLE_HOME)
#
counterfactual_outcomes.laboral_gradient()
#
UNBALANCED_SAMPLE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/unbalanced sample"
setwd(UNBALANCED_SAMPLE_HOME)
#
counterfactual_outcomes.laboral_gradient() %>%
  Dataframe.export_output(label="sample balance lab.xlsx",
                          output_home=UNBALANCED_SAMPLE_HOME)
