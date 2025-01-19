#
# 0. routing to home of the application, and home of main evaluation
# methodology
#
APP_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade"
EVALUATION_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/program evaluation"
SCRIPT_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
OFERENTES_HOME= "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
INFO_BASICA_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/info basica"

# 
# 1. Build the data model for the treatment
#
library(dplyr)
#
setwd(SCRIPT_HOME)
source("ruta de empleabilidad v2.R" )
#
Sample_from_Ruta_de_Empleabilidad.model_treatment=function(
  
  Standardized_records_df=ruta_de_empleabilidad.Session_records.Standardize.cache,
  
  Treatment_regex=list(
    "Entrevista"=c("orientacion laboral"), 
    "Competencias transversales"=c("transversales"),
    "Búsqueda de empleo"=c("busqueda de"),
    "Migrantes"=c("migrantes"),
    "Laborales"=c("competencias laborales"),
    "Virtuales"=c("virtual")
  )
  
){
  
  #
  #
  Standardized_records_df=
    Standardized_records_df %>%
    mutate(nombre=Textual_feature.basic_standardization(nombre)) 
  
  #
  #
  recibieron_servicios_de_otros_prestadores=
    Standardized_records_df  %>% 
    dplyr:::filter(.,  !grepl("se registro", tolower(nombre)))  %>% 
    dplyr:::filter(.,  !grepl("^n", tolower(estado)) &
                       !grepl("^c", tolower(estado))) %>%                      
    dplyr:::filter(.,  !grepl("minuto", tolower(prestador)))  %>%               # {table(.$estado)}
  .[["documento"]] %>% unique()
  #
  se_registraron_con_el_minuto_de_dios=
    Standardized_records_df  %>% 
    dplyr:::filter(.,  grepl("se registr", tolower(nombre)))  %>% 
    #dplyr:::filter(.,  !grepl("^n", tolower(estado)) &
    #                   !grepl("^c", tolower(estado))) %>%                      
    dplyr:::filter(.,  grepl("minuto", tolower(prestador)))  %>%                # {table(.$estado)}
    .[["documento"]] %>% unique()
  #
  recibieron_servicios_del_minuto_de_dios=
    Standardized_records_df  %>% 
    dplyr:::filter(.,  !grepl("se registr", tolower(nombre)))  %>% 
    dplyr:::filter(.,  !grepl("^n", tolower(estado)) &
                       !grepl("^c", tolower(estado))) %>%                      
    dplyr:::filter(.,  grepl("minuto", tolower(prestador)))  %>%                # {table(.$estado)}
    .[["documento"]] %>% unique()
  #
  no_recibieron_servicios_de_nadie=
  setdiff( se_registraron_con_el_minuto_de_dios,
           unique(c(recibieron_servicios_del_minuto_de_dios, recibieron_servicios_de_otros_prestadores)))
  

  
  Standardized_records_df=
  Standardized_records_df %>%
    mutate(nombre=Textual_feature.basic_standardization(nombre)) 
  
  #
  # (1.1) grupo de tratamiento
  #
  
  tratados_df=
  recibieron_servicios_del_minuto_de_dios %>% 
    
    lapply(., function(some_doc){
      
      
      list(
      
      data.frame(num_documento=some_doc),
      
      Standardized_records_df %>% 
        dplyr::filter(documento %in% some_doc) %>%
        .[["nombre"]] %>% 
        paste(., collapse=" ") %>%
        {
          sapply(Treatment_regex, function(some_tr){
            n=stringr::str_count(., some_tr)
            ifelse(n>0, n, NA)
          })
        } %>% t() %>% as.data.frame()) %>% bind_cols()
      
    }) %>% bind_rows()
  #
  stopifnot("duplicated el in treated df"=all(duplicated(tratados_df$num_documento)==FALSE))
  
  
  #
  # (1.2) grupo de control
  #
  
  no_tratados_df=
    
    no_recibieron_servicios_de_nadie %>% 
    
    lapply(., function(some_doc){
      
      
      list(
        
        data.frame(num_documento=some_doc),
        
        Standardized_records_df %>% 
          dplyr::filter(documento %in% some_doc) %>%
          .[["nombre"]] %>% 
          paste(., collapse=" ") %>%
          {
            sapply(Treatment_regex, function(some_tr){
              0
            })
          } %>% t() %>% as.data.frame()) %>% bind_cols()
      
    }) %>% bind_rows()
  #
  stopifnot("duplicated el in treated df"=all(duplicated(no_tratados_df$num_documento)==FALSE))
  
  #
  # (1.3) ensemble estimation data
  #
  tratatos_y_control_puro_df=
  list(
  tratados_df %>%
    dplyr::mutate(group="some treatment"),
  
  no_tratados_df %>%
    dplyr::mutate(group="pure control")) %>% 
    
    dplyr::bind_rows() 
  
  #
  stopifnot("duplicated el in treated df"=all(duplicated(tratatos_y_control_puro_df$num_documento)==FALSE))
  
  stopifnot("overlap between control and treatment groups"=
  intersect(
  tratatos_y_control_puro_df$num_documento[tratatos_y_control_puro_df$group=="some treatment"],
  tratatos_y_control_puro_df$num_documento[tratatos_y_control_puro_df$group=="pure control"]) %>%
    length()==0)
  #
  return(tratatos_y_control_puro_df)
}
#
Sample_from_Ruta_de_Empleabilidad.model_treatment.cache= Sample_from_Ruta_de_Empleabilidad.model_treatment() 


# 
# 2. Build the data model for the covariates /counterfactual outcomes
#

#
# (0) register the data reader for the control dataset
#


#
library(dplyr)
db.oferentes_inscritos=function(
 file="oferentes_inscritos.xlsx"
){
 setwd(OFERENTES_HOME)
 readxl::read_excel(file)
}
#
db.oferentes_inscritos() %>% View()    
 
 
 
 #
 # (1) data modeller for the control dataset
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
       substr(s, 0, 25) 
     }) %>% basic_standardization()
 }
 #
 stata_valid_names(c("Variable_invalida1",
                     "Variable//.,invalida2",
                     "Variableinvalida3_con_nombre_super_largo",
                     "Variable_con_acentuación"
 ))
 #
 Dataframe.dummy_expansion_on_factor=function(
   Dataframe=db.oferentes_inscritos(),
   factor="estudios",
   nan_replacement_value="No reportado",
   names_correction=stata_valid_names,
   names_prefix="educ_"
 ){
   #
   Dataframe[[factor]][is.na(Dataframe[[factor]])]=nan_replacement_value
   #
   covariate_levels=unique(Dataframe[[factor]])
   #
   an=data.frame(row.names = 1:nrow(Dataframe))
   #
   sapply(covariate_levels, function(some_covariate_level){
     an<<-dplyr::mutate(an, new_var=  1*(Dataframe[[factor]]==some_covariate_level)+0     )
     names(an)[length(names(an))]<<-some_covariate_level
   })
   #
   names(an)=names_correction(names(an))
   names(an)=paste(names_prefix, names(an), sep="")
   #
   an
 }
 #
 Dataframe.dummy_expansion_on_factor() %>% View()
 #
 Dataframe.dummy_expansion_on_factor(factor="laboral_states",   
                                     names_prefix ="lab_") %>% View()
 #
 # sociable unit for modelling the counterfactuals outcomes /covariates information
 #
 Oferentes_inscritos_df.Model_Counterfactual_outcomes=function(
   
   Oferentes_inscritos_df=db.oferentes_inscritos(),
   
   feature_markers=list(
     
     armed_conflict=c("victimas del conflicto armado", 
                     "personas en procesos de reintegracion", 
                     "atencion especializada-proyecto victimas"),
     
     laboral_policies=c(
       "fondo de oportunidades para el empleo",
       "40 mil primeros empleos", 
       "estado joven"
     ),
     
     ethnic_group=c("afrocolombianos", 
                    "indigenas",
                    "negros",
                    "raizales"),
     
     diverse_functionality=c("fisica",
                             "cognitiva o intelectual",
                             "auditiva",
                             "visual",
                             "psicosocial",
                             "sordoceguera"))
   
 ){
   #
   # map the the raw data set to the features that relate to the
   # counterfactual outcomes
   #
   #
   Oferentes_inscritos_df %>% 
     
     dplyr::transmute(
       
       num_documento=documento,
       
       edad=edad,
       
       mujer= ifelse(genero=="F", 1, 0),
       
       migrante= (tipo_doc %in% c("Cédula de Extranjeria",
                                  "Documento Nacional de Identificación",
                                  "Pasaporte", 
                                  "Permiso Especial de Permanencia"))*1+0,
       
       compound_feature= paste(government_program,condiciones,discapacidades, sep=" meh ") %>%
         Textual_feature.basic_standardization()
       ) %>% 
     
     {
       for (feature in names(feature_markers)){
         
         .[[feature]]=
           stringr::str_detect(
             .$compound_feature,
             paste( 
               paste("(", feature_markers[[feature]], ")" , sep=""),
               collapse="|" )
           )*1
       };.} 
  
 }
#
Oferentes_inscritos_df.Model_Counterfactual_outcomes.cache=Oferentes_inscritos_df.Model_Counterfactual_outcomes() 
#
table(Oferentes_inscritos_df.Model_Counterfactual_outcomes.cache$diverse_functionality)
 
 


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
Standardized_virtual_record.job_facts=function(
  Structured_virtual_record=Raw_virtual_record.standardize() 
){

  
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
      (.$ingreso<fecha_participacion_programa & .$retiro<fecha_participacion_programa)~.$retiro-.$ingreso,
      (.$ingreso<fecha_participacion_programa & .$retiro>fecha_participacion_programa)~fecha_participacion_programa-.$ingreso,
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
        (.$certificacion<fecha_participacion_programa)~.$horas,
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
    
    (fechas_de_ingreso[j] <=   fecha_participacion_programa &
             fecha_participacion_programa <= fechas_de_retiro[j])
             
    
  })), "yes", "no")
         
         
  
  #
  data.frame(num_documento=num_doc, 
             nivel_estudio=nivel_completo,
             aspiracion_salarial=aspiracion_salarial,
             fecha_participacion_programa=fecha_participacion_programa,
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
Virtual_profiles_df.employability_model=function(
  Virtual_profiles_df=Virtual_profiles_df.model_virtual_cv.cache
){
  
  i=0
  Virtual_profiles_df %>%  
    .[["hoja_de_vida"]] %>%
    #head(., 1000) %>%
    lapply(., function(some_raw_record){
      #i<<-i+1
      if(i%%500==5){print(i)}
      some_raw_record %>%
        Raw_record.structure() %>%
        Structured_virtual_record.model_employability()
    }) 
    
}
#
Virtual_profiles_df.employability_model.cache=Virtual_profiles_df.employability_model()
Virtual_profiles_df.employability_model.cache %>% View()

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
# 3. Build the data model for the colocations
#


#
#
db.oferentes_inscritos=function(
  file="oferentes_inscritos.xlsx"
){
  setwd(OFERENTES_HOME)
  readxl::read_excel(file)
}
#
db.oferentes_inscritos() %>% View()
#
db.info_basica_oferentes=function(){
  setwd(INFO_BASICA_HOME)
  
  list.files(pattern="^oferentes_info_basica") %>%
    lapply(., function(f){
      read.csv(f, encoding = "UTF-8")
    }) %>% bind_rows() %>% 
    mutate(
      info_basica=X0
    ) %>%
    mutate(
      num_documento=stringr::str_extract( info_basica, "Número documento: [0-9]+"),
      fecha_actualizacion=stringr::str_extract( info_basica, "Fecha última actualización: [0-9]+ de [A-Za-z]+ [0-9]+" )
    )  %>%
    mutate(
      num_documento= trimws( stringr::str_replace( num_documento, "^.*[:]", "")),
      fecha_actualizacion= trimws( stringr::str_replace( fecha_actualizacion, "^.*[:]", ""))
    ) %>%
    dplyr::filter(
      !duplicated( num_documento)
    )
}
#
db.info_basica_oferentes() %>% View()

#
Info_basica_oferentes.fechas_de_actualizacion=function(
  
  Info_basica_oferentes=db.info_basica_oferentes()
  
){
  Info_basica_oferentes %>% 
    dplyr::transmute(
      num_documento,
      fecha_actualizacion
    ) %>%
    dplyr::mutate(
    z_fecha_actualizacion=
      lubridate::dmy(
        paste(
          stringr::str_extract(fecha_actualizacion, "[0-9]{1,2}"),
          stringr::str_extract(fecha_actualizacion, "[A-Z][a-z]+"),
          stringr::str_extract(fecha_actualizacion, "[0-9]{4}")))
    )
}
#
Info_basica_oferentes.fechas_de_actualizacion.cache=Info_basica_oferentes.fechas_de_actualizacion()
Info_basica_oferentes.fechas_de_actualizacion() %>% View()
Info_basica_oferentes.fechas_de_actualizacion.cache=Info_basica_oferentes.fechas_de_actualizacion()
#
table(Info_basica_oferentes.fechas_de_actualizacion.cache$z_fecha_actualizacion>as.Date("2021-01-01"))
#
Num_doc.fecha_actualizacion=function(
  Num_doc="51999619",
  Fechas_de_actualizacion=Info_basica_oferentes.fechas_de_actualizacion.cache
){
  
  Fechas_de_actualizacion %>% 
    dplyr::filter(num_documento==Num_doc) %>% 
    .[["z_fecha_actualizacion"]] 
  
}
#
Num_doc.fecha_actualizacion()
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
  Num_doc="51999619",
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
setwd(SCRIPT_HOME)
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
Standardized_virtual_record.model_job_facts=function(
  Structured_virtual_record=Raw_virtual_record.standardize(),
  umbral_actualizar_hoja_de_vida=as.Date("2021-01-01")
){
  
  #
  num_doc=
    stringr::str_extract(Structured_virtual_record$"'PERFIL LABORAL:'", "No. documento: [^[:punct:]]*") %>%
    stringr::str_extract(.,"[0-9]+")
  #
  fecha_actualizacion_hoja_de_vida=Num_doc.fecha_actualizacion(num_doc)
  fecha_registro_hoja_de_vida=Num_doc.fecha_registro(num_doc)
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
  
  if(fecha_actualizacion_hoja_de_vida<umbral_actualizar_hoja_de_vida){
    an=data.frame(num_documento=num_doc, 
                  empleo_despues_de_registro=NaN,
                  fecha_registro_hoja_de_vida=fecha_registro_hoja_de_vida,
                  fecha_actualizacion_hoja_de_vida=fecha_actualizacion_hoja_de_vida
                  )
    return(an)
  } else
  {
    an=data.frame(num_documento=num_doc,
                  empleo_despues_de_registro=
                    data.frame(ingreso=fechas_de_ingreso) %>%
                    dplyr::filter(ingreso>fecha_registro_hoja_de_vida) %>% nrow(),
                  fecha_registro_hoja_de_vida=fecha_registro_hoja_de_vida,
                  fecha_actualizacion_hoja_de_vida=fecha_actualizacion_hoja_de_vida
                  )
    #
    return(an)
  
  }
}
#
Standardized_virtual_record.model_job_facts() %>% View()
#
#
Virtual_profiles_df.recent_job_model=function(
  Virtual_profiles_df=Virtual_profiles_df.model_virtual_cv.cache
){
  
  i=0
  Virtual_profiles_df %>%  
    .[["hoja_de_vida"]] %>% 
    #head(., 1000) %>%
    lapply(., function(some_raw_record){
      i<<-i+1
      if(i%%1==0){print(i)}
      some_raw_record %>%
        Raw_record.standardize() %>%
        Standardized_virtual_record.model_job_facts()
    }) %>% bind_rows()
  
}
#
Virtual_profiles_df.recent_job_model() %>% View()
    
    
    

#
#
library(dplyr)
Colocaciones_df.Model_Outcome=function(
    Colocaciones_df=db.from_files_data_reader_virtual_profiles()  %>% 
    transmute( doc_identidad=cedula, 
               seleccion=seleccion
    ) %>% 
    dplyr::filter(
      !duplicated(doc_identidad)
    ) %>%  
    dplyr::transmute(
      num_documento=.$"doc_identidad",
      seleccion=.$"seleccion"
    )
){
  Colocaciones_df %>% 
    dplyr::transmute(
      num_documento=num_documento,
      alguna_colocacion=ifelse( 
        .$seleccion!="No existen procesos",
        1, 0)
    )
}
#
Colocaciones_df.Model_Outcome() %>% View()


# 
# 4. Consolidate dataset for the propensity score estimation
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
      substr(s, 0, 25) 
    }) %>% basic_standardization()
}
#
Dataframe.apply_stata_valid_names=function(
  Dataframe
){
 names(Dataframe)=stata_valid_names(names(Dataframe))
 Dataframe
}
#
Treatment_Model.Full_Evaluation_Data_Model=function(
  Treatment_Model=Sample_from_Ruta_de_Empleabilidad.model_treatment.cache,
  Demographic_model=Oferentes_inscritos_df.Model_Counterfactual_outcomes.cache,
  Employability_Model=Big_Data_df.model_big_data_covariates.cache,
  Outcome_Model=Colocaciones_df.Model_Outcome()
  
){
  
  #
  #
  an=
  list(Treatment_Model,
       Demographic_model,
       Employability_Model,
       Outcome_Model
       ) %>% {
         Reduce(x=., f= function(x,y) {dplyr::left_join(x, y, "num_documento")}
       )} %>%
    
    Dataframe.apply_stata_valid_names()
  
}
#
evaluation_df0=
Treatment_Model.Full_Evaluation_Data_Model() 
#
table(evaluation_df0$alguna_colocacion)

#
# 5. export the pipeline
#
setwd(EVALUATION_HOME)
haven::write_dta(evaluation_df0, sprintf("%s/program evaluation dataset.dta", EVALUATION_HOME))
                

   