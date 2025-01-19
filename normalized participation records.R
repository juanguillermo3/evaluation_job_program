#
#
# normalized participation records
# --------

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

#
#setwd(CODE_HOME)
#source("genderdize records.R")
#
#setwd(CODE_HOME)
#source("quality control.R")
#
#modelo_de_control=informacion_de_control.modelo_de_control() 
#modelo_de_control %>% View()

setwd(CODE_HOME)
source("persistence utils.R")

#
# custom modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

#
# normalized participation records
# --------
#

#
library(dplyr)
file_reader=
  function(file){
    df=
    read.csv(file) %>% 
      Dataframe.vars_as_character()
    if (!("cedula" %in% names(df))){
      df=
      df %>% 
        mutate(
          cedula=
            stringr::str_extract(
            stringr::str_extract(ruta, "'[0-9][:alnum:]+'" ),
            "[0-9][:alnum:]+") %>% as.character() 
        )%>% 
        #
        mutate(ruta=stringr::str_extract(ruta, "'ruta'[:] .+" ))%>% 
        mutate(ruta=stringr::str_extract(ruta, "[^,]+" ))%>% 
        mutate(ruta=stringr::str_replace(ruta, "'ruta'[:]", "" )) %>%
        mutate("schema"=1)
        return(df) 
    };
    df %>%
     mutate("schema"=2)
  }
#
setwd(RUTA_HOME)
#file_reader("scraper delivery (20220224-180449).csv") %>% View()
#
#file_reader("scraper delivery 688.csv") %>% View()

#
library(dplyr)
db.participation_records=function(
  sample_prefix="scraper delivery",
  encoding="UTF-8",
  retained=c("cedula", "ruta", "schema")
){
  suppressMessages({
    #
    library(dplyr)
    setwd(RUTA_HOME)
    list.files(pattern=sample_prefix) %>%
      lapply(function(some_csv){
        file_reader(some_csv)
      }) %>%
      bind_rows() %>%
      dplyr::select(retained) %>%
      Dataframe.reencode() %>%
      Dataframe.map_NAS(mapped_to = "")
  })
}
#
db.participation_records_cache=db.participation_records()
db.participation_records_cache %>% View()

#
Participation_records.normalized_model=function(
  participation_records=db.participation_records_cache
){
  
  participation_records %>%
    
    transmute( doc_num=cedula, 
               raw_info_ruta=ruta,
               schema=schema) %>%
    
    dplyr::mutate(
      raw_info_ruta=raw_info_ruta
    ) %>%
    
    dplyr::mutate( 
      raw_info_ruta= .$raw_info_ruta %>%
        as.character() %>%
        stringr::str_replace_all(.,",", "<END OF SESSION RECORD>" ),
      
      algun_ofrecimiento=raw_info_ruta!=""
    ) %>%
    dplyr::filter(
      !duplicated(doc_num)
    )
}
#
Participation_records.normalized_model() %>% View()

#
#
Ruta_de_empleabilidad_df.Personal_session_records=function(
  Ruta_de_empleabilidad_df=Participation_records.normalized_model()
){
  
  #
  session_records=
    
    Ruta_de_empleabilidad_df %>%
    
    split(.[["doc_num"]]) %>%
    
    lapply(., function(personal_records_df){
      
      #if(is.na(.)=="") {return(NA)}
      an=
        list(
          doc_num=personal_records_df$doc_num,
          sesiones=unlist(stringr::str_split(personal_records_df$raw_info_ruta,"<END OF SESSION RECORD>"))
        )
      
      if (is.na(an$sesiones)){an$sesiones=""}
      an
    }) 
}
#
Ruta_de_empleabilidad_df.Personal_session_records() %>% head()
Ruta_de_empleabilidad_df.Personal_session_records() %>% tail()
Ruta_de_empleabilidad_df.Personal_session_records() %>% length()

#
merge_fields=function(field1,
                      field2){
  field1[is.na(field1)|field1=="NA"]=""
  field2[is.na(field2)|field2=="NA"]=""
  paste(as.character(field1), as.character(field2), sep="")
}
#
ruta_de_empleabilidad.Session_records.Standardize=function(
  session_records=Ruta_de_empleabilidad_df.Personal_session_records()
){

  #  
  an=
    lapply(session_records , function(personal_records){
      #print(personal_records)
      if ( all(unlist(personal_records$sesiones)=="")) {return(NA)}
    
      an=personal_records$sesiones %>%
        lapply(., function(ind_session_record){
          
          ind_session_record
          #
          fields=stringr::str_extract_all(ind_session_record,"n[A-Z][^:]*")
          fields=unlist(fields)
          #
          values=stringr::str_split(ind_session_record,paste( "(", fields, ")", sep="", collapse="|") ) %>% unlist()
          values=values[2:length(values)]
          
          try(
            {
              an=data.frame(values) %>% t() %>% as.data.frame()
              names(an)=fields
              an$documento=personal_records$doc_num
              an 
            })

        }) %>% bind_rows()
      
      an
    })
  #      
  an=an[sapply(an, function(df){length(df)>2})] %>% bind_rows() 
  #an 
  

  #
  names(an)=names(an) %>% 
    text2vec::word_tokenizer() %>% 
    sapply(., function(n){paste(n, collapse="_")}) %>%
    tolower()
  
  an=
  an %>%
    lapply(., function(data_col){
      
        text2vec::word_tokenizer(data_col, pos_keep="/") %>%
        
        sapply(., function(sublist){
          paste(sublist, collapse =" ")
        })
      
    }) %>% as.data.frame() 
  names(an)=stringr::str_replace(names(an), pattern="^n", "")
  an =
    an %>%
    dplyr::transmute(.,
           nombre=merge_fields(.$"nombre", .$"nombre.1" ),
           prestador=merge_fields(.$"prestador_punto_de_atención", .$"prestador_punto_de_atención_asesor"),
           estado=estado,
           doc_num=documento,
           fecha=.$"fecha_programada",
           fecha_final=.$"fecha_final"
    ) 
  #
  an=
  list(an, data.frame(doc_num=names(session_records),
                      nombre="se registró",
                      prestador= "CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS",
                      estado="Asistió"
                      ))%>% bind_rows()
  
  an %>% Dataframe.order("doc_num")
}
#
ruta_de_empleabilidad.Session_records.Standardize.cache=
  ruta_de_empleabilidad.Session_records.Standardize() 
#
ruta_de_empleabilidad.Session_records.Standardize.cache %>% View()

#
Participacion_en_actividades.modelo_participaciones_normalizado=function(
  Participacion_en_actividades=ruta_de_empleabilidad.Session_records.Standardize.cache
){
  Participacion_en_actividades %>%
    dplyr::distinct() %>%
    Dataframe.expand_regex_features(
      text_source="nombre",
      features=list(
        "orientacion"="orientación laboral",
        "busqueda"="búsqueda de empleo",
        "transversales"="competencias transversales"
      ),
      name_prefix="activ_",
      standardization=tolower
    ) %>%
    Dataframe.expand_regex_features(
      text_source="prestador",
      features=list(
        "cmd"="minuto de dios"
      ),
      name_prefix="prest_",
      standardization=tolower
    ) %>%
    Dataframe.expand_regex_features(
      text_source="estado",
      features=list(
        "asistencia"="(^aprob?)|(^asisti?)|(^finalizado)|(^en curso)|(^realizada)"
      ),
      name_prefix="prest_",
      standardization=tolower
    ) %>%
    dplyr::mutate(
      dia=stringr::str_extract(fecha, pattern="^[0-9]{2}"),
      mes=stringr::str_extract(fecha, pattern=" [0-9]{2}"),
      year=stringr::str_extract(fecha, pattern="[0-9]{4}"),
      fecha=lubridate::dmy(paste(dia,mes,year, sep="-"))
    ) %>%
    dplyr::select(-c("fecha_final", "dia", "mes", "year"))
}
#
Participacion_en_actividades.modelo_participaciones_normalizado() %>%
  View()

#
#
Participacion_en_actividades.modelo_participaciones_normalizado() %>%
  Dataframe.share_with_app(
    label="particacion actividades",
    app_route =PARTICIPATION_RECORDS_HOME )


#
# analytics
# --------
#

library(dplyr)
db.cargar_participaciones=function(){
  setwd(PARTICIPATION_RECORDS_HOME)
  readxl::read_excel("particacion actividades.xlsx")
}
#
db.cargar_participaciones() %>%
  View()


#
participaciones_df.en_el_tiempo=function(
  
  participaciones_df=db.cargar_participaciones(),
  
  fechas_de_corte=seq(as.Date("2017/1/1"), 
                  as.Date("2022/1/1"),
                  by = "quarter")
){
  #
  #
  library(ggplot2)
  library(ggthemes)
  participaciones_df %>%
    dplyr::mutate(
      periodo=cut(fecha %>% as.Date(),
                  breaks = fechas_de_corte %>% as.Date(),
                  right = TRUE, 
                  include.lowest = TRUE)
    ) %>%
    split(.$periodo) %>%
    #
    #
    lapply(function(sub_df){
      data.frame(
        periodo=sub_df$periodo[1],
        size=nrow(sub_df)
      )
    }) %>%
    #
    #
    bind_rows() %>%
    #
    #
    mutate(
      year=lubridate::year(periodo),
      ) %>%
    mutate(
      
    ) %>%
    #
    #
    ggplot(aes(x=periodo, y=size))+
    geom_col(fill=rgb(0,0,0,.3)) +
    facet_wrap(~ year, 
               nrow = 1,
               scales = "free_x"
               )+
    theme_stata()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))
}
#
participaciones_df.en_el_tiempo(
  participaciones_df= db.cargar_participaciones() 
) +
  labs(
    title = "Participación efectiva en participación laboral",
    subtitle = "Cantidad personas atendidas por trimestre",
    caption = "Construcción propia a partir de registros de participación",
    x="periodo trimestral",
    y="# asistentes"
  )
  