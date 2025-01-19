#
# routes
# --------
#

#rm(list=ls())
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
source("fechas de registro.R", encoding="UTF-8")

setwd(CODE_HOME)
source("experimental cases.R", encoding="UTF-8")

setwd(CODE_HOME)
source("demographic model for cmd population.R")

setwd(CODE_HOME)
source("perfiles laborales.R", encoding="UTF-8")

setwd(CODE_HOME)
source("data quality control.R")
db.informacion_de_control() %>% View()

library(dplyr)

#
# app modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

setwd(CODE_HOME)
source("persistence utils.R", encoding="UTF-8")

setwd(CODE_HOME)
source("utensilios para fechas.R", encoding="UTF-8")

#
# confg
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("constants.R")

#
# candidatos
# --------

#https://debuggingdata.com/post/r/regular-expressions-look-arounds/

#
textual_record.regex_features=function(
  textual_record,
  regex_features=list(
    process= "(?<=[(]vacante[)][:] )[0-9-]+",
    fecha="(?<=creaci..n[:] )(.| )+")
  
){
  sapply(regex_features, function(regex){
    stringr::str_extract(textual_record, regex)
  }) %>% t() %>% as.data.frame()
}

#
folder_proceso.candidates_record=function(
  folder_proceso,
  regex_parameters="parameters[.]csv$",
  regex_candidates="^candidatos"
){
  
  #
  setwd(CANDIDATES_HOME)
  setwd(folder_proceso)
  
  #
  record=list.files(pattern=regex_parameters) %>%
    head(1) %>%
    read.csv() %>% 
    {.$process_details %>% head(1)}
  
  #
  a=
    record %>%
    textual_record.regex_features() 
  
  if(
    all(
    stringr::str_extract_all(record, pattern="[0-9]+") %>%
    unlist() %>% 
    head(5) %>%
    as.numeric() %>% {.==0}) 
  ){
    return(a %>% dplyr::mutate(doc_num=NA_character_ ))
  } 
  

  b=
  list.files(pattern= regex_candidates) %>%
    head(1) %>% 
    read.xlsx() %>%
    dplyr::transmute(
      doc_num=Número.de.Identificación,
      tipo_postulacion=Estado.Ingreso,
      resultado=Estado.Actual
    )
  
  full_join(a,b, by = character()) %>%
    return()
}
setwd(CANDIDATES_HOME)
folder_proceso.candidates_record(folder_proceso="333374-75029") %>% View()
folder_proceso.candidates_record(folder_proceso="1626155706-2") %>% View()
folder_proceso.candidates_record(folder_proceso= "1626193529-43") %>% View()

#
list.keep_dataframes=function(
  list
){
  list[sapply(list, function(l_){"data.frame" %in% class(l_)})]
}
#
db.source_candidates=function(
  
  regex_procesos="^[0-9-]+$",
  
  cache_name="candidates_df_cached_in_memory",
  new_event=FALSE
){
  
  #
  if ( cache_name %in% names(.GlobalEnv) & !new_event ){
    print(sprintf("%s fetched from cache", cache_name))
    return(.GlobalEnv[[cache_name]])
  }
  
  #
  setwd(CANDIDATES_HOME)
  list.files(pattern=regex_procesos) %>%
    #head() %>%
    lapply(function(folder_proceso){
      tryCatch({
      setwd(CANDIDATES_HOME)
      #print(folder_proceso)
      folder_proceso %>% folder_proceso.candidates_record()
      }, error=function(e){print(folder_proceso)} )
    }) %>%
    list.keep_dataframes() %>%
    bind_rows() %>%
    Dataframe.reencode() %>%
  
  #
  {
    print(sprintf("%s not found, but newly created in cache", cache_name))
    .GlobalEnv[[cache_name]]=.
    } %>%
    return()
}
#
db.source_candidates() %>% View()
db.source_candidates(new_event = TRUE) %>% View()
#

#
candidates_table=
  db.source_candidates() %>%
  Dataframe.alter_table_Dataframe_add_primary_key(
    primary_key_components=c("process","doc_num")
  ) %>% 
  dplyr::mutate( 
    Fue_colocado=resultado=="Colocado"
    ) %>%
  dplyr::mutate(
    fecha=remediar_fechas(fecha, sep=" ")
  ) %>% 
  dplyr::mutate(
    fecha=as.Date(fecha, format="%d %m %Y")
  ) 
#
candidates_table %>%
  Dataframe.share_with_app(
    label="candidates_table",
    app_route = CANDIDATES_HOME
  )

#
# serie de tiempo que me muestre postulaciones en el tiempo,
# con linea vertial covid/post-covid, por sexo
#

#
candidates.genderdize.postulations_over_time.plot=
  function(
    candidates=db.candidates() %>%
      Dataframe.alter_table_Dataframe_add_primary_key(
        primary_key_components=c("process","doc_num")
      ),
    fechas_de_corte=seq(as.Date("2017/1/1"), 
                        as.Date("2022/1/1"),
                        by = "quarter"),
    tipos_de_postulacion=c("Autopostulado")
  ){
    
    library(ggplot2)
    library(ggthemes)
    #
    candidates %>%
      
      dplyr::filter( tipo_postulacion %in% tipos_de_postulacion ) %>% 
    
    
     people_with_ids_df.genderdize() %>%
      mutate(
        fecha=stringr::str_replace(fecha, pattern="[^,]+,", replacement="")
      ) %>%
      mutate(
        fecha=stringr::str_squish(fecha)
      ) %>%  
      mutate(
        fecha=remediar_fechas(fecha, sep="-")
      ) %>%  
      mutate(
        fecha=as.Date(fecha, format="%d-%m-%Y")
      ) %>% 
      na.omit() %>% 
      
      mutate(
        month=lubridate::month(fecha),
        year=lubridate:::year(fecha)) %>%
      
      dplyr::mutate(
        periodo=cut(fecha %>% as.Date(),
                    breaks = fechas_de_corte %>% as.Date(),
                    right = TRUE, 
                    include.lowest = TRUE)
      ) %>%
      
      split(paste(.$gender, .$periodo, sep="x")) %>%     
      lapply(function(sub_df){
        sub_df=sub_df  
        data.frame(
          size=nrow(sub_df),
          sexo=sub_df$gender[1],
          periodo=sub_df$periodo[1],
          year=sub_df$year[1])
      }) %>% 
      bind_rows() %>%
      na.omit() %>%
      
      #
      ggplot(aes(x=periodo, y=size, group=sexo))+
      geom_col(aes(fill=sexo, col=sexo),
               alpha=.5,
               position=position_dodge(width=.7),
               width=.7
      )+
      geom_text(
        aes(label=size),
        position=position_dodge(width=.7),
        width=.7,
        size=3
      )+
      
      facet_wrap(~ year, 
                 nrow = 1,
                 scales = "free_x"
      )+
      theme_stata()
  }


candidates.genderdize.postulations_over_time.plot(
  tipos_de_postulacion = 
    c("Autopostulado",
      "Autopostulado Asistido",
      "Remitido por prestador")) 
#
candidates.genderdize.postulations_over_time.plot(
  tipos_de_postulacion = 
    c("Autopostulado")) 
#
candidates.genderdize.postulations_over_time.plot(
  tipos_de_postulacion = 
    c(
      "Remitido por prestador")) 
