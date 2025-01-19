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

#
# other services
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

setwd(CODE_HOME)
source("persistence utils.R", encoding="UTF-8")

setwd(CODE_HOME)
source("fechas de registro.R", encoding="UTF-8")

#
# people assisted
# --------
#

#
library(dplyr)
db.cargar_participaciones=function(
  incluir_registros=TRUE
){
  
  #
  if ("db.cargar_participaciones.cache" %in% names(.GlobalEnv)){
    print("dataset loaded from cache")
    return(.GlobalEnv[["db.cargar_participaciones.cache"]])
  }
  #
  # else...
  #
  setwd(PARTICIPATION_RECORDS_HOME)
  new_df=
    readxl::read_excel("particacion actividades.xlsx") %>%
    dplyr::mutate(nombre_actividad=nombre)
  .GlobalEnv[["db.cargar_participaciones.cache"]]=new_df
  print("dataset loded from filesys")
  return(new_df)
}
#
db.cargar_participaciones() %>% View()
#

#
participaciones_df.attended_by_cmd=function(
  participaciones_df=db.cargar_participaciones(),
  regex_minuto="minuto",
  text_normalizer=function(str){tolower(str)}
){
  participaciones_df %>%
    dplyr::mutate(prestador=text_normalizer(prestador)) %>%
    dplyr::filter(stringr::str_detect(prestador, regex_minuto)) %>%
    dplyr::arrange(doc_num) %>%
    bind_rows()
}
#
participaciones_df.attended_by_cmd() %>% View()

#
participaciones_df.last_participation=function(
  participaciones_df=
    participaciones_df.attended_by_cmd() 
){
  participaciones_df %>% 
    dplyr::arrange(doc_num, fecha) %>%
    split(.$doc_num) %>%
    lapply(function(sub_df){
      
      sub_df %>%
        head(1)
      
    }) %>%
    bind_rows() %>%
    dplyr::mutate(fecha=as.Date(fecha)) %>%
    return()
}
#
participaciones_df.last_participation() %>% View()

#
participaciones_df.attended_by_non_cmd=function(
  participaciones_df=db.cargar_participaciones(),
  regex_minuto="minuto",
  text_normalizer=function(str){tolower(str)}
){
  participaciones_df %>%
    dplyr::mutate(prestador=text_normalizer(prestador)) %>%
    dplyr::filter(!stringr::str_detect(prestador, regex_minuto)) %>%
    dplyr::filter(!stringr::str_detect(estado, "^N")) %>%
    dplyr::arrange(doc_num) %>%
    bind_rows()
}
participaciones_df.attended_by_non_cmd() %>% View()
#
participaciones_df.attended_by_non_cmd.people_assisted=
  function( 
    non_comd_participations=participaciones_df.attended_by_non_cmd() 
    ){
    non_comd_participations %>% 
      split(.$doc_num) %>%
      lapply(function(sub_df){
        data.frame(
          doc_num=sub_df$doc_num[1],
          num_activities=nrow(sub_df)
        )
      }) %>%
      bind_rows()
  }
#
participaciones_df.attended_by_non_cmd.people_assisted() %>% View()

#
Dataframe.crear_porcentajes=function(
  Dataframe,
  variable_de_conteos,
  tamano_poblacion
){
  Dataframe[[ sprintf("%s_porcentaje", variable_de_conteos) ]]=
    round((Dataframe[[ variable_de_conteos]]/tamano_poblacion)*100,3)
  return(Dataframe)
}
#
participaciones_df.principales_actividades=function(
  participaciones_df=participaciones_df.attended_by_cmd(),
  key_terms=c(
    "orientación",
    "competencias",
    "búsqueda",
    "migrante"
  ),
  text_normalizer=function(str){tolower(str)}
){
  #
  participaciones_df = participaciones_df %>%
    dplyr::mutate(nombre_actividad=text_normalizer(  participaciones_df$nombre_actividad))
  num_cedulas= length(unique(participaciones_df$doc_num))
  acts=participaciones_df$nombre_actividad %>% unique()
  #
  new_df=
  lapply(acts, function(act){
    sub_df=
    participaciones_df %>%
      dplyr::filter(nombre_actividad==act)  %>%
      dplyr::distinct(doc_num, .keep_all=TRUE)
    data.frame(
      nombre_actividad=sprintf("Actividad '%s'",act ),
      personas_atendidas=length(sub_df$doc_num)) %>%
      Dataframe.crear_porcentajes("personas_atendidas", num_cedulas)
  }) %>%
    bind_rows()
  #
  new_df2=
  lapply(key_terms, 
         function(term){
           data.frame(
             nombre_actividad=sprintf("Palabra clave '%s'",term ),
             personas_atendidas=sum(stringr::str_count(participaciones_df$nombre_actividad, term))
           ) %>%
             Dataframe.crear_porcentajes("personas_atendidas", num_cedulas)
         })
  #
  list(new_df, new_df2)  %>%
    bind_rows() %>%
    dplyr::mutate(
      personas_atendidas=as.numeric( personas_atendidas)
    ) %>%
    dplyr::arrange(
    personas_atendidas*(-1) 
  ) 
}
#
participaciones_df.principales_actividades() %>% View()

#
participaciones_df.asistencias_efectivas=function(
  participaciones_df=participaciones_df.attended_by_cmd(),
  regex_estados_validos="(^asist)|(^aprob)|(^final)",
  text_normalizer=function(str){tolower(str)}
){
  participaciones_df %>% 
    dplyr::mutate(
      estado=text_normalizer(estado)
    ) %>%
    dplyr::filter(
      stringr::str_detect(estado, regex_estados_validos )
    ) 
} 
participaciones_df.asistencias_efectivas() %>% View()
#
participaciones_df.people_assisted_by_cmd=function(
  participaciones_df=participaciones_df.attended_by_cmd(),
  key_terms=c(
    "orientación",
    "competencias",
    "búsqueda",
    "migrante"
  ),
  text_normalizer=function(str){tolower(str)}
){
  
  participaciones_df %>%
    participaciones_df.asistencias_efectivas() %>%
    dplyr::mutate(nombre_actividad=text_normalizer(nombre_actividad)) %>% 
    split(.$doc_num) %>%
    lapply(function(doc_df){
    
      doc_df = doc_df 
        
      new_df=
      data.frame(
        doc_num=doc_df$doc_num[1],
        activ_cualquiera=nrow(doc_df)-1
      )
      for (term in key_terms){
        new_df[[sprintf("activ_%s",term)]]=sum(stringr::str_count(doc_df$nombre_actividad, term))
      }
      new_df
    }) %>%
    bind_rows() 
  # %>%
  #   Dataframe.aggregate(
  #     aggregated = c("activ_orientacion", "activ_busqueda", "activ_transversales" ),
  #     label="activ_cualquiera"
  #   )
  
}
#
participaciones_df.people_assisted_by_cmd() %>%
  Dataframe.apply_valid_names_for_stata() %>% View()

#
people_assisted_by_cmd.genderized_overview=function(
  people_assisted_by_cmd=
    participaciones_df.people_assisted_by_cmd() %>%
      Dataframe.apply_valid_names_for_stata()
){
  people_assisted_by_cmd %>% 
    
    #
    people_with_ids_df.genderdize() %>% 
    split(.$gender) %>%
    #
    lapply(function(sub_df){
    
      #
      list(
      data.frame(sexo=sub_df$"gender"[1], n_submuestra=nrow(sub_df)),
      #
      sub_df %>%
        dplyr::select(grep(names(.), pattern="activ_")) %>%
        Dataframe.apply_treshold() %>% 
        lapply(function(col){sum(col)}) %>%
        bind_cols()
      ) %>%
        bind_cols() 
      

    }) %>%
    bind_rows() %>%
    Dataframe.order(c(
      "sexo",
      "n_submuestra",
      grep(names(.), pattern="activ_")
      )
    ) %>%
    Dataframe.totalize()
}
#
people_assisted_by_cmd.genderized_overview() %>% View()
#
people_assisted_by_cmd.genderized_overview() %>%
  #
  Dataframe.export_output(
    label="participacion resumen",
    output_home = EXPERIMENTAL_CASES_HOME,
    new_output_array = TRUE
  )
#

#
#
# experimental cases
# --------

#
exclude_fist_element=function(x){
  return(x[2:length(x)])
}
#
people_assisted_by_cmd.experimental_cases=function(
  #
  people_assisted_by_cmd=
    participaciones_df.people_assisted_by_cmd(),
  #
  people_unassisted_by_non_cmd=
    participaciones_df.attended_by_non_cmd.people_assisted()
  #
){
  
  people_assisted_by_cmd %>% 
    
    people_with_ids_df.genderdize() %>%  
    
    dplyr::mutate(
      
      treatment_case={
        people_assisted_by_cmd  %>% 
          dplyr::select(grep(names(.), pattern="activ_")) %>%
        apply(MARGIN=1, function(row){
          #
          if(all(row==0)){return("control")}
          #
          paste(
            exclude_fist_element(names(row)[row>=1])
            , collapse=" AND ")
          #
        })}
    ) %>%
    
    dplyr::left_join(people_unassisted_by_non_cmd) %>%
    
    dplyr::mutate(previous_activities={.$num_activities[is.na(.$num_activities)]=0;.$num_activities})  %>%
    
    dplyr::select(-num_activities)
}
#
people_assisted_by_cmd.experimental_cases.cache=people_assisted_by_cmd.experimental_cases()
#
people_assisted_by_cmd.experimental_cases.cache %>% View()
#
#
people_assisted_by_cmd.experimental_cases.cache %>%
  Dataframe.share_with_app(label="experimental cases", 
                           app_route=EXPERIMENTAL_CASES_HOME)

#
people_assisted_by_cmd.experimental_cases.genderized_overview=
  function(
    people_assisted_by_cmd.experimental_cases=people_assisted_by_cmd.experimental_cases.cache
  ){
    per_gender=
    people_assisted_by_cmd.experimental_cases %>%
      split(.$gender) %>%
      lapply(function(sub_df){
        table(sub_df$treatment_case) %>%
          as.data.frame() %>%
          Dataframe.new_names(c("cadena_de_servicios", sprintf("frecuencia_%s",sub_df$gender[1]))) 
      }) 
    dplyr::left_join(per_gender$mujer,per_gender$hombre,by="cadena_de_servicios") %>%
      dplyr::arrange(frecuencia_mujer*-1) %>%
      Dataframe.totalize( )
  }
#
people_assisted_by_cmd.experimental_cases.genderized_overview() %>% View()

#
people_assisted_by_cmd.experimental_cases.genderized_overview() %>%
  #
  Dataframe.export_output(
    label="overview of experimental cases",
    output_home = EXPERIMENTAL_CASES_HOME,
    new_output_array = TRUE
  )


#
# analytics
# --------

#
db.experimental_cases=function(){
  setwd(EXPERIMENTAL_CASES_HOME)
  readxl::read_excel("experimental cases.xlsx")
}
#
db.experimental_cases() %>% View()

#
experimental_cases.genderized_histogram=function(
  experimental_cases=db.experimental_cases(),
  treatment="activ_cualquiera"
){
  library(ggplot2)
  library(ggthemes)
  experimental_cases %>%
    #
    #
    dplyr::mutate(
      sexo=gender,
      tratamiento=.[[treatment]]
    ) %>% 
    dplyr::mutate(
      tratamiento=factor(tratamiento) 
    ) %>% 
    
    na.omit() %>%
    
    #
    #
    ggplot(aes(x=tratamiento))+
    geom_bar(aes(fill=sexo, col=sexo),
             alpha=.7,
             position=position_dodge(width=1))+
    theme_stata()+
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))
}
#
experimental_cases.genderized_histogram()+
  labs(
    title = "Participacion individual en poblacion de estimacion",
    subtitle = "Cantidad de actividades de los casos experimentales",
    caption = "A partir de registros sobre intermediacion laboral",
    x="Numero de participaciones",
    y="Cantidad de personas"
  )
#

#
# discarded functionality
# --------

#
#
permutate_pattern=function(
  labels=c(
    "Menos de 1 SMMLV",
    "1 SMMLV",
    "1 a 2 SMMLV",
    "2 a 4 SMMLV",
    "4 a 6 SMMLV",
    "6 a 9 SMMLV",
    "15 a 19 SMMLV",
    "A convenir"),
  pattern="[0-9]+ a [0-9]+",
  replacement=function(str){
    stringr::str_replace_all(str, " ", "-")
  }
){
  #
  # define a pattern to be detected
  # define a replacement value for that pattern
  
  labels %>%
    lapply(function(l){
      first_hit=stringr::str_extract(l, pattern)
      if(is.na(first_hit)){return(l)}
      stringr::str_replace(l, first_hit, replacement(first_hit))
    })
}
#
permutate_pattern()
#
tokenizar_labels=function(
  labels=c(
    "Menos de 1 SMMLV",
    "1 SMMLV",
    "1 a 2 SMMLV",
    "2 a 4 SMMLV",
    "4 a 6 SMMLV",
    "6 a 9 SMMLV",
    "15 a 19 SMMLV",
    "A convenir"),
  split_char=" "
){
  labels %>%
    lapply(function(l){
      stringr::str_split(l,split_char) %>%
        unlist() %>%
        paste(collapse=" \n ")
    })
}
#
tokenizar_labels()
#
experimental_cases.genderized_overview.plot=function(
  genderized_overview=experimental_cases.genderized_overview(),
  tratamiento=c(
    "trat_cualquiera",
    "trat_orientacion",
    "trat_transversales",
    "trat_busqueda")
){
  library(ggplot2)
  library(ggthemes)
  genderized_overview %>%
    tidyr::pivot_longer(cols = setdiff(names(.), "sexo") ) %>%
    #dplyr::filter(sexo!="Totales") %>%
    
    dplyr::filter(name!="n_submuestra") %>%
    
    dplyr::mutate(
      zoom=ifelse(
        name %in% c("trat_transversales","trat_busqueda"),
        "zoomed-in",
        "same scale"
        
      )) %>%
    
    dplyr::mutate(name=factor(name, 
                              levels=c("grupo_control",tratamiento),
                              labels=
                                c("grupo_control",tratamiento) %>%
                                tokenizar_labels(split_char = "_")
    ))  %>%
    ggplot(aes(x=name, y=value, group=sexo))+
    geom_col(aes(fill=sexo, col=sexo),
             alpha=.7,
             position=position_dodge(width = .7),
             width=.7) +
    geom_text(aes(y=value,label=value),
              size=3,
              colour="black",
              position=position_dodge(width=.7))+
    facet_wrap(~ zoom, 
               nrow= 1,
               scales = "free"
    )+
    theme_stata()+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))
  #theme(legend.position="none")
  #coord_flip()
}
#
experimental_cases.genderized_overview.plot() +
  labs(
    title = "Casos experimentales",
    subtitle = "Cantidad de sujetos en los grupos comparados, por g?nero",
    caption = "A partir de los registros de intermediaci?n laboral",
    x="N?mero de personas",
    y="Grupo"
  ) 
#
