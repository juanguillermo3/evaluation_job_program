#
# 0.1 set up de R
#
#rm(list=ls())


#
# 0.2 routing
#
#
APP_HOME= "C://Users//josio//OneDrive - Fedesarrollo//proyecto grade" 
RUTA_HOME = "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/ruta de empleabilidad" 



#
#
library(dplyr)
db.Sample_from_Ruta_de_Empleabilidad=function(
  sample_file= "muestra de la ruta de empleabilidad 4.xlsx"
){
  #
  #
  library(dplyr)
  setwd(RUTA_HOME)
  readxl::read_excel( sample_file, col_types = "text") %>%
    
    transmute( doc_identidad=cedula, 
               # raw_info_ruta= .[["ruta empleabilidad"]] %>% as.character() %>%
               # stringr::str_replace_all(.,"^.'", "<FIRST RECORD> \\nTipo sesion: " ),
               raw_info_ruta= .[["ruta empleabilidad"]]  %>% as.character() %>%
                 stringr::str_replace_all(.,",", "<END OF SESSION RECORD>" ),
               algun_ofrecimiento=raw_info_ruta!="",
               procesos_seleccionado=seleccionado)  
}
#
db.Sample_from_Ruta_de_Empleabilidad() %>% View()


#
#
library(dplyr)
db.from_files_data_reader_virtual_profiles=function(
  sample_prefix="scraper delivery",
  encoding="UTF-8"
){
  #
  #
  library(dplyr)
  setwd(RUTA_HOME)
  l=
  list.files(pattern=sample_prefix) %>%
    
    lapply(., function(some_csv){
      
      df=try({
      read.csv(some_csv,
               fileEncoding ="UTF-8" ) %>% 
        lapply(., function(data_col){
          as.character(data_col)
        }) %>%
          as.data.frame() 
      })
    }) 
  l=l[sapply(l, function(x){"data.frame" %in% class(x)})]
  l %>% bind_rows() %>%
    ggplot()
  
}
#
db.from_files_data_reader_virtual_profiles() %>% View()
#
Virtual_profiles_df.basic_curation_of_route_of_employability=
  function(
  Virtual_profiles_df=db.from_files_data_reader_virtual_profiles()
){
  
    
    Virtual_profiles_df %>%
      transmute( doc_identidad=cedula, 
                 # raw_info_ruta= .[["ruta empleabilidad"]] %>% as.character() %>%
                 # stringr::str_replace_all(.,"^.'", "<FIRST RECORD> \\nTipo sesion: " ),
                 raw_info_ruta= .[["ruta"]]  %>% as.character() %>%
                   stringr::str_replace_all(.,",", "<END OF SESSION RECORD>" ),
                 algun_ofrecimiento=raw_info_ruta!=""
                 ) %>%
    dplyr::filter(
      !duplicated(doc_identidad)
    )
}
#
Virtual_profiles_df.basic_curation_of_route_of_employability_cache=Virtual_profiles_df.basic_curation_of_route_of_employability() 
#
Virtual_profiles_df.basic_curation_of_route_of_employability_cache %>% View()


#
#
Ruta_de_empleabilidad_df.Personal_session_records=function(
  Ruta_de_empleabilidad_df=Virtual_profiles_df.basic_curation_of_route_of_employability_cache
){
  
  #
  session_records=
    
    Ruta_de_empleabilidad_df %>%
    
    split(.[["doc_identidad"]]) %>%
    
    lapply(., function(personal_records_df){
      
      #if(is.na(.)=="") {return(NA)}
      an=
      list(
        doc_identidad=personal_records_df$doc_identidad,
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
# 0.4 muestrear informaci?n de la ruta de empleabilidad
#
Ruta_de_empleabilidad_df.sample_non_missing_values=function(
  Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad(),
  sample_size=10
){
  
  #
  Ruta_de_empleabilidad_df %>% 
    
    dplyr::filter(algun_ofrecimiento==TRUE) %>% 
    
    .[["raw_info_ruta"]] %>%
    
    sample(.,  sample_size)}
#
Ruta_de_empleabilidad_df.sample_non_missing_values()
#
Ruta_de_empleabilidad_df.sample_non_missing_values(
  Virtual_profiles_df.basic_curation_cache
)


#
merge_fields=function(field1,
                      field2){
  field1[is.na(field1)|field1=="NA"]=""
  field2[is.na(field2)|field2=="NA"]=""
  paste(as.character(field1), as.character(field2), sep="")
}
#
# 1.2 registros de las sesiones como un data frame
#
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
              an$documento=personal_records$doc_identidad
              an 
            })

        }) %>% bind_rows()
      
      an
    })
  #      
  an=an[sapply(an, function(df){length(df)>2})] %>% bind_rows() 
  an 
  

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
           documento=documento,
           fecha=.$"fecha_programada",
           fecha_final=.$"fecha_final"
    ) 
  #
  an=
  list(an, data.frame(documento=names(session_records),
                      nombre="se registró",
                      prestador= "CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS"
                      
                      )) %>% bind_rows()
  an
}
#
ruta_de_empleabilidad.Session_records.Standardize()  %>% View()
#
ruta_de_empleabilidad.Session_records.Standardize.cache=ruta_de_empleabilidad.Session_records.Standardize() 

ruta_de_empleabilidad.Session_records.Standardize.cache %>% View()











#
Textual_feature.basic_standardization=function(
  names
){
  #
  tolower(names) %>%
    iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
  #
}
# #
# nlp.Textual_feature.basic_standardization.prototype_bigram_feature=function(
#   Textual_feature=ruta_de_empleabilidad.Session_records.Standardize.cache$nombre
# ){
# 
#     text2vec::create_vocabulary(text2vec::itoken(Textual_feature %>% Textual_feature.basic_standardization()), c(2,2)) %>%
#     as.data.frame() %>%
#     dplyr::arrange(term_count*(-1))
# }
# #
# nlp.Textual_feature.basic_standardization.prototype_bigram_feature() %>% View()
# #
# nlp.textual_feature.prototype_bigram_feature.plot=function(
#   Textual_feature=ruta_de_empleabilidad.Session_records.Standardize.cache$nombre,
#   num_bigrams=30
# ){
#   library(ggplot2)
#   nlp.Textual_feature.basic_standardization.prototype_bigram_feature(Textual_feature) %>%
#     dplyr::arrange(term_count*(-1)) %>%
#     dplyr::filter(1:nrow(.)<num_bigrams) %>%
#     dplyr::arrange(term_count*(1)) %>%
#     dplyr::mutate(term=factor(term, levels=term)) %>%
#     ggplot(., aes(x=term, y=term_count))+
#     geom_col(fill=rgb(0,0,0,.3))+
#     coord_flip()
# }
# #
# nlp.textual_feature.prototype_bigram_feature.plot(
# )
# #
# #
# nlp.textual_feature.prototype_bigram_feature.plot(
#   Textual_feature=
#     ruta_de_empleabilidad.Session_records.Standardize.cache %>% 
#     dplyr:::filter(.,  grepl("minuto", tolower(prestador))  ) %>% 
#     .$nombre
# )
# 
# 
# #
# #
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
  people_df=Standardized_records_df,
  Oferentes_df=Oferentes_inscritos.db.oferentes_inscritos()
){

  dplyr::left_join(
    people_df,
    Oferentes_df %>%
      dplyr::transmute(
        documento=documento,
        gender=ifelse(genero=="F", "mujer", "hombre")
      ),
    by="documento"
  )

}
# #
# people_with_ids_df.genderize() %>% View()
# 
# 
# #
# #
# Standardized_records_df.prestados_es_minuto_de_dios.plot_treatment=function(
#   Standardized_records_df=ruta_de_empleabilidad.Session_records.Standardize.cache,
#   Treatment_regex=list(
#     "Entrevista"=c("orientacion laboral"), 
#     "Competencias transversales"=c("transversales"),
#     "Búsqueda de empleo"=c("busqueda de"),
#     "Migrantes"=c("migrantes"),
#     "Laborales"=c("competencias laborales"),
#     "Virtuales"=c("virtual")
#   ),
#   Oferentes_df=Oferentes_inscritos.db.oferentes_inscritos() 
# ){
# 
#   W=nrow(Oferentes_df)/(Standardized_records_df %>% .$"documento" %>% unique() %>% length() )
#   
#     Standardized_records_df %>% 
#     #
#     # subset sessions provided by the minuto de dios
#     #
#     dplyr:::filter(.,  grepl("minuto", tolower(prestador))) %>% 
#     dplyr:::filter(.,  !grepl("^n", tolower(estado))) %>% 
#     #
#     # standardize langauge of session record
#     #
#     mutate(nombre=Textual_feature.basic_standardization(nombre)) %>% 
#     #
#     # split per gender
#     #
#     people_with_ids_df.genderize() %>% 
#     split(.$"gender") %>% 
#     lapply(., function(gender_df){
#       
#       gender_df %>%
#         {
#           lapply(names(Treatment_regex), function(tr_regex){
#             data.frame(sex=.$"gender"[1],
#                        treatment=tr_regex,
#                        num_personas=sum(grepl(Treatment_regex[[tr_regex]], .$nombre)))
#           }) 
#         } %>% bind_rows() 
#       
#     }) %>% bind_rows() %>%
#     dplyr::mutate(
#       num_personas=as.integer(W*(num_personas))
#     ) %>%
#     dplyr::arrange(num_personas*(-1)) %>%
#     dplyr::mutate(treatment=factor(treatment, levels=treatment[.$sex=="mujer"])) %>%
#     ggplot(., aes(x=treatment, y=num_personas, group=sex))+
#     geom_col(aes(fill=sex), 
#              position=position_dodge2(
#                width=0.2,
#                padding=.2))+
#               
#     geom_text(aes(label=num_personas), position=position_dodge(width = .9))+ 
#     theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
#     labs(
#       title="Funcionamiento histórico de la ruta de empleabilidad",
#       subtitle = "Cantidad de participantes, por actividad",
#       caption=""
#       )+
#       xlab("Actividad")+
#       ylab("# personas")
# }
# #
# Standardized_records_df.prestados_es_minuto_de_dios.plot_treatment(
# 
# )
# #
# Standardized_records_df.prestados_es_minuto_de_dios.plot_treatment(
#   Treatment_regex=list(
#     "Entrevista"=c("orientacion laboral"), 
#     "Competencias transversales"=c("transversales"),
#     "Búsqueda de empleo"=c("busqueda de"),
#     "Especiales migrantes"=c("migrantes"),
#     "Grupal"=c("activid")
#     )
# ) 
# 
# 
# Standardized_records_df.prestados_es_minuto_de_dios.plot_treatment(
#   Treatment_regex=list(
#     "Entrevista"=c("orientacion laboral"), 
#     "Competencias transversales"=c("transversales"),
#     "Búsqueda de empleo"=c("busqueda de"),
#     "Mitigacion"=c("mitiga"))
# ) 
# 
# 
# #
# Standardized_records_df.prestados_es_minuto_de_dios.plot_treatment_twoways=function(
#   Standardized_records_df=ruta_de_empleabilidad.Session_records.Standardize.cache,
#   Treatment_regex=list(
#     "Entrevista"=c("orientacion laboral"), 
#     "Competencias transversales"=c("transversales"),
#     "Búsqueda de empleo"=c("busqueda de"))
# ){
#   #
#   compound_table=
#   expand.grid(treatment1=as.character(names(Treatment_regex)),
#               treatment2=as.character(names(Treatment_regex))) %>% as.data.frame()
# 
#   
#   #
#   library(ggplot2)
#     Standardized_records_df %>%
#     dplyr:::filter(.,  grepl("minuto", tolower(prestador))) %>% 
#     dplyr:::filter(.,  !grepl("^n", tolower(estado))) %>% 
#     mutate(nombre=Textual_feature.basic_standardization(nombre)) %>% 
#     split(.$documento) %>% 
#     lapply(., function(doc_df){
#       data.frame(doc_num=doc_df$documento, 
#                  sessions=paste(doc_df$nombre, collapse=" AND "))[1,]
#     }) %>%
#     bind_rows()  %>% 
#     {
#       lapply(1:nrow(compound_table), function(compound_case){
#         #
#         data.frame(treatment1=compound_table$treatment1[[compound_case]], 
#                    treatment2=compound_table$treatment2[[compound_case]], 
#                    num_personas=
#                      sum(grepl( Treatment_regex[[compound_table$treatment1[[compound_case]]]], .$sessions) &
#                            grepl( Treatment_regex[[compound_table$treatment2[[compound_case]]]], .$sessions)))
#       }) 
#     } 
# }
# #
# Standardized_records_df.prestados_es_minuto_de_dios.plot_treatment_twoways()
# 
# #
# # call it a service
# 
# Session_records.Standardize.cache
# ruta_de_empleabilidad.Session_records.Standardize.cache
# 
# #
# ruta_de_empleabilidad.Session_records.Standardize.cache %>% View()
# 
# 
# #
# #
# Sessions_df.prestador_es_minuto_de_dios.sesiones_como_tratamiento=function(
#   Session_df=Session_records.Standardize(
#     session_records = Batch_of_Documentos.Personal_session_records(
#       Batch_of_Documentos=
#         db.Sample_from_Ruta_de_Empleabilidad() %>%
#         .[["doc_identidad"]] ))
# ){
#   #
#   #
#   people_per_type_of_session=
#   Session_df %>% 
#     dplyr::filter(prestador_punto_de_atenci?n=="CORPORACI?N EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" ) %>% 
#     split( .[["tipo_sesion" ]]) %>% 
#     sapply(., function(partial_df_per_type){
#       nrow(partial_df_per_type)
#       partial_df_per_type %>%
#       dplyr::filter(estado=="Asisti?" ) %>%
#       dplyr::filter(!duplicated(documento) ) %>% nrow()
#     })
#   
#   data.frame("Tipo_de_actividad"=names(people_per_type_of_session) %>% as.character(),
#              "Cantidad_de_personas_afectadas"=people_per_type_of_session %>% as.numeric()*(15000/1000)) %>%
#   mutate("Proporcion_de_tratamiento"= round( (Cantidad_de_personas_afectadas/15000)*100, 3)  )
#   
# }
# #
# Sessions_df.prestador_es_minuto_de_dios.sesiones_como_tratamiento() 
# 
# 
# #
# #
# Sessions_df.prestador_es_minuto_de_dios.focalizacion_de_tratamiento_balance=function(
#   
#   Session_df=Session_records.Standardize(
#     session_records = Batch_of_Documentos.Personal_session_records(
#       Batch_of_Documentos=
#         db.Sample_from_Ruta_de_Empleabilidad() %>%
#         .[["doc_identidad"]] )),
#   
#   Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad(),
#   
#   Oferentes_inscritos_df=db.Oferentes_Inscritos() 
# ){
#   #
#   #
#   balanced_sample_per_group=list()
#   #
#   balanced_sample_per_group[["cedulas_tratamiento"]]=
#     Session_df %>% 
#     dplyr::filter(prestador_punto_de_atenci?n=="CORPORACI?N EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" ) %>%
#     .[["documento"]]
#   #
#   balanced_sample_per_group[["cedulas_control"]]=
#   Ruta_de_empleabilidad_df %>%
#     dplyr::filter(is.na(algun_ofrecimiento)) %>% 
#     dplyr::sample_n(., length(balanced_sample_per_group$cedulas_tratamiento)) %>%
#     .[["doc_identidad"]] 
#   
#   #  
#   intersect(balanced_sample_per_group$cedulas_tratamiento, balanced_sample_per_group$cedulas_control)
#   lapply(balanced_sample_per_group , head)
#   lapply(balanced_sample_per_group , length)
#   
#   #
#   lapply(balanced_sample_per_group, function(cedulas_grupo){
#     length(cedulas_grupo)
#     
#     partial_df=
#     Oferentes_inscritos_df %>%
#       dplyr::filter(
#         Oferentes_inscritos_df$documento %in% cedulas_grupo
#       ) 
#     
#     data.frame(
#       promedio_de_edad= round(mean(partial_df$edad), 3),
#       proporcion_de_mujeres= round(mean(partial_df$genero=="F")*100, 3),
#       
#       educacion_universitaria= 
#         round(mean(partial_df$estudios %in% c("Universitaria", "Maestr?a", "Especializaci?n", "Doctorado" ), na.rm=TRUE), 3),
#       
#       programas_de_gobierno= round(mean(!is.na(partial_df$government_program))*100, 3)
#       
#     )
#     
#   }) %>% bind_rows()
#   
# }
# #
# Sessions_df.prestador_es_minuto_de_dios.focalizacion_de_tratamiento_balance()
# 
# 
# 
# 
# #
# Sessions_df.prestador_es_minuto_de_dios.focalizacion_de_tratamiento_balance=function(
#   
#   Session_df=Session_records.Standardize(
#     session_records = Batch_of_Documentos.Personal_session_records(
#       Batch_of_Documentos=
#         db.Sample_from_Ruta_de_Empleabilidad() %>%
#         .[["doc_identidad"]] )),
#   
#   Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad(),
#   
#   Oferentes_inscritos_df=db.Oferentes_Inscritos() 
# ){
#   #
#   #
#   balanced_sample_per_group=list()
#   #
#   balanced_sample_per_group[["cedulas_tratamiento"]]=
#     Session_df %>% 
#     dplyr::filter(prestador_punto_de_atenci?n=="CORPORACI?N EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" 
#                   & tipo_sesion== "Competencias Claves y Transversales" ) %>%
#     .[["documento"]]
#   #
#   balanced_sample_per_group[["cedulas_control"]]=
#     Ruta_de_empleabilidad_df %>%
#     dplyr::filter(is.na(algun_ofrecimiento)) %>% 
#     dplyr::sample_n(., length(balanced_sample_per_group$cedulas_tratamiento), replace=TRUE) %>%
#     .[["doc_identidad"]] 
#   
#   #  
#   intersect(balanced_sample_per_group$cedulas_tratamiento, balanced_sample_per_group$cedulas_control)
#   lapply(balanced_sample_per_group , head)
#   lapply(balanced_sample_per_group , length)
#   
#   #
#   lapply(balanced_sample_per_group, function(cedulas_grupo){
#     length(cedulas_grupo)
#     
#     partial_df=
#       Oferentes_inscritos_df %>%
#       dplyr::filter(
#         Oferentes_inscritos_df$documento %in% cedulas_grupo
#       ) 
#     
#     data.frame(
#       promedio_de_edad= round(mean(partial_df$edad), 3),
#       proporcion_de_mujeres= round(mean(partial_df$genero=="F")*100, 3),
#       
#       educacion_universitaria= 
#         round(mean(partial_df$estudios %in% c("Universitaria", "Maestr?a", "Especializaci?n", "Doctorado" ), na.rm=TRUE), 3),
#       
#       programas_de_gobierno= round(mean(!is.na(partial_df$government_program))*100, 3)
#       
#     )
#     
#   }) %>% bind_rows()
#   
# }
# #
# Sessions_df.prestador_es_minuto_de_dios.focalizacion_de_tratamiento_balance()
# 
# 
# 
# 
# 
# #
# #
# Sessions_df.prestador_es_minuto_de_dios.describir_resultado=function(
#   Session_df=Session_records.Standardize(
#     session_records = Batch_of_Documentos.Personal_session_records(
#       Batch_of_Documentos=
#         db.Sample_from_Ruta_de_Empleabilidad() %>%
#         .[["doc_identidad"]] )),
#   tipo="Competencias Claves y Transversales"
# ){
#   
#   #
#   balanced_sample_per_group=list()
#   #
#   balanced_sample_per_group[["cedulas_tratamiento"]]=
#     Session_df %>% 
#     dplyr::filter(prestador_punto_de_atenci?n=="CORPORACI?N EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" 
#                   & tipo_sesion==  tipo) %>%
#     .[["documento"]]
#   #
#   balanced_sample_per_group[["cedulas_control"]]=
#     Ruta_de_empleabilidad_df %>%
#     dplyr::filter(is.na(algun_ofrecimiento)) %>% 
#     dplyr::sample_n(., length(balanced_sample_per_group$cedulas_tratamiento), replace=TRUE) %>%
#     .[["doc_identidad"]] 
#   
#   sapply(balanced_sample_per_group, function(cedulas_grupo){
#     df=
#     dplyr::filter(Ruta_de_empleabilidad_df,Ruta_de_empleabilidad_df$doc_identidad %in% cedulas_grupo) 
#     round(mean(grepl(df$procesos_seleccionado, pattern="Colocado"))*100,3)
#   })
# }
# #
# Sessions_df.prestador_es_minuto_de_dios.describir_resultado()
# Sessions_df.prestador_es_minuto_de_dios.describir_resultado(tipo="Entrevista")
# 
# 
# #
# #
# #
# sort(prop.table(table(Session_df$prestador_punto_de_atenci?n))*100, decreasing = TRUE)[1:5]
# #
# Sessions_df.prestador_es_minuto_de_dios.sesiones_como_tratamiento() 
# #
# Sessions_df.prestador_es_minuto_de_dios.focalizacion_de_tratamiento_balance()
# #
# Sessions_df.prestador_es_minuto_de_dios.describir_resultado()
# 
# 
# #
# #
# Sessions_df.prestador_es_minuto_de_dios.tratados_a_traves_del_tiempo=function(
#   Session_df=Session_records.Standardize(
#     session_records = Batch_of_Documentos.Personal_session_records(
#       Batch_of_Documentos=
#         db.Sample_from_Ruta_de_Empleabilidad() %>%
#         .[["doc_identidad"]] ))
# ){
#   
#   i_am_my_name=function(x){
#     names(x)=x
#     x
#   }
#   #
#   comienzo_mes=
#   seq.Date(from=lubridate::dmy("01/01/2017"), 
#            to=lubridate::dmy("01/01/2021"), 
#            by = "month") %>%
#     i_am_my_name()
#   
#   temp_df=
#   Session_df %>% 
#     dplyr::filter(prestador_punto_de_atenci?n=="CORPORACI?N EL MINUTO DE DIOS/CORPORACION MINUTO DE DIOS- BOGOTA D.C." ) %>%
#     mutate(fecha_terminada=
#              sapply(fecha_programada, function(f){
#                unlist(stringr::str_split(f, "-")[[1]])
#              })
#            )
#   
#   fechas_terminadas2=
#   lubridate::dmy(temp_df$fecha_terminada %>% unlist())
#   
#   n=
#   sapply(comienzo_mes, function(comienzo){
#     sum(comienzo>fechas_terminadas2)
#   })*5
#   
#   plot( as.factor(comienzo_mes), n,
#         main="# Personas atendidas por ruta de atencion",
#         col=rgb(0,0,0,.3))
# }
# #
# Sessions_df.prestador_es_minuto_de_dios.tratados_a_traves_del_tiempo()
# 
#   
# 
# 
# 
# #
# # papelera de reciclaje para este modulo
# #
# 
# 
# 
# #
# #
# Ruta_de_empleabilidad_df.colocaciones_por_oferente=function(
#   Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad()
# ){
#   #
#   Ruta_de_empleabilidad_df  %>%
#     
#     mutate(informacion_bruta_colocaciones=procesos_seleccionado) %>%
#     
#     #dplyr::filter(informacion_bruta_colocaciones!="No existen procesos") %>% 
#     
#     split(.[["doc_identidad"]]) %>% 
#     
#     lapply(., function(cedula_df){
#       
#       cedula_df[["informacion_bruta_colocaciones"]] %>% 
#         
#         stringr::str_split("\r\n") %>% unlist() %>%
#         
#         stringr::str_replace(.,  "Cerrada por vencimiento Colocado.*$", "")
#       
#     })
#   
# }
# #
# Ruta_de_empleabilidad_df.colocaciones_por_oferente() %>% View()
# Ruta_de_empleabilidad_df.colocaciones_por_oferente() %>% head()
# Ruta_de_empleabilidad_df.colocaciones_por_oferente() %>% length()
# #
# Ruta_de_empleabilidad_df.training_sessions(
#   Virtual_profiles_df.basic_curation_of_route_of_employability_cache
# ) %>% head()
# 
# 
# #
# basic_standardization=function(
#   names
# ){
#   #
#   tolower(names) %>%
#     #
#     iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
#   #
# }
# 
# 
# #
# #
# frequency_models=
#   Ruta_de_empleabilidad_df.colocaciones_por_oferente() %>%
#   basic_standardization() %>%
#   text2vec::word_tokenizer() %>%
#   unlist() %>% {
#     
#     t=sort(table(.), decresing=FALSE)
#     t=sort(table(.), decresing=FALSE)
#     t[t>1]
#     
#   }
# 
# #
# #
# #
# #
# Ruta_de_empleabilidad_df.tabla_de_colocaciones=function(
#   Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad()
# ){
#   #
#   Ruta_de_empleabilidad_df  %>%
#     
#     mutate(informacion_bruta_colocaciones=procesos_seleccionado) %>%
#     
#     dplyr::filter(informacion_bruta_colocaciones!="No existen procesos") %>% 
#     
#     .[["informacion_bruta_colocaciones"]] %>% 
#     
#     stringr::str_split("\r\n")
# }
# 
# 
# #
# #
# Ruta_de_empleabilidad_df.tabla_colocaciones=function(
#   Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad()
# ){
#   
#   #
#   #
#   Ruta_de_empleabilidad_df %>%
#     mutate(
#       informacion_bruta_colocaciones=procesos_seleccionado
#     ) %>% View()
#   
#   
#   
# }



