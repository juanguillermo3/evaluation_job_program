#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")


#
#  (1.1) data reader para el texto de las vacantes
#
basic_standardization=function(
  names
){
  #
  tolower(names) %>% iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
}
#
library(dplyr)
db.raw_text_from_vacancies=function(
  
  scrapped_files=c("info vacantes.csv", "delivery scrapper vacantes 20211123-013735.csv")
  
  ){
  
  #
  setwd(VACANTES_HOME)
  #
  list.files(pattern="((info vacantes)|(delivery.+)).csv$") %>%
  #
  lapply( function(some_file){
    some_file %>%
      read.csv(encoding="UTF-8")%>% 
      {names(.)=c("n", "vacante");.} %>% 
      dplyr::select(vacante) %>%
      mutate(z_vacante=basic_standardization(vacante))
  }) %>% bind_rows()

}
#
db.raw_text_from_vacancies() %>% View()


#
#  (1.2) data reader para la info de las vacantes
#
Vacantes_df.vacantes_model=function(
  Vacantes_df=db.raw_text_from_vacancies(),
  feature_markers=c("codigo_vacante"="[[0-9][-]]{5,15}",
                    "salario"=":[^:]*smmlv",
                    "tipo de contrato"="smmlv(.*)distribucion"
  )
){
  #
  an=Vacantes_df %>%
    mutate(vacante)
  lapply(names(feature_markers), function(some_marker_name){
    an<<-an %>% mutate(new_var=stringr::str_extract(an$z_vacante, feature_markers[[some_marker_name]] ))
    names(an)[length(names(an))]<<-some_marker_name
  })
  an
}
#
Vacantes_df.vacantes_model() %>% View()


#
# vacantes disponibles
#
library(dplyr)
db.cargar_info_completa_vacantes=function(
  Texto_vacantes=Vacantes_df.vacantes_model(feature_markers=c("codigo_vacante"="[[0-9][-]]{5,20}"))
  
){
  library(dplyr)
  setwd(VACANTES_HOME)
  list.files(pattern="Detalle_Vacantes") %>%
    lapply(., function(file_path){
      readxl::read_excel(file_path) 
    }) %>% bind_rows() %>% 
    dplyr::filter(!(is.na(.$"Fecha Publicación"))) %>%
    dplyr::transmute(
      codigo_vacante=.$"Código Proceso",
      cargo=.$"Cargo",
      num_vacantes=.$"Puestos de Trabajo",
      fecha_publicacion=as.Date(.$"Fecha Publicación", format="%d/%m/%Y") 
    ) %>%
    dplyr::left_join(., Texto_vacantes)
}
#
db.cargar_info_completa_vacantes() %>% View()




#
#
#
Digital_skills_df=
  Virtual_profiles.Virtual_profiles_df.Ensemble_features(
    Records=Virtual_profiles_df.model_virtual_cv(),
    Information_channel = "hoja_de_vida",
    feature_markers = c(
      "Edad", 
      "Estado civil",
      "Nivel educativo",
      "Estado",
      "Años de experiencia",
      "Aspiración salarial",
      "Procesador de texto [(]Ejemplo[:] Word[)](.){2}Herramienta",
      "Hoja de cálculo [(]Ejemplo[:] Excel[)](.){2}Herramienta",
      "Otros(.){2}Herramienta",
      "Interesado en ofertas de teletrabajo"
    )
  )  %>% Dataframe.apply_valid_names_for_stata() %>% 
  mutate(
    digital_skills=paste(procesador_de_texto_ejemp, hoja_de_calculo_ejemplo_e, otros_2_herramienta, sep=", "),
    some_digital_skill=ifelse(digital_skills!="NA, NA, NA", 1, 0),
    specialized_software=ifelse( !is.na( otros_2_herramienta), 1, 0),
  )
#
Digital_skills_df %>% View()


#
# fast proptotyping of textual features
#
Textual_feature.basic_standardization=function(
  names
){
  #
  tolower(names) %>% iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
}
#
#
Vector.prioritize=function(
  Vector
){
  t=table(Vector)
  #
  data.frame(
    value=names(t) %>% as.character(),
    freq=t %>% as.numeric()
  ) %>% 
    dplyr::arrange(freq*(1)) %>% 
    dplyr::mutate(
      cum_sum= cumsum(freq),
      cum_prop= round((cum_sum/sum(freq))*100, 3)
    ) 
}
#
#
nlp.Textual_feature.ensemble_vocabulary=function(
  Textual_feature=c(Digital_skills_df$procesador_de_texto_ejemp, 
                    Digital_skills_df$hoja_de_calculo_ejemplo_e,
                    Digital_skills_df$otros_2_herramienta)
                 
){
  Textual_feature %>% 
    na.omit() %>%
    {.[.!=""]} %>% 
    sapply(., function(t){
      stringr::str_replace_all(t, " "," " )
    }) %>% 
    Textual_feature.basic_standardization() %>%
    Vector.prioritize() %>% 
    mutate(term_count=freq) %>%
    dplyr::filter(term_count>1) %>%
    dplyr::arrange(term_count*(-1)) 
}
#
nlp.Textual_feature.ensemble_vocabulary() %>% View()
#
nlp.Textual_feature.ensemble_vocabulary(
  Textual_feature=Digital_skills_df$otros_2_herramienta
) %>% View()
#
nlp.Textual_feature.basic_standardization.prototype_bigram_feature=function(
  Textual_feature=ruta_de_empleabilidad.Session_records.Standardize.cache$nombre
){
  
  text2vec::create_vocabulary(text2vec::itoken(Textual_feature %>% Textual_feature.basic_standardization()), c(2,2)) %>%
    as.data.frame() %>%
    dplyr::arrange(term_count*(-1))
}
#
nlp.Textual_feature.basic_standardization.prototype_bigram_feature() %>% View()
#



#
#
Vacancies_texts.label_technologies=function(
  
  Vacancies_texts_df=db.cargar_info_completa_vacantes(),
  neg_set=c("correo electronico", 
            "arc",
            "correo",
            "basico",
            "funciones"),
  
  Competencias_digitales=  nlp.Textual_feature.ensemble_vocabulary(
    c(Digital_skills_df$procesador_de_texto_ejemp, 
      Digital_skills_df$hoja_de_calculo_ejemplo_e)
  )%>% dplyr::mutate(
    tech=value
  ) %>% 
    dplyr::filter(
      nchar(tech)>=3
    ) %>% 
    dplyr::filter(
      1:nrow(.)<50
    ) %>% 
    dplyr::filter(
      !stringr::str_detect(
        tech ,"^tod"
      )) ,
  Software_especializado=  nlp.Textual_feature.ensemble_vocabulary(
    Digital_skills_df$otros_2_herramienta
  )%>% dplyr::mutate(
    tech=value
  ) %>% 
    dplyr::filter(
      nchar(tech)>=3
    ) %>% 
    dplyr::filter(
      1:nrow(.)<50
    ) %>% 
    dplyr::filter(
      !stringr::str_detect(
        tech ,"^tod"
      )
    ) 
){
  
  #
  competencias_regex=paste("(",
                   setdiff(Competencias_digitales$tech, neg_set)
                   ,
                   ")", sep=" ",collapse="|")
  #
  Software_regex=paste("(",
                           setdiff(Software_especializado$tech, neg_set)
                           ,
                           ")", sep=" ",collapse="|")
  #
  Vacancies_texts_df %>%
    
    dplyr::mutate(standardized_text=z_vacante) %>% 
    
    dplyr::mutate(
      
      competentencias_flag=sapply(.$z_vacante, function(some_text){
      
      stringr::str_detect(some_text, competencias_regex)
      
    }),
    
    competencias=sapply(.$z_vacante, function(some_text){
      
      stringr::str_extract(some_text, competencias_regex)
      
    }))  %>% 
    
    dplyr::mutate(
      
      software_flag=sapply(.$z_vacante, function(some_text){
        
        stringr::str_detect(some_text, Software_regex)
        
      }),
      
      software=sapply(.$z_vacante, function(some_text){
        
        stringr::str_extract(some_text,  Software_regex)
        
      })) 
}
#
Vacancies_texts.label_technologies.cache=Vacancies_texts.label_technologies()
#
Vacancies_texts.label_technologies.cache %>% View()

#
#
Vacancies_texts_df.write_to_data_base=function(
  Vacancies_texts_df=Vacancies_texts.label_technologies.cache 
){
  setwd(VACANTES_HOME)
  #
  #rownames(Vacancies_texts_df)=NULL
  write.table(Vacancies_texts_df,
              sep="\t",
              file="vacantes.txt", 
              fileEncoding="UTF-8")
  #
  print("vacantes persistidas en la base de datos")
}
#
Vacancies_texts_df.write_to_data_base() 
#
db.read_vacancies=function(
  vacantes_file="vacantes.txt"
){
  setwd(VACANTES_HOME)
  #
  read.delim("vacantes.txt", sep="\t",  fileEncoding="UTF-8") %>%
    dplyr::filter( !duplicated( .$codigo_vacante)) %>%
    {row.names(.)=NULL;.}
  
}
#
db.read_vacancies() %>% View()
db.read_vacancies() %>% names()



#
# puestos de trabajo publicados a través del tiempo
#
Vacantes_df.plot_puestos_de_trabajo=function(
  Vacantes_df=Vacancies_texts.label_technologies.cache ,
  fases_covid= lubridate::dmy(
    c("01/01/2017", "20/03/2019", "20/03/2020", "20/03/2021" )
  ) %>%
    {names(.)=c("set-up", "pre-covid",  "covid", "aftermath" );.},

  cut_off=.99
){
  
  t=quantile(Vacantes_df$num_vacantes, cut_off, na.rm=TRUE)
  
  
  #
  # (0) modelar cargos a traves del tiempo
  #
  library(ggplot2)
  Vacantes_df %>% 
    dplyr::filter(.$num_vacantes<t) %>%
    dplyr::filter(!(is.na(.$fecha_publicacion))) %>%
    dplyr::transmute(
      cargo=.$cargo,
      num_vacantes=.$"num_vacantes",
      fecha_publicacion=as.Date(.$fecha_publicacion, format="%Y-%m-%d") 
    ) %>% 
    #
    # (1) segmentar segun la fase del covid
    #
    dplyr::mutate(
      fase_covid=cut(.$fecha_publicacion,
                     breaks =  c(fases_covid, "Inf"),
                     labels=names(fases_covid))
    )  %>% 
    #
    # (2) contar vacantes por mes año
    #
    dplyr::mutate( year_month= paste(lubridate::year(.$fecha_publicacion),
                                     lubridate::month(.$fecha_publicacion),
                                     sep="_"))  %>% 
    split(.$year_month) %>%
    lapply(., function(some_year_month_df){
      some_year_month_df %>% {
      data.frame(year_month=.$year_month[1], 
                 num_vacantes=sum(.$num_vacantes, na.rm=TRUE),
                 min_date=min(.$fecha_publicacion),
                 fase_covid=.$fase_covid[1]
                 )
      }
    }) %>%
    #
    #
    #
    bind_rows() %>%
    dplyr::arrange(min_date) %>%
    dplyr::mutate(
      year_month=factor(year_month, 
                        levels=year_month)
    ) %>%
    #
    #
    {
    ggplot(., aes(x=year_month, y=num_vacantes, group=fase_covid))+
    geom_col(aes(fill=fase_covid),alpha=0.3)+
    labs(title="# of jobs available for year-month") +
    theme(axis.text.x = element_text(angle = 45)) + 
    scale_x_discrete(labels=sapply(1:length(.$year_month), function(index){
      ifelse(index%%2==0, as.character(.$year_month)[[index]], "" )
    }))+
      labs(title="Vacantes a través del tiempo",
           subtitle="Número de vacantes publicadas, por mes",
           caption=sprintf("sobre %s vacantes con intermadiación del Miuto de Dios",
                           sum(.$num_vacantes)))+
        geom_vline(xintercept=.$year_month[.$year_mont=="2020_4"], lty="dashed")
    }
}
#
Vacantes_df.plot_puestos_de_trabajo()
#


#
#
Vacantes_df.factor_incremento_covid_global=function(
  
  Vacantes_df=Vacancies_texts.label_technologies.cache,
  
  fases_covid= lubridate::dmy(
    c("01/01/2017", "20/03/2019", "20/03/2020", "20/03/2021" )
  ) %>%
    {names(.)=c("set-up", "pre-covid",  "covid", "aftermath" );.},
  
  cut_off=.99,
  
  model_cases=list( "General", "competencias_digitales", "software_especializado")
  
){
  
  t=quantile(Vacantes_df$num_vacantes, cut_off, na.rm=TRUE)
  #
  # (0) modelar cargos a traves del tiempo
  #
  library(ggplot2)
  Vacantes_df %>% 
    dplyr::filter(.$num_vacantes<t) %>%
    dplyr::filter(!(is.na(.$fecha_publicacion))) %>%
    dplyr::transmute(
      cargo=.$cargo,
      num_vacantes=.$"num_vacantes",
      fecha_publicacion=as.Date(.$fecha_publicacion, format="%Y-%m-%d"),
      General=TRUE,
      competencias_digitales=competentencias_flag,
      software_especializado=software_flag
    ) %>% 
    #
    # (1) segmentar segun la fase del covid
    #
    dplyr::mutate(
      fase_covid=cut(.$fecha_publicacion,
                     breaks =  c(fases_covid, "Inf"),
                     labels=names(fases_covid))
    )  %>%  
    {
      
      lapply(model_cases, function(some_case){
        
        case_df=
        dplyr::filter(., .[[some_case]] ) %>% 
          {
            data.frame(caso=some_case,
                       pre_covid=sum(.$num_vacantes[.$fase_covid=="pre-covid"], na.rm=TRUE),
                       covid=sum(.$num_vacantes[.$fase_covid=="covid"], na.rm=TRUE)) %>%
            mutate(factor_covid=round(covid/pre_covid,3 ))
          }
      }) 
      
    } %>% bind_rows()
}
#
Vacantes_df.factor_incremento_covid_global()

Vacantes_df.factor_incremento_covid_global.plot=function(
  Vacantes_df=Vacancies_texts.label_technologies.cache,
  
  fases_covid= lubridate::dmy(
    c("01/01/2017", "20/03/2019", "20/03/2020", "20/03/2021" )
  ) %>%
    {names(.)=c("set-up", "pre-covid",  "covid", "aftermath" );.},
  
  cut_off=.99,
  
  model_cases=list( "General", "competencias_digitales", "software_especializado")
){
  
  library(ggplot2)
  Vacantes_df.factor_incremento_covid_global(
    fases_covid=fases_covid,
    cut_off= cut_off,
    model_cases=model_cases
  ) %>%
    tidyr::pivot_longer(cols=c("pre_covid", "covid"), 
                        names_to = "fase_covid") %>%
    mutate(
      caso= factor(caso,
                     levels=c("General",
                              "competencias_digitales",
                              "software_especializado")),
      fase_covid=factor(fase_covid, c("pre_covid", "covid") )
    ) %>%
    {
    ggplot(., aes( group=fase_covid))+
    geom_col(aes(x=caso, 
                 y=value,
                 fill=fase_covid), position=position_dodge2(
      width=0.2,
      padding=.2),
      alpha=.3)+
    
    geom_text(aes(x=caso, 
                  y=value,
                  fill=fase_covid,
                  label=value),
              position=position_dodge(width = .9))+
    facet_wrap(vars(caso), scales="free")+
    
    labs(title="Evolución de las vacantes según fase del covid",
               subtitle="Número de vacantes según fase",
               caption=
                 sprintf("El factor de incremento global fue de %s", 
                         .$factor_covid[.$caso=="General"][1]
                         )
               )
    }+
    
    xlab("Tipo vacante")+
    ylab("Número vacantes")

}
#
Vacantes_df.factor_incremento_covid_global.plot()


  


#
#
Vacantes_df.Modelo_de_fases=function(
  Vacantes_df=db.cargar_vacantes_disponibles(),
  fases_covid= lubridate::dmy(
    c("01/01/2017", "20/03/2019", "20/03/2020", "20/03/2021" )
  ) %>%
    {names(.)=c("set-up", "pre-covid",  "covid", "aftermath" );.},
  
  cut_off=.99
){
  
  t=quantile(Vacantes_df$"Puestos de Trabajo", cut_off, na.rm=TRUE)
  
  
  #
  # (0) modelar cargos a traves del tiempo
  #
  library(ggplot2)
  Vacantes_df %>% 
    dplyr::filter(Vacantes_df$"Puestos de Trabajo"<t) %>%
    dplyr::filter(!(is.na(.$"Fecha Publicación"))) %>%
    dplyr::transmute(
      cargo=.$"Cargo",
      num_vacantes=.$"Puestos de Trabajo",
      fecha_publicacion=as.Date(.$"Fecha Publicación", format="%d/%m/%Y") 
    ) %>%
    #
    # (1) segmentar segun la fase del covid
    #
    dplyr::mutate(
      fase_covid=cut(.$fecha_publicacion,
                     breaks =  c(fases_covid, "Inf"),
                     labels=names(fases_covid))
    )  %>% 
    #
    # (2) contar vacantes por mes año
    #
    dplyr::mutate( year_month= paste(lubridate::year(.$fecha_publicacion),
                                     lubridate::month(.$fecha_publicacion),
                                     sep="_"))  %>% 
    split(.$fase_covid)
  
}
#
Vacantes_df.Modelo_de_fases() %>% names()


#
#
Vacantes_df.tecnologias=function(
  Vacantes_df=db.cargar_vacantes_disponibles()
){
  Vacantes_df
}
Vacantes_df.tecnologias() %>% View()

#
#
Vacantes_df.Modelo_de_fases=function(
  Vacantes_df=db.cargar_vacantes_disponibles(),
  fases_covid= lubridate::dmy(
    c("01/01/2017", "20/03/2019", "20/03/2020", "20/03/2021" )
  ) %>%
    {names(.)=c("set-up", "pre-covid",  "covid", "aftermath" );.},
  
  cut_off=.99
){
  
}





#
#
Vacantes_df.factor_incremento_covid=function(
  Vacantes_df=db.cargar_vacantes_disponibles(),
  fases_covid= lubridate::dmy(
    "01/01/2017",
    "20/03/2019",
    "20/03/2020"),
  periodo_ref="2019-03-20",
  periodo_comp="2020-03-20",
  cut_off=.99,
  min_vacantes=1
){
  
  #
  # (0) modelar cargos a traves del tiempo
  #
  library(ggplot2)
  Vacantes_df %>% 
    dplyr::filter(Vacantes_df$"Puestos de Trabajo"<t) %>%
    dplyr::filter(!(is.na(.$"Fecha Publicación"))) %>%
    dplyr::transmute(
      cargo=.$"Cargo",
      num_vacantes=.$"Puestos de Trabajo",
      fecha_publicacion=as.Date(.$"Fecha Publicación", format="%d/%m/%Y") 
    ) %>%
    #
    # (1) segmentar segun la fase del covid
    #
    dplyr::mutate(
      fase_covid=cut(.$fecha_publicacion, breaks =  c(fases_covid, "Inf"))
    )  %>% 
    #
    # (2) modelo de factor de incremento, a nivel de cargo
    #
    {
      modelo_df=.
      i_am_my_name=function(x){names(x);x}
      i_am_my_name(unique(.$cargo)) %>%
        #
        # Outter over cargos
        #
        lapply(., function(some_cargo){
          #
          # inner over comparison periods
          #
          data.frame(
            cargo=some_cargo,
            pre_covid=
              modelo_df %>% 
              dplyr::filter(fase_covid==periodo_ref ) %>% 
              dplyr::filter(cargo==some_cargo) %>% {
                sum(.$num_vacantes, na.rm=TRUE)
              },
            post_covid=
              modelo_df %>% 
              dplyr::filter(fase_covid==periodo_comp ) %>% 
              dplyr::filter(cargo==some_cargo) %>% {
                sum(.$num_vacantes, na.rm=TRUE)
              })
        })}  %>% bind_rows() 
  
  list(   
  df %>% 
    dplyr::filter(df$pre_covid>min_vacantes) %>% 
    dplyr::mutate(factor_covid= round(post_covid/pre_covid,3) ) %>%
    dplyr::arrange(factor_covid*(-1)),
  Vacantes_df.factor_incremento_covid_global()) %>% bind_rows()
}
#
Vacantes_df.factor_incremento_covid() %>% View()


#
#
Vacantes_df.factor_incremento_covid.plot=function(
  Vacantes_df=db.cargar_vacantes_disponibles(),
  highligted=c("Agente de Call center",
               "Asesor call center",
               "Asesor servicio al cliente",
               "Ingeniero de sistemas",
               "Desarrollador de software",
               "Desarrollador web"),
  non_highligted_sample_size=8,
  fases_covid= lubridate::dmy(
    "01/01/2017",
    "20/03/2019",
    "20/03/2020"),
  periodo_ref="2019-03-20",
  periodo_comp="2020-03-20",
  cut_off=.99,
  min_vacantes=1
){
  
  library(ggplot2)
  Vacantes_df.factor_incremento_covid(
    fases_covid= fases_covid,
    periodo_ref=periodo_ref,
    periodo_comp=periodo_comp,
    cut_off=.99,
    min_vacantes=1
  ) %>%
    dplyr::filter(factor_covid>0)%>%
    dplyr::mutate(
    "Tipo vacante"=ifelse(cargo %in% highligted, 
                          "Relacionada con TIC",
                          "Genéricas")
  ) %>% split(.$"Tipo vacante") %>% {
    list(
    .$"Relacionada con TIC",
    dplyr::sample_n(.$"Genéricas", non_highligted_sample_size),
    dplyr::filter(.$"Genéricas", cargo=="Todos los cargos")
    )
  } %>% bind_rows() %>%
    dplyr::arrange(
      factor_covid*(1)
    ) %>%
    dplyr::mutate(
      clasificacion=factor(.$"Tipo vacante")
    ) %>%
    dplyr::mutate(cargo=factor(cargo, levels=cargo)) %>%
    ggplot(., aes(x=cargo, y=factor_covid, group=clasificacion ))+
    geom_col(aes(fill=clasificacion ), alpha=.3)+
    geom_text(aes(label=factor_covid), nudge_y = 2.5, size=3)+
    coord_flip()+
    labs(title="Incremento de ofertas durante pandemia",
         subtitle=sprintf("Factor de incremento durante pandemia, por cargo"),
         caption="Basado en un modelo con tales caracteristicas"
           )
}
#
Vacantes_df.factor_incremento_covid.plot(
  
)





#
#
Vacantes_df.factor_incremento_covid.plot=function(
  Vacantes_df=db.cargar_vacantes_disponibles(),
  highligted=c("Agente de Call center",
               "Asesor call center",
               "Asesor servicio al cliente",
               "Ingeniero de sistemas",
               "Desarrollador de software",
               "Desarrollador web"),
  non_highligted_sample_size=8,
  fases_covid= lubridate::dmy(
    "01/01/2017",
    "20/03/2019",
    "20/03/2020"),
  periodo_ref="2019-03-20",
  periodo_comp="2020-03-20",
  cut_off=.99,
  min_vacantes=1
){
  
  library(ggplot2)
  Vacantes_df.factor_incremento_covid(
    fases_covid= fases_covid,
    periodo_ref=periodo_ref,
    periodo_comp=periodo_comp,
    cut_off=.99,
    min_vacantes=1
  ) %>%
    dplyr::filter(factor_covid>0)%>%
    dplyr::mutate(
      "Tipo vacante"=ifelse(cargo %in% highligted, 
                            "Relacionada con TIC",
                            "Genéricas")
    ) %>% split(.$"fases_covid") %>%
    lappl(., function(fase_df){
      
      sapply(fase_df, function(df){
        
      })
    })
    
    
    
    
    {

    } %>% bind_rows() 
    dplyr::mutate(cargo=factor(cargo, levels=cargo)) %>%
    ggplot(., aes(x=cargo, y=factor_covid, group=clasificacion ))+
    geom_col(aes(fill=clasificacion ), alpha=.3)+
    geom_text(aes(label=factor_covid), nudge_y = 2.5, size=3)+
    coord_flip()+
    labs(title="Incremento de ofertas durante pandemia",
         subtitle=sprintf("Factor de incremento durante pandemia, por cargo"),
         caption=""
    )
}
#

Vacantes_df.factor_incremento_covid.plot()











#
# modelo para recuperar sectores de las vacantes
#
Vacantes_df.cargos_a_traves_del_tiempo=function(
  Vacantes_df=db.cargar_vacantes_disponibles()
){
  
  Vacantes_df %>% 
    dplyr::transmute(
      proceso=.$"Código Proceso",
      cargo=.$"Cargo",
      fecha_registro=.$"Fecha Registro",
      num_puestos=.$"Puestos de Trabajo"
    )
}
#
Vacantes_df.cargos_a_traves_del_tiempo() %>% View()
#
Vacantes_df.cargos_a_traves_del_tiempo(
  Vacantes_df=Vacantes_df.trim_over_dispersion_on_processes()
) %>% View()


#
Cargos_a_traves_del_tiempo_df.Xfases_covid.principales_cargos=function(
  Cargos_a_traves_del_tiempo_df=Vacantes_df.cargos_a_traves_del_tiempo(),
  fases_covid= lubridate::dmy("01/01/2017", "20/03/2019","20/03/2020")
){
  
  #
  #
  Dataframe.prioritize_factor=function(
    Dataframe=Cargos_a_traves_del_tiempo_df,
    factor="cargo"
  ){
    t=table(Dataframe[[factor]])
    #
    data.frame(
      value=names(t) %>% as.character(),
      freq=t %>% as.numeric()
    ) %>% 
      dplyr::arrange(freq*(-1)) %>% 
      dplyr::mutate(
        cum_sum= cumsum(freq),
        cum_prop= round((cum_sum/sum(freq))*100, 3)
      ) 
  }
  
  
  #
  modelo_de_cargos_por_fase=
  Cargos_a_traves_del_tiempo_df %>% 
    #
    dplyr::mutate(
      fecha_registro=lubridate::dmy(.$fecha_registro)
    ) %>%
    dplyr::mutate(
      fase_covid=cut(.$fecha_registro, breaks =  c(fases_covid, "Inf"))
    ) %>%       
    # {
    #   sum(table(.$fase_covid))
    # }
    split(.$fase_covid) %>% 
    lapply(., function(data_frame_per_fase){
      
      data.frame(
        min_fecha=min(data_frame_per_fase$fecha_registro),
        max_fecha=max(data_frame_per_fase$fecha_registro),
        fase_covid=data_frame_per_fase$fase_covid[1])
      
      data_frame_per_fase %>% 
        Dataframe.prioritize_factor()
      
    }) 
  
  #
  # reportar factores de incremento con respecto al periodo de referencia:
  #
  #
  ref_model=modelo_de_cargos_por_fase$"2019-03-20" %>%
    split(.$"value") %>% lapply(., function(per_cargo){
      per_cargo$"freq"
    })
  #
  modelo_de_cargos_por_fase %>%
    lapply(., function(modelo_por_fase){
      
      modelo_por_fase %>% 
        mutate(
        pre_covid_factor=sapply( 1:nrow(modelo_por_fase), 
                                function(some_index){
                                  ifelse(modelo_por_fase$value[some_index] %in% names(ref_model),
                                     round((modelo_por_fase$freq[some_index]/ref_model[[
                                       modelo_por_fase$value[some_index]
                                     ]]),3),
                                     "undefined")
                                  })
        )
      
    })
    
}
#

#
cxcovid=Cargos_a_traves_del_tiempo_df.Xfases_covid.principales_cargos()
#cxcovid$"2019-03-20" %>% View()
cxcovid$"2019-03-20" %>% View()
cxcovid$"2020-03-20" %>% View()
#
getwd()
openxlsx::write.xlsx(cxcovid, "modelo de cargos por fase2.xlsx")

#
procesosXCargo=Vacantes_df.procesos_por_cargo()


#
#
db.read_colocados_df=function(){
  library(dplyr)
  DATA_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes colocados"
  setwd(DATA_HOME)
  list.files( pattern="Colocados" ) %>%
    lapply(., function(colocados_file){
      readxl::read_excel(colocados_file)
    }) %>% bind_rows()
}
#
df_colocados=db.read_colocados_df()
#
df_colocados  %>% View()
dim(df_colocados)


#
mean(unlist(procesosXCargo) %in% df_colocados$"Código Vacante")






table(Vacantes_df$`Tipo de Vacante`)

quantile(table(Vacantes_df$"Puestos de Trabajo"))

round(sort(prop.table(table(Vacantes_df$"Tipo de contrato")), decreasing = TRUE)*100, 3)





#
i_am_my_name=function(x){
  names(x)=x;x
}
#
#
Vacantes_df.cargos.plot=function(
  Vacantes_df=db.cargar_vacantes_disponibles()
){
  
  
  
  #
  Vacantes_df.Cargo= Vacantes_df$Cargo %>% i_am_my_name %>% unique()
  
  #
  Vacantes_df.weighted_jobs= lapply(1:nrow(Vacantes_df),  function(row){
    
    rep(Vacantes_df$Cargo[row], Vacantes_df$"Puestos de Trabajo"[row])
    
  }) %>% unlist()
  
  #
  t=table(Vacantes_df.weighted_jobs)
  
  #
  library(ggplot2)
  data.frame(
    cargo= names(t) %>% as.character(),
    num_jobs=t %>% as.numeric()
  ) %>% 
    dplyr::arrange(num_jobs*(-1)) %>% 
    
    dplyr::filter(
      1:nrow(.)<20
    ) %>%
    dplyr::arrange(num_jobs*(1)) %>%
    
    dplyr::mutate(
      cargo=factor(cargo, levels=cargo)
    ) %>%
    
    ggplot(., aes(x=cargo, y= num_jobs))+
    geom_col(alpha=0.4)+
    coord_flip()+
    
    labs(title=" Top 20 frequency rank in job label over full sample period") 
  
}
#
Vacantes_df.cargos.plot()


#
#
Vacantes_df.per_semester.plot_cargos=function(
  Vacantes_df=db.cargar_vacantes_disponibles(),
  years=2019:2020
){
  
 
 
 #
 s1=
 lapply(years %>% i_am_my_name, function(some_year){
   list(
     min_date= lubridate::dmy( sprintf("01/01/%s",some_year)),
     max_date= lubridate::dmy( sprintf("30/06/%s",some_year))
   )
 })
 #
 names(s1)=paste(names(s1), "-I", sep="")
 #
 s2=
   lapply(years %>% i_am_my_name, function(some_year){
     list(
       min_date= lubridate::dmy(sprintf("01/07/%s",some_year)),
       max_date= lubridate::dmy(sprintf("30/12/%s",some_year))
     )
   })
 #
 names(s2)=paste(names(s2), "-II", sep="")
 #
 
 S=c(s1, s2)
 
 Vacantes_df$"Fecha Publicación"=as.Date(Vacantes_df$"Fecha Publicación", format="%d/%m/%Y") 
 Vacantes_df$"Fecha Vencimiento"=as.Date(Vacantes_df$"Fecha Vencimiento", format="%d/%m/%Y") 
 
 lapply( names(S), function(some_semester_lab){
   
   #
   some_semester=S[[some_semester_lab]] 
   
   #
   df=
   Vacantes_df %>% 
     dplyr::filter(
       (Vacantes_df$"Fecha Vencimiento">=some_semester$min_date) & 
         (Vacantes_df$"Fecha Publicación"<=some_semester$max_date) &
         Vacantes_df$"Estado Actual"=="Publicada"
     ) 
   #
   df.weighted_jobs= lapply(1:nrow(df),  function(row){
     
     rep(df$Cargo[row], df$"Puestos de Trabajo"[row])
     
   }) %>% unlist()
   
   #
   t=table(df.weighted_jobs)
   
   #
   library(ggplot2)
   data.frame(
     cargo= names(t) %>% as.character(),
     num_jobs=t %>% as.numeric()
   ) %>% 
     dplyr::arrange(num_jobs*(-1)) %>% 
     
     dplyr::filter(
       cargo!="Cajero"
       #1:nrow(.)<8
     ) %>%
     dplyr::arrange(num_jobs*(1)) %>%
     
     dplyr::mutate(
       cargo=factor(cargo, levels=cargo),
       period=some_semester_lab
     )
   
 }) %>% bind_rows() %>%
   
   dplyr::arrange(num_jobs*(-1)) %>% 
   
   dplyr::filter(
     cargo!="Cajero",
     1:nrow(.)<8
   ) %>%
   
   ggplot(., aes(x=cargo, y=num_jobs, group=cargo))+
   facet_grid(~ period, scales = "free", space = "free")+
   geom_col(alpha=0.4, aes(fill=cargo), position=position_dodge() )+
   coord_flip()

}
#
Vacantes_df.per_semester.plot_cargos(
  years=2019:2021
)

#
#  3. tipo de contratacion a traves del tiempo
#
Vacantes_df.plot_puestos_de_trabajo.por_tipo_contrato=function(
  Vacantes_df=db.cargar_vacantes_disponibles()
){
  
  #
  #
  Y_M=
    data.frame(
      
      date=seq(lubridate::ymd('2017-01-01'),lubridate::ymd('2021-06-01'), by = 'day')
      
    ) %>% 
    
    dplyr::mutate(
      year=lubridate::year(date),
      month=lubridate::month(date),
      y_m=paste(year, month, sep="_")
      
    ) %>% split(.[["y_m"]]) %>% 
    
    lapply(., function(partial_df){
      
      list(
        min_date=min(partial_df$date),
        max_date=max(partial_df$date)
      )
      
      
    }) 
  #
  #head(Y_M)
  
  #
  #
  Vacantes_df$"Fecha Publicación"=as.Date(Vacantes_df$"Fecha Publicación", format="%d/%m/%Y") 
  Vacantes_df$"Fecha Vencimiento"=as.Date(Vacantes_df$"Fecha Vencimiento", format="%d/%m/%Y") 
  
  
  
  #
  #
  vp=
    lapply(Y_M, function(some_ym){
      
      Vacantes_df %>% 
        dplyr::filter(
          (some_ym$min_date<=Vacantes_df$"Fecha Publicación") & 
            (Vacantes_df$"Fecha Publicación"<=some_ym$max_date) &
            Vacantes_df$"Estado Actual"=="Publicada"
        ) %>% 
        {
          t=table(.[["Tipo de contrato"]])
          
          data.frame(
            jobs=t %>% as.numeric(),
            jobs_type=names(t) %>% as.character()
          )
        }
    }) %>% bind_rows()
  
  
  #
  #
  library(ggplot2)
  cbind(
  vp,
  data.frame(
    min_date=sapply(Y_M, function(x){as.character(x$min_date)}),
    row.names = NULL
  )) %>% View()
    
    mutate(
      min_date=as.Date( min_date)
    ) %>%
    
    dplyr::arrange(min_date) %>%
    
    mutate(
      y_m=factor(y_m, levels=y_m)
    ) %>%
    
    
    
    
  
}
#
Vacantes_df.plot_puestos_de_trabajo()






#
# modelo para recuperar sectores de las vacantes
#
Vacantes_df.procesos_por_cargo=function(
  Vacantes_df=db.cargar_vacantes_disponibles()
){
  
  Vacantes_df %>% 
    dplyr::transmute(
      proceso=.$"Código Proceso",
      cargo=.$"Cargo",
      fecha_registro=.$"Fecha Registro"
    ) %>%
    split(.$cargo) %>%
    lapply(., function(cargo_df){
      cargo_df$proceso
    }) 
  
}
#
Vacantes_df.procesos_por_cargo() %>% View()

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
library(dplyr)
db.raw_data_from_vacantes=function(sample_file="info vacantes.csv"){
  setwd(DATA_VACANTES_HOME)
  an=read.csv(sample_file, encoding="UTF-8") 
  names(an)=c("n", "vacante")
  an %>% dplyr::select(vacante) %>% mutate(z_vacante=basic_standardization(vacante))
}
#
db.raw_data_from_vacantes() %>% View()
#
#
Vacantes_df.vacantes_model=function(
  Vacantes_df=db.raw_data_from_vacantes(),
  feature_markers=c("codigo_de_vacante"="[[0-9][-]]{5,10}",
                    "salario"=":[^:]*smmlv",
                    "tipo de contrato"="smmlv(.*)distribucion"
                    )
){
  #
    an=Vacantes_df %>%
      mutate(vacante=)
    lapply(names(feature_markers), function(some_marker_name){
      an<<-an %>% mutate(new_var=stringr::str_extract(an$z_vacante, feature_markers[[some_marker_name]] ))
      names(an)[length(names(an))]<<-some_marker_name
    })
    an
}
#
Vacantes_df.vacantes_model()
#
Vacantes_df.filter_per_parameters=function(
  Vacantes_df=Vacantes_df.vacantes_model()
){
  Vacantes_df %>% 
    #
    # maybe some label propagation?
    #
    dplyr::filter( grepl("bogota",.$z_vacante) ) %>%
    dplyr::filter( (!grepl("experiencia",.$z_vacante))|(grepl("sin experiencia", .$z_vacante)) )
    
}
#
nrow(Vacantes_df.filter_per_parameters())/nrow(Vacantes_df.vacantes_model())


#
Dataframe.prioritize_factor=function(
  Dataframe,
  factor
){
  t=table(Dataframe[[factor]])
  #
  data.frame(
    value=names(t) %>% as.character(),
    freq=t %>% as.numeric()
  ) %>% 
    dplyr::arrange(freq*(-1)) %>% 
    dplyr::mutate(
      cum_sum= cumsum(freq),
      cum_prop= round((cum_sum/sum(freq))*100, 3)
    ) 
}
#
Vacantes_df.plot_wages_offered=function(
  Vacantes_df=Vacantes_df.filter_per_parameters()
){
  library(ggplot2)
  Vacantes_df %>% 
    mutate(salario=stringr::str_replace(salario, ":", replacement="")) %>%
    Dataframe.prioritize_factor("salario") %>%
    mutate(expectativa_salarial=factor(.$"value", levels=.$"value"),
           dummy_=1) %>%
    ggplot(., aes(x=expectativa_salarial, y=freq, group= dummy_))+
    geom_col(fill=rgb(0,0,0,.3))+ 
    geom_text(aes(label=freq))+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
    geom_line(aes(y=cum_prop*(1)*(max(freq)/100)), lty="longdash")+
    geom_label(aes(y=cum_prop*(1)*(max(freq)/100),label=cum_prop), check_overlap = TRUE)+
    labs(title="salarios ofrecidos a la poblacion SISPE/Uniminuto",
         subtitle="pareto de los ofrecimientos salariales",
         caption=sprintf("sobre un total de %s vacantes", nrow(Vacantes_df))
         )+
    xlab("salario ofrecido")
    
}
Vacantes_df.plot_wages_offered(
  Vacantes_df.filter_per_parameters()
)
#
Vacantes_df.plot_wages_offered(
  Vacantes_df.vacantes_model()
)
#
Vacantes_df.plot_wages_offered()
#
nrow(Vacantes_df.filter_per_parameters())/nrow(Vacantes_df.vacantes_model())


dplyr::sample_n(Vacantes_df.filter_per_parameters(), 30) %>%
  openxlsx::write.xlsx("tarea de etiquetar la jornada laboral.xlsx")



  
