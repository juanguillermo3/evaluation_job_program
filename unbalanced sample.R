#
# routes
# --------
#

rm(list=ls())
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

#
# app modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

setwd(CODE_HOME)
source("persistence utils.R", encoding="UTF-8")

#
# confg
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("constants.R")

#
# unbalanced sample
# --------
#

#
# modelo demografico
# --------

muestra_cmd.modelo_demografico() %>%  View()

#
# intermediacion laboral
# --------

db.experimental_cases() %>% View()

#
# perfiles laborales
# --------

db.perfiles_laborales() %>% View()


#
# unbalanced sample (demographics + treatment status)
# --------

#
experimental_cases.unbalanced_sample=function(
  experimental_cases=db.experimental_cases(),
  modelo_demografico=muestra_cmd.modelo_demografico(),
  modelo_hoja_de_vida=db.perfiles_laborales(),
  modelo_fechas_de_registro=
    db.informacion_de_control() %>%
    dplyr::mutate(
      mes_ano_registro=
        fecha_actualizacion2 %>% stringr::str_extract("[^ ]+") %>% as.Date(format="%Y-%m-%d")) %>%
    dplyr::mutate(ano_registro=lubridate::year(mes_ano_registro)) %>%
    dplyr::mutate(
      registro_post_covid= ifelse(mes_ano_registro>FECHA_INICIO_COVID, "post-covid", "pre-covid")
    )
){
  dplyr::inner_join(
    experimental_cases,
    modelo_demografico) %>% 
    dplyr::inner_join(
      .,
      modelo_hoja_de_vida) %>% 
    dplyr::inner_join(
      .,
      modelo_fechas_de_registro)
}
#
experimental_cases.unbalanced_sample.cache=
  experimental_cases.unbalanced_sample()
#
experimental_cases.unbalanced_sample.cache %>% 
  View()
 
#
experimental_cases.unbalanced_sample.cache %>%
  Dataframe.share_with_app(label="unbalanced sample",
                           app_route=UNBALANCED_SAMPLE_HOME)

#
# analytics
# --------
#

#
list.keep_dataframes=function(
  list
){
  list[sapply(list, function(l_){"data.frame" %in% class(l_)})]
}
#
Dataframe.round_numbers=
  function(Dataframe, digits=4){
  
    Dataframe %>%
    lapply(function(data_col){
      tryCatch({
      round(data_col, digits)
      }, error=function(e){return(data_col)})
    })  %>%
      as.data.frame()
}
#
unbalanced_sample.covariates_table_comparison=function(
  
  unbalanced_sample= 
    experimental_cases.unbalanced_sample.cache %>%
    dplyr::filter(laboral_states=="desempleado"),
  
  logro_bachillerato=c("secundaria completa","univ o posterior"),
  logro_universitario=c("univ o posterior")
){
  
  #
  options(scipen=999)
  test_df=
  unbalanced_sample %>%
    dplyr::transmute(
      
      participante=activ_cualquiera>0,
      
      es_mujer=(sexo=="mujer"),
      tiene_bachillerato_completo=(estudios %in% logro_bachillerato),
      tasa_universitaria_o_superior=(estudios %in% logro_universitario),
      edad=edad,
      edad_menor_20=edad <20,
      edad_20_a_30=(edad <=20 & edad <30),
      edad_mas_de_30=edad>=30,
      es_migrante=es_migrante,
      
      tiene_experiencia=experiencia>0,
      prom_experiencia=experiencia,
      prom_certificacion=certificacion,
      idioma_extranjero=(idioma_extranjero==1),
      competencias_digitales=(competencias_digitales==1)) 
    
    names(test_df) %>%
    lapply(function(data_col){
      try({
        
      S=split( test_df[[data_col]] , test_df$participante)
      t=t.test(S[[1]],S[[2]])
      
      t$estimate %>% 
        t() %>% 
        as.data.frame() %>%
        Dataframe.new_names(c("no_participantes","participantes")) %>%
        dplyr::mutate(
          diff=participantes-no_participantes,
          p_val=t$p.value
        ) %>%
        dplyr::mutate(Variable=data_col,)
    })
    }) %>%
    
    list.keep_dataframes() %>%
    bind_rows() %>%
      Dataframe.round_numbers() %>%
      Dataframe.order("Variable") %>%
      dplyr::mutate( 
        diff=
        dplyr::case_when(
          p_val<.01~sprintf("%s***", diff),
          p_val<.05~sprintf("%s**", diff),
          p_val<.1~sprintf("%s*", diff),
          TRUE~sprintf("%s", diff)
        )
        ) %>%
      Dataframe.insert(c("Subsample_size",
                         table(test_df$participante)[1],
                         table(test_df$participante)[2],
                         NaN,
                         NaN))
    
}
unbalanced_sample.covariates_table_comparison() %>% View()

unbalanced_sample.covariates_table_comparison() %>%
  Dataframe.export_output(label="prueba de diferenciad de medias",
                          output_home=UNBALANCED_SAMPLE_HOME)

#
unbalanced_sample.demographic_gradient=function(
  unbalanced_sample= experimental_cases.unbalanced_sample.cache,
  logro_bachillerato=c("secundaria completa","univ o posterior"),
  logro_universitario=c("univ o posterior")
){
  unbalanced_sample %>% 
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
        prom_edad=round(mean(case_df$edad ),3),
        edad_menor_20=round(mean(case_df$edad <20  )*100,3),
        edad_20_a_30=round(mean(case_df$edad <=20 & case_df$edad <30  )*100,3),
        edad_mas_de_30=round(mean(case_df$edad>=30  )*100,3),
        #
        prop_migrante=round(mean(case_df$es_migrante)*100,3)
      )
    }) %>%
    bind_rows() 
}
#
#
unbalanced_sample.demographic_gradient()  %>%
unbalanced_sample.demographic_gradient(
  unbalanced_sample = experimental_cases.unbalanced_sample.cache %>% 
    dplyr::filter(laboral_states=="desempleado")) %>% Vi
#
Dataframe.export_output(label="sample balance dem",
                        output_home=UNBALANCED_SAMPLE_HOME,
                        new_output_array = TRUE)
#
#
unbalanced_sample.laboral_gradient=function(
  unbalanced_sample =experimental_cases.unbalanced_sample.cache
){
  unbalanced_sample %>% 
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
unbalanced_sample.laboral_gradient(
  unbalanced_sample =
    experimental_cases.unbalanced_sample.cache %>% 
      dplyr::filter(laboral_states=="desempleado")
) %>%
  Dataframe.export_output(label="sample balance lab.xlsx",
                          output_home=UNBALANCED_SAMPLE_HOME)
df=
list(
unbalanced_sample.demographic_gradient() %>% t() %>% as.data.frame(),
unbalanced_sample.laboral_gradient() %>% t()%>% as.data.frame()) %>%
  bind_rows()
df$feature=row.names(df)
df %>%
  Dataframe.export_output(label="total sample balance lab.xlsx",
                          output_home=UNBALANCED_SAMPLE_HOME)
#
Dataframe.fields_layout=function(
  Dataframe,
  header_index=1
){
  Dataframe %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(field=row.names(.)) %>%
    dplyr::select( c("field",setdiff(names(.),"field" )) ) %>%
    {
      names(.)=.[ header_index,];
      .
    } %>%
    dplyr::filter(
      1:nrow(.)!=  header_index
    )%>%
    { 
      row.names(.)=NULL;
      .
    }%>%
    return()
}
#
list(
  unbalanced_sample.demographic_gradient(
    unbalanced_sample = experimental_cases.unbalanced_sample.cache %>% 
      dplyr::filter(laboral_states=="desempleado")) %>%
    Dataframe.fields_layout(),
  unbalanced_sample.laboral_gradient(
    unbalanced_sample = experimental_cases.unbalanced_sample.cache %>% 
      dplyr::filter(laboral_states=="desempleado"))%>%
    Dataframe.fields_layout()
  ) %>%
  bind_rows() %>% 
  dplyr::mutate(
    diferencia=round((as.numeric(.[["participante"]])-as.numeric(.[["no participante"]])), 3)
  )  %>%
  Dataframe.export_output(label="diferencia de medias en covarables",
                          output_home=UNBALANCED_SAMPLE_HOME)
 