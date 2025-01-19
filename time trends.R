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
source("data candidates.R")

setwd(CODE_HOME)
source("data covid sample.R")

setwd(CODE_HOME)
source("genderdize records.R")

setwd(CODE_HOME)
source("data last participation.R" , encoding="UTF-8")
db.last_participation() %>% View()

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
# tendencias en resultados
# --------


#
db.last_participation()
  
#
ultimas_participaciones.muestra_porsubperiodo.plot=function(
  
  ultimas_participaciones=db.last_participation(),
  
  measurement_times=seq(as.Date("2017/1/1"), 
                        as.Date("2022/1/1"),
                        by = "2 weeks"),
  
  measurement_delta=30*6
  
){
  library(ggthemes)
  library(ggplot2)
  
  ultimas_participaciones$fecha_ultima_part=as.Date(ultimas_participaciones$fecha)
  
  measurement_times %>%
    lapply(function( measurement_time){
      data.frame(
        time=measurement_time,
        time_limit=measurement_time-measurement_delta,
        sample_size={
          ultimas_participaciones %>%
            dplyr::filter(
              measurement_time-fecha_ultima_part>0 &
                measurement_time-fecha_ultima_part< measurement_delta
            ) %>%
            nrow()
        })
    }) %>%
    bind_rows() %>%
    
    ggplot(aes(x=time, y=sample_size))+
    geom_col(fill=rgb(0,0,0,.3))
}
ultimas_participaciones.muestra_porsubperiodo.plot() 


#
gendered_df.count_gender=function(
  gendered_df,
  counted_gender
){
  gendered_df %>%
    dplyr::filter(gender==  counted_gender) %>%
    nrow()
}
#
ultimas_participaciones.genderdize.muestra_porsubperiodo.plot=function(
  
  ultimas_participaciones= db.last_participation(), 

  measurement_times=seq(as.Date("2017/1/1"), 
                        as.Date("2022/1/1"),
                        by = "2 weeks"),
  
  measurement_delta=30*6
  
){
  
  library(ggthemes)
  library(ggplot2)
  
  ultimas_participaciones$fecha_ultima_part=as.Date(ultimas_participaciones$fecha)
  ultimas_participaciones=people_with_ids_df.genderdize( ultimas_participaciones)
  
  measurement_times %>%
    lapply(function( measurement_time){
      
      list(
      data.frame(
        time=measurement_time,
        time_limit=measurement_time-measurement_delta,
        sex="hombre"),
      #
      data.frame(
        time=measurement_time,
        time_limit=measurement_time-measurement_delta,
        sex="mujer")
      
      ) %>% bind_rows()
        
    }) %>%
    bind_rows() %>% 
    
    split(1:nrow(.)) %>%
    
    lapply(function(sub_df){
      
      sub_df2=
      ultimas_participaciones %>%
        dplyr::filter(gender==sub_df$sex) %>%
        dplyr::filter(
          sub_df$time_limit < fecha_ultima_part &
            fecha_ultima_part < sub_df$time
        )
      
      sub_df %>%
        dplyr::mutate(
          size=nrow(.)
        ) %>%
        dplyr::mutate(
          sample_size=  sub_df2 %>% nrow()
        ) 
      
    }) %>%
    bind_rows() %>%
    
    ggplot(aes(x=time, y=sample_size, group=sex))+
    geom_col(aes(fill=sex), position=position_dodge())
}
ultimas_participaciones.genderdize.muestra_porsubperiodo.plot()  



#
list.keep_dataframes=function(
  list
){
  list[sapply(list, function(l_){"data.frame" %in% class(l_)})]
}
#
ultimas_participaciones.genderdize_outcome_over_time.plot=function(
  
  ultimas_participaciones= db.last_participation(),  
  
  postulaciones=db.candidates(),
  
  measurement_times=seq(as.Date("2019/1/1"), 
                        as.Date("2021/1/1"),
                        by = "2 weeks"),
  
  measurement_delta=30*6
  
){
  
  library(ggthemes)
  library(ggplot2)
  
  ultimas_participaciones$fecha_ultima_part=as.Date(ultimas_participaciones$fecha)
  ultimas_participaciones=people_with_ids_df.genderdize( ultimas_participaciones)
  
  measurement_times %>%
    lapply(function( measurement_time){
      
      list(
        data.frame(
          upper_date=measurement_time,
          lower_date=measurement_time-measurement_delta,
          sex="hombre"),
        #
        data.frame(
          upper_date=measurement_time,
          lower_date=measurement_time-measurement_delta,
          sex="mujer")
        
      ) %>% bind_rows()
      
    }) %>%
    bind_rows() %>% 
    
    split(1:nrow(.)) %>%
    
    lapply(function(sub_df){
      
      try({
      
      #print(sub_df)
      gente_activa_en_periodo=
        ultimas_participaciones %>%
        dplyr::filter(gender==sub_df$sex %>% head(1)) %>%
        dplyr::filter(
          sub_df$lower_date %>% head(1) < fecha_ultima_part &
            fecha_ultima_part < sub_df$upper_date %>% head(1)
        ) %>%
        dplyr::select(doc_num, fecha_ultima_part)
      
      postulaciones_en_periodo=
      postulaciones %>% 
        Dataframe.delimite_dates(
          lower_date=sub_df$lower_date %>% tail(1),
          upper_date=sub_df$upper_date %>% tail(1)
        ) %>% 
        
        dplyr::filter(
          doc_num %in%  gente_activa_en_periodo$doc_num
        ) %>%
        
        split(.$doc_num) %>%
        lapply(function(sub_df2){
          data.frame(
            doc_num=dplyr::first(sub_df2$doc_num),
            num_postulaciones=sub_df2 %>% nrow()
            )
        }) %>%
        bind_rows()
        
      postulaciones_de_gente_activa=
      gente_activa_en_periodo %>%
      dplyr::left_join(postulaciones_en_periodo) %>%
        Dataframe.map_NAS() 
      
      sub_df %>%
        dplyr::mutate(
          size=nrow(postulaciones_de_gente_activa)
        ) %>%
        dplyr::mutate(
          mean_postulations= mean(postulaciones_de_gente_activa$num_postulaciones)
        ) 
      })
      
    }) %>%
    list.keep_dataframes() %>%
    bind_rows() %>% 
    ggplot(aes(x=upper_date, y=mean_postulations, group=sex))+
    geom_line(aes(color=sex), position=position_dodge())+
    geom_vline(xintercept=FECHA_INICIO_COVID, linetype="dashed", color = "black")
}
#

#
colocaciones_plot=
ultimas_participaciones.genderdize_outcome_over_time.plot(
  postulaciones=
    db.candidates() %>%
    dplyr::filter(Fue_colocado)
)+
  theme
#
autopostulaciones_plot=
ultimas_participaciones.genderdize_outcome_over_time.plot(
  postulaciones=
    db.candidates() %>%
    dplyr::filter(tipo_postulacion %in% "Autopostulado" )
)
#
remisiones_plot=
ultimas_participaciones.genderdize_outcome_over_time.plot(
  postulaciones=
    db.candidates() %>%
    dplyr::filter(tipo_postulacion %in% "Remitido por prestador" )
) 


#
ultimas_participaciones.genderdize_outcome_over_time.plot(
  postulaciones=
    db.candidates() %>%
    dplyr::filter(tipo_postulacion %in% "Autopostulado" )
) 

