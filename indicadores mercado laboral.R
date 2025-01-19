#
#
# dimensiones de participacion laboral a tavés de la pandemia
# ---------


#
MARCO_MUESTRAL_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/marco muestral"
#
library(dplyr)
db.read_marco_muestral=function(){
  setwd(MARCO_MUESTRAL_HOME)
  list.files()
  read.csv("marco muestral.csv", encoding="UTF-8", sep=";")
}
#
db.read_marco_muestral() %>% View()

#
#
ENTRADAS_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
#
Dataframe.vars_as_character=function(
  Dataframe=db.read_marco_muestral()
){
  Dataframe %>% 
    lapply(function(data_col){
      as.character(data_col)
    }) %>%
    as.data.frame()
}
#
Dataframe.vars_as_character() %>% View()
#
db.read_entradas=function(){
  #
  setwd(ENTRADAS_HOME)
  list.files(pattern="Detalle_") %>%
    lapply(function(data_file){
      openxlsx::read.xlsx(data_file) %>%
        Dataframe.vars_as_character()
    }) %>%
    bind_rows()
}
#
db.read_entradas() %>% View()

#
entradas_df.normalize=function(
  entradas_df=db.read_entradas()
){
  entradas_df %>%
    dplyr::arrange(.$"Número.Documento")
}
#
entradas_df.normalize() %>% View()

#
#
Dataframe.mandatory_model=function(
  df,
  mandatory_model
){
  df %>% 
    dplyr::filter(
      df %>%
        dplyr::select( mandatory_model) %>%
        apply( MARGIN=1, 
               function(row_data){
                 all(!is.na(row_data))
               })
    )
}
#
entradas_normalizadas_df.model_entradas=function(
  entradas_normalizadas_df=entradas_df.normalize(),
  Mandatory_model=c("doc_num", 
                    "fecha_registro",
                    "fecha_actualizacion",
                    "fecha_cambio_prestador",
                    "estado_laboral")
){
  #
  # (1) select
  #
  entradas_normalizadas_df %>% 
    dplyr::transmute(
      doc_num=.$"Número.Documento",
      fecha_registro=.$"Fecha.Registro",
      fecha_actualizacion=.$"Fecha.Actualización",
      fecha_cambio_prestador=.$"Fecha.Cambio.Prestador",
      estado_laboral=.$"Situación.Laboral",
      gender=ifelse(.$"Género"=="M", "hombre", "mujer"),
      completeness=.$"X..Hoja.Vida"
      ) %>%
    #
    # (2) emsemble
    #
    dplyr::mutate(
      registro_es_ultima_actualizacion=(fecha_registro==fecha_actualizacion)
    ) %>%
    dplyr::arrange(registro_es_ultima_actualizacion) %>%
    dplyr::mutate(
      es_desempleado=ifelse(estado_laboral=="Desempleado", TRUE, FALSE)
    ) %>%
    dplyr::mutate(
      tiene_hoja_completa=ifelse(completeness %in% 
                                   c("COMPLETA", "95", "90"), TRUE, FALSE)
    ) %>%
    #
    # (3) mandate
    #
    Dataframe.mandatory_model(mandatory_model = Mandatory_model)
}
#
entradas_normalizadas_df.model_entradas() %>% View()


#
months=list(
  "01"="enero",
  "02"="febrero",
  "03"="marzo",
  "04"="abril",
  "05"="mayo",
  "06"="junio",
  "07"="julio",
  "08"="agosto",
  "09"="septiembre",
  "10"="octubre",
  "11"="noviembre",
  "12"="diciembre"
)
#
actualizaciones_df.model_for_laboral_indicator=function(
  actualizaciones_df=entradas_normalizadas_df.model_entradas(),
  fields=c("doc_num",
           "edad", 
           "nivel_educativo", 
           "gender",  
           "fecha_actualizacion",
           "registro_es_ultima_actualizacion",
           "es_desempleado",
           "tiene_hoja_completa")
){
  #
  #
  actualizaciones_df %>%
    
    dplyr::select(
      any_of(fields)
      ) %>%
    
    mutate(
      day=stringr::str_extract(fecha_actualizacion, "^[0-9]{2}"),
      month=stringr::str_extract(fecha_actualizacion,"(?<=[/])[0-9]{2}"),
      year=stringr::str_extract(fecha_actualizacion, "[0-9]{4}")
    ) %>% 
    mutate(
      month= plyr::mapvalues(month, months, names(months))
    ) %>%
    mutate(
      fecha_actualizacion2= as.POSIXct(lubridate::dmy(paste(day, month, year, sep="-")))
    ) 
    
}
#
actualizaciones_df.model_for_laboral_indicator() %>% View()

#
VALIDACION_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes validacion"
#
db.actualizar_validaciones=function(
  validacion_de_registros=actualizaciones_df.model_for_laboral_indicator()
){
  #
  setwd(VALIDACION_HOME)
  #
  openxlsx::write.xlsx(validacion de registros, "validacion_de_registros.xlsx")
}
#
db.actualizar_validaciones()


#
model_for_laboral_indicator.plot_time_series=function(
  model_for_laboral_indicator=actualizaciones_df.model_for_laboral_indicator(),
  dates=seq.Date(lubridate::dmy("01-01-2020"),lubridate::dmy("01-01-2022"), by="day")
){
  
  modelxgender=
    
    model_for_laboral_indicator %>%
    
    dplyr::filter( .$"es_desempleado" & .$"tiene_hoja_completa"  ) 
    
    split(.$gender)
  
  library(ggplot2)
  data.frame(date=dates) %>%
    mutate(
      male_rate=sapply(date, function(d){
        sum(d>modelxgender$hombre$fecha_actualizacion2, na.rm=TRUE)
        #/(nrow(modelxgender$hombre))*100
      })) %>%
    mutate(
      female_rate=sapply(date, function(d){
        sum(d>modelxgender$mujer$fecha_actualizacion2, na.rm=TRUE)
        #/(nrow(modelxgender$mujer))*100
      })) %>%  
    ggplot(aes(x=date))+
    geom_line(aes(y=male_rate), col=rgb(1,0,0))+
      geom_line(aes(y=female_rate), col=rgb(0,0,1))+
    
    labs(
      title="registro en la plataforma sispe/minuto de dios",
      subtitle="usuarios que se registraron como desempleados, por sexo"
    )
}
#
model_for_laboral_indicator.plot_time_series(
  dates=seq.Date(lubridate::dmy("01-01-2018"),lubridate::dmy("01-01-2022"), by="day")
)


#
#
ggplot_time_serie.label_covid=function(
  
  p=model_for_laboral_indicator.plot_time_series(
    dates=seq.Date(lubridate::dmy("01-01-2017"),lubridate::dmy("01-01-2022"), by="day")
  ),
  covid_date=lubridate::dmy("20-03-2020")
  
    ){
  
  p+geom_vline(xintercept=covid_date, linetype="dashed", color=rgb(0,0,0))
  
}
#
ggplot_time_serie.label_covid()