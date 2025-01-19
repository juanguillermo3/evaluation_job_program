
#
# 0.1 enrutamiento de este servicio hacia la informacion de las colocaciones
#
#rm(list=ls())
#APP_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/"
COLOCACIONES_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes colocados"


#
# 1.0 lector de datos para colocaciones
#
db.colocaciones_df=function(
  COLOCACIONES_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes colocados"
){
  setwd(COLOCACIONES_HOME)
  #
  library(dplyr)
  list.files(pattern="Detalle_Total_Colocados") %>%
  lapply(.,function(some_file){
    readxl::read_excel(some_file)
  }) %>% bind_rows() %>% 
    transmute(
      tipo_documento=.[["Tipo Documento"]],
      num_documento=.[["Número Documento" ]],
      fecha_colocacion=.[["Fecha Colocación" ]],
      sector_colocacion=.[["Sector"]],
      codigo_vacante=.[["Código Vacante"]],
      tipo_contrato=.[["Tipo Contrato"]],
    ) %>% 
    
    mutate(llave_colocacion= paste(num_documento,codigo_vacante, sep="+")) %>%
    dplyr::filter( !duplicated(llave_colocacion) ) 
  
}
#
db.colocaciones_df() %>% View()


#
# 1.1 pintar numero de colocaciones a través del tiempo
#
Colocaciones_df.plot_colocaciones_en_el_tiempo=function(
  colocaciones_df=db.colocaciones_df()
){
  
  
  library(ggplot2)
  
  #
  # (1) trabajo preliminar sobre variables de fechas
  #
  colocaciones_df %>%
    mutate(
      fecha_colocacion= as.POSIXct(colocaciones_df$fecha_colocacion,
                                   format="%d/%m/%Y %H:%M:%OS"),
      month=lubridate::month(fecha_colocacion),
      year=lubridate::year(fecha_colocacion),
      month_year=paste(month, year, sep="-")
      
    ) %>%  
    
    #
    # (2) resumir dataframes parciales por mes año
    #
    
    split(.[["month_year"]]) %>% 
    
    lapply(., function(month_year_df){
      
      data.frame(
        year=month_year_df$year[1],
        month=month_year_df$month[1],
        year_month=month_year_df$month_year[1],
        num_colocaciones=nrow(month_year_df))
    
    }) %>% bind_rows() %>%
    
    #
    # (3) rordenar dataframe resultante por mes año,
    #     y mes año como factor ordenado
    #
    
    dplyr::arrange(
      year,
      month
      
    ) %>%
    mutate(
      year_month=factor(year_month, levels=year_month)
    ) %>%
    
    #
    # (4) lógica del gráfio
    #
    
    ggplot(., aes(x=year_month, y=num_colocaciones))+
    
    geom_col(
      fill=rgb(0,0,0,.3)
    )+
    theme(axis.text.x = element_text(angle = 90))
}
#
Colocaciones_df.plot_colocaciones_en_el_tiempo()


#
Colocaciones_df.tipo_de_contratp=function(
  colocaciones_df=db.colocaciones_df()
){
  
  table(colocaciones_df$)
}

#
# 1.2 
#



# trabajo: cambiar la lógica de resumen del dataframe por mes año, 
# para que en lugar de contar las colocaciones totales. cuente las
# colocaciones  por cada sector!. es decir, se debe producir
# un dataframe tal que exista una fila por cada secor en el paso (2).


# (2) cambiar la lógica del gráfico, para que las series de tiempo
# estén agrupádas por sector (usar el parámetro group de ggplot)


# (3) agregar un parámetro para que el gráfico permita filtrar por año?



#
#
Colocaciones_df.plot_colocaciones_por_sector=function(
  colocaciones_df=db.colocaciones_df(),
  excluded_sectors=c("")
){

  
  library(ggplot2)
  
  #
  # (1) trabajo preliminar sobre variables de fechas
  #
  colocaciones_df %>%
    mutate(
      fecha_colocacion= as.POSIXct(colocaciones_df$fecha_colocacion,
                                   format="%d/%m/%Y %H:%M:%OS"),
      
      fase_covid= ifelse(
        fecha_colocacion> lubridate::dmy("20/03/2020"),
        "post-covid",
        "pre-covid"),
      
      fase_covid=factor(
        fase_covid,
        levels=c("pre-covid", "post-covid")
      )
      
      ) %>%
    #
    # (2) resumir dataframes parciales por mes año
    #
    
    split(.[["fase_covid"]]) %>% 
    
    lapply(., function(partial_df){
      
    
      data.frame(
        partial_df$fase_covid[1],
        num_colocaciones=nrow(partial_df))
      #
      colocaciones_por_sector=table(partial_df$sector_colocacion)
      #
      names(colocaciones_por_sector) %>%
        lapply(., function(some_sector){
          data.frame(
            fase_covid=partial_df$fase_covid[1],
            sector=some_sector,
            num_colocaciones=sum(partial_df$sector_colocacion==some_sector, na.rm=TRUE)
          )
        }) %>% 
        
        bind_rows() %>%
        
        dplyr::arrange(num_colocaciones*(-1)) %>%
        dplyr::mutate( 
          acummulated=cumsum(num_colocaciones)/sum(num_colocaciones)*100
          ) %>% 
        dplyr::filter(
          !(acummulated>95)
        )
      
    }) %>% bind_rows() %>% 
    #
    # (3) rordenar dataframe resultante por mes año,
    #     y mes año como factor ordenado
    #
    
    dplyr::arrange(
      num_colocaciones*(1)
      
    ) %>% 
    mutate(
      sector=factor(sector,
                    levels=unique(sector),
                    labels=substr(unique(sector), 1, 45)
                    ),
      
    ) %>% 
    
    dplyr::filter(
      !(sector %in% excluded_sectors)
    ) %>% View()
    
    #
    # (4) lógica del gráfio
    #
    
    ggplot(., aes(y=sector, x=num_colocaciones))+
    
    geom_col(
      fill=rgb(0,0,0,.3)
      
    )+ 
    
    facet_grid(cols = vars(fase_covid))+
    
    theme(axis.text.x = element_text(angle = 90))

}
#
Colocaciones_df.plot_colocaciones_por_sector()
#
Colocaciones_df.plot_colocaciones_por_sector(
  excluded_sectors = "Actividades de empleo"
)