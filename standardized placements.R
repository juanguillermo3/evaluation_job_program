#
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
setwd(CODE_HOME)
source("genderdize records.R")
#
setwd(CODE_HOME)
source("quality control.R")
#
modelo_de_control=informacion_de_control.modelo_de_control() 
modelo_de_control %>% View()
#
setwd(CODE_HOME)
source("job vacancy parameters.R")


#
# custom modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

#
# colocaciones
# --------
#

#
#
db.registros_colocaciones=function(
){
  setwd(CONTRATACIONES2_HOME)
  #
  library(dplyr)
  list.files(pattern="Detalle_Total_Colocados") %>%
    lapply(.,function(some_file){
      readxl::read_excel(some_file)
    }) %>% bind_rows() 
  
}
#
db.registros_colocaciones() %>% View()


#
#
colocaciones_df.modelo_colocaciones_normalizado=function(
  colocaciones_df=db.registros_colocaciones()
){
  #
  # subset a las contrataciones: solo contrataciones con control de calidad
  # join con parámetros de la contratación
  
  colocaciones_df %>% 
  
    #
    # (0) select
    #
    transmute(
      doc_num=.[["Número Documento"]],
      proceso=.[["Código Vacante"]],
      fecha_colocacion=.[["Fecha Colocación" ]],
      sector=.[["Sector"]],
      tipo_contrato=.[["Tipo Contrato"]]
    ) %>% 
    
    #Tabla_con_cedulas.aplicar_modelo_de_control() %>%
    
    Tabla_de_vacantes.incorporar_parametros(params="salario") %>%
    #
    # (2) transform
    #
    mutate(llave_colocacion= paste(doc_num,proceso, sep="+")) %>%
    #
    # (3) normalize
    #
    dplyr::filter( !duplicated(llave_colocacion) ) 
}
#
colocaciones_df.modelo_colocaciones_normalizado() %>% View()

#
#
modelo_colocaciones.colocaciones_el_tiempo=function(
  modelo_colocaciones=
    colocaciones_df.modelo_colocaciones_normalizado(),
  t_range=
    c(
      #paste(1:12, 2017, sep="-"),
      #paste(1:12, 2018, sep="-"),
      paste(1:12, 2019, sep="-"),
      paste(1:12, 2020, sep="-"),
      paste(1:12, 2021, sep="-"),
      paste(1:12, 2022, sep="-")
    )
){
  
  #
  library(ggplot2)
  #
  modelo_colocaciones %>%
    
    people_with_ids_df.genderdize() %>% 
    
    #Tabla_con_cedulas.aplicar_modelo_de_control() %>%
    
    mutate(
      fecha_colocacion= as.POSIXct(fecha_colocacion,
                                   format="%d/%m/%Y %H:%M:%OS"),
      month=lubridate::month(fecha_colocacion),
      year=lubridate::year(fecha_colocacion),
      month_year=paste(month, year, sep="-")
      
    ) %>%  
    #
    #
    
    split(.[["gender"]]) %>%   
      
      lapply(function(gender_df){
        
        gender_df %>%
        split(.[["month_year"]]) %>%
        lapply(., function(month_year_df){
          
          data.frame(
            year=month_year_df$year[1],
            month=month_year_df$month[1],
            year_month=month_year_df$month_year[1],
            num_colocaciones=nrow(month_year_df),
            gender=month_year_df$gender[1])
          
        }) %>% bind_rows()
        
      }) %>% bind_rows() %>%
    
    #
    #
    dplyr::arrange(
      year,
      month
      
    ) %>%
    dplyr::filter( year_month %in% t_range) %>%
    mutate(
      year_month=factor(year_month, levels=t_range)
    ) %>%

    {
      
    ggplot(., aes(x=year_month, 
                  y=num_colocaciones,
                  group=gender
                  ))+
    
    geom_point(
      aes(color=gender),
      size=3
    )+
    geom_line(
      aes(color=gender)
    )+
    theme(axis.text.x = element_text(angle = 90))+
    
    labs(
      title="Colocaciones a través del tiempo",
      subtitle="Colocaciones por mes año muestra CMD dentro del SISPE",
      caption=sprintf("Para %s colocaciones sobre %s personas",
                      sum(.$num_colocaciones),
                      nrow(modelo_de_control)
                      ))+
    xlab("mes año")+
    ylab("número de colocaciones")+
    geom_vline(xintercept="3-2020",
               linetype="dashed", 
               color=rgb(0,0,0))
    }
}
#
modelo_colocaciones.colocaciones_el_tiempo()

#
#
modelo_colocaciones.persistir_colocaciones=function(
  modelo_colocaciones=colocaciones_df.modelo_colocaciones_normalizado()
){
  setwd(CONTRATACIONES_ESTANDARIZADAS_HOME)
  #
  openxlsx::write.xlsx(modelo_colocaciones, "colocaciones estandarizadas.xlsx")
}
#
modelo_colocaciones.persistir_colocaciones()

#
#
modelo_colocaciones.colocaciones_por_tipo_contrato=function(
  modelo_colocaciones=colocaciones_df.modelo_colocaciones_normalizado(),
  normalize=TRUE
){
  
  #
  library(ggplot2)
  #
  modelo_colocaciones %>%
    
    people_with_ids_df.genderdize() %>% 
    
    #Tabla_con_cedulas.aplicar_modelo_de_control() %>%  
    #
    #
    split(.[["gender"]]) %>%   
    
    lapply(function(gender_df){
      
      gender_df %>%
        
        Dataframe.count_over_factor(
          "tipo_contrato",
          normalize=normalize
          ) %>%

        mutate(sexo=gender_df$"gender"[1]) %>%
        
        Dataframe.order("sexo")

    }) %>% bind_rows()
}
#
modelo_colocaciones.colocaciones_por_tipo_contrato()
#
modelo_colocaciones.colocaciones_por_tipo_contrato.plot=function(
  Modelo_colocaciones=colocaciones_df.modelo_colocaciones_normalizado(),
  Normalize=TRUE
){
  library(ggplot2)
  #
  Modelo_colocaciones %>%
    modelo_colocaciones.colocaciones_por_tipo_contrato(
      normalize = Normalize
    ) %>%
    tidyr::pivot_longer(cols=-"sexo" ,names_to="tipo_contrato") %>%
    ggplot(aes(y=value,x=tipo_contrato, group=sexo))+
    geom_col(aes(fill=sexo), position=position_dodge())+
    
    geom_text(aes(label=value),
              size=3,
              colour="black",
              position=position_dodge(width=1.0))
}
#
modelo_colocaciones.colocaciones_por_tipo_contrato.plot()
#
modelo_colocaciones.colocaciones_por_tipo_contrato.plot(
  Normalize = FALSE
) 

#
#
modelo_colocaciones.colocaciones_salario=function(
  modelo_colocaciones=colocaciones_df.modelo_colocaciones_normalizado(),
  normalize=TRUE
){
  
  #
  library(ggplot2)
  #
  modelo_colocaciones %>%
    
    people_with_ids_df.genderdize() %>% 
    
    #Tabla_con_cedulas.aplicar_modelo_de_control() %>%  
    #
    #
    split(.[["gender"]]) %>%   
    
    lapply(function(gender_df){
      
      gender_df %>%
        
        Dataframe.count_over_factor(
          "salario",
          normalize=normalize
        ) %>%
        
        mutate(sexo=gender_df$"gender"[1]) %>%
        
        Dataframe.order("sexo")
      
    }) %>% bind_rows()
}
#
modelo_colocaciones.colocaciones_salario()
#
modelo_colocaciones.colocaciones_por_salario.plot=function(
  Modelo_colocaciones=colocaciones_df.modelo_colocaciones_normalizado(),
  Normalize=TRUE
){
  library(ggplot2)
  #
  Modelo_colocaciones %>%
    modelo_colocaciones.colocaciones_salario(
      normalize = Normalize
    ) %>%
    tidyr::pivot_longer(cols=-"sexo" ,names_to="salario") %>%
    ggplot(aes(y=value,x=salario, group=sexo))+
    geom_col(aes(fill=sexo), position=position_dodge())+
    
    geom_text(aes(label=value),
              size=3,
              colour="black",
              position=position_dodge(width=1.0))
}
#
modelo_colocaciones.colocaciones_por_salario.plot()
#
modelo_colocaciones.colocaciones_por_salario.plot(
  Normalize = FALSE
) 