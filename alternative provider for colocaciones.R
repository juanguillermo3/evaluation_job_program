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
source("utensilios para fechas.R")

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
  file_prefix="procesos laborales"
){
  setwd(PROCESS_HOME)
  #
  library(dplyr)
  list.files(pattern=file_prefix) %>%
    lapply(.,function(some_file){
      read.csv(some_file) %>%
        Dataframe.reencode() %>%
        Dataframe.vars_as_character() 
    }) %>% bind_rows() 
}
#
db.registros_colocaciones.cache=db.registros_colocaciones() 

#
colocaciones_df.modelo_colocaciones_normalizado=function(
  colocaciones_df=db.registros_colocaciones.cache
){
  #
  # subset a las contrataciones: solo contrataciones con control de calidad
  # join con parámetros de la contratación
  
  colocaciones_df %>% 
    #
    # (0) select
    #
    transmute(
      doc_num=.[["Cedula"]],
      proceso=.[["proceso"]],
      fecha_colocacion=.[["fecha_limite"]],
      
      salario=.[["rango_salario"]],
      tipo_contrato=.[["tipo_contrato"]],
      
      scrapped="scrapped"
    ) %>% 
    
    #Tabla_con_cedulas.aplicar_modelo_de_control() %>%
    
    #Tabla_de_vacantes.incorporar_parametros() %>%
    #
    # (2) transform
    #
    mutate(llave_colocacion= paste(doc_num, proceso, sep="+")) %>%
    #
    # (3) normalize
    #
    dplyr::filter( !duplicated(llave_colocacion) ) 
}
#
colocaciones_df.modelo_colocaciones_normalizado() %>% View()

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
# analytics
# ---------

#
db.contrataciones_normalizadas=function(){
  #
  setwd(CONTRATACIONES_ESTANDARIZADAS_HOME)
  #
  readxl::read_excel("colocaciones estandarizadas.xlsx")
}
#
db.contrataciones_normalizadas() %>% View()


#
modelo_colocaciones.colocaciones_el_tiempo=function(
  modelo_colocaciones=db.contrataciones_normalizadas(),
  fechas_de_corte=seq(as.Date("2017/1/1"), 
                      as.Date("2022/1/1"),
                      by = "quarter")
){
  
  #
  library(ggplot2)
  library(ggthemes)
  #
  modelo_colocaciones %>% 
    
    people_with_ids_df.genderdize() %>% 
    
    dplyr::mutate(
      fecha_colocacion=remediar_fechas(fecha_colocacion)
    ) %>% 
    
    mutate(
      fecha= as.POSIXct(fecha_colocacion,format="%d %m %Y"),
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
        
        sub_df=sub_df %>% na.omit() 
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
    theme_stata()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8))+
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))+
    theme(legend.title = element_text(size=8),legend.text = element_text(size=8))+
    theme(legend.key.height = unit(.5, 'cm'), legend.key.width = unit(.5, 'cm'))+
    labs(
      caption = 
        sprintf("Para un total de  %s colocaciones sobre población CMD (%s personas)",
                sum(!is.na(modelo_colocaciones$proceso)),
                nrow(modelo_colocaciones)
                )
    )
}
#
modelo_colocaciones.colocaciones_el_tiempo()+
  labs(
    title = "Colocaciones a través del tiempo",
    subtitle = "Número de colocaciones por trimestre y sexo",
    x="trimestre",
    y="# colocaciones"
  )

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
    "A convenir")
){
  labels %>%
    lapply(function(l){
      stringr::str_split(l," ") %>%
        unlist() %>%
        paste(collapse=" \n ")
    })
}
#
tokenizar_labels()
  
#
modelo_colocaciones.colocaciones_salario=function(
  modelo_colocaciones=db.contrataciones_normalizadas(),
  niveles_salario=c(
    "Menos de 1 SMMLV",
    "1 SMMLV",
    "1 a 2 SMMLV",
    "2 a 4 SMMLV",
    "4 a 6 SMMLV",
    "6 a 9 SMMLV",
    "15 a 19 SMMLV",
    "A convenir")
){
  #
  library(ggplot2)
  modelo_colocaciones %>% 
    dplyr::filter(
      tipo_contrato!=""
    ) %>%
    people_with_ids_df.genderdize() %>%
    split(.[["gender"]]) %>% 
    lapply(function(gender_df){
      gender_df %>%
        Dataframe.count_over_factor(
          "salario",
          normalize=TRUE
        ) %>%
        mutate(sexo=gender_df$"gender"[1]) %>%
        Dataframe.order(c("sexo", niveles_salario))
    }) %>% bind_rows() %>%
    Dataframe.map_NAS()
}
#
modelo_colocaciones.colocaciones_salario() 
#
modelo_colocaciones.colocaciones_por_salario.plot=function(
  Modelo_colocaciones=db.contrataciones_normalizadas(),
  niveles_salario=c(
    "Menos de 1 SMMLV",
    "1 SMMLV",
    "1 a 2 SMMLV",
    "2 a 4 SMMLV",
    "4 a 6 SMMLV",
    "6 a 9 SMMLV",
    "15 a 19 SMMLV",
    "A convenir")
){
  library(ggplot2)
  library(ggthemes)
  #
  Modelo_colocaciones %>%
    modelo_colocaciones.colocaciones_salario() %>%
    tidyr::pivot_longer(cols=-"sexo" ,names_to="salario") %>%
    
    dplyr::mutate( 
      salario=factor(salario,
                     levels = niveles_salario,
                     labels = niveles_salario %>%
                       permutate_pattern() %>%
                       tokenizar_labels())
      ) %>%
    
    ggplot(aes(y=value,x=salario, group=sexo))+
    
    geom_col(aes(fill=sexo, col=sexo),
             position=position_dodge(width=.7),
             alpha=.9,
             width = 0.7)+
    
    geom_text(aes(y=value+2.5,label=value),
              size=3,
              colour="black",
              position=position_dodge(width=1.0))+
    theme_stata()+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size=8))+
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))+
    theme(legend.title = element_text(size=8),legend.text = element_text(size=8))+
    theme(legend.key.height = unit(.5, 'cm'), legend.key.width = unit(.5, 'cm'))
}
#
modelo_colocaciones.colocaciones_por_salario.plot()+
  labs(
    title = "Nivel salarial en los contratos concedidos",
    subtitle = "Porcentaje de las colocaciones en cada nivel salarial, por género",
    caption = "A partir de los registros de las colocaciones",
    x="Nivel salarial",
    y="% por género"
  )

#
modelo_colocaciones.colocaciones_por_tipo_contrato=function(
  modelo_colocaciones=db.contrataciones_normalizadas(),
  niveles_contrato=c(
    "Término Fijo", 
    "Obra",
    "Término Indefinido",
    "Prest. de Servicios",
    "Aprendizaje",
    "Otro",
    "Temporal")
){
  
  #
  library(ggplot2)
  modelo_colocaciones %>% 
    dplyr::filter(
      tipo_contrato!=""
    ) %>%
    people_with_ids_df.genderdize() %>% 
    split(.[["gender"]]) %>%   
    lapply(function(gender_df){
      gender_df %>% 
        Dataframe.count_over_factor(
          "tipo_contrato",
          normalize=TRUE
        ) %>%
        mutate(sexo=gender_df$"gender"[1]) %>%
        Dataframe.order(c("sexo", niveles_contrato))
    }) %>% bind_rows() %>%
    Dataframe.map_NAS()
}
#
modelo_colocaciones.colocaciones_por_tipo_contrato()
#
modelo_colocaciones.colocaciones_por_tipo_contrato.plot=function(
  Modelo_colocaciones=db.contrataciones_normalizadas(),
  niveles_contrato=c(
    "Término Fijo", 
    "Obra",
    "Término Indefinido",
    "Prest. de Servicios",
    "Aprendizaje",
    "Otro",
    "Temporal")
){
  library(ggplot2)
  #
  Modelo_colocaciones %>%
    modelo_colocaciones.colocaciones_por_tipo_contrato() %>%
    tidyr::pivot_longer(cols=-"sexo" ,names_to="tipo_contrato") %>%
    dplyr::mutate( 
      tipo_contrato=factor(
        tipo_contrato, 
        levels = niveles_contrato,
        labels = niveles_contrato %>% tokenizar_labels())
      ) %>%
    ggplot(aes(y=value,x=tipo_contrato, group=sexo))+
    
    geom_col(aes(fill=sexo, col=sexo),
             position=position_dodge(width=.7),
             alpha=.9,
             width = 0.7)+
    
    geom_text(aes(y=value+2.5,label=value),
              size=3,
              colour="black",
              position=position_dodge(width=1.0))+
    theme_stata()+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size=8))+
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))+
    theme(legend.title = element_text(size=8),legend.text = element_text(size=8))+
    theme(legend.key.height = unit(.5, 'cm'), legend.key.width = unit(.5, 'cm'))
}
#
modelo_colocaciones.colocaciones_por_tipo_contrato.plot()+
  labs(
    title = "Tipo de contrato en los contratos concedidos",
    subtitle = "Porcentaje de las colocaciones en cada tipo de contrato, por género",
    caption = "A partir de los registros de las colocaciones",
    x="Tipo de contrato",
    y="% por género"
  )


