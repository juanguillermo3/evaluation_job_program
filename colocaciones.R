#
# base de colocaciones
# --------
#

rm(list=ls())
DATA_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes colocados"
setwd(DATA_HOME)

#
library(dplyr)

#
#
db.read_colocados_df=function(){
  list.files( pattern="Colocados" ) %>%
    lapply(., function(colocados_file){
      readxl::read_excel(colocados_file)
    }) %>% bind_rows()
}
#
db.read_colocados_df() %>% View()

colocados_df=db.read_colocados_df()


#
#
Tabla_de_colocados.colocados_en_el_tiempo=function(
  Tabla_de_colocados=colocados_df
){
  
    library(ggplot2)
  
    caption_statement=sprintf("a partir de los registros de colocaciones, con %s colocaciones registradas", nrow(Tabla_de_colocados))
  
  
    Tabla_de_colocados %>% 
      dplyr::transmute(
        sexo=dplyr::case_when(
          .[["sexo"]]=="M"~"hombre",
          .[["sexo"]]=="F"~"mujer",
          TRUE~""
        ),
        fecha_colocacion=as.character(.[["Fecha Colocación"]]),
        year= stringr::str_extract(.[["Fecha Colocación"]], pattern="[0-9]{4}")
      ) %>%  
    
      split(., .[["year"]]) %>%
      
      lapply(., function(year_df){
        
         year_df %>%
          split(., .[["sexo"]]) %>%
          
          lapply(., function(year_sex_df){
            
            data.frame(
              year=year_sex_df$year[1],
              sexo=year_sex_df$sexo[1],
              num_colocaciones=nrow(year_sex_df)
            )
            
          }) %>% bind_rows()
          
      }) %>% bind_rows() %>%
      
      ggplot(., aes(x=year, y=num_colocaciones, group=sexo))+
      geom_col(aes(fill=sexo), alpha=0.4, position = "dodge")+
      geom_text(aes(label=num_colocaciones), 
                position = position_dodge(width = 1))+
      labs(
        title = "Colocaciones a traves de los años",
        subtitle="Número de colocaciones en cada año, por sexo",
        caption=caption_statement
      )+
      ylab("número de colocaciones")+
      xlab("año")
  
}
#
Tabla_de_colocados.colocados_en_el_tiempo()


Tabla_de_colocados.tipos_de_contrato=function(
  Tabla_de_colocados=colocados_df,
  min_freq=8
){
  
  t=table(Tabla_de_colocados$Sector) %>% sort()
  #
  p_75=summary(t %>% as.numeric())["3rd Qu."]
  #
  count_x_sector=
  data.frame(
    sector=names(t) %>% as.character(t),
    numero_colocados=t %>% as.numeric()
  ) %>% 
    dplyr::mutate(
      sector=dplyr::case_when(
        .[["numero_colocados"]]<min_freq~"Otro sector",
        TRUE~.[["sector"]]
      ))
  row.names(count_x_sector)=NULL
  
  count_x_sector$sector
  

  Tabla_de_colocados %>% 
    dplyr::transmute(
      sector=.[["Sector"]],
      tipo_de_contrato=.[["Tipo Contrato"]],
      sector= ifelse(
        (sector %in%  count_x_sector$sector), sector, "Otro sector"),
      sectorXtipo_de_contrato=paste(sector, tipo_de_contrato, sep="X"),
    ) %>%
    
    split(., .[["sectorXtipo_de_contrato"]]) %>%
    
    lapply(.,function(partial_df){
      data.frame(
        sector=partial_df[["sector"]][1],
        tipo_de_contrato=partial_df[["tipo_de_contrato"]][1],
        num_colocaciones=nrow(partial_df)
      )
    }) %>%
    bind_rows() %>% 
    dplyr::arrange(
      num_colocaciones*(-1)
    ) %>%
  
    mutate( sector_label= factor(sector, 
                                 levels= c(names(t), "Otro sector"),
                                 labels= substr(c(names(t), "Otros sectores"), 1,30))
              ) %>%
    ggplot(., aes(x=sector_label, y=num_colocaciones, group=tipo_de_contrato))+
    geom_col(aes(fill=tipo_de_contrato))+
    labs(
      title = "Colocaciones por sector y tipo de contrato",
      subtitle="Número de colocaciones por cada sector, desagregado por tipo de contrato",
      caption=caption_statement
    )+
    ylab("número de colocaciones")+
    xlab("sector")+
    coord_flip()
  
}
#
Tabla_de_colocados.tipos_de_contrato()

t=round(prop.table(table(colocados_df$`Tipo Contrato`))*100, 3) %>% sort(decreasing = TRUE)
data.frame(
 "Tipo de contrato"=names(t) %>% as.character(),
 "Proporcion de colocaciones"=t %>% as.numeric())
