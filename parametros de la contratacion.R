#
#
basic_standardization=function(
  names
){
  #
  tolower(names) %>% iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
}
#
library(dplyr)
#
db.raw_text_from_vacancies=function(
  
  scrapped_files=c("info vacantes.csv", "delivery scrapper vacantes 20211123-013735.csv")
  
){
  
  #
  setwd(VACANTES_HOME)
  #
  lapply( scrapped_files, function(some_file){
    some_file %>%
      read.csv(encoding="UTF-8")%>% 
      {names(.)=c("n", "vacante");.} %>% 
      dplyr::select(vacante) %>%
      mutate(texto_vacante_estandarizado=basic_standardization(vacante))
  }) %>% bind_rows()
  
}
#
db.raw_text_from_vacancies() %>% View()
#
Texto_vacantes_df.parametros_contrato=function(
  Texto_vacantes_data=db.raw_text_from_vacancies()
){
  
  Texto_vacantes_data %>%
    dplyr::mutate(
      fecha_creacion=stringr::str_extract(texto_vacante_estandarizado, "[0-9]{2}/[0-9]{2}/[0-9]{4}"),
      fecha_creacion= lubridate::dmy(fecha_creacion)
    ) %>%
    dplyr::mutate(
      year=lubridate::year(fecha_creacion),
      month=lubridate::month(fecha_creacion),
      day=lubridate::day(fecha_creacion)
    ) %>%
    dplyr::mutate(
      codigo_vacante=stringr::str_extract(texto_vacante_estandarizado, "[[0-9][-]]{5,15}")
    ) %>%
    dplyr::mutate(
      salario=stringr::str_extract(texto_vacante_estandarizado, "(:[^:]*smmlv)|(: a convenir)"),
      salario=stringr::str_replace_all(salario, pattern="[:punct:]", replacement="")
    ) %>%
    dplyr::mutate(
      tipo_de_contrato=stringr::str_extract(texto_vacante_estandarizado,
                                            "smmlv(.*)distribucion"),
      tipo_de_contrato=stringr::str_replace_all(tipo_de_contrato, 
                                                pattern="(smmlv)|(distribucion)", 
                                                replacement="")
    ) %>%
    dplyr::select(
      c(
      "fecha_creacion",             
      "year",                       
      "month",                      
      "day",                        
      "codigo_vacante",             
      "salario",                    
      "tipo_de_contrato")
    )
}
#
Texto_vacantes_df.parametros_contrato() %>% View()


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
  Vacantes_df=Texto_vacantes_df.parametros_contrato()
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
  Texto_vacantes_df.parametros_contrato()
)



