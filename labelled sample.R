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
source("placements.R", encoding="UTF-8")

#
# app modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

setwd(CODE_HOME)
source("persistence utils.R", encoding="UTF-8")

#
# labelled sample
# --------
#

#
db.read_unlabelled_sample=function(
){
  setwd(ASSESTMENT_HOME)
  readxl::read_xlsx("tabla_para_validar_balance_de_muestra.xlsx")
}
#
db.read_unlabelled_sample() %>% View()

#
db.covid_wise_placements=function(){
  #
  setwd(LABELLED_SAMPLE_HOME)
  readxl::read_excel("covid design sample by paula.xlsx") %>% 
    dplyr::select(c("doc_num", grep("(dd)|(alguna_contratacion)",names(.))))
}
db.covid_wise_placements() %>% View()



#
unlabeled_sample.label_with_outcomes=
  function(
    unlabeled_sample=db.read_unlabelled_sample() ,
    outcomes1=db.colocaciones_por_oferente(),
    outcomes1=db.covid_wise_placements() 
  ){
    dplyr::left_join(
      unlabeled_sample,
      outcomes) %>%
      dplyr::left_join(
        
      )
  }
#
unlabeled_sample.label_with_outcomes() %>% View()
#
unlabeled_sample.label_with_outcomes.cache=
  unlabeled_sample.label_with_outcomes()
#

unlabeled_sample.label_with_outcomes.cache %>% 
  #
  Dataframe.share_with_app(
    label="final_evalaution_sample",
    app_route = LABELLED_SAMPLE_HOME
  )




#
covid_sample=
  db.covid_wise_placements()  %>%
  dplyr::left_join(
    unlabeled_sample.label_with_outcomes.cache,
    by="doc_num"
  ) 
#
covid_sample %>%
  #
  Dataframe.share_with_app(
    label="labelled covid sample",
    app_route = LABELLED_SAMPLE_HOME
  )

#
# analytics
# --------

#
labelled_sample.diff_in_means=function(
  labelled_sample=unlabeled_sample.label_with_outcomes()
){
  labelled_sample %>% 
     mutate(
          ruta_empleabilidad=
          dplyr::case_when(
          activ_cualquiera==0~"0- control",
          activ_cualquiera<=2~"1-2 participacion(es)",
          activ_cualquiera>=3~">3 participacion(es)",
          TRUE~"this record should´n exist"
          )) %>%
    dplyr::mutate(
      alguna_colocacion=ifelse(num_contrataciones>0, 1, 0 )
      ) %>%
    split(paste(.$sexo, .$ruta_empleabilidad)) %>%
    lapply(function(sub_df){
      data.frame(
        sexo=sub_df$sexo[1],
        ruta_empleabilidad=sub_df$ruta_empleabilidad[1],
        n_submuestra=nrow(sub_df),
        num_colocaciones=sum(sub_df$alguna_colocacion),
        tasa_colocacion=round(mean(sub_df$alguna_colocacion)*100, 3),
        tasa_colocacion_altos_salarios=round(mean(sub_df$salarios_altos)*100, 3)
        )
    }) %>%
    bind_rows() %>%
    dplyr::arrange(sexo, ruta_empleabilidad)
}
#
labelled_sample.diff_in_means()
#
diff_in_means.plot=function(
  diff_in_means=labelled_sample.diff_in_means(),
  salarios_var="tasa_colocacion_altos_salarios",
  niveles_participacion=
    c("0- control",
      "1- participacion(es)",
      "2- participacion(es)",
      "1-2 participacion(es)",
      ">3 participacion(es)")
){
  library(ggplot2)
  library(ggthemes)
  
  diff_in_means %>%
    dplyr::mutate(
      ruta_empleabilidad=
        factor(ruta_empleabilidad,
               levels=niveles_participacion)
    ) %>%
    dplyr::mutate(
      plotted_var=.[[salarios_var]]
    ) %>%
    ggplot(aes(y= plotted_var, x= ruta_empleabilidad , group=sexo))+
    geom_point(aes(fill=sexo, col=sexo),
               size=4,
               #alpha=.4,
               width = 0.7)+
    geom_line(aes(fill=sexo, col=sexo),
              lwde=3, lty="dashed")+
    geom_text(aes(y= plotted_var+.7, label=tasa_colocacion),
              size=3,
              colour="black")+
    theme_stata()+
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))+
    theme(legend.title = element_text(size=8),legend.text = element_text(size=8))+
    theme(legend.key.height = unit(.5, 'cm'), legend.key.width = unit(.5, 'cm'))
}
#
diff_in_means.plot(
  salarios_var="tasa_colocacion"
)+
  labs(
    title = "Tasa de colocación e intermediacion laboral",
    subtitle = "Tasa de colocacion según número de actividades de intermediación laboral",
    caption = 
      sprintf("A partir %s  registros de colocaciones y participaciones",
              sum(labelled_sample.diff_in_means()$n_submuestra)),
    x="# de actividades que asistió",
    y="Tasa de colocación"
  )
#
diff_in_means.plot(
  salarios_var="tasa_colocacion_altos_salarios"
)+
  labs(
    title = "Tasa de colocación e intermediacion laboral",
    subtitle = "Tasa de colocacion segun nivel de participacion",
    caption = 
      sprintf("A partir %s  registros de colocaciones y participaciones",
              sum(labelled_sample.diff_in_means()$n_submuestra)),
    x="# de actividades que asistió",
    y="Tasa de colocación"
  )

diff_in_means.plot(
  diff_in_means=
    unlabeled_sample.label_with_outcomes.cache %>%
    dplyr::filter(laboral_states=="desempleado") %>%
    labelled_sample.diff_in_means()
)+
  labs(
    title = "Tasa de colocación e intermediacion laboral",
    subtitle = "Tasa de colocacion segun participacion en actividades de intermediación",
    caption = 
      sprintf("A partir %s  registros de colocaciones y participaciones",
              sum(labelled_sample.diff_in_means()$n_submuestra)),
    x="# de actividades de intermediación que asistió",
    y="Tasa de colocación"
  )

#
unlabeled_sample.label_with_outcomes() %>% 
  
  labelled_sample.diff_in_means() %>%
  
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = c("n_submuestra", "num_colocaciones", "tasa_colocacion")
    
  ) %>%
  
  Dataframe.export_output(label="dif de medias",
                          output_home=LABELLED_SAMPLE_HOME,
                          new_output_array = TRUE)
