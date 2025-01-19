#
# routes
# --------
#

rm(list=ls())
CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

# app data
# --------
#

setwd(CODE_HOME)
list.files(pattern="data")
source("data experimental cases.R")
db.experimental_cases() %>% View()

setwd(CODE_HOME)
source("data covid sample.R")
db.labellled_sample() %>% View()


# app services
# --------
#

setwd(CODE_HOME)
source("genderdize records.R")

setwd(CODE_HOME)
source("placements.R", encoding="UTF-8")


#
# app services
# --------
#

#
setwd(LABELLED_SAMPLE_HOME)
#
db.labellled_sample=function(
  
){
  setwd(LABELLED_SAMPLE_HOME)
  #
  readxl::read_excel("labelled sample.xlsx")
}
db.labellled_sample() %>% View()
#
labelled_sample.diff_in_means=function(
  labelled_sample=db.labellled_sample()
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
labelled_sample.diff_in_means(
  labelled_sample=
    db.labellled_sample() %>% 
    dplyr::filter(laboral_states=="desempleado") 
)
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
    ggplot(aes(y= plotted_var, x= ruta_empleabilidad , group=sexo),
           position=position_dodge(width=.4))+
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
  labelled_sample.diff_in_means(
    labelled_sample=
      db.labellled_sample() %>% 
      dplyr::filter(laboral_states=="desempleado") 
  ),
  salarios_var="tasa_colocacion"
)+
  labs(
    subtitle = "Tasa de colocacion según número de actividades",
    caption = 
      sprintf("A partir %s  registros de oferentes desempleados",
              db.labellled_sample() %>% 
                dplyr::filter(laboral_states=="desempleado") %>%
                nrow),
    x="# de actividades que asistió",
    y="Tasa de colocación"
  )


#
# specific services
# --------
#

#
experimental_cases=db.experimental_cases()
min_sample_size=25
placements=db.colocaciones_por_oferente() 
#
library(ggplot2)
experimental_cases %>%
  dplyr::left_join(placements) %>% 
  split(.$treatment_case) %>%
  lapply(function(sub_df){
    
    #
    # (1.2) summarize the # of activites and placements
    #
    data.frame(treatment_case=sub_df$treatment_case[1],
               sample_size=nrow(sub_df),
               average_activities=mean(sub_df$activ_cualquiera),
               placement_rate=round(mean(sub_df$alguna_contratacion, na.rm=TRUE)*100,3)
    ) %>%
      dplyr::mutate(average_activities=round(average_activities,3)) 
  }) %>%
  bind_rows() %>%
  dplyr::arrange(average_activities*(-1)) %>%
  dplyr::filter(sample_size>min_sample_size) %>%
  
  dplyr::mutate(
    treatment_case=iconv(treatment_case,from="UTF-8",to="ASCII//TRANSLIT")
  ) %>%
  
  dplyr::mutate(
    treatment_case=stringr::str_replace_all(treatment_case, "AND", "\n AND \n")
  ) %>%
  
  ggplot(aes(x=average_activities, y=placement_rate, size=placement_rate))+
  geom_point(aes(col=treatment_case), alpha=.3)+
  ggrepel::geom_label_repel(
    aes(
      fill=treatment_case, 
      label=sprintf("%s, (%s)",treatment_case, placement_rate )),
    fontface = "bold",
    colour="white",
    size=3
  )+
  theme_stata()+
  theme(legend.position="none")+
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))+
  theme(legend.title = element_text(size=8),legend.text = element_text(size=8))+
  theme(legend.key.height = unit(.5, 'cm'), legend.key.width = unit(.5, 'cm'))+
  labs(
    subtitle="Tasa de colocación según servicios recibidos"
  )+
  xlab("cantidad de actividades (promedio)")+
  ylab("tasa de colocación")



#
# covid design
# --------
#

covid_sample=db.covid_sample()
experimental_cases=db.experimental_cases()
threshold=2
library(ggthemes)
#
library(ggplot2)
covid_sample %>%
  #
  people_with_ids_df.genderdize() %>%
  #
  dplyr::left_join(
    experimental_cases %>%
      dplyr::transmute(doc_num=doc_num,activ_cualquiera=activ_cualquiera)) %>%
  #
  dplyr::mutate(
    high_participation=
      dplyr::case_when(
        activ_cualquiera>=threshold~"participación alta",
        activ_cualquiera==0~"ninguna participación",
        TRUE~""
      )) %>% 
  
  split(paste(.$gender, .$registro_post_covid, .$high_participation, sep="and")) %>%
  lapply(function(sub_df){
    data.frame(
      subsample_size=nrow(sub_df),
      gender=sub_df$gender[1],
      registro_post_covid=sub_df$registro_post_covid[1],
      high_participation=sub_df$high_participation[1],
      placement_rate=round(mean(sub_df$some_placement)*100,3)
    )
  }) %>%
  bind_rows() %>%
  
  dplyr::filter(
    high_participation!=""
  ) %>%
  dplyr::mutate(
    sample=paste(gender, registro_post_covid, sep=" and ")
  ) %>%
  ggplot(aes(x=placement_rate, y=sample, group=sample))+
  geom_point(aes(color=high_participation), size=4)+
  geom_line(lty="dashed")+
  theme_stata()+
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust=1))+
  theme(legend.title = element_text(size=8),legend.text = element_text(size=8))+
  theme(legend.key.height = unit(.5, 'cm'), legend.key.width = unit(.5, 'cm'))+
  labs(
    subtitle="Impacto de intermediación según fase covid"
  )+
  xlab("tasa de colocación")+
  ylab("submuestra")
    