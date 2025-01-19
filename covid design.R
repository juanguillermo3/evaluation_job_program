# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

# app data
# --------
#

library(dplyr)
setwd(CODE_HOME)
list.files(pattern="data")
source("data labelled sample.R")
db.labellled_sample() %>% View()

setwd(CODE_HOME)
source("data placements.R", encoding="UTF-8")
db.placements() %>% View()

setwd(CODE_HOME)
source("data quality control.R")
db.informacion_de_control() %>% View()

# app services
# --------
#

setwd(CODE_HOME)
source("genderdize records.R")

setwd(CODE_HOME)
source("utensilios para fechas.R", encoding="UTF-8") 

setwd(CODE_HOME)
source("persistence utils.R", encoding="UTF-8")

# confg
# --------
#

setwd(CODE_HOME)
list.files(pattern="sys")
source("sys constants.R")

# covid design
# ---------

#
#
control_df.pre_and_post=function(
  control_df=db.informacion_de_control(),
  covid_model=
    c(as.POSIXct("2018-07-01"),as.POSIXct(FECHA_INICIO_COVID),as.POSIXct("2022-01-01"))
    
){
  control_df %>% 
    
    dplyr::mutate(
      registro_post_covid=
        ifelse(fecha_actualizacion2>=FECHA_INICIO_COVID, "post-covid", "pre-covid")
      
    ) %>%
    
    dplyr::select(c("doc_num", "fecha_actualizacion2", "registro_post_covid" )) %>%
    
    dplyr::mutate(covid_model=cut(fecha_actualizacion2, breaks =  covid_model)) %>%
    
    na.omit()
}
#
control_df.pre_and_post() %>% View()

#
when_registers=function(
  registers=control_df.covid_design(),
  limits
){
  registers %>%
    dplyr::filter(fecha_actualizacion2>limits[1],
                  fecha_actualizacion2<limits[2])
}
when_registers(limits=c(FECHA_INICIO_COVID,FECHA_INICIO_COVID+365*(1.5))) %>% nrow()
when_registers(limits=c(FECHA_INICIO_COVID-365*(1.5),FECHA_INICIO_COVID)) %>% nrow()
when_registers()%>% View()
#
when_placements=function(
  placements=db.placements(),
  limits
){
  placements %>%
    dplyr::filter(fecha_colocacion>limits[1],fecha_colocacion<limits[2])
}
when_placements(limits=c(FECHA_INICIO_COVID,FECHA_INICIO_COVID+365*(1.5))) %>% nrow()
when_placements(limits=c(FECHA_INICIO_COVID-365*(1.5),FECHA_INICIO_COVID)) %>% nrow()
#
#
covid_design.labelled_sample_for_covid_design.count_placements_in_period=
  function(
    covid_design=control_df.pre_and_post()
  ){
    #
    #
    covid_design %>%
      #
      split(.$"covid_model") %>%
      #
      lapply(function(sub_df){
        sub_placements=
          when_placements(
            limits=  #
              {if(as.Date(sub_df$covid_model[1])<FECHA_INICIO_COVID){
                c(as.Date(sub_df$covid_model[1]), FECHA_INICIO_COVID)
              } else {
                c(FECHA_INICIO_COVID, FECHA_INICIO_COVID+365*2)
              }})
        df=
        sub_df %>%
          dplyr::mutate(
            some_placement=as.numeric(doc_num %in% sub_placements$doc_num)
          )
        #mean(df$some_placement)*100
      }) %>%
    #
    bind_rows()
  }
#
covid_design.labelled_sample_for_covid_design.count_placements_in_period() %>% View()


#
db.evaluation_matrix=function(){
  setwd("C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/labelled sample")
  readxl::read_excel("labelled sample.xlsx")
}
db.evaluation_matrix() %>% View()
#
covid_design.labelled_sample_for_covid_design=
  function(
    covid_design=covid_design.labelled_sample_for_covid_design.count_placements_in_period(),
    evalution_matrix=db.evaluation_matrix()
  ){
    covid_design %>%
      na.omit() %>%
      dplyr::left_join(
        evalution_matrix,
        by="doc_num"
      )
  }
covid_design.labelled_sample_for_covid_design() %>% View()
df=covid_design.labelled_sample_for_covid_design()
#
Dataframe.share_with_app(df,
                         label="other labelled covid design",
                         app_route =LABELLED_SAMPLE_HOME )



#
#
ensemble_control_group=function(
  full_sample=db.labellled_sample(),
  Limits=c(FECHA_INICIO_COVID-365*(4),FECHA_INICIO_COVID)
){
  placements=when_placements(limits=Limits)
  when_registers(limits=c("2015-01-01", "2025-01-01")) %>%
    dplyr::mutate(
      some_placement_pre=as.numeric(doc_num %in% placements$doc_num)
    )
}
ensemble_control_group() %>% View()
#
ensemble_treatment_group0=function(
  pre_covid_placements=ensemble_control_group()
){
  pre_covid_placements %>%
    dplyr::filter(some_placement==0) %>%
    dplyr::mutate(registro_post_covid="survivor")
}
#
ensemble_treatment_group0() %>% View()
#
ensemble_treatment_group=function(
  full_sample=db.labellled_sample(),
  Limits=c(FECHA_INICIO_COVID,FECHA_INICIO_COVID+(365*(4)))
){
  placements=when_placements(limits=Limits)
  when_registers(limits=c("2015-01-01", "2025-01-01")) %>%
    dplyr::mutate(
      some_placement_post=as.numeric(doc_num %in% placements$doc_num)
    )
}
ensemble_treatment_group() %>% View()

# covid sample
# --------
#

covid_sample=function(
  control_group=ensemble_control_group(),
  treatment_group=ensemble_treatment_group()
){
  dplyr::left_join( control_group,  treatment_group, by="doc_num")
}
covid_sample() %>% View()
#
covid_sample.genderized_overview=function(
  covid_sample=covid_sample()
){
  covid_sample %>%
    people_with_ids_df.genderdize() %>% 
    split(paste(.$gender, .$registro_post_covid, sep="and")) %>%
    lapply(function(sub_df){
      data.frame(
        gender=sub_df$gender[1],
        registro_post_covid=sub_df$registro_post_covid[1],
        placement_rate=round(mean(sub_df$some_placement)*100,3)
      )
    }) %>%
    bind_rows()
}
#
covid_sample.genderized_overview()

df=covid_sample()
Dataframe.share_with_app(df,label="muestra covid", app_route =COVID_DESIGN_HOME )