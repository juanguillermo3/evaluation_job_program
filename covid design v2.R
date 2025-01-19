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
source("data fechas de registro.R", encoding="UTF-8")
db.fechas_de_registro() %>% View()

setwd(CODE_HOME)
source("data last participation.R" , encoding="UTF-8")
db.last_participation() %>% View()

setwd(CODE_HOME)
source("data placements.R", encoding="UTF-8")
db.placements() %>% View()

setwd(CODE_HOME)
source("data candidates.R" , encoding="UTF-8")
db.candidates() %>% View()

# app services
# --------
#

setwd(CODE_HOME)
source("genderdize records.R")

setwd(CODE_HOME)
source("utensilios para fechas.R", encoding="UTF-8") 

setwd(CODE_HOME)
source("persistence utils.R", encoding="UTF-8")

setwd(CODE_HOME)
source("dataframe extention.R")


# confg
# --------
#

setwd(CODE_HOME)
list.files(pattern="sys")
source("sys constants.R")

# covid design
# ---------

#
last_participation.pre_and_post=function(
  last_participation=
    db.last_participation() 
){
  last_participation %>%
    dplyr::mutate(fecha_ultima_part=as.Date(fecha)) %>%
    dplyr::mutate(fecha=as.Date(fecha)) %>%
    dplyr::mutate(
      registro_post_covid=
        ifelse(fecha_ultima_part>=(FECHA_INICIO_COVID), "post-covid", "pre-covid")
    ) 
}
#
last_participation.pre_and_post() %>% View()
table(df$registro_post_covid)

# counting events on specific time windows
# ---------

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
when_postulations=function(
  postulations=db.candidates(),
  limits
){
  postulations %>%
    dplyr::filter(fecha>limits[1],fecha<limits[2])
}
when_postulations(limits=c(FECHA_INICIO_COVID,FECHA_INICIO_COVID+365*(1.5))) %>% nrow()
when_postulations(limits=c(FECHA_INICIO_COVID-365*(1.5),FECHA_INICIO_COVID)) %>% nrow()

# ensembling the covid evaluation database
# ---------

#
Dataframe.Groupped_count_of_events=function(
  Dataframe,
  entity_col
){
  Dataframe %>% 
    split( Dataframe[[entity_col]]) %>%
    lapply(function(sub_df){
      data.frame(
        entity=sub_df[[entity_col]] %>% head(1),
        count=sub_df %>% nrow()
      )
    }) %>%
    bind_rows()
}
#
Dataframe.Groupped_count_of_events(
  Dataframe=when_placements(limits=c(FECHA_INICIO_COVID,FECHA_INICIO_COVID+365*(1.5))),
  entity_col="doc_num"
) %>% View()




#
covid_design.count_outcomes_in_periods=
  function(
    
    covid_design=
      last_participation.pre_and_post() %>% 
      
      Dataframe.delimite_dates(
        upper_date = db.candidates() %>% {max(.[["fecha"]])},
        lower_date = db.candidates() %>% {min(.[["fecha"]])}
      ),
    
    cache_name="covid_design_df_cached_in_memory",
    new_event=FALSE
    
  ){
    #
    if ( cache_name %in% names(.GlobalEnv) & !new_event ){
      print(sprintf("%s fetched from cache", cache_name))
      return(.GlobalEnv[[cache_name]])
    }
    
    #
    covid_design %>% 
      #
      split(.$"registro_post_covid") %>%
      #
      lapply(function(sub_df){
        
        #
        period_wise_limits=
        {if(sub_df$registro_post_covid[1]=="pre-covid"){
          c(as.Date("2005-01-01"), FECHA_INICIO_COVID)
        } else {
          c(FECHA_INICIO_COVID, as.Date("2025-01-01"))
        }}
        
        #
        sub_placements=
          when_placements(limits=period_wise_limits) %>%
          Dataframe.Groupped_count_of_events(entity_col="doc_num")  %>% 
          dplyr::rename(doc_num=entity, phase_placements=count) 
        #
        sub_postulations=
          when_postulations(limits=period_wise_limits) %>%
          Dataframe.Groupped_count_of_events(entity_col="doc_num") %>% 
          dplyr::rename(doc_num=entity, phase_postulaciones=count) 
        #
        sub_postulations.auto=when_postulations(
          postulations = db.candidates() %>% dplyr::filter(tipo_postulacion=="Autopostulado"),
          limits=period_wise_limits) %>%
          Dataframe.Groupped_count_of_events(entity_col="doc_num") %>% 
          dplyr::rename(doc_num=entity, phase_auto_postulaciones=count) 
        #
        sub_postulations.remmited=when_postulations(
          postulations = db.candidates() %>% dplyr::filter(tipo_postulacion=="Remitido por prestador"),
          limits=period_wise_limits) %>%
          Dataframe.Groupped_count_of_events(entity_col="doc_num") %>% 
          dplyr::rename(doc_num=entity, phase_remisiones=count) 
        #
        
        #
        list(
          sub_df,
          sub_placements,
          sub_postulations,
          sub_postulations.auto,
          sub_postulations.remmited
        ) %>%
          Reduce(f=function(x,y){
            
            dplyr::left_join(x,y, by="doc_num") %>%
              Dataframe.map_NAS()
          }) 

      }) %>%
    #
      bind_rows() %>%
      
      #
      {
        print(sprintf("%s not found, but newly created in cache", cache_name))
        .GlobalEnv[[cache_name]]=.
      } %>%
      return()
  }
#
covid_design.count_outcomes_in_periods(new_event = TRUE) %>% View()
#
covid_design.count_outcomes_in_periods() %>% View()


#
rate.pretty=function(
  rate
){
  round(rate*100,3)
}
#
mean.pretty=function(
  mean,
  rate_factor=100
){
  round(mean*rate_factor,3)
}
#
db.labelled_evaluation_matrix=function(){
  setwd("C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/labelled sample")
  readxl::read_excel("labelled sample.xlsx")
}
#
covid_design.count_outcomes_in_periods() %>%
  
  dplyr::left_join(
    db.labelled_evaluation_matrix() %>% 
      dplyr::select(doc_num, activ_cualquiera)
  ) %>% 
  
  dplyr::filter(
    activ_cualquiera>0
  ) %>%
  
  people_with_ids_df.genderdize() %>%
  split(paste(.$gender, .$registro_post_covid)) %>%  
  lapply(function(sub_df){
    sub_df %>% 
      Dataframe.select_on_regex("phase")  %>%
      sapply(function(data_col){
        mean(as.numeric(data_col)) %>% mean.pretty()
        #mean(as.numeric(data_col)) %>% mean.pretty()
      })
  })



#
outcomes.labelled_sample_for_covid_design=
  function(
    outcomes=covid_design.count_outcomes_in_periods(),
    evalution_matrix=db.labelled_evaluation_matrix(),
    treshold_range=1:3
  ){
    #
    new_df=
      outcomes %>%
      dplyr::inner_join(
        evalution_matrix,
        by="doc_num"
      )
    #
    new_df$dd_post_covid=as.numeric(new_df$registro_post_covid.x=="post-covid")
    #
    for(some_treshold in treshold_range){
      #
      #
      tr_temp_var=sprintf("dd_Treatment_%s", some_treshold )
      new_df[[tr_temp_var]]=
        dplyr::case_when(
          new_df$activ_cualquiera>=some_treshold~1,
          new_df$activ_cualquiera==0~0,
          TRUE~NaN)
      #
      #
      new_df[[ sprintf("dd_Post_CovidXTreatment_%s", some_treshold ) ]]= 
        new_df$dd_post_covid*new_df[[tr_temp_var]]
    }
      
      #print(sprintf("Post_CovidXTreatment_%s", some_treshold ))
      
    #
    #names(new_df) %>% tail()
    new_df %>% return()

  }
outcomes.labelled_sample_for_covid_design() %>% View()


#
df=outcomes.labelled_sample_for_covid_design()
#
Dataframe.share_with_app(df,
                         label="covid design sample by paula",
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