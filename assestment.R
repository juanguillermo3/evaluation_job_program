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
source("genderdize records.R")
#
setwd(CODE_HOME)
source("demographic model for cmd population.R")
#
setwd(CODE_HOME)
source("perfiles laborales.R", encoding="UTF-8")


#
# app modules
# --------
#

#
setwd(CODE_HOME)
source("dataframe extention.R")
#
setwd(CODE_HOME)
source("persistence utils.R")

#
# confg
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("constants.R")

#
#

#
library(dplyr)
db.cargar_experimentos=function(){
  setwd(EXPERIMENTS_HOME)
  readxl::read_excel( "casos_experimentales.xlsx")
}
db.cargar_experimentos() %>% View()

#
db.cargar_fechas=function(){
  setwd(ASSISTANCE_HOME)
  readxl::read_excel( "fecha_ultima_asistencia.xlsx")
}
db.cargar_fechas() %>% View()
#
fechas.modelo_fechas=function(){
  db.cargar_fechas() %>%
    dplyr::mutate(
      year=lubridate::year(registro),
      momento_registro=registro %>% as.POSIXct() %>% as.numeric(),
      actividad_post_covid= ifelse(ultima_actividad>FECHA_INICIO_COVID, "post-covid", "pre-covid")
    )
}
fechas.modelo_fechas() %>%
  View()

#
#
Experimentos.join_with_features=function(
  Experimentos=
    db.cargar_experimentos() %>%
      dplyr::select(-c(grep("activ", names(.), value=TRUE))),
  modelo_demografico=muestra_cmd.modelo_demografico(),
  modelo_hoja_de_vida=db.perfiles_laborales(),
  modelo_fechas=fechas.modelo_fechas()
){
  Experimentos %>% 
    dplyr::left_join(
      modelo_demografico
    ) %>%
    dplyr::left_join(
      modelo_hoja_de_vida )%>%
    dplyr::left_join(
      modelo_fechas )
}
Experimentos.join_with_features() %>% View()

#
df=Experimentos.join_with_features() 
#
df %>% 
  Dataframe.share_with_app(
    app_route= ASSESTMENT_HOME,
    label="tabla_para_validar_balance_de_muestra"
  )

#
#
unbalanced_sample.covariates_table_comparison=function(
  
  unbalanced_sample= Experimentos.join_with_features(),
  
  logro_bachillerato=c("secundaria completa","univ o posterior"),
  logro_universitario=c("univ o posterior"),
  
  experiment="al_menos_3"
){
  
  #
  options(scipen=999)
  test_df=
    unbalanced_sample %>%
    dplyr::transmute(
      
      participante=unbalanced_sample[[ experiment]],
      
      es_mujer=(sexo=="mujer"),
      tiene_bachillerato_completo=(estudios %in% logro_bachillerato),
      tasa_universitaria_o_superior=(estudios %in% logro_universitario),
      edad=edad,
      edad_menor_20=edad <20,
      edad_20_a_30=(edad <=20 & edad <30),
      edad_mas_de_30=edad>=30,
      es_migrante=es_migrante,
      
      tiene_experiencia=experiencia>0,
      prom_experiencia=experiencia,
      prom_certificacion=certificacion,
      idioma_extranjero=(idioma_extranjero==1),
      competencias_digitales=(competencias_digitales==1)) 
  
  names(test_df) %>%
    lapply(function(data_col){
      try({
        
        S=split( test_df[[data_col]] , test_df$participante)
        t=t.test(S[[1]],S[[2]])
        
        t$estimate %>% 
          t() %>% 
          as.data.frame() %>%
          Dataframe.new_names(c("no_participantes","participantes")) %>%
          dplyr::mutate(
            diff=participantes-no_participantes,
            p_val=t$p.value
          ) %>%
          dplyr::mutate(Variable=data_col,)
      })
    }) %>%
    
    list.keep_dataframes() %>%
    bind_rows() %>%
    Dataframe.round_numbers() %>%
    Dataframe.order("Variable") %>%
    dplyr::mutate( 
      diff=
        dplyr::case_when(
          p_val<.01~sprintf("%s***", diff),
          p_val<.05~sprintf("%s**", diff),
          p_val<.1~sprintf("%s*", diff),
          TRUE~sprintf("%s", diff)
        )
    ) %>%
    Dataframe.insert(c("Subsample_size",
                       table(test_df$participante)[1],
                       table(test_df$participante)[2],
                       NaN,
                       NaN))
  
}
#
df=unbalanced_sample.covariates_table_comparison()
#
df %>% Dataframe.export_output(
  label="balance_de_muestra",
  output_home = ASSESTMENT_HOME
)
  