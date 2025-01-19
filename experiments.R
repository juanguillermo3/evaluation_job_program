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

#
setwd(CODE_HOME)
source("genderdize records.R")

#
setwd(CODE_HOME)
source("data candidates.R" , encoding="UTF-8")
db.candidates() %>% View()

#
# app modules
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")
#

setwd(CODE_HOME)
source("persistence utils.R")

#
#

#
library(dplyr)
db.cargar_asistencia=function(){
  setwd(ASSISTANCE_HOME)
  readxl::read_excel("assistance_dataset.xlsx")
}
db.cargar_asistencia() %>% View()


#
#
data_de_asistencia.minar_experimentos=function(
  data_de_asistencia=db.cargar_asistencia(),
  data_de_candidaturas=db.candidates(),
  trhesholds=1:4
){
  
  new_df=
  data_de_asistencia %>%
    dplyr::mutate(
      alguna_candidatura=as.numeric(doc_num %in% data_de_candidaturas$doc_num)
    ) %>% 
    dplyr::mutate(
      grupo_control=ifelse(.$activ_totales==0 &  alguna_candidatura==1, 1, 0)
    )  
  for ( trheshold in trhesholds){
    new_df[
      sprintf("al_menos_%s", trheshold)
    ]=dplyr::case_when(
      new_df$activ_totales>(trheshold-1) &  new_df$alguna_candidatura==1 ~1,
      new_df$grupo_control==1~0,
      TRUE~NaN
    )
  }
  return(new_df)
}
data_de_asistencia.minar_experimentos() %>% View()
#
df=data_de_asistencia.minar_experimentos() 
#
df %>% 
  Dataframe.share_with_app(
    app_route = EXPERIMENTS_HOME,
    label="casos_experimentales"
  )


#
#
Experimentos.genderdize.summary=function(
  Experimentos=data_de_asistencia.minar_experimentos()
){
  Experimentos %>%
    people_with_ids_df.genderdize() %>%
    split(.$gender) %>%
    lapply(function(sub_df){
      list(
        
        data.frame(
        Sexo=dplyr::first(sub_df$gender),
        Subsample_size=sub_df %>% nrow(),
        Controls=sum(sub_df$grupo_control)),
      
        sub_df %>% 
          dplyr::select( grep("al_menos",names(.), value=TRUE)) %>%
          lapply(function(col){
            sum(col, na.rm=TRUE)
          }) %>%
          as.data.frame()
      ) %>%
        bind_cols()
  
    }) %>%
    bind_rows() %>%
    Dataframe.totalize()
}
Experimentos.genderdize.summary()%>% View()
#
df=Experimentos.genderdize.summary()
df %>% 
  Dataframe.export_output(
    label="casos_experimentales_por_genero",
    output_home = EXPERIMENTS_HOME
  )

