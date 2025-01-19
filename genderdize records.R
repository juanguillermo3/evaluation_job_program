#
# genderdize records
# --------
#

#
OFERENTES_HOME= "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
#
library(dplyr)
Oferentes_inscritos.db.oferentes_inscritos=function(
){
  setwd(OFERENTES_HOME)
  readxl::read_excel("oferentes_inscritos.xlsx") %>%
    dplyr::mutate(doc_num=documento) %>%
    dplyr::select(doc_num, genero) %>%
    dplyr::transmute(
      doc_num=doc_num,
      gender=ifelse(genero=="F", "mujer", "hombre")
    )
}
#
#Oferentes_inscritos.db.oferentes_inscritos() %>% View()
#
people_with_ids_df.genderdize=function(
  people_df,
  genders_df= Oferentes_inscritos.db.oferentes_inscritos() 
){
  dplyr::left_join(
    people_df,
    genders_df,
    by="doc_num"
  )
}
#
#people_with_ids_df.genderdize() %>% View()