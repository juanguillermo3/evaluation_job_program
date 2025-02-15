#
# routes
# --------
#

CODE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/codigo de aplicacion"
setwd(CODE_HOME)
source("routes.R")

#
# demographic model for cmd sample
# --------
#

#
library(dplyr)
db.muestra_cmd=function(){
  setwd(MUESTRA_HOME)
  openxlsx::read.xlsx("muestra cmd.xlsx")
}
#
library(dplyr)
db.pop_cmd=function(){
    setwd(OFERENTES_HOME)
    readxl::read_excel("oferentes_inscritos.xlsx") %>% 
      dplyr::transmute(
        doc_num=documento,
        fecha_registro=mes_a�o_registro,
        tipo_doc=tipo_doc,
        genero=ifelse(genero=="F", "mujer", "hombre"),
        edad=edad,
        estudios=estudios,
        laboral_states=laboral_states
      )
}
#
#db.muestra_cmd() %>% View()
#
muestra_cmd.modelo_demografico=function(
  muestra_cmd=db.pop_cmd(),
  retained=c(
    "doc_num",
    "genero",
    "edad",
    "estudios",
    "tipo_doc",
    "laboral_states",
    "fecha_registro")
  
){
  muestra_cmd %>%
    #
    # (1)
    #
    dplyr::select(retained) %>%
    #
    # (2)
    #
    dplyr::mutate(
      sexo=genero
    ) %>%
    dplyr::mutate(
      estudios=
        dplyr::case_when(
          estudios %in% c("educ desconocida", 
                          "Preescolar", 
                          "B�sica Primaria(1-5)",
                          "B�sica Secundaria(6-9)")~"desconocida, hasta basica secundaria",
          estudios %in% c("Media(10-13)")~"secundaria completa",
          estudios %in% c("T�cnica Laboral", "T�cnica Profesional", "Tecnol�gica")~"tecnica",
          estudios %in% c("Universitaria", "Especializaci�n", "Maestr�a", "Doctorado")~"univ o posterior",
          is.na(estudios) ~"desconocida, hasta basica secundaria",
          TRUE ~"this case shouldnt exist",
        )) %>%
    dplyr::mutate(
      es_migrante=ifelse(tipo_doc %in% c( "C�dula de Extranjeria",
                                          "Documento Nacional de Identificaci�n",
                                          "Pasaporte",
                                          "Permiso Especial de Permanencia"
      ), 1, 0))%>%
    dplyr::mutate(
      laboral_states=
        dplyr::case_when(
          laboral_states %in% c("Cesante por Emergencia Sanitaria", "Desempleado")~"desempleado",
          laboral_states %in% c("Empleado", "Independiente", "Primer Empleo"  )~"empleado",
          laboral_states %in% c("NO REGISTRA")|is.na(laboral_states)~"empleo desconocido",
          TRUE ~"this case shouldnt exist"
        )) %>%
    dplyr::mutate(
      fecha_registro=
        as.Date(stringr::str_extract( fecha_registro, "[^ ]+"), format="%d/%m/%y")
    )  %>%
    Dataframe.complain_for_vars(c("doc_num",
                                  "genero",
                                  "edad",
                                  "estudios",
                                  "tipo_doc",
                                  "laboral_states",
                                  "es_migrante")) %>%
    Dataframe.mandatory_model(c("doc_num",
                                "genero",
                                "edad",
                                "estudios",
                                "tipo_doc",
                                "laboral_states",
                                "es_migrante"))
}
#
#muestra_cmd.modelo_demografico() %>% View()
