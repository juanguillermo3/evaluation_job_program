


library(dplyr)
 %>% View()


Session_records.Standardize(
  Batch_of_Documentos.Personal_session_records(
    Batch_of_Documentos= db.Sample_from_Ruta_de_Empleabilidad() %>%.[["doc_identidad"]]  )) %>% View()


#
# sociable entity that builds from ruta de empleabilidad:
# sociable entity as the rounding program for a module definition
#
Sample_from_Ruta_de_Empleabilidad.model_treatment=function(
  
  Sample_from_Ruta_de_Empleabilidad=db.Sample_from_Ruta_de_Empleabilidad(),
  
  valid_treatment=c("Competencias Claves y Transversales"),
  
  valid_states=c("Aprobó", "Asistió"),
  
  Qualia_of_treatment=c("MINUTO", "COLSUBSIDIO", "ALCALDÍA")
){
  
  #
  # (0) create & standardize session records from sample data of the ruta
  #
  Curated_and_Standized_session_records=
  Session_records.Standardize(
    Batch_of_Documentos.Personal_session_records(
      Batch_of_Documentos= Sample_from_Ruta_de_Empleabilidad %>%.[["doc_identidad"]]  )) %>% 
    Session_records_df.curate()  %>% 
    dplyr::filter(estado %in% valid_states) %>%
    dplyr::filter( tipo_sesion  %in% valid_treatment )
  #
  # (1) model the treatments
  #
  treatment_df=
  Curated_and_Standized_session_records %>% 
    split(Curated_and_Standized_session_records$documento) %>% 
    lapply(., function(df_per_cedula){
      
      
      c(
      
        num_documento=df_per_cedula$documento[1],
      
      sapply(Qualia_of_treatment, function(some_qualia){
        
        sum(grepl(df_per_cedula$prestador_atencion, pattern=some_qualia)) 
        
      }))
      
    }) %>% bind_rows() 
    
  #
  # (2) model the control
  #
  treatment_df2=
  setdiff( Sample_from_Ruta_de_Empleabilidad %>% .[["doc_identidad"]],
           treatment_df$num_documento) %>% 
    lapply(., function(
      some_document
    ){
      c(num_documento =some_document, sapply(Qualia_of_treatment, function(some_qualia){0}))
    } ) %>% bind_rows() 
  #
  # (3)
  #
  treatment_model= list(treatment_df, treatment_df2) %>% bind_rows()
  an=treatment_model
  #
  return(an)
    
}
#
Sample_from_Ruta_de_Empleabilidad.model_treatment() %>% View()



      
      
      
   