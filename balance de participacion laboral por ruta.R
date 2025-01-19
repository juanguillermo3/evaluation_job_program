#
#
Sessions_df.prestador_es_minuto_de_dios.tasa_colocacion_por_grupo=function(
  Oferentes_inscritos_df=db.oferentes_inscritos(),
  Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad()
){
  
  
  #
  #
  Sesiones_ruta_empleabilidad=
    Session_records.Standardize(
      session_records =
        Batch_of_Documentos.Personal_session_records(
          Batch_of_Documentos =  Ruta_de_empleabilidad_df %>% .[["doc_identidad"]] 
        ) ) 
  
  #
  cedulas_en_grupo_de_control= 
    Ruta_de_empleabilidad_df %>% 
    dplyr::filter(is.na(algun_ofrecimiento)) %>% 
    .[["doc_identidad"]] 
  #
  #
  cedulas_alguna_actividad= 
    Sesiones_ruta_empleabilidad$documento %>% unique()
  #
  fake_sessions=function(cedulas, tipo_falso){
    
    data.frame(
      documento=cedulas,
      tipo_sesion=tipo_falso,
      prestador_punto_de_atención="CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C",
      estado="Asistió" )
    
  }
  #
  #fake_sessions(cedulas=cedulas_en_grupo_de_control, tipo_falso="grupo de control" ) %>% View()
  #fake_sessions(cedulas=cedulas_alguna_actividad, tipo_falso="alguna actividad" ) %>% View()
  
  #
  Sesiones_ruta_empleabilidad=
    dplyr::bind_rows(
      Sesiones_ruta_empleabilidad,
      fake_sessions(cedulas=cedulas_en_grupo_de_control, tipo_falso="a.grupo de control" ),
      fake_sessions(cedulas=cedulas_alguna_actividad, tipo_falso="b.alguna actividad" )
    ) 
  
  #
  W= nrow( Oferentes_inscritos_df)/nrow( Ruta_de_empleabilidad_df)
  
  
  #
  #
  actividades_compuestas_del_minuto_de_dios=
    
    Sesiones_ruta_empleabilidad %>% 
    
    dplyr::filter(prestador_punto_de_atención=="CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" ) %>%
    
    {names(table(.[["tipo_sesion"]]))} %>% as.list()
  #
  names(actividades_compuestas_del_minuto_de_dios)=actividades_compuestas_del_minuto_de_dios
  
  
  
  #
  # loop over compound activities to build a df
  #
  lapply(actividades_compuestas_del_minuto_de_dios, function(some_compound_activity){
    
    #
    #
    Sesiones_ruta_empleabilidad %>% 
      
      dplyr::filter(prestador_punto_de_atención=="CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" ) %>% 
      
      dplyr::filter(tipo_sesion %in% some_compound_activity ) %>%
      
      dplyr::filter(!duplicated(documento) ) %>% 
      
      dplyr::filter(estado %in% c("Aprobó","Asistió","En Curso","Finalizado"))  %>%
      
      {
        #
        #
        cedulas_de_participantes=.[["documento"]]
        
        
        #
        #
        data_de_participantes=
          Oferentes_inscritos_df %>%
          dplyr::filter(
            documento %in% cedulas_de_participantes
          ) 
        
        #
        #
        data.frame(
          Tipo_de_actividad="",
          
          Cantidad_de_personas_afectadas=as.integer(length(data_de_participantes$documento)*W),
          
          "Proporcion_de_personas_afectadas"= 
            round( (length(.[["documento"]])*W/nrow( Oferentes_inscritos_df))*100, 3),
          
          promedio_de_edad= round(mean(data_de_participantes$edad), 3),
          
          numero_de_mujeres= as.integer( sum(data_de_participantes$genero=="F")*W),
          
          proporcion_de_mujeres= round(mean(data_de_participantes$genero=="F")*100, 3),
          
          tamaño_de_muestra=length(data_de_participantes$documento),
          
          numero_de_colocaciones={
            
            df=
            Ruta_de_empleabilidad_df %>%
              dplyr::filter( Ruta_de_empleabilidad_df$doc_identidad %in% data_de_participantes$documento  ) %>%
              dplyr::filter( procesos_seleccionado!="No existen procesos"  )
            
              as.integer(nrow(df)*W)
            
          },
          
          tasa_de_colocacion={
            
            df=
            Ruta_de_empleabilidad_df %>%
              dplyr::filter( Ruta_de_empleabilidad_df$doc_identidad %in% data_de_participantes$documento  )
              
              round(mean( df$procesos_seleccionado!="No existen procesos"  )*100, 3)
            
          },
          
          num_colocaciones_mujeres={
            
            df=
              Ruta_de_empleabilidad_df %>%
              dplyr::filter( Ruta_de_empleabilidad_df$doc_identidad %in%
                               data_de_participantes$documento[data_de_participantes$genero=="F"]  ) %>%
              dplyr::filter( procesos_seleccionado!="No existen procesos"  )

            as.integer(nrow(df)*W)
            
            
          },
          
          tasa_colocacion_mujeres={
          
          df=
            Ruta_de_empleabilidad_df %>%
            dplyr::filter( Ruta_de_empleabilidad_df$doc_identidad %in% 
                             data_de_participantes$documento[data_de_participantes$genero=="F"]  )
          
          round(mean( df$procesos_seleccionado!="No existen procesos"  )*100, 3)
            
          },
          
          tasa_colocacion_hombres={
            
            df=
              Ruta_de_empleabilidad_df %>%
              dplyr::filter( Ruta_de_empleabilidad_df$doc_identidad %in% 
                               data_de_participantes$documento[data_de_participantes$genero=="M"]  )
            
            round(mean( df$procesos_seleccionado!="No existen procesos"  )*100, 3)
            
          }
          
          
        )
        
        
      } ## context of dataframe
    
  }) %>% ## loop over compound activities 
    bind_rows() %>% 
    
    mutate(Tipo_de_actividad=names(actividades_compuestas_del_minuto_de_dios))  %>%
    
    dplyr::arrange(proporcion_de_mujeres*(1)) %>%
    
    dplyr::filter(tamaño_de_muestra>30) %>% dplyr::select(-c(tamaño_de_muestra))
  
}
#
Sessions_df.prestador_es_minuto_de_dios.tasa_colocacion_por_grupo() %>% View()






S