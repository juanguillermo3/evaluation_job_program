
#
#
OFERENTES_HOME=
  "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
#
setwd(OFERENTES_HOME)
#
self_as_name=function(x){
  names(x)=x;x
}

#
#
#
setwd(LOCAL_FILE_SYS_HOME)
library(dplyr)
list.files(
  pattern="entradas"
) %>% self_as_name() %>%
  
  lapply(., function(file_entradas_oferentes){
    dim(
      readxl::read_excel(file_entradas_oferentes))
  })




raw_data=readxl::read_excel(list.files(
  pattern="entradas"
)[1]) %>% View()




#
#
setwd(OFERENTES_HOME)
library(dplyr)
#
oferentes_inscritos=
list.files(
  pattern="entradas"
) %>% self_as_name() %>%
  
  lapply(., function(file_entradas_oferentes){
    
      readxl::read_excel(file_entradas_oferentes) %>% 
      dplyr::transmute(
        nombre_completo=paste(.[["Nombres"]], .[["Apellidos"]], sep=" "),
        tipo_doc=.[["Tipo Documento"]],
        documento=.[["Número Documento"]],
        tipo_registro=.[["Canal de Registro"]],
        edad=.[["Edad"]],                           
        genero=.[["Género"]],
        estudios=.[["Nivel de Estudio"]],
        titulo=.[["Título Homologado"]],
        ciudad=.[["Ciudad de Residencia"]],
        mes_año_registro=.[["Fecha Registro"]], 
        government_program=.[["Programa de Gobierno"]],
        condiciones=.[["Condiciones Especiales"]],         
        discapacidades=.[["Detalle Discapacidades"]],
        laboral_states=.[["Situación Laboral"]],              
        Ámbito=.[["Pertenece A"]]) %>%
      arrange(tipo_doc)
    
  }) %>% bind_rows()
      

#
#
sum(duplicated((oferentes_inscritos$documento)))
oferentes_inscritos=dplyr::filter(oferentes_inscritos, !duplicated(oferentes_inscritos$documento))

#
names(oferentes_inscritos)
#
dim(Oferentes_inscritos)

#
#
Oferentes_inscritos.persistir_en_base_de_datos=function(
  Oferentes_inscritos=oferentes_inscritos
){
  setwd(LOCAL_FILE_SYS_HOME)
  #
  openxlsx::write.xlsx(Oferentes_inscritos, file="oferentes_inscritos.xlsx")
}
#
Oferentes_inscritos.persistir_en_base_de_datos()



#
#
OFERENTES_HOME= "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
#
library(dplyr)
db.oferentes_inscritos=function(
  file="oferentes_inscritos.xlsx"
){
  setwd(OFERENTES_HOME)
  readxl::read_excel(file)
}
#
db.oferentes_inscritos() %>% View()


RUTA_HOME = "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/ruta de empleabilidad" 
db.from_files_data_reader_virtual_profiles=function(
  sample_prefix="scraper delivery",
  encoding="UTF-8"
){
  #
  #
  library(dplyr)
  setwd(RUTA_HOME)
  list.files(pattern=sample_prefix) %>%
    
    lapply(., function(some_csv){
      
      read.csv(some_csv) %>% 
        lapply(., function(data_col){
          as.character(data_col)
        }) %>%
        as.data.frame()
      
    }) %>% bind_rows()
}
#
db.from_files_data_reader_virtual_profiles() %>% View()





# (0) presentacion general de la muestra

dim(Oferentes_inscritos)

#
#
Oferentes_inscritos.descripcion_general=function(
  Oferentes_inscritos=db.oferentes_inscritos()
){
  
  #
  sum(!duplicated( Oferentes_inscritos$documento))
  #
  names( Oferentes_inscritos)
}
#
Oferentes_inscritos.descripcion_general()

# (1) demografia de la poblacion objetivo

#
#
Oferentes_inscritos.demografia=function(
  Oferentes_inscritos=db.oferentes_inscritos()
){
  
  #
  round(c(promedio=mean(Oferentes_inscritos$edad), 
          quantile(Oferentes_inscritos$edad)), 2)
  
  split(Oferentes_inscritos, Oferentes_inscritos$genero) %>% 
    lapply(., function(partial_gender_df){
      
      c(sexo=ifelse(partial_gender_df$genero[1]=="F","mujer", "hombre"),
        
        round(c(perdidos=sum(is.na(partial_gender_df$edad)),
                observaciones=sum(!is.na(partial_gender_df$edad)),
                "porcentaje sobre oferentes"= round((nrow(partial_gender_df)/nrow(Oferentes_inscritos))*100, 3),
                promedio=mean(partial_gender_df$edad), 
                
                quantile(partial_gender_df$edad)
        ),2)) 
      
    }) %>% bind_rows() %>% as.data.frame()
}
#
Oferentes_inscritos.demografia(

) 

SALIDAS_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/evaluacion del programa"
setwd(SALIDAS_HOME)

Oferentes_inscritos.demografia(
  
) %>% bind_rows() %>% as.data.frame() %>%
  
  



# (2) niveles de educación

#
#
tasa_bonita=function(x){
  t= round(prop.table(table(x))*100, 3)
  an=as.numeric(t)
  names(an)=names(t)
  return(an)
}
#
setwd(OFERENTES_HOME)
Oferentes_inscritos.educacion=function(
  Oferentes_inscritos=readxl::read_excel("oferentes_inscritos.xlsx")
){
  
  #
  #
  missing_statament=sprintf("excluyendo %s de registros con valores perdidos",
                            tasa_bonita(is.na(Oferentes_inscritos$estudios))[["TRUE"]])
  
  #oferentes_inscritos$estudios[is.na(oferentes_inscritos$estudios)]="valor perdido"
  #
  library(ggplot2)
  split(Oferentes_inscritos, Oferentes_inscritos$genero) %>% 
    lapply(., function(partial_gender_df){
      
      round(c(promedio=mean(partial_gender_df$edad), 
              quantile(partial_gender_df$edad),
              perdidos=sum(is.na(partial_gender_df$edad))
      ),2)
      
      c(gender=partial_gender_df$genero[1], prop.table(table(partial_gender_df$estudios))) 
      
      
    }) %>%
    
    bind_rows() %>% 
    
    tidyr::pivot_longer(cols=names(table(Oferentes_inscritos$estudios))) %>%
    
    mutate(
      tasa=round(as.numeric(value)*100, 3),
      nivel_educativo=factor(name,
                             levels=c(
                               "valor perdido",
                               "Preescolar", 
                               "Básica Primaria(1-5)",  
                               "Básica Secundaria(6-9)",
                               "Media(10-13)", 
                               "Técnica Laboral",   
                               "Técnica Profesional",  
                               "Tecnológica",
                               "Universitaria", 
                               "Especialización",
                               "Maestría", 
                               "Doctorado")),
      
      genero=factor(gender, levels=c("M","F"), labels=c("hombre", "mujer"))
    ) %>%
    
    ggplot(., aes(x=nivel_educativo, y=tasa, group=genero))+
    
    geom_col(aes(fill=genero),position=position_dodge(width=1.0), alpha=.3)+
    
    coord_flip()+
    
    labs(title="niveles educativos",
         subtitle = "% de muestra por nivel educativo y sexo",
         caption=missing_statament)+
    xlab("nivel educativo")+
    ylab("porcentaje")
}
#
Oferentes_inscritos.educacion()


#
Oferentes_inscritos.geografia=function(
  Oferentes_inscritos=readxl::read_excel("oferentes_inscritos.xlsx")
){
  
  #
  #
  missing_statament=sprintf("excluyendo %s de registros con valores perdidos. El %s pertenece a Bogotá",
                            tasa_bonita(is.na(Oferentes_inscritos$ciudad))[["TRUE"]],
                            tasa_bonita(Oferentes_inscritos$ciudad=="BOGOTÁ, D.C." )[["TRUE"]])
  #
  t=table(Oferentes_inscritos$Ámbito)
  # 
  names(t)
  #
  pie(t, 
      labels = names(t),
      main="")
  title(main = "distribucion geografica por ciudad de residencia", 
        sub =  missing_statament)
}
#
Oferentes_inscritos.geografia()


#
#
basic_standardization=function(
  string
){
  #
  tolower(string) %>%
    #
    iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
  #
}
#
#
Oferentes_inscritos.plot_some_circumpstances=function(
  Oferentes_inscritos=readxl::read_excel("oferentes_inscritos.xlsx"),
  plotted="government_program"
){
  
  #
  #
  missing_statament=sprintf("excluyendo %s de registros con valores perdidos",  
                            tasa_bonita(is.na(Oferentes_inscritos[[plotted]]))["TRUE"] ) 
                           
  
  t=
  Oferentes_inscritos %>%
    .[[plotted]] %>%
    na.omit() %>% 
    basic_standardization() %>%
    lapply(., function(l){
      stringr::str_split(l, ",")
    }) %>%
    unlist() %>%
    trimws(which="both") %>%
    table()
  
  #
  library(ggplot2)
  data.frame(
    value=names(t) %>% as.character(),
    count=t %>% as.numeric()
  ) %>% 
    dplyr::arrange(count*(1)) %>%
    mutate(
      value=factor(value,
                   levels=value)
    ) %>%
    dplyr::filter(
      count>1
    ) %>%
    ggplot(., aes(x=value, y=count))+
    geom_col(alpha=0.4)+
    geom_text(aes(label=count))+
  
    labs(title="programas de gobierno",
         subtitle = sprintf("numero de observaciones por cada programa"),
         caption=missing_statament)+
    xlab("programa de gobierno")+
    ylab("conteo")+
    coord_flip()
}
#
Oferentes_inscritos.plot_some_circumpstances(
  
)
#
Oferentes_inscritos.plot_some_circumpstances(
  plotted="condiciones"
)

table(colocados_df)


#
# shared kernel between the services
#

#
#
RUTA_HOME = "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/ruta de empleabilidad" 
#
db.Sample_from_Ruta_de_Empleabilidad=function(
  sample_file= "muestra de la ruta de empleabilidad 4.xlsx"
){
  #
  #
  library(dplyr)
  setwd(RUTA_HOME)
  readxl::read_excel( sample_file, col_types = "text") %>%
    
    transmute( doc_identidad=cedula, 
               # raw_info_ruta= .[["ruta empleabilidad"]] %>% as.character() %>%
               # stringr::str_replace_all(.,"^.'", "<FIRST RECORD> \\nTipo sesion: " ),
               raw_info_ruta= .[["ruta empleabilidad"]]  %>% as.character() %>%
                 stringr::str_replace_all(.,",", "<END OF SESSION RECORD>" ),
               algun_ofrecimiento=raw_info_ruta!="",
               procesos_seleccionado=seleccionado)  
}
#
db.Sample_from_Ruta_de_Empleabilidad() %>% View()



#
#
Ruta_de_empleabilidad_df.describe_colocations=function(
  Oferentes_inscritos_df=db.oferentes_inscritos(),
  Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad()
){
  #
  #
  library(dplyr)
  
  #
  in_sample_counts=
  Ruta_de_empleabilidad_df %>%
    mutate( al_menos_una_colocacion=ifelse(procesos_seleccionado!="No existen procesos", "sí", "no"))  %>%
  .[["al_menos_una_colocacion"]] %>% table() 
  
  #
  cute_proportions=function(numerical_type){
    #
    numerical_type=as.numeric(numerical_type)
    #
    an=round(numerical_type/sum(numerical_type, na.rm=TRUE)*100, 3)
    #
    return(an)
  }
  
  #
  W=nrow(Oferentes_inscritos_df)/nrow(Ruta_de_empleabilidad_df)
    
  #
  data.frame(
    "alguna colocacion"=names(in_sample_counts) %>% as.character(),
    "num personas"=round(in_sample_counts*W) %>% as.numeric(),
    "proporcion"=cute_proportions(in_sample_counts)
  )
}
#
Ruta_de_empleabilidad_df.describe_colocations()  %>% View()


#
#
#
Batch_of_Documentos.Personal_session_records=function(
  Batch_of_Documentos=
    db.Sample_from_Ruta_de_Empleabilidad() %>%
    .[["doc_identidad"]] %>% sample(10),
  
  Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad()
  
){
  
  in_line_names=function(x, N){names(x)=N; x}
  
  session_records=
    #
    # (0) extraer info de la ruta de empleabilidad sobre los registros seleccionados
    #
    dplyr::filter(Ruta_de_empleabilidad_df, doc_identidad %in% Batch_of_Documentos ) %>%
    .[["raw_info_ruta"]] %>%
    #
    # (1) convertir en lista los registros por cada cedula
    #
    lapply(., function(personal_records){
      if(is.na(.)=="") {return(NA)}
      stringr::str_split(personal_records,"<END OF SESSION RECORD>")
    })
  
  #
  # (2) convertir en lista los registros por cada cedula
  #
  lapply(1:length(Batch_of_Documentos), function(row_index){
    
    list(documento= Batch_of_Documentos[[row_index]],
         sesiones=paste("\\nTipo sesion:", unlist(session_records[[row_index]])))
    
  })
}
#
Batch_of_Documentos.Personal_session_records() %>% length()
#
Batch_of_Documentos.Personal_session_records(
  Batch_of_Documentos=
    db.Sample_from_Ruta_de_Empleabilidad() %>%
    .[["doc_identidad"]] 
) %>% length()



#
# 
# estandarizar la representacion de las sesiones de la ruta de empleabildiad
#
Session_records.Standardize=function(
  session_records=Batch_of_Documentos.Personal_session_records() 
){
  
  #  
  an=
    lapply(session_records, function(personal_records){
      
      if ( all(is.na(personal_records$sesiones))) {return(NA)}
      
      an=
        personal_records$sesiones %>%
        lapply(., function(ind_session_record){
          ind_session_record
          
          #
          fields=stringr::str_extract_all(ind_session_record,"n[A-Z][^:]*")
          fields=unlist(fields)
          
          #
          values=stringr::str_split(ind_session_record,paste( "(", fields, ")", sep="", collapse="|") ) %>% unlist()
          values=values[2:length(values)]
          
          c(fields, values)
          
          try(
            {
              an=data.frame(values) %>% t() %>% as.data.frame()
              names(an)=fields
              an$documento=personal_records$documento
              an 
            })
          
        }) %>% bind_rows()
      
      an
    })
  
  an=an[sapply(an, function(df){ncol(df)>2})] %>% bind_rows() 
  
  #
  
  #
  names(an)=names(an) %>% 
    text2vec::word_tokenizer() %>% 
    sapply(., function(n){paste(n, collapse="_")}) %>%
    tolower()
  
  an=
    an %>%
    lapply(., function(data_col){
      
      text2vec::word_tokenizer(data_col, pos_keep="/") %>%
        
        sapply(., function(sublist){
          paste(sublist, collapse =" ")
        })
      
    }) %>% as.data.frame() 
  
  names(an)=stringr::str_replace(names(an), pattern="^n", "")
  an
  
}
#
Session_records.Standardize()  %>% View()



#
#
Session_df=
Session_records.Standardize(
  
  session_records =
    
    Batch_of_Documentos.Personal_session_records(
      Batch_of_Documentos = db.Sample_from_Ruta_de_Empleabilidad() %>% .[["doc_identidad"]] 
    ) ) %>% 
  dplyr::filter(prestador_punto_de_atención=="CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" ) 
#
W=nrow(Oferentes_inscritos_df)/nrow(Ruta_de_empleabilidad_df)
W*nrow(Session_df)


#
sort(table(Session_df$prestador_punto_de_atención), decreasing = TRUE)[1:5]*5
#
table(Session_df$prestador_punto_de_atención)



#
#
Oferentes_inscritos.describir_ruta_empleabilidad=function(
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
  actividades_compuestas_del_minuto_de_dios[["al menos una actividad"]]=
    unlist(actividades_compuestas_del_minuto_de_dios)
    
  
    
  lapply(actividades_compuestas_del_minuto_de_dios, function(some_compound_activity){
    
    #
    #
    Sesiones_ruta_empleabilidad %>% 
      
      dplyr::filter(prestador_punto_de_atención=="CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" ) %>% 
      
      dplyr::filter(tipo_sesion %in% some_compound_activity ) %>% 
      
      dplyr::filter(!duplicated(documento) ) %>%
      
      dplyr::filter(estado %in% c("Aprobó","Asistió","En Curso","Finalizado"))  %>%
    
    {
      data.frame("Tipo_de_actividad"="",
                 
                 "Cantidad_de_personas_afectadas"= as.integer(length(.[["documento"]])*W),
                 
                 "Proporcion_de_personas_afectadas"= 
                   round( (length(.[["documento"]])*W/nrow( Oferentes_inscritos_df))*100, 3))
      }
    
  }) %>% bind_rows() %>% 
    
    mutate(Tipo_de_actividad=names(actividades_compuestas_del_minuto_de_dios))  %>%
    
    dplyr::arrange(Cantidad_de_personas_afectadas*(-1)) 

}
#
Oferentes_inscritos.describir_ruta_empleabilidad()




















#  
#
Sesiones_ruta_empleabilidad=Session_df
Sesiones_ruta_empleabilidad$nombre[Sesiones_ruta_empleabilidad$nombre=="NA"]=""
Sesiones_ruta_empleabilidad$nombre.1[Sesiones_ruta_empleabilidad$nombre.1=="NA"]=""

#
#
AN=
Sesiones_ruta_empleabilidad %>% 
  mutate(
    nombre_completo=paste(nombre, nombre.1, sep="")
  ) %>% 
  
  dplyr::filter(prestador_punto_de_atención=="CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" ) %>% 
  
  split( .[["tipo_sesion" ]]) %>%  
  
  
  lapply(., function(partial_df_per_type){
    nrow(partial_df_per_type)
    
    v=
      partial_df_per_type %>%
      dplyr::filter(estado %in% c("Aprobó","Asistió","En Curso","Finalizado")) %>%
      dplyr::filter(!duplicated(documento) ) %>% .[["nombre_completo"]]
    
    if (length(v)<30){return("sample to small")}
    
    head(v)
    an=round(sort( prop.table( table(v)), decreasing=TRUE)*100,2)[1:5]
    an[!is.na(an)]  t()
  })

  AN[AN!= "sample to small"] 
  
  
  
  
  

  


  
  #
  #
  Sessions_df.prestador_es_minuto_de_dios.focalizacion_de_tratamiento_balance=function(
    
    Session_df=Session_records.Standardize(
      session_records = Batch_of_Documentos.Personal_session_records(
        Batch_of_Documentos=
          db.Sample_from_Ruta_de_Empleabilidad() %>%
          .[["doc_identidad"]] )),
    
    Ruta_de_empleabilidad_df=db.Sample_from_Ruta_de_Empleabilidad(),
    
    Oferentes_inscritos_df=db.Oferentes_Inscritos() 
  ){
    #
    #
    balanced_sample_per_group=list()
    #
    balanced_sample_per_group[["cedulas_tratamiento"]]=
      Session_df %>% 
      dplyr::filter(prestador_punto_de_atención=="CORPORACIÓN EL MINUTO DE DIOS CORPORACION MINUTO DE DIOS BOGOTA D.C" 
                    & tipo_sesion== "Competencias Claves y Transversales" ) %>%
      .[["documento"]]
    #
    balanced_sample_per_group[["cedulas_control"]]=
      Ruta_de_empleabilidad_df %>%
      dplyr::filter(is.na(algun_ofrecimiento)) %>% 
      dplyr::sample_n(., length(balanced_sample_per_group$cedulas_tratamiento), replace=TRUE) %>%
      .[["doc_identidad"]] 
    
    #  
    intersect(balanced_sample_per_group$cedulas_tratamiento, balanced_sample_per_group$cedulas_control)
    lapply(balanced_sample_per_group , head)
    lapply(balanced_sample_per_group , length)
    
    #
    lapply(balanced_sample_per_group, function(cedulas_grupo){
      length(cedulas_grupo)
      
      partial_df=
        Ruta_de_empleabilidad_df %>%
        dplyr::filter(
          Ruta_de_empleabilidad_df$doc_identidad %in% cedulas_grupo
        ) 
      
      data.frame(
        "porcentaje de colocaciones"= round(mean(partial_df$procesos_seleccionado!="No existen procesos")*100, 3)
        
      )
      
    }) %>% bind_rows()
  }
  #
  Sessions_df.prestador_es_minuto_de_dios.focalizacion_de_tratamiento_balance()
    

  #
  Sessions_df.prestador_es_minuto_de_dios.focalizacion_de_tratamiento_balance()
  
