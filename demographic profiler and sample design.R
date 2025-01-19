
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


#
#
RUTA_HOME = "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/raw participation data" 
#
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
      read.csv(some_csv,) %>% 
        lapply(., function(data_col){
          as.character(data_col)
        }) %>%
        as.data.frame()
    }) %>% bind_rows()  %>% {
      write.table(paste(.$cv, "<END OF RECORD>"), "tempfile.txt")
      cbind(
        dplyr::select(.,-cv) ,cv=read.table("tempfile.txt", encoding="UTF-8")[,1]
      )
    }
}
#
db.from_files_data_reader_virtual_profiles_cache=db.from_files_data_reader_virtual_profiles() 
db.from_files_data_reader_virtual_profiles_cache %>% View()
head(db.from_files_data_reader_virtual_profiles_cache$cv)


#
#
Virtual_profiles_df.model_virtual_cv=function(
  Virtual_profiles_df=db.from_files_data_reader_virtual_profiles_cache
){
  Virtual_profiles_df %>% 
    transmute(
      num_documento=as.character(cedula),
      hoja_de_vida=cv
    ) %>%
    dplyr::filter(!duplicated(.$num_documento))
}
#
Virtual_profiles_df.model_virtual_cv() %>% View()



#
#
library(dplyr)
Virtual_profiles.Virtual_profiles_df.Ensemble_features=function(
  Records=Virtual_profiles_df.model_virtual_cv(),
  Information_channel="hoja_de_vida",
  feature_markers=c("Edad", 
                    "Estado civil",
                    "Nivel educativo",
                    "Estado",
                    "Años de experiencia",
                    "Aspiración salarial")
){
  #
  #
  list(
    num_documento=as.character(Records$num_documento),
    
    Records[[Information_channel]] %>%
      lapply(., function(some_record){
        sapply(feature_markers, function(some_marker){
          an=stringr::str_extract(some_record, sprintf("%s: [^[:punct:]]*", some_marker) )
          an=stringr::str_replace(an, sprintf("%s: ", some_marker), "" )
        })
      }) %>% bind_rows() ) %>% bind_cols() 
}
#
Virtual_profiles.Virtual_profiles_df.Ensemble_features() %>% View()




#
#
basic_standardization=function(
  names
){
  #
  tolower(names) %>%
    #
    iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
  #
}
#
stata_valid_names=function(names){
  text2vec::word_tokenizer(names) %>% 
    sapply(., function(some_name_units){
      s=paste(some_name_units, collapse="_")
      s=stringr::str_replace_all(s, "[.]", "_")
      substr(s, 0, 25) 
    }) %>% basic_standardization()
}
#
stata_valid_names(c("Variable_invalida1",
                    "Variable//.,invalida2",
                    "Variableinvalida3_con_nombre_super_largo",
                    "Variable_con_acentuaciÃ³n",
                    "nomnbre.invalido"
))
#
Dataframe.apply_valid_names_for_stata=function(
  Dataframe
){
  names(Dataframe)=stata_valid_names(names(Dataframe))
  Dataframe
}

#
#
Digital_skills_df=
Virtual_profiles.Virtual_profiles_df.Ensemble_features(
  Records=Virtual_profiles_df.model_virtual_cv(),
  Information_channel = "hoja_de_vida",
  feature_markers = c(
    "Edad", 
    "Estado civil",
    "Nivel educativo",
    "Estado",
    "Años de experiencia",
    "Aspiración salarial",
    "Procesador de texto [(]Ejemplo[:] Word[)](.){2}Herramienta",
    "Hoja de cálculo [(]Ejemplo[:] Excel[)](.){2}Herramienta",
    "Otros(.){2}Herramienta",
    "Interesado en ofertas de teletrabajo"
    )
)  %>% Dataframe.apply_valid_names_for_stata() %>% 
  mutate(
    digital_skills=paste(procesador_de_texto_ejemp, hoja_de_calculo_ejemplo_e, otros_2_herramienta, sep=", "),
    some_digital_skill=ifelse(digital_skills!="NA, NA, NA", 1, 0),
    specialized_software=ifelse( !is.na( otros_2_herramienta), 1, 0),
  ) 
#
table(Digital_skills_df$some_digital_skill)
table(Digital_skills_df$specialized_software)
Digital_skills_df.cache=Digital_skills_df




#
#
Featured_Virtual_profiles_df.demographic_model=function(
  
  Featured_Virtual_profiles_df= Virtual_profiles.Virtual_profiles_df.Ensemble_features(),
  
  Oferentes_df=db.oferentes_inscritos(),
  
  Education_model=list(
    "sin bachillerato"=c("Preescolar", "Básica Primaria" , "Básica Secundaria" ),
    "bachillerato o técnico"= c("Media", "Técnica Laboral", "Técnica Profesional", "Tecnológica" ),
    "universitario o superior"= c("Universitaria", "Especialización", "Maestría","Doctorado"  )),
  
  Marital_model=list(
    "en pareja"=c("Unión Libre", "Casado"),
    "sin pareja"=c("Soltero")),
  
  Age_breaks=c(.05,.25, .5, .75, 1)
  
){
  Virtual_profiles_df=Featured_Virtual_profiles_df 
  Virtual_profiles_df2= Featured_Virtual_profiles_df %>%
    dplyr::mutate(
      num_documento=num_documento,
      age=as.numeric(stringr::str_extract_all(Edad, "[0-9]{2}"))
    ) %>% 
    dplyr::filter(
      age <= 57
    ) 
  
  m=Virtual_profiles_df2$"Estado civil"
  for (value in names(Marital_model)){
    
    m=plyr::mapvalues(m, 
                      Marital_model[[value]],
                      rep(value, length(Marital_model[[value]])))
  }
  m[ !m %in% names(Marital_model) ]="NA"
  
  e=Virtual_profiles_df2$"Nivel educativo"
  for (value in names(Education_model)){
    
    e=plyr::mapvalues(e, 
                      Education_model[[value]],
                      rep(value, length(Education_model[[value]])))
  }
  e[ !e %in% names(Education_model) ]="NA"
  
  
  dplyr::left_join(
    Virtual_profiles_df %>%
      dplyr::transmute(
        num_documento=num_documento,
        age=as.numeric(stringr::str_extract_all(Edad, "[0-9]{2}"))
      ) %>% 
      dplyr::filter(
        age <= 57
      ) %>%
      
      dplyr::mutate(
        marital_model=m,
        educ_model=e
      )  %>% 
      mutate(age_group=cut(age,
                           breaks = quantile(.$age, Age_breaks, na.rm=TRUE),
                           include.lowest = TRUE,
                           ordered_result = TRUE,
                           right=FALSE)),
    
    #
    Oferentes_df %>% 
      dplyr::transmute(
        num_documento=documento,
        sexo=genero,
        sexo=ifelse(sexo=="F", "mujer", "hombre"))) 
  
}
#
Featured_Virtual_profiles_df.demographic_model.cache=Featured_Virtual_profiles_df.demographic_model() 
#
Featured_Virtual_profiles_df.demographic_model.cache %>% View()



#
#
Demographic_model.per_tech_factor=function(
  Demographic_model=Featured_Virtual_profiles_df.demographic_model.cache,
  Digital_skills_model=Digital_skills_df.cache,
  some_digital_skill="some_digital_skill",
  specialized_software="specialized_software",
  factor="educ_model",
 Oferentes_df=db.oferentes_inscritos()
){
  
  #
  W=nrow(Oferentes_df)/nrow(Digital_skills_model)
  #
  ar=
  Digital_skills_model %>% 
    dplyr::left_join(Demographic_model) %>% 
    dplyr::filter(
      !is.na(sexo) &
        !is.na(educ_model) & 
        educ_model!="NA" &
        !is.na(marital_model) &
        marital_model !="NA") %>%
    dplyr::filter(age<57) %>% 
    mutate(
      subsample_factor=.[[factor]],
      subsample=paste(.[["sexo"]], .[[factor]], sep="X") 
    ) %>% 
    split(.$subsample) %>% 
    lapply(., function(partial_df){
      data.frame(
        subsample_factor=partial_df$subsample_factor[1],
        sex=partial_df$sexo[1],
        subsample=partial_df$subsample[1],
        num_people=as.integer( nrow(partial_df)*W),
        some_digital_skill=as.integer( sum(partial_df$some_digital_skill)*W),
        specialized_software=as.integer( sum(partial_df$specialized_software)*W)
      )
    }) %>% bind_rows() %>% 
    split(.$sex) %>% lapply(., function(sex_subample){
      sex_subample %>%
        mutate(
          prop_some_digital_skill= round((some_digital_skill/num_people)*100,3),
          prop_specialized_software= round((specialized_software/num_people)*100,3) 
          ) %>%
        dplyr::arrange(prop_some_digital_skill) %>% dplyr::select( -c("subsample"))
    }) 
}
#
Demographic_model.per_tech_factor()
Demographic_model.per_tech_factor(factor="marital_model")
Demographic_model.per_tech_factor(factor="age_group")
Demographic_model.per_tech_factor(factor="aspiracion_salarial")


getwd()
setwd("C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/")
ar %>%  
  dplyr::select(
    c("num_documento", 
      "some_digital_skill",
      "specialized_software",
      "age",
      "marital_model", 
      "educ_model",
      "age_group", 
      "sexo") 
  ) %>% 
  dplyr::mutate(
    telefono=sapply(1:nrow(.), function(r){
      paste("3", paste(sample(0:9, 9, replace=TRUE), collapse=""), sep="")
    })
  ) %>% 
  openxlsx::write.xlsx("sujetos experimentales ejemplo.xlsx")
  


#
#
Demographic_model.per_tech_factor.plot=function(
  Demographic_model=Featured_Virtual_profiles_df.demographic_model.cache,
  Digital_skills_model=Digital_skills_df.cache,
  some_digital_skill="some_digital_skill",
  specialized_software="specialized_software",
  factor="educ_model",
  Oferentes_df=db.oferentes_inscritos()
  ){
  
  W=nrow(Oferentes_df)/nrow(Digital_skills_model)
  library(ggplot2)
  #
  # (1) compute composition of the sample
  #
  Demographic_model.per_tech_factor(
    Demographic_model=Demographic_model, 
    Digital_skills_model=Digital_skills_model,
    some_digital_skill=some_digital_skill,
    specialized_software=specialized_software,
    factor=factor,
    Oferentes_df=Oferentes_df) %>%
    bind_rows() %>%
  #
  # (2) prune unreliable samples
  #
  dplyr::filter(num_people/W>30) %>%
  dplyr::arrange(
    some_digital_skill*(1)
  ) %>%
  dplyr::mutate(
    subsample_factor=factor(subsample_factor, 
                            levels=subsample_factor[sex=="mujer"])
  ) %>% {
    
    #
    # (3)
    #
    ggplot(., aes(x= subsample_factor))+
    geom_col(aes(y=num_people), alpha=.3)+
    geom_text(aes(y=num_people+50, 
                  label=num_people),
              size=4, alpha=.7)+
    geom_col(aes(y=some_digital_skill), alpha=.7)+
    geom_text(aes(y=some_digital_skill, 
                  label=some_digital_skill),
              size=4)+
    facet_grid(cols=vars(sex))+
    coord_flip()+
    labs(title="Perfilamiento por habilidades digitales",
         subtitle=sprintf("# de personas con competencias  digitales (gris oscuro) vs muestra total (gris claro) por niveles de %s",  factor ),
         caption=sprintf("Poblacion total %s, 
         Total mujeres %s
         Mujeres con competencias digitales %s", 
                         nrow(Oferentes_df),
                         sum(.$num_people[.$sex=="mujer"]),
                         sum(.$some_digital_skill[.$sex=="mujer"])
                         ))+
    xlab("grupo")+
    ylab("# personas")
  }
}
#
#
Demographic_model.per_tech_factor.plot()
#
Demographic_model.per_tech_factor.plot(
  factor="aspiracion_salarial"
)


#
#
model_df=dplyr::left_join(Demographic_model,Digital_skills_model )
#
Demographic_and_digital_skills_model=list(
    Sample_size=function(model_df, w=W){
      as.integer( ((nrow(model_df)*w)/nrow(Oferentes_df))*100 )
    },
    Women_proportion=function(model_df){
      round( mean(model_df$sexo=="mujer", na.rm=TRUE)*100, 3)
    }
)
#
Demographic_model.real_comparison=function(
  Demographic_model=Featured_Virtual_profiles_df.demographic_model.cache,
  Digital_skills_model=Digital_skills_df.cache,
  some_digital_skill="some_digital_skill",
  specialized_software="specialized_software",
  features=Demographic_and_digital_skills_model,
  Oferentes_df=db.oferentes_inscritos()
){
  
  W=nrow(Oferentes_df)/nrow(Digital_skills_model)
  library(ggplot2)
  
  sample_levels=list(
    "competencias digitales"=1,
    "muestra completa"=c(0,1)
  )
  #
  dplyr::left_join(Demographic_model,Digital_skills_model ) %>%
    {
    lapply( names(sample_levels), function(some_sample_lev){
      dplyr::filter(., some_digital_skill %in% sample_levels[[some_sample_lev]])  %>%
       dplyr::mutate(sample=some_sample_lev)
    })
    } %>%
    lapply(., function(some_subsample){
      
      sapply(features, function(some_feature){
         some_feature(some_subsample)
    }) %>% data.frame() %>%
        mutate(sample=some_subsample$sample[1],
               feature=names(features))
    })  
}
#
Demographic_model.real_comparison()
#
Demographic_model.real_comparison.plot=function(
  Demographic_model=Featured_Virtual_profiles_df.demographic_model.cache,
  Digital_skills_model=Digital_skills_df.cache,
  some_digital_skill="some_digital_skill",
  specialized_software="specialized_software",
  features=Demographic_and_digital_skills_model,
  Oferentes_df=db.oferentes_inscritos()
){
  library(ggplot2)
  Demographic_model.real_comparison(
    Demographic_model=Demographic_model,
    Digital_skills_model=Digital_skills_model,
    some_digital_skill=some_digital_skill,
    specialized_software=specialized_software,
    features=features,
    Oferentes_df= Oferentes_df
  ) %>% bind_rows() %>%
    {
      names(.)[1]="value";.
    } %>%
    dplyr::mutate(
      sample=factor(sample)
    ) %>%
    ggplot(., aes(x=feature, y=value, group=feature))+
    geom_line(lty="dashed", lwd=.8)+
    geom_point(aes(color=sample), size=4, alpha=.7)+
   
    geom_text(aes(label=value), check_overlap = TRUE, nudge_x = 0.18)+
    coord_flip()+
    labs(title="Perfilamiento por habilidades digitales",
         subtitle=sprintf("Indicadores de composición de la muestra" ),
         caption="")+
    xlab("Indicador")+
    ylab("%")
}
#
Demographic_model.real_comparison.plot()
Demographic_model.real_comparison.plot(
  features=list(
    Women_proportion=function(model_df){
      round( mean(model_df$sexo=="mujer", na.rm=TRUE)*100, 2)
    },
    College_educ=function(model_df){
      round( mean(
        as.numeric(model_df$educ_model=="universitario o superior")
        , na.rm=TRUE)*100, 2)
    },
    Some_Experience=function(model_df){
      round( mean(
        as.numeric(stringr::str_extract(model_df$anos_de_experiencia, "^[0-9]{1,2}"))>0
        , na.rm=TRUE)*100, 2)
    },
    Remote_Work=function(model_df){
      round( mean(model_df$interesado_en_ofertas_de_=="Si", na.rm=TRUE)*100, 2)
    },
    Specialized_Software=function(model_df){
      round( mean(model_df$specialized_software==1, na.rm=TRUE)*100, 2)
    }
  )
)





#
#
Dataframe.prioritize_factor=function(
  Dataframe,
  factor
){
  t=table(Dataframe[[factor]])
  #
  data.frame(
    value=names(t) %>% as.character(),
    freq=t %>% as.numeric()
  ) %>% 
    dplyr::arrange(freq*(-1)) %>% 
    dplyr::mutate(
      cum_sum= cumsum(freq),
      cum_prop= round((cum_sum/sum(freq))*100, 3)
    ) 
}
#
Virtual_profiles_df.plot_wage_expectations=function(
  Virtual_profiles_df=Virtual_profiles.Virtual_profiles_df.Ensemble_features()
){
  library(ggplot2)
  Dataframe.prioritize_factor(Virtual_profiles_df, "Aspiración salarial") %>%
    mutate(expectativa_salarial=factor(.$"value", levels=.$"value"),
           dummy_=1) %>%
    ggplot(., aes(x=expectativa_salarial, y=freq, group= dummy_))+
      geom_col(fill=rgb(0,0,0,.3))+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
    geom_line(aes(y=cum_prop*(1)*(max(freq)/100)), lty="longdash")+
    geom_label(aes(y=cum_prop*(1)*(max(freq)/100),label=cum_prop), check_overlap = TRUE)+
    labs(title="expectativas salariales de la poblacion SISPE/Uniminuto",
         subtitle="pareto de las expectativas salariales")
}
Virtual_profiles_df.plot_wage_expectations()


#
#
basic_standardization=function(
  names
){
  #
  tolower(names) %>%
    #
    iconv(.,from="UTF-8", sub="",  to="ASCII//TRANSLIT")
  #
}
#
stata_valid_names=function(names){
  text2vec::word_tokenizer(names) %>% 
    sapply(., function(some_name_units){
      s=paste(some_name_units, collapse="_")
      s=stringr::str_replace_all(s, "[.]", "_")
      substr(s, 0, 25) 
    }) %>% basic_standardization()
}
#
stata_valid_names(c("Variable_invalida1",
                    "Variable//.,invalida2",
                    "Variableinvalida3_con_nombre_super_largo",
                    "Variable_con_acentuación",
                    "nomnbre.invalido"
))
#
Dataframe.apply_valid_names_for_stata=function(
  Dataframe
){
  names(Dataframe)=stata_valid_names(names(Dataframe))
  Dataframe
}
#



#
#
OFERENTES_HOME= "C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/oferentes entradas"
#
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
#
#
Virtual_profiles_df.age_model=function(
  Virtual_profiles_df=From_features.demographics_model(),
  Oferentes_df=db.oferentes_inscritos(),
  Age_models=list("sample wide"=c(0,1), "sample halves"=c(0,.5,1), "sample quarters"=c(0,.25, .5, .75, 1) )
){
  
  #
  #
  Oferentes_df.gender=
    Oferentes_df %>%
    dplyr::transmute(
      num_documento=documento,
      sex=ifelse( genero=="M", "hombre", "mujer"))
  
  #
  #
  Virtual_profiles_df=dplyr::left_join(Virtual_profiles_df, Oferentes_df.gender, by="num_documento" )
  
  
  #
  #
  curated_sample=
    Virtual_profiles_df %>% 
    lapply(., function(some_data_col){
      some_data_col[is.na(some_data_col)]="unk"
      some_data_col
    }) %>% as.data.frame() %>% 
    Dataframe.apply_valid_names_for_stata() %>% 
    dplyr::mutate(
      num_documento=as.character(num_documento),
      age=as.numeric(stringr::str_extract_all(edad, "[0-9]{2}"))) 
  
  
  #
  median_or_technical=c("Media","Técnica Laboral", "Técnica Profesional", "Tecnológica")
  #
  college=c("Universitaria","Maestría Doctorado","Especialización")
  #
  W=(nrow(Oferentes_df)/nrow(curated_sample))
  
  i_am_my_name=function(x){names(x)=x;x}
  #
  lapply(Age_models, function(some_age_model){
    some_age_model %>% i_am_my_name()
    
    df=
      curated_sample %>% 
      mutate(age_group=cut(age,
                           breaks = quantile(.$age, some_age_model, na.rm=TRUE),
                           include.lowest = TRUE,
                           ordered_result = TRUE,
                           right=FALSE,
                           
      ))  %>% 
      split( paste(.$age_group, .$sex, sep="X") ) %>%
      lapply(., function(age_group_df){
        
        nrow(age_group_df)
        data.frame(age_group=age_group_df$age_group[1],
                   sex=age_group_df$sex[1],
                   subsample_size=nrow(age_group_df),
                   population_size=floor(W*nrow(age_group_df)),
                   mean_age=round(mean(age_group_df$age),3),
                   prop_soltero=round(mean(age_group_df$estado_civil=="Soltero")*100,3),
                   prop_casado=round(mean(age_group_df$estado_civil=="Casado")*100,3),
                   prop_unión=round(mean(age_group_df$estado_civil=="Unión Libre")*100,3)
        )}) %>% bind_rows() %>%
      dplyr::filter(!is.na(age_group)) %>%
      dplyr::filter(sex!="unk")
  }) 
  
}
#
Virtual_profiles_df.age_model() %>% View()







#
#
Virtual_profiles_df.demographic_model=function(
  
  Virtual_profiles_df=db.from_files_data_reader_virtual_profiles(),
  
  Oferentes_df=db.oferentes_inscritos(),
  
  Education_model=list(
    "sin bachillerato"=c("Preescolar", "Básica Primaria" , "Básica Secundaria" ),
    "bachillerato o técnico"= c("Media", "Técnica Laboral", "Técnica Profesional", "Tecnológica" ),
    "universitario o superior"= c("Universitaria", "Especialización", "Maestría","Doctorado"  )),
  
   Marital_model=list(
     "en pareja"=c("Unión Libre", "Casado"),
     "sin pareja"=c("Soltero")),
  
  Age_breaks=c(.05,.25, .5, .75, 1)
  
){
  
  Virtual_profiles_df2= Virtual_profiles_df %>%
    dplyr::mutate(
      num_documento=num_documento,
      age=as.numeric(stringr::str_extract_all(Edad, "[0-9]{2}"))
    ) %>% 
    dplyr::filter(
      age <= 57
    ) 
  
  m=Virtual_profiles_df2$"Estado civil"
  for (value in names(Marital_model)){
    
    m=plyr::mapvalues(m, 
                        Marital_model[[value]],
                        rep(value, length(Marital_model[[value]])))
  }
  m[ !m %in% names(Marital_model) ]="NA"
  
  e=Virtual_profiles_df2$"Nivel educativo"
  for (value in names(Education_model)){
    
    e=plyr::mapvalues(e, 
                        Education_model[[value]],
                        rep(value, length(Education_model[[value]])))
  }
  e[ !e %in% names(Education_model) ]="NA"
  
  
  dplyr::left_join(
  Virtual_profiles_df %>%
    dplyr::transmute(
      num_documento=num_documento,
      age=as.numeric(stringr::str_extract_all(Edad, "[0-9]{2}"))
    ) %>% 
    dplyr::filter(
      age <= 57
    ) %>%

    dplyr::mutate(
      marital_model=m,
      educ_model=e
    )  %>% 
    mutate(age_group=cut(age,
                         breaks = quantile(.$age, Age_breaks, na.rm=TRUE),
                         include.lowest = TRUE,
                         ordered_result = TRUE,
                         right=FALSE)),
  
  #
  Oferentes_df %>% 
    dplyr::transmute(
      num_documento=documento,
      sexo=genero,
      sexo=ifelse(sexo=="F", "mujer", "hombre"))) 
   
}
#
Virtual_profiles_df.demographic_model.cache=
Virtual_profiles_df.demographic_model(
  Virtual_profiles_df=
  Virtual_profiles_df.Ensemble_features(Records=Virtual_profiles_df.model_sample_units(),
                                        Information_channel="hoja_de_vida",
                                        feature_markers=c("Edad","Estado civil","Nivel educativo")),
 
  
  
)



#
# descriptiva para reunion de grade: colocaciones vs 
# caracteristicas de las personas
#

colocados_model=
colocados_df %>% 
  dplyr::transmute(doc_num=.$"Número Documento",
                   sexo=.$"sexo",
                   tipo_contrato=.$"Tipo Contrato")


colocados_model %>% 
  split(.$sexo)  %>%
  lapply(., function(sex_df){

    sort(prop.table( table(sex_df$tipo_contrato))*100)
    
  })

Virtual_profiles_df.demographic_model.cache %>% 
  dplyr::filter(
    marital_model!="NA" & !(is.na(marital_model))
  ) %>%
  split(paste(.$"sexo", .$"marital_model", sep="x")) %>%
    lapply(., function(sex_df){
    
      df=
      colocados_model %>% 
        dplyr::filter(.$doc_num %in% sex_df$num_documento) 
      sort(prop.table( table(df$tipo_contrato))*100)
     
    })


#
#
Virtual_profiles_df.demographic_model.plot=function(
  Demographic_model_df=Featured_Virtual_profiles_df.demographic_model.cache, 
  age_range=c(0,57),
  Oferentes_df=db.oferentes_inscritos()
){
  
  #
  library(ggplot2)
  W=(nrow(Oferentes_df)/nrow(Demographic_model_df))
  
  #
  Demographic_model_df %>% 
    dplyr::filter(
      (age >=  age_range[1]) &  (age <= age_range[2])
    ) %>% {
      
      
    mutate(., profile=paste(.$"sexo", 
                         .$"age_group", 
                         .$"marital_model",
                         .$"educ_model", 
                         sep="X" )) 
                    
      
      }%>% 
    split(.$profile) %>% 
    lapply(., function(partial_df){
      data.frame(
        sexo=partial_df$"sexo"[1],
        age_group=partial_df$"age_group"[1],
        marital_model=partial_df$"marital_model"[1], 
        educ_model=partial_df$"educ_model"[1],
        num_personas=nrow(partial_df))
    }
      
    ) %>% bind_rows() %>%
    mutate(
      profile=paste(.$"marital_model", .$"educ_model", sep="X" )
    ) %>%
    dplyr::filter(
      marital_model!="NA" & educ_model!="NA" & !is.na(sexo)
    ) %>% bind_rows() %>%
    mutate(
      num_personas=round(num_personas*W)
    ) %>% {
    ggplot(., aes(x=age_group, y=num_personas), group= profile)+
    geom_col(aes(fill=profile), alpha=.3, position = "dodge")+
    
    geom_text(aes(x=age_group, y=num_personas, label = num_personas, group = profile),
              size=3,
              position = position_dodge(width = 1))+
    facet_grid(rows = vars(sexo))+
    labs(
      title="Demographic profiles of SPE/Uniminuto population",
      caption=(sprintf("from a frame population of size %s, with %s complete records , where %s are women",
                       nrow(Oferentes_df),
                       sum(.$num_personas),
                       sum(.$num_personas[.$sexo=="mujer"])
                       )))
    }
}
#
Virtual_profiles_df.demographic_model.plot(
  Demographic_model_df=Featured_Virtual_profiles_df.demographic_model.cache
) 
#
Virtual_profiles_df.demographic_model.plot(
  Demographic_model_df=Virtual_profiles_df.demographic_model.cache
) 
  



#
#
Demographic_model_df %>%
  dplyr::filter(
    !is.na(sexo) &
      !is.na(educ_model) & 
      educ_model!="NA" &
      !is.na(marital_model) &
      marital_model !="NA"
  
  ) %>%
  split(., paste(.$educ_model, .$sexo, sep="x")) %>% 
  lapply(., function(partial_df){
    data.frame(
      sexo=partial_df$sexo[1],
      educ_model=partial_df$educ_model[1],
      personas=nrow(partial_df)
    )
  }) %>% bind_rows() %>%
  mutate(
    personas=round(personas*W)
  ) %>%
  dplyr::arrange(
    personas*(-1)
  ) %>%
  dplyr::mutate(
    educ_model=factor(educ_model,
                      levels=c("sin bachillerato", 
                               "bachillerato o técnico",
                               "universitario o superior")
                      )
  ) %>% 
  {
  ggplot(., aes(y= educ_model, x=personas))+
  geom_col(fill=rgb(0,0,0,.3))+
  geom_text(
    aes(label=personas)
  )+
  facet_grid(~sexo)+
  coord_flip()+
  labs(
    title="Composición demográfica de la muestra",
    subtitle="Cantidad de personas por género y nivel educativo",
    caption=sprintf("poblacion total: %s, registros completos: %s, de mujeres; %s",
                    nrow(Demographic_model_df)*W,
                    sum(.$personas),
                    sum(.$personas[.$sexo=="mujer"] )
                    )
  )+
  ylab("grupo")+
  xlab("número de personas") + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  }



#
#
Demographic_model_df %>%
  dplyr::filter(
    !is.na(sexo) &
      !is.na(educ_model) & 
      educ_model!="NA" &
      !is.na(marital_model) &
      marital_model !="NA"
    
  ) %>%
  split(., paste(.$marital_model, .$sexo, sep="x")) %>% 
  lapply(., function(partial_df){
    data.frame(
      sexo=partial_df$sexo[1],
      marital_model=partial_df$marital_model[1],
      personas=nrow(partial_df)
    )
  }) %>% bind_rows() %>%
  mutate(
    personas=round(personas*W)
  ) %>%
  dplyr::arrange(
    personas*(-1)
  ) %>%
  dplyr::mutate(
    marital_model=factor(marital_model)
  ) %>% 
  {
    ggplot(., aes(y= marital_model, x=personas))+
      geom_col(fill=rgb(0,0,0,.3))+
      geom_text(
        aes(label=personas)
      )+
      facet_grid(~sexo)+
      coord_flip()+
      labs(
        title="Composición demográfica de la muestra",
        subtitle="Cantidad de personas por género y tenencia de pareja",
        caption=sprintf("poblacion total: %s, registros completos: %s, de mujeres; %s",
                        nrow(Demographic_model_df)*W,
                        sum(.$personas),
                        sum(.$personas[.$sexo=="mujer"] )
        )
      )+
      ylab("grupo")+
      xlab("número de personas") + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  }




#
#
#
#
Demographic_model_df %>%
  dplyr::filter(
    !is.na(sexo) & !is.na(educ_model) & educ_model!="NA"
  ) %>%
  split(., paste(.$educ_model, .$sexo, sep="x")) %>% 
  lapply(., function(partial_df){
    data.frame(
      sexo=partial_df$sexo[1],
      educ_model=partial_df$educ_model[1],
      personas=nrow(partial_df)
    )
  }) %>% bind_rows() %>%
  mutate(
    personas=round(personas*W)
  ) %>%
  ggplot(., aes(x= educ_model, y=personas))+
  geom_col(fill=rgb(0,0,0,.3))+
  geom_text(
    aes(label=personas)
  )+
  facet_grid(~sexo)+
  coord_flip()
#
#
Demographic_model_df %>%
  dplyr::filter(
    !is.na(sexo) & !is.na(educ_model) & educ_model!="NA"
  ) %>%
  split(., paste(.$educ_model, .$sexo, sep="x")) %>% 
  lapply(., function(partial_df){
    data.frame(
      sexo=partial_df$sexo[1],
      educ_model=partial_df$educ_model[1],
      personas=nrow(partial_df)
    )
  }) %>% bind_rows() %>%
  mutate(
    personas=round(personas*W)
  ) %>%
  ggplot(., aes(x= educ_model, y=personas))+
  geom_col(fill=rgb(0,0,0,.3))+
  geom_text(
    aes(label=personas)
  )+
  facet_grid(~sexo)+
  coord_flip()


#
#
Demographic_model_df %>%
  dplyr::filter(
    !is.na(sexo) & !is.na( marital_model) &  marital_model !="NA"
  ) %>%
  split(., paste(.$ marital_model, .$sexo, sep="x")) %>% 
  lapply(., function(partial_df){
    data.frame(
      sexo=partial_df$sexo[1],
      marital_model=partial_df$marital_model[1],
      personas=nrow(partial_df)
    )
  }) %>% bind_rows() %>%
  mutate(
    personas=round(personas*W)
  ) %>%
  ggplot(., aes(x=marital_model, y=personas))+
  geom_col(fill=rgb(0,0,0,.3))+
  geom_text(
    aes(label=personas)
  )+
  facet_grid(~sexo)+
  coord_flip()


Demographic_model_df$marital_model


#
# por estado civil
#
library(ggplot2)
W=(nrow(Oferentes_df)/nrow(Demographic_model_df))

#
Demographic_model_df %>% 
  dplyr::filter(
    (age >=  age_range[1]) &  (age <= age_range[2])
  ) %>% 
    
    
    mutate(profile=paste(.$"sexo", 
                         .$"age_group", 
                         .$"educ_model",
                         sep="X" )) %>% 
  split(.$profile) %>% 
  lapply(., function(partial_df){
    data.frame(
      sexo=partial_df$"sexo"[1],
      age_group=partial_df$"age_group"[1],
      marital_model=partial_df$"marital_model"[1], 
      educ_model=partial_df$"educ_model"[1],
      num_personas=nrow(partial_df))
  }
  
  ) %>% bind_rows() %>%
  mutate(
    profile=paste(.$"educ_model", sep="X" )
  ) %>%
  dplyr::filter(
    marital_model!="NA" & educ_model!="NA" & !is.na(sexo)
  ) %>% bind_rows() %>%
  mutate(
    num_personas=round(num_personas*W)
  ) %>% {
    ggplot(., aes(x=age_group, y=num_personas), group= profile)+
      geom_col(aes(fill=profile), alpha=.3, position = "dodge")+
      
      geom_text(aes(x=age_group, y=num_personas, label = num_personas, group = profile),
                position = position_dodge(width = 1))+
      facet_grid(rows = vars(sexo))+
      labs(
        title="Demographic profiles of SPE/Uniminuto population",
        caption=(sprintf("from a frame population of size %s, with %s complete records , where %s are women",
                         nrow(Oferentes_df),
                         sum(.$num_personas),
                         sum(.$num_personas[.$sexo=="mujer"])
        )))
  }



#
# por nivel educativo
#
library(ggplot2)
W=(nrow(Oferentes_df)/nrow(Demographic_model_df))

#
Demographic_model_df %>% 
  dplyr::filter(
    (age >=  age_range[1]) &  (age <= age_range[2])
  ) %>% 
  
  
  mutate(profile=paste(.$"sexo", 
                       .$"age_group", 
                       .$"marital_model",
                       sep="X" )) %>% 
  split(.$profile) %>% 
  lapply(., function(partial_df){
    data.frame(
      sexo=partial_df$"sexo"[1],
      age_group=partial_df$"age_group"[1],
      marital_model=partial_df$"marital_model"[1], 
      educ_model=partial_df$"educ_model"[1],
      num_personas=nrow(partial_df))
  }
  
  ) %>% bind_rows() %>%
  mutate(
    profile=paste(.$"marital_model", sep="X" )
  ) %>%
  dplyr::filter(
    marital_model!="NA" & educ_model!="NA" & !is.na(sexo)
  ) %>% bind_rows() %>%
  mutate(
    num_personas=round(num_personas*W)
  ) %>% {
    ggplot(., aes(x=age_group, y=num_personas), group= profile)+
      geom_col(aes(fill=profile), alpha=.3, position = "dodge")+
      
      geom_text(aes(x=age_group, y=num_personas, label = num_personas, group = profile),
                position = position_dodge(width = 1))+
      facet_grid(rows = vars(sexo))+
      labs(
        title="Demographic profiles of SPE/Uniminuto population",
        caption=(sprintf("from a frame population of size %s, with %s complete records , where %s are women",
                         nrow(Oferentes_df),
                         sum(.$num_personas),
                         sum(.$num_personas[.$sexo=="mujer"])
        )))
  }