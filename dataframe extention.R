# Dataframe extentions
# --------
#

#
Dataframe.count_over_factor=function(
  Dataframe,
  counted,
  normalize=FALSE
){
  #
  # map Dataframe to a Dataframe that counts values for counted data column
  #
  
  #
  pr_as_pretty_rate=function(x){
    round(x*100,3)
  }
  #
  table(Dataframe[[counted]]) %>%
    {
      data.frame(
        value=names(.) %>% as.character(),
        count=as.numeric(.))
    }  %>%
    {
      if(normalize){
        
        mutate(., count=
                 round(.[["count"]]/sum(.[["count"]])*100, 3)
        )
        
      } else {
        .
      }
      
    }%>%
    tidyr::pivot_wider(
      names_from = value,
      values_from=count
    )
}
#
Dataframe.order=function(
  Dataframe=db.contrataciones_normalizadas(),
  order= c("tipo_contrato", "salario") 
){
  #
  # map Dataframe to a Dataframe that counts values for counted data column
  #
  
  Dataframe %>%
    {
      .[, 
        intersect( c(order, setdiff(names(.), order) ), names(.)) 
      ]
    }
}
#
#
Dataframe.map_NAS=function(
  Dataframe=db.contrataciones_normalizadas(),
  mapped_to=0
){
  #
  # map Dataframe to a Dataframe that counts values for counted data column
  #
  
  Dataframe %>%
    {
      .[is.na(.) 
      ]=mapped_to;.
    }
}
#
#Dataframe.map_NAS() %>% View()
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

str_seq.replace_initial_numbers=
  function(
    str_seq=n
  ){
    str_seq %>%
      sapply(
        function(str){
          if(
          stringr::str_detect(
            str, "^[0-9]"
          )){
            str=paste( "v_",str, sep="")
          };str 
          
        }
      )
  }
#
#str_seq.replace_initial_numbers()

#
# stata_valid_names(c("Variable_invalida1",
#                     "Variable//.,invalida2",
#                     "Variableinvalida3_con_nombre_super_largo",
#                     "Variable_con_acentuaciÃÂÃÂ³n",
#                     "nomnbre.invalido"
# ))
#
Dataframe.apply_valid_names_for_stata=function(
  Dataframe=db.contrataciones_normalizadas()
){
  names(Dataframe)=stata_valid_names(names(Dataframe)) %>%
    str_seq.replace_initial_numbers
  Dataframe
}
#
# Dataframe.apply_valid_names_for_stata() %>% names()
#
Dataframe.mandatory_model=function(
  df,
  mandatory_model
){
  df %>% 
    dplyr::filter(
      df %>%
        dplyr::select( mandatory_model) %>%
        apply( MARGIN=1, 
               function(row_data){
                 all(!is.na(row_data))
               })
    )
}
#
#
#
Dataframe.vars_as_character=function(
  Dataframe
){
  Dataframe %>% 
    lapply(function(data_col){
      as.character(data_col)
    }) %>%
    as.data.frame()
}
#
#Dataframe.vars_as_character() 
#
Dataframe.reencode=function(
  Dataframe,
  FileEncoding="UTF-8"
){
  #
  Dataframe %>% write.table("tempfile.txt")
  an= read.table("tempfile.txt", encoding="UTF-8")
  return(an)
}
#
#Dataframe.reencode() 
#


#
Dataframe.aggregate=function(
  Dataframe,
  aggregated,
  label, 
  Na.rm=TRUE
){
  
  
  Dataframe %>% 
    {
      dplyr::mutate(
        .,
        temp_var=
          {
            Dataframe %>%
              dplyr::select(aggregated) %>% 
              apply(X=., 
                    MARGIN=1, 
                    FUN=function(row_data){
                      sum(row_data, na.rm = Na.rm)
                    }) 
          }
      )
    } %>%
    {
      .[[label]]=.[["temp_var"]];.
      
    } %>% 
    dplyr::select(-"temp_var") 
}
#
# Dataframe.aggregate(
#   Dataframe=db.indicadores_por_oferente(),
#   aggregated=c(
#     "aprendizaje",
#     "obra",
#     "otro",
#     "prest_de_servicios",
#     "temporal",
#     "termino_fijo",
#     "termino_indefinido"),
#   label="cualquier_contrato"
# ) %>% View()


#
Dataframe.complain_for_vars=function(
  Dataframe,
  mandatory_vars
){
  stopifnot(
    "some mandatory vars not found in Dataframe"=
      all( (mandatory_vars %in% names(Dataframe)) )
  )
  #
  Dataframe
}
#
# Dataframe.complain_for_vars(
#   data.frame(),
#   mandatory_vars="unexistent"
# )
#
# Dataframe.complain_for_vars(
#   data.frame("existent1"="", "existent2"=""),
#   mandatory_vars="existent1"
# )


#
Dataframe.totalize=function(
  Dataframe,
  i_am_not_totalizable=NaN,
  Na.rm=TRUE,
  group_name_col=1
){
  #
  an=
  rbind(Dataframe,
       sapply(names(Dataframe),function(data_col){
         ifelse(
           !(data_col %in% i_am_not_totalizable),
           sum(Dataframe[[data_col]]  %>% as.numeric(), na.rm=Na.rm),
           "-"
         )})) 
  if(is.numeric( group_name_col)){
    an[nrow(an),group_name_col]="Totales"  
  }
  an
}
#
#Dataframe.totalize()

#
Dataframe.prefix=function(
  Dataframe,
  prefix="var_"
){
  names(Dataframe)=paste(prefix, names(Dataframe), sep="")
  Dataframe
}

#
Dataframe.new_names=function(
  Dataframe,
  new_names
){
  names(Dataframe)=new_names;Dataframe
}

#
Dataframe.count_values=function(
  Dataframe,
  counted
){
  new_df=
  table(Dataframe[[counted]]) %>%
    as.data.frame()
  new_df[[1]]=as.character(new_df[[1]])
  new_df
}

#
Dataframe.insert=function(
  Dataframe,
  row_of_values
){
  rbind(Dataframe, row_of_values)
}

Dataframe.apply_treshold=function(
  Dataframe,
  treshold=1
){
  Dataframe[Dataframe>=treshold]=1
  Dataframe[Dataframe<treshold]=0
  return(Dataframe)
}

#
Dataframe.alter_table_Dataframe_add_primary_key=function(
  Dataframe,
  primary_key_components
){
  new_df=Dataframe
  new_df$primary_key=""
  
  for (component in primary_key_components){
    new_df=
    new_df %>%
      dplyr::mutate(
        primary_key=paste(primary_key, .[[component]], sep="+" )
      )
  }
  
  new_df %>%
    dplyr::filter(!duplicated(primary_key)) %>%
  return(new_df)
}

#
Dataframe.select_on_regex=function(
  Dataframe,
  selecting_regex
){
  Dataframe %>%
    dplyr::select(
      grep(names(.), pattern=selecting_regex, value=TRUE)
    ) %>%
    return()
}


#
Dataframe.delimite_dates=function(
  Dataframe,
  lower_date,
  upper_date
){
  Dataframe %>%
    
    dplyr::filter(
      fecha  > lower_date
    ) %>%
    dplyr::filter(
      fecha  <  upper_date
    ) %>%
    
    return()
}


#
# regex associated behavour
# --------

#
Textual_feature.basic_standardization=function(
  names
){
  #
  tolower(names) %>%
    iconv(to="ASCII//TRANSLIT")
  #
}
#
i_am_my_name=function(x){
  names(x)=x;x
}
#
Dataframe.expand_regex_features=function(
  Dataframe,
  text_source,
  features=table(Dataframe[[text_source]]) %>% names() %>% i_am_my_name(),
  standardization=Textual_feature.basic_standardization,
  name_prefix
  
){
  #
  state_df= 
    Dataframe %>%
    dplyr::mutate(
      z_textual_source=standardization(.[[text_source]])
    )
  #
  for (feature_name in names(features)){
    state_df[[sprintf("%s%s", name_prefix, feature_name)]]=
      ifelse(
        stringr::str_detect(state_df$z_textual_source, pattern=features[[feature_name]]),1,0)
  }
  state_df %>%
    dplyr::select(-"z_textual_source")
}
#
#
Dataframe.extract_regex_fields=function(
  Dataframe,
  text_source,
  features=table(Dataframe[[text_source]]) %>% names() %>% i_am_my_name(),
  standardization=Textual_feature.basic_standardization,
  name_prefix
){
  #
  state_df= 
    Dataframe %>%
    dplyr::mutate(
      z_textual_source=standardization(.[[text_source]])
    )
  #
  for (feature_name in names(features)){
    state_df[[sprintf("%s%s", name_prefix, feature_name)]]=
      stringr::str_extract(state_df$z_textual_source, pattern=features[[feature_name]])
  }
  state_df %>%
    dplyr::select(-"z_textual_source")
}

#
Dataframe.fetch=function(
  Dataframe,
  fetched
){
  Dataframe %>% 
    {
      Dataframe[[fetched]]
    }
}


#
list.keep_dataframes=function(
  list
){
  list[sapply(list, function(l_){"data.frame" %in% class(l_)})]
}
#
Dataframe.round_numbers=
  function(Dataframe, digits=4){
    
    Dataframe %>%
      lapply(function(data_col){
        tryCatch({
          round(data_col, digits)
        }, error=function(e){return(data_col)})
      })  %>%
      as.data.frame()
  }