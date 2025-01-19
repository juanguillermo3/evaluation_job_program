
#
# dev data
#
setwd("C:/Users/josio/Downloads")
library(dplyr)
dev_data=read.csv("prototipo.csv") %>%
  dplyr::select(c("sesion","composicion_hogar"))
View(dev_data)

#
# main function
#
expand_codes_as_new_vars=function(
  
  vector=dev_data$composicion_hogar, # data_col where information lies
  
  codes= list(        # mapping to variable names
    "1"="persona_1",
    "2"="persona_2",
    "3"="persona_3",
    "4"="persona_4",
    "5"="persona_que_no_existe"
  ),
  
  prefix="lives_with_", # prefix to variable names
  
  separator_regex= "[.,]" # separator for information facts
    
){
  #
  # returns a dataframe with one indicador variable per element in code
  
  #
  state_df=data.frame(raw=vector)
  
  #
  for (code in names(codes)){
    
    code=code
    value=codes[[code]]

    state_df[[
      
      sprintf( "%s%s", prefix, value)
      
      ]]= sapply(
      vector, function(some_value){
        ifelse(
        code %in% unlist(stringr::str_split(some_value, pattern=separator_regex)),
        "yes",
        "no")
      })
  }
  
  state_df

}

#
# unit tests
#

#
# (1) defaalt behaviour
#
expand_codes_as_new_vars() %>% View()

#
# (2) variation:
#
expand_codes_as_new_vars(
  
  vector=dev_data$composicion_hogar, # same source
  
  codes= list(  # custom mapping
    "1"="banana",
    "3"="patata"
  ),
  
  prefix="fruit_type_" # custom prefix
) %>% View()


#
# use example
#

list(
  
dev_data, 

expand_codes_as_new_vars(
  
  vector=dev_data$composicion_hogar, # data source
  
  codes= list(  # custom mapping
    "1"="pareja",
    "2"="hijos",
    "3"="abuelo",
    "99"="alien",
    "100"="zombie",
    "101"="pet"),
  
  prefix="lives_with_", # custom prefix
  
  separator_regex= "[.,]" # custom separator
  ) 
  
) %>% 
  
  bind_cols() %>%
  
  View()