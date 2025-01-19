#
# fechas
# --------

remediar_fechas=function(
  columna_fechas,
  month_codes=list(
    "(E|e)nero"="01",
    "(F|f)ebrero"="02",
    "(M|m)arzo"="03",
    "(A|a)bril"="04",
    "(M|m)ayo"="05",
    "(J|j)unio"="06",
    "(J|j)ulio"="07",
    "(A|a)gosto"="08",
    "(S|s)eptiembre"="09",
    "(O|o)ctubre"="10",
    "(N|n)oviembre"="11",
    "(D|d)iciembre"="12"
  ),
  sep=" "
){
  #
  new_vec=columna_fechas
  #
  for (rm in names(month_codes)){
    new_vec=stringr::str_replace_all(new_vec, rm, month_codes[[rm]])
  }
  #
  new_vec=stringr::str_replace_all(new_vec, "[^[0-9] ]", "")
  new_vec=stringr::str_replace_all(new_vec, " +", sep)
  return(new_vec)
}
#
remediar_fechas(
  columna_fechas = c("31 de Octubre de 2020",
                     "7 de Enero de 2021",
                     "12 de Junio de 2021",
                     "17 de Mayo de 2019",
                     "26 de Septiembre de 2017")
)
