#
# persistence utils
# -------

library(openxlsx)
#
Dataframe.share_with_app=function(
  Dataframe,
  label,
  app_route
){
  file_name=sprintf("%s.xlsx",label)
  suppressWarnings(dir.create(app_route))
  setwd(app_route)
  suppressWarnings(file.rename(from=sprintf(file_name), to=sprintf("%s (back-up).xlsx", label)))
  suppressWarnings(file.remove(from=sprintf(file_name)))
  Dataframe %>%
    openxlsx::write.xlsx(file_name)
  stopifnot("error: file not created"=file_name %in% list.files())
}

#
# statistical output must be a module-level singleton
#
Dataframe.export_output=function(
  Dataframe,
  label,
  output_home=getwd(),
  new_output_array=FALSE,
  output_file_name="output.xlsx"
){
  
  #
  if(new_output_array){
    .GlobalEnv[["statistical_output"]]=list()
  } 
  #
  .GlobalEnv[["statistical_output"]][[label]]=Dataframe
  #
  dir.create(output_home)
  setwd(output_home)
  dir.create("output")
  setwd("output")
  openxlsx::write.xlsx(.GlobalEnv[["statistical_output"]],output_file_name)
  
}