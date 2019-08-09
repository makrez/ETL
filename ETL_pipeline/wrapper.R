transform.table <- function(df){
  
  transform_object <- read.csv(here("csv_source/transform_object.csv"),
                               header = TRUE)
  
  df <- transform.AssignDatatype(df = df, 
                                 transform_object = transform_object)
  
  df <- transform.renameObject(df = df, 
                               transform_object = transform_object,
                               col_old_name = Old_name,
                               col_new_name = New_name
  )
  
  return(df)
}
