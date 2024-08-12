my_stack <- function(file_name){

  df <- read.csv(file_name)
  col_names <- c('X','sif','prec','anomalie','lon','lat','optional')
  colnames(df) <- col_names
  df['year'] <- stringr::str_extract(file_name,"(\\d)+")
  return(df)

}
