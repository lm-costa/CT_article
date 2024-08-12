
library(raster)

my_raster_extractor <- function(raster_file,points_file){
  raster <- raster(raster_file)
  points <- read.csv(points_file) |> dplyr::select(-c(X,year))
  file_name <- stringr::str_split(raster_file,
                             "/",
                             simplify = TRUE)[,3]
  file_name <- stringr::str_split(file_name,'.tif',simplify = T)[,1]

  new_file_name <- paste0('data/cor/',file_name,'_prec_sif.csv')

  my_points <- points
  sp::coordinates(my_points) <- c('lon','lat')

  my_values_ <- extract(raster,my_points)

  df <- cbind(my_points,my_values_)
  write.csv(df,new_file_name)
}
