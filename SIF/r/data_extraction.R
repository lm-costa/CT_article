# my functions
source('SIF/r/function.R')

####
files_names <- list.files("SIF/data-raw/",
                          pattern = "nc",
                          full.names = TRUE)

#### Extracting

sif_df <- purrr::map_df(files_names, my_ncdf4_extractor)
dplyr::glimpse(xco2)

readr::write_rds(sif_df,'SIF/data/sif_full.rds')
