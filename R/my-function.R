states <- geobr::read_state(showProgress = FALSE) #Brazilian states shp
biomes <- geobr::read_biomes(showProgress = FALSE) #brazilian biomes shp
conservation <- geobr::read_conservation_units(showProgress = FALSE) #brazilian conservation units shp
indigenous <- geobr::read_indigenous_land(showProgress = FALSE) #brazilian indigeous land shp

get_geobr_pol <- function(i) {
  states$geom |> purrr::pluck(i) |> as.matrix()
}

get_geobr_biomes_pol <- function(i) {
  biomes$geom |> purrr::pluck(i) |> as.matrix()
}

get_geobr_conservation_pol <- function(i) {
  conservation$geom |> purrr::pluck(i) |> as.matrix()
}

get_geobr_indigenous_pol <- function(i) {
  indigenous$geom |> purrr::pluck(i) |> as.matrix()
}


def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}

###
abbrev_states <- states$abbrev_state
list_pol <- map(1:27, get_geobr_pol)
names(list_pol) <- abbrev_states

get_geobr_state <- function(x,y){
  x <- as.vector(x[1])
  y <- as.vector(y[1])
  resul <- "Other"
  lgv <- FALSE
  for(i in 1:27){
    lgv <- def_pol(x,y,list_pol[[i]])
    if(lgv){
      resul <- names(list_pol[i])
    }else{
      resul <- resul
    }
  }
  return(as.vector(resul))
}

####
names_biomes<- biomes |>
  filter(name_biome!='Sistema Costeiro') |>
  pull(name_biome)

list_pol_biomes <- map(1:6, get_geobr_biomes_pol)
names(list_pol_biomes) <- names_biomes

get_geobr_biomes <- function(x,y){
  x <- as.vector(x[1])
  y <- as.vector(y[1])
  resul <- "Other"
  lgv <- FALSE
  for(i in 1:6){
    lgv <- def_pol(x,y,list_pol_biomes[[i]])
    if(lgv){
      resul <- names(list_pol_biomes[i])
    }else{
      resul <- resul
    }
  }
  return(as.vector(resul))
}

###
list_pol_conservation <- map(1:1934, get_geobr_conservation_pol)
list_pol_indigenous <- map(1:615, get_geobr_indigenous_pol)
names(list_pol_biomes) <- names_biomes

get_geobr_conservation <- function(x,y){
  x <- as.vector(x[1])
  y <- as.vector(y[1])
  lgv <- FALSE
  for(i in 1:1934){
    lgv <- def_pol(x,y,list_pol_conservation[[i]])
    if(lgv) break
  }
  return(lgv)
}

get_geobr_indigenous <- function(x,y){
  x <- as.vector(x[1])
  y <- as.vector(y[1])
  lgv <- FALSE
  for(i in 1:615){
    lgv <- def_pol(x,y,list_pol_indigenous[[i]])
    if(lgv) break
  }
  return(lgv)
}


conservation$geom %>% tibble() #1,934
indigenous$geom %>% tibble() #615



# function to read a file
my_file_read <- function(sector_name){
  read.csv(sector_name) %>%
    select(!starts_with("other")) %>%
    mutate(directory = sector_name)
}

### rainfall data download
power_data_download <- function(lon,lat, startdate, enddate){
  df <- nasapower::get_power(
    community = 'ag',
    lonlat = c(lon,lat),
    pars = c('ALLSKY_SFC_SW_DWN','RH2M','T2M','PRECTOTCORR','WS2M','WD2M'),
    dates = c(startdate,enddate),
    temporal_api = 'daily'
  )
  write.csv(df,paste0('precipitation/data-raw/',lon,'_',lat,'.csv'))
}

### CT data download
download_arquivo <- function(url, dir){
  download.file(url, dir)
  return(dir)
}
