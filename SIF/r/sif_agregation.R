source('SIF/r/function.R')


#### expanded grid for brazil

dist <- 0.5
grid_br <- expand.grid(lon=seq(-74,
                               -27,dist),
                       lat=seq(-34,
                               6,
                               dist))
plot(grid_br)


br <- geobr::read_country(showProgress = FALSE)
region <- geobr::read_region(showProgress = FALSE)

pol_br <- br$geom |> purrr::pluck(1) |> as.matrix()
pol_north <- region$geom |> purrr::pluck(1) |> as.matrix()
pol_northeast <- region$geom |> purrr::pluck(2) |> as.matrix()
pol_southeast <- region$geom |> purrr::pluck(3) |> as.matrix()
pol_south <- region$geom |> purrr::pluck(4) |> as.matrix()
pol_midwest<- region$geom |> purrr::pluck(5) |> as.matrix()

# correcting poligions

pol_br <- pol_br[pol_br[,1]<=-34,]
pol_br <- pol_br[!((pol_br[,1]>=-38.8 & pol_br[,1]<=-38.6) &
                     (pol_br[,2]>= -19 & pol_br[,2]<= -16)),]

pol_northeast <- pol_northeast[pol_northeast[,1]<=-34,]
pol_northeast <- pol_northeast[!((pol_northeast[,1]>=-38.7 &
                                    pol_northeast[,1]<=-38.6) &
                                   pol_northeast[,2]<= -15),]

pol_southeast <- pol_southeast[pol_southeast[,1]<=-30,]


### filtering expanded grid to the brazil boundries

grid_br_cut <- grid_br |>
  dplyr::mutate(
    flag_br = def_pol(lon,lat,pol_br),
    flag_north = def_pol(lon,lat,pol_north),
    flag_northeast = def_pol(lon,lat,pol_northeast),
    flag_midwest= def_pol(lon,lat,pol_midwest),
    flag_southeast = def_pol(lon,lat,pol_southeast),
    flag_south = def_pol(lon,lat,pol_south)
  ) |>
  tidyr::pivot_longer(
    tidyr::starts_with('flag'),
    names_to = 'region',
    values_to = 'flag'
  ) |>
  dplyr::filter(flag) |>
  dplyr::select(lon,lat) |>
  dplyr::group_by(lon,lat) |>
  dplyr::summarise(
    n_obs = dplyr::n()
  )

plot(grid_br_cut$lon,grid_br_cut$lat)


#### aggregation
sifdf <- readr::read_rds('SIF/data/sif_full.rds')

sif_full <- sifdf |> dplyr::mutate(
  date=lubridate::as_datetime(time,origin='1990-01-01 00:00:00 UTC'),
  date = lubridate::as_date(date),
  year =lubridate::year(date),
  month = lubridate::month(date)
)

max(sif_full$year)


for(i in 2015:2023){
  aux_sif <- sif_full |>
    dplyr::filter(year==i)
  vct_sif <- vector();dist_sif <- vector();
  lon_grid <- vector();lat_grid <- vector();
  for(k in 1:nrow(aux_sif)){
    d <- sqrt((aux_sif$lon[k]-grid_br_cut$lon)^2+
                (aux_sif$lat[k]-grid_br_cut$lat)^2
    )
    min_index <- order(d)[1]
    vct_sif[k] <- aux_sif$sif_757[min_index]
    dist_sif[k] <- d[order(d)[1]]
    lon_grid[k] <- grid_br_cut$lon[min_index]
    lat_grid[k] <- grid_br_cut$lat[min_index]
  }
  aux_sif$dist_sif<- dist_sif
  aux_sif$sif_new <- vct_sif
  aux_sif$lon_grid <- lon_grid
  aux_sif$lat_grid <- lat_grid
  if(i == 2015){
    sif_full_cut <- aux_sif
  }else{
    sif_full_cut <- rbind(sif_full_cut,aux_sif)
  }
}


sif_full_cut|>
  dplyr::mutate(
    dist_conf = sqrt((lon - lon_grid)^2 + (lat - lat_grid)^2)
  ) |>
  dplyr::glimpse()

nrow(sif_full_cut |>
       dplyr::mutate(
         dist_conf = sqrt((lon - lon_grid)^2 + (lat - lat_grid)^2),
         dist_bol = dist_sif - dist_conf
       ) |>
       dplyr::filter(dist_bol ==0)) == nrow(sif_full_cut)


readr::write_rds(sif_full_cut,'SIF/data/sif_0.5deg_full_trend.rds')
