sif <- readr::read_rds('SIF/data/sif_0.5deg_full_trend.rds')

sif_anual <- sif |>
  dplyr::filter(dist_sif<0.25) |>
  dplyr::mutate(
    lon = lon_grid,
    lat = lat_grid,
  ) |>
  dplyr::select(-c(lon_grid,lat_grid)) |>
  dplyr::group_by(lon,lat,year) |>
  dplyr::summarise(
    sif_mean= mean(sif_757,na.rm=TRUE)
  )

prec <- readr::read_rds('precipitation/data/nasa_power_data.rds')

prec_anual <- prec |>
  dplyr::group_by(
    LON,LAT,YEAR
  ) |>
  dplyr::summarise(
    prec_tot = sum(PRECTOTCORR)
  ) |>
  dplyr::mutate(
    lon = LON,
    lat = LAT,
    year = YEAR
  ) |>
  dplyr::ungroup() |>
  dplyr::select(lon,lat,year,prec_tot)



new_df <- sif_anual |>
  dplyr::left_join(prec_anual) |>
  na.omit() |>
  dplyr::filter(year<2023 & year > 2014)

years <- new_df |> dplyr::pull(year) |> unique()


for( i in 1:length(years)){
  write.csv(
    new_df |>
      dplyr::filter(
        year == years[i]
      ),
    paste0('data/prec_sif/prec_sif_year_',years[i],'.csv')
  )
}
