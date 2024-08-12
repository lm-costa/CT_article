source('R/raster_extractor.R')
source('R/stack.R')
source('R/correl.R')

raster_files <- list.files('data/tifs_ano',
                           pattern = '.tif',
                           full.names = T)

prec_sif_files <- list.files('data/prec_sif/',
                             pattern = '.csv',
                             full.names = T)


tictoc::tic()
purrr::map2(raster_files,prec_sif_files,my_raster_extractor)
tictoc::toc()

tables_names <- list.files('data/cor/',full.names = T)
tictoc::tic()
df <- purrr::map_df(tables_names,my_stack)
tictoc::toc()



dfn <- df |>
  dplyr::select(lon,lat,sif,prec,anomalie,year) |>
  na.omit()


df_tible <- dfn |>
  tibble::as_tibble() |>
  dplyr::group_by(lon, lat,year) |>
  dplyr::summarise(sif = mean(sif, na.rm=TRUE),
                   ano = mean(anomalie,na.rm=T),
                   prec = mean(prec,na.rm=T)) |>
  dplyr::mutate(
    id_time = year
  ) |>
  dplyr::group_by(lon,lat) |>
  dplyr::mutate(
    nobs = dplyr::n()
  ) |>
  dplyr::filter(nobs>2) |>
  tidyr::nest()






df_tible_new <- df_tible |>
  dplyr::mutate(
    cor_ano_sif = purrr::map(data,my_cor_sif_ano, out='cor'),
    p_value_ano_sif = purrr::map(data,my_cor_sif_ano, out='pvalue'),
    cor_ano_prec= purrr::map(data,my_cor_ano_prec, out='cor'),
    p_value_ano_prec = purrr::map(data,my_cor_ano_prec, out='pvalue'),
    cor_sif_prec= purrr::map(data,my_cor_sif_prec, out='cor'),
    p_value_sif_prec = purrr::map(data,my_cor_sif_prec, out='pvalue')
  )


df_ano_sif <- df_tible_new |>
  tidyr::unnest(cols = c(cor_ano_sif, p_value_ano_sif)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, cor_ano_sif, p_value_ano_sif)


df_ano_prec <- df_tible_new |>
  tidyr::unnest(cols = c(cor_ano_prec, p_value_ano_prec)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, cor_ano_prec, p_value_ano_prec)

df_sif_prec <- df_tible_new |>
  tidyr::unnest(cols = c(cor_sif_prec, p_value_sif_prec)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, cor_sif_prec, p_value_sif_prec)



df_ano_prec |>
  dplyr::filter(p_value_ano_prec<0.1) |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat,fill=cor_ano_prec ))+
  ggplot2::geom_tile()

write.csv(df_ano_prec |>
            dplyr::filter(p_value_ano_prec<0.1),
          'data/cor/final_corel_co2_prec.csv')

df_ano_sif |>
  dplyr::filter(p_value_ano_sif<0.1) |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat,fill=cor_ano_sif ))+
  ggplot2::geom_tile()

write.csv(df_ano_sif |>
            dplyr::filter(p_value_ano_sif<0.1),
          'data/cor/final_corel_co2_sif.csv')
df_sif_prec |>
  dplyr::filter(p_value_sif_prec<0.1) |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat,fill=cor_sif_prec ))+
  ggplot2::geom_tile()


write.csv(df_sif_prec |>
            dplyr::filter(p_value_sif_prec<0.1),
          'data/cor/final_corel_prec_sif.csv')
