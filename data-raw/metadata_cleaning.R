library(tidyverse)
library(ggsci)
source("R/my-function.R")

# download  climate trace data ---------------------------------
my_url <- "https://downloads.climatetrace.org/v02/country_packages/BRA.zip"
download.file(my_url, destfile = "data-raw/BRA.zip", mode = "wb")
unzip("data-raw/BRA.zip", exdir = "data-raw/BRA")

# emissions-sources -----------------------------------------------
# searching sector files
tbl_directorys <- as_tibble(
  list.files("data-raw/BRA/", full.names = TRUE, recursive = TRUE)) |>
  filter(str_detect(value, "emissions_sources.csv"))

# Extraindo os caminhos dos arquvios
value <- tbl_directorys |> pull(value)


# ------------------------------------------------
# stacking the data
data_stk <- map_dfr(value, my_file_read)
glimpse(data_stk)

# Tratanto as colunas de data, nome de setores e sub setores
data_stk <- data_stk |>
  mutate(
    start_time = as_date(start_time),
    end_time = as_date(end_time),
    created_date = as_date(created_date),
    modified_date = as_date(modified_date),
    year = lubridate::year(end_time)
  )  |>
  mutate(
    sector_name = str_split(directory,
                            "/",
                            simplify = TRUE)[,5],
    sub_sector = str_split(directory,
                           "/",
                           simplify = TRUE)[,6],
    sub_sector = str_remove(sub_sector,"_emissions_sources.csv|_country_emissions.csv")
  )

data_stk$sector_name |> unique()
data_stk$sub_sector|> unique()



# agrregating by coordinate and name
base_sigla_uf <- data_stk |>
  group_by(source_name, lon, lat)  |>
  summarise(
    ano = mean(year)
  )  |>
  mutate(
    sigla_uf = get_geobr_state(lon,lat),
    biome = get_geobr_biomes(lon,lat),
    flag_conservation = get_geobr_conservation(lon,lat),
    flag_indigenous = get_geobr_indigenous(lon,lat)
  )


### testing  biome classification
base_sigla_uf |>
  ggplot(aes(x=lon,y=lat,col=biome))+
  geom_point()


### Correcting classification
base_sigla_uf |>
  mutate(
    biome_n = case_when(
        biome=='Other'& lon>= -45 & lat < -6~'AF',
        biome == "Amazônia" ~ "AMZ",
        biome=='Other'& lon< -45 & lat >=-10 ~'AMZ',
        biome == 'Mata Atlântica' & lon> -40 & lat < -20 ~'Other',
        biome == 'Mata Atlântica' & lon> -34 & lat > -5 ~'Other',
        biome == 'Mata Atlântica' ~ 'AF',
        biome=='Cerrado'~'CERR',
        biome =='Pampa'~'PMP',
        biome == 'Pantanal' ~ 'PNT',
        biome=='Caatinga'~'CAAT',
        TRUE ~ 'Other'
      )
  )  |>
  ggplot(aes(x=lon,y=lat,color=biome_n))+
  geom_point()

base_sigla_uf <- base_sigla_uf  |>
  mutate(
    biomes =
      case_when(
        biome=='Other'& lon>=-45 & lat <0~'AF',
        biome=='Amazônia'~'AMZ',
        biome=='Other'& lon< -45 & lat >=-10 ~'AMZ',
        biome == 'Mata Atlântica' & lon> -40 & lat < -20 ~'Other',
        biome == 'Mata Atlântica' & lon> -34 & lat > -5 ~'Other',
        biome == 'Mata Atlântica' ~ 'AF',
        biome=='Cerrado'~'CERR',
        biome =='Pampa'~'PMP',
        biome == 'Pantanal' ~ 'PNT',
        biome=='Caatinga'~'CAAT',
        .default = 'Other'
      )
    )

base_sigla_uf  |>
  ggplot(aes(x=lon,y=lat,col=biomes))+
  geom_point()

###
# classifing by the city  ----------------------------------------
base_sigla_uf$sigla_uf |> unique()
citys <- geobr::read_municipality()
resul <- vector()
state_br <- base_sigla_uf$sigla_uf
tictoc::tic()
for(i in 1:nrow(base_sigla_uf)){
  if(state_br[i]!="Other"){
    my_citys_obj <- citys %>%
      filter(abbrev_state == state_br[i])
    n_citys <- nrow(my_citys_obj)
    my_citys_names <- my_citys_obj %>% pull(name_muni)
    resul[i] <- "Other"
    for(j in 1:n_citys){
      pol_city <- my_citys_obj$geom  %>%
        purrr::pluck(j) %>%
        as.matrix()
      if(def_pol(base_sigla_uf$lon[i],
                 base_sigla_uf$lat[i],
                 pol_city)){
        resul[i] <- my_citys_names[j]
      }
    }
  }
}
tictoc::toc()
base_sigla_uf$city_ref <- resul



# finalizing the data cleaning ---------------------------------------------------------
# national base reading
brazil_ids <- read_rds("data/df_nome.rds")

# mesclando as bases
base_sigla_uf <- left_join(base_sigla_uf,brazil_ids |>
            group_by(sigla_uf,nome_regiao)  |>
            summarise(count=n())  |>
            select(sigla_uf,nome_regiao),
          by = c("sigla_uf"))

# Mesclando e salvando o arquivo final
dados_sigla <- left_join(
  data_stk,
  base_sigla_uf |>
    ungroup()  |>
    select(source_name, lon, lat, sigla_uf, nome_regiao, biomes,
           flag_indigenous, flag_conservation, city_ref),
  by = c("source_name","lat","lon")
)  |>  as_tibble()

dados_sigla$nome_regiao|> unique()

write_rds(dados_sigla |>
             rename(biome = biomes), "data/emissions_sources.rds")

# country data -----------------------------------------------------------------
# sector names searching
tbl_directorys <- as_tibble(
  list.files("data-raw/BRA/", full.names = TRUE, recursive = TRUE))  |>
  filter(str_detect(value, "country_emissions.csv"))

# extracting names
value <- tbl_directorys |> pull(value)

#stacking data

data_country <- map_dfr(value, my_file_read)  |>
  as_tibble()
glimpse(data_country)

data_country <- data_country |>
  # filter(gas == "co2e_100yr") %>%
  mutate(
    start_time = as_date(start_time),
    end_time = as_date(end_time),
    created_date = as_date(created_date),
    modified_date = as_date(modified_date),
    year = lubridate::year(end_time)
  ) %>%
  mutate(
    sector_name = str_split(directory,
                            "/",
                            simplify = TRUE)[,5],
    sector_name = str_remove(sector_name,"_country_emissions.csv")
  )

data_country$sector_name[1]

data_country |>
  select( sector_name ) |>
  distinct()
write_rds(data_country, "data/country_emissions.rds")
