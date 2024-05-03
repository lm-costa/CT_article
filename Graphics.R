library(tidyverse)
library(ggsci)
library(geobr)
source("R/grafico.R")
source("R/my-function.R")



## data reading

emissions_sources <- read_rds("data/emissions_sources.rds") %>%
  mutate(source_name_1 = str_to_title(source_name)) %>%
  filter(year >=2015, year<=2023)

emissions_sources$year %>% unique()
states <- read_rds("data/states.rds") %>%
  mutate(name_region = ifelse(name_region == "Centro Oeste","Centro-Oeste",
                              name_region))


brazil_ids <- read_rds("data/df_nome.rds")
glimpse(emissions_sources)
nomes_uf <- c(brazil_ids$nome_uf %>% unique(),"Brazil")
abbrev_states <- brazil_ids$sigla_uf %>% unique()
region_names <- brazil_ids$nome_regiao %>% unique()



########
granular <- emissions_sources %>%
  filter(
    gas == "co2e_100yr",
    !source_name %in% nomes_uf,
    !sub_sector %in% c("forest-land-clearing",
                       "forest-land-degradation",
                       "shrubgrass-fires",
                       "forest-land-fires",
                       "wetland-fires",
                       "removals")
  ) %>%
  group_by(year, sector_name) %>%
  summarise(
    emission = sum(emissions_quantity, na.rm=TRUE)
  ) %>%
  ungroup()

dados_country <- read_rds("data/country_emissions.rds")
tab_country_emissions <- dados_country %>%
  filter(gas == "co2e_100yr",
         year < 2023) %>%
  # filter(!original_inventory_sector %in% c("forest-land-clearing",
  #                               "forest-land-degradation",
  #                               "shrubgrass-fires",
  #                               "forest-land-fires",
  #                               "wetland-fires",
  #                               "removals")) %>%
  group_by(year,sector_name) %>%
  filter(sector_name != "forestry_and_land_use") %>%
  group_by(year) %>%
  summarize(emission = sum(emissions_quantity,
                           na.rm = TRUE)) |>
  mutate(
    emission_c = as.character(emission/1e9),
    emission_c = str_sub(emission_c,1,4)
  ); tab_country_emissions


country <- dados_country %>%
  group_by(year,sector_name) %>%
  filter(sector_name != "forestry_and_land_use",
         gas == "co2e_100yr") %>%
  summarize(emission = sum(emissions_quantity,
                           na.rm = TRUE))
add <- rbind(granular %>%
               filter(sector_name == "forestry_and_land_use"), country)
df1 <- add %>%
  filter(sector_name == "forestry_and_land_use") %>%
  group_by(year) %>%
  summarise(
    emission = sum(emission)
  )

df2 <- dados_country %>%
  # filter(!original_inventory_sector %in% c("forest-land-clearing",
  #                               "forest-land-degradation",
  #                               "shrubgrass-fires",
  #                               "forest-land-fires",
  #                               "wetland-fires",
  #                               "removals")) %>%
  group_by(year,sector_name) %>%
  filter(sector_name != "forestry_and_land_use",
         gas == "co2e_100yr") %>%
  summarize(emission = sum(emissions_quantity,
                           na.rm = TRUE))
df1$sector_name <- "forestry_and_land_use"



### Figure 1a CT

netemissions <- rbind(df1,df2) %>%
  group_by(year) %>%
  summarise(
    emission = sum(emission)
  ) %>%
  filter(year != 2023)

high <- rbind(df1,df2) %>%
  mutate(emission = ifelse(emission < 0, 0, emission)
  ) %>%
  filter(year != 2023) %>%
  group_by(year) %>%
  summarise(
    emission = sum(emission)
  ) %>% pull(emission)

colors <- c("#00A087FF", "#4DBBD5FF", "#E64B35FF", "#3C5488FF",
                      "#F39B7FFF", "#8491B4FF",
                      "#91D1C2FF", "#DC0000FF", "#7E6148FF", "#B09C85FF")


rbind(df1,df2) %>%
  filter(year != 2023) %>%
  mutate(
    sector_name = sector_name %>% as_factor()
  ) %>%
  ggplot(aes(x=year,y=emission/1e9,
             fill=sector_name)) +
  geom_col() +
  annotate("text",
           x=2015:2022,
           y=high/1e9+.35,
           label = paste0("(",round(netemissions$emission/1e9,2),")"),
           size=4, fontface="italic") +
  geom_col(color="black") +
  theme_bw() +
  scale_fill_manual(values = colors) +
  labs(x="",
       y="Emission (G ton)",
       fill = "Sector")+
  theme(
    axis.text.x = element_text(size = rel(1.25)),
    axis.title.x = element_text(size = rel(1.5)),
    axis.text.y = element_text(size = rel(1.25)),
    axis.title.y = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1.3)),
    legend.title = element_text(size = rel(1.3)),
    legend.position = 'bottom'
  ) +
  annotate("text",
           x=2015:2022,
           y=high/1e9+.13,
           label = tab_country_emissions$emission_c,
           size=4, fontface="bold")

ggsave('img/figure_1a.png',
                units="in", width=12, height=6,
                dpi=300)


#########  Figure 1b SEEG ######

seeg_data <- read.csv('data-raw/seeg_emission.csv',sep = ";",header = T) %>%
  pivot_longer(cols = 'X2015':'X2022',
               names_to = 'year',
               values_to = 'emission') %>%
  mutate(
    year = str_remove(year,'X') %>% as.numeric()
  )

sector_color <- c( "#4DBBD5FF", "#E64B35FF","#00A087FF", "#8491B4FF","#B09C85FF")

altura <- seeg_data %>%
  mutate(emission = ifelse(emission < 0, 0, emission)
  ) %>%
  group_by(year) %>%
  summarise(
    emission = sum(emission)
  ) %>% pull(emission)

net_emission <- seeg_data %>%
  group_by(year) %>%
  summarise(emission = sum(emission))

emission <- seeg_data %>%
  filter(Type=='emission') %>%
  group_by(year) %>%
  summarise(emission = sum(emission))

seeg_data %>%
  ggplot(aes(x=year,y=emission,fill=Sector))+
  geom_col()+
  labs(x="",
       y="Emission (G ton)",
       fill = "Sector") +
  geom_col(color="black") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = rel(1.25)),
    axis.title.x = element_text(size = rel(1.5)),
    axis.text.y = element_text(size = rel(1.25)),
    axis.title.y = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1.3)),
    legend.title = element_text(size = rel(1.3)),
    legend.position = 'bottom')+
  scale_fill_manual(values = sector_color)+
  annotate("text",
           x=2015:2022,
           y=altura/.95,
           label = round(emission$emission,2),
           size=4, fontface="bold")+
  annotate("text",
           x=2015:2022,
           y=altura/.8,
           label = paste0("(",round(net_emission$emission,2),")"),
           size=4, fontface="italic")

ggsave('img/figure_1b.png',
       units="in", width=12, height=6,
       dpi=300)


#######   Figure 2 anomalie
### the anomalie data will be used on arcmap

anos <- 2015:2022
for(i in seq_along(anos)){
  emission_median <- emissions_sources |>
    pull(emissions_quantity) |>
    median(na.rm=TRUE)

  a <-  emissions_sources |>
    filter(year == anos[i],
           gas == "co2e_100yr",
           !source_name %in% nomes_uf
    ) |>
    group_by(lon,lat,city_ref) |>
    summarise(
      anomalie = (mean(emissions_quantity,na.rm=T)- emission_median)/1e6,
      class_anomalie = ifelse(anomalie<0,'Removal','Emission'),
      class_nova = case_when(
        anomalie <= -1 ~ '< -1',
        anomalie < 0 ~ '< 0',
        anomalie < 1 ~ '< 1',
        anomalie < 5 ~ '< 5',
        anomalie >= 5 ~ '>= 5'
      )
    ) |>
    filter(class_anomalie=='Removal'|class_anomalie=='Emission')

  write.csv(a,paste0('data/anomalias_',anos[i],'_.csv'))
}



##### Figure 3
# the precipitation pre-processing are insid precipitation folder

cores_biome <- c("#00A087FF", "#4DBBD5FF", "#E64B35FF", "#3C5488FF",
                            "#F39B7FFF", "#8491B4FF",
                            "#91D1C2FF", "#DC0000FF", "#7E6148FF", "#B09C85FF")
precipitation <- read_rds("data/precipitation_resumed.rds")  %>%
  rename(biome = biomes_n) %>%
  group_by(biome) %>%
  mutate(
    coef_escala = case_when(
      biome == "AF" ~ mean(prec_mean)*1.5,
      biome == "AMZ" ~ mean(prec_mean)*5,
      biome == "CAAT" ~ mean(prec_mean)*2.5,
      biome == "CERR" ~ mean(prec_mean)*1.5,
      biome == "PMP" ~ mean(prec_mean)*10,
      biome == "PNT" ~ mean(prec_mean)*10
    )
  )

precipitation %>%
  ggplot(aes(x=year, y=prec_mean)) +
  geom_point() + geom_line() +
  facet_wrap(.~biome, scales = "free")

tab_cer <- expand.grid(year=2015:2022,
                       biome = "CERR",
                       sector_name = "fossil_fuel_operations",
                       emission = 0
) %>%
  tibble()

tab_pmp <- expand.grid(year=2015:2022,
                       biome = "PMP",
                       sector_name = "mineral_extraction",
                       emission = 0
) %>%
  tibble()

tab_pnt<- expand.grid(year=2015:2022,
                      biome = "PNT",
                      sector_name = c("power","fossil_fuel_operations"),
                      emission = 0
) %>%
  tibble()

tab_aux <- emissions_sources %>%
  filter(
    #flag_indigenous,
    #flag_conservation,
    year < 2023,
    gas == "co2e_100yr",
    !source_name %in% nomes_uf,
    !sub_sector %in% c("forest-land-clearing",
                       "forest-land-degradation",
                       "shrubgrass-fires",
                       "forest-land-fires",
                       "wetland-fires",
                       "removals")
  ) %>%
  filter(biome != "Other") %>%
  group_by(year, biome, sector_name) %>%
  summarise(
    emission = sum(emissions_quantity, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  bind_rows(tab_cer,tab_pmp,tab_pnt) %>%
  mutate(
    sector_name = sector_name %>% as_factor() %>% fct_relevel("forestry_and_land_use","agriculture","fossil_fuel_operations",
                                                              "manufacturing", "mineral_extraction" , "power",
                                                              "transportation","waste")
  ) %>%  left_join(
    precipitation,
    by = c("biome", "year")
  )


meus_biomas <- tab_aux %>% pull(biome) %>% unique()
paste0(meus_biomas,
       c(" 2000 500",
         " 4000 1000",
         " 1000 250",
         " 1500 500",
         " 2000 500",
         " 1500 500"
       )) -> meus_biomas

purrr::map(meus_biomas, ~{
  mpar <- str_split(.x," ",simplify = TRUE)
  yp_max <- as.numeric(mpar[1,2])
  yp_i <- as.numeric(mpar[1,3])
  coef <- tab_aux %>%
    filter(biome == mpar[1,1] ) %>%
    pull(coef_escala) %>%
    mean()

  tab_aux  %>%
    filter(biome == mpar[1,1]) %>%
    mutate(emission = emission/1e9) %>%
    ggplot(aes(x=year,y=emission)) +
    geom_col(aes(fill=sector_name),color="black") +
    scale_fill_manual(values = cores_biome) +
    geom_line(aes(x=year, y=prec_mean/coef),color="blue") +
    geom_point(aes(x=year, y=prec_mean/coef),color="blue",
               size = 2) +
    scale_y_continuous(
      name= "",
      sec.axis = sec_axis(name="Precipitation (mm)",
                          trans = ~.*coef,
                          breaks = seq(0, yp_max, yp_i) )
    ) +
    labs(
      title = mpar[1,1],
      x = "",
      fill = "Sector")+
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(size = rel(1.25)),
      axis.title.x = element_text(size = rel(1.75)),
      axis.text.y = element_text(size = rel(1.7)),
      axis.title.y.left =  element_text(size = rel(1.75),vjust = +1),
      axis.title.y.right = element_text(size = rel(1.75),vjust = +2),
      legend.text = element_text(size = rel(1.3)),
      # legend.title = element_text(size = rel(1.3))
    )
  ggsave(paste0('img/figure_4',mpar[1,1],'.png'),
         units="in", width=8, height=6,
         dpi=300)

})

#### Figure 4
## the preprocessing of sif are inside the SIF folder
## the data processed here are gonna be used in arcmap

source("SIF/r/function.R")
sif_file <- list.files('SIF/data/',full.names = T)
sif_df <- read_rds(sif_file[1]) |> filter(year>2014 & year <2023)

sifdf_filter <- sif_df |>
  dplyr::filter(dist_sif<0.25) |>
  dplyr::mutate(
    lon = lon_grid,
    lat = lat_grid,
  ) |>
  dplyr::select(-c(lon_grid,lat_grid)) |>
  dplyr::group_by(lon,lat,year,month) |>
  dplyr::summarise(
    sif_mean= mean(sif_757,na.rm=TRUE),
    sif_sd = sd(sif_757,na.rm=TRUE),
    sza = mean(sza),
    nobs = dplyr::n(),
    sif_ep = sif_sd/sqrt(nobs),
    cv = 100*sif_sd/sif_mean
  ) |>
  dplyr::mutate(
    date = lubridate::make_date(year,month,'15')
  )

sif_nest <- sifdf_filter |>
  tibble::as_tibble() |>
  dplyr::mutate(year =lubridate::year(date),
                quarter = lubridate::quarter(date),
                quarter_year = lubridate::make_date(year, quarter, 1)) |>
  dplyr::group_by(lon, lat,date) |>
  dplyr::summarise(sif = mean(sif_mean, na.rm=TRUE)) |>
  dplyr::mutate(
    id_time = date
  ) |>
  dplyr::group_by(lon,lat) |>
  tidyr::nest()

### calculating for each pixel
sif_nest_ <- sif_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )

sif_nest_new <- sif_nest_ |>
  dplyr::filter(n_obs > 12) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_sif <- sif_nest_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_sif <- sif_nest_new |> dplyr::pull(beta_line) |> quantile(.25)

write.csv(sif_nest_new |>
            dplyr::mutate(
              sif = dplyr::case_when(
                beta_line > q3_sif ~ 'Enhancement',
                beta_line < q1_sif ~'Decreasing',
                .default = 'Non Significant'
              )
            ) |>
            dplyr::filter(sif!='Non Significant'),
          "SIF/data/trend.csv")


states |> #example
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=sif_nest_new |>
                        dplyr::mutate(
                          sif = dplyr::case_when(
                            beta_line > q3_sif ~ 'Enhancement',
                            beta_line < q1_sif ~'Decreasing',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(sif!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,col=sif),
  )+
  ggplot2::scale_color_viridis_d()+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (Wm'^-2~'sr'^-1~mu~'m'^-1~')'))


#### Figure 6
emissions_sources |>
  filter(year>2014 & year < 2023) |>
  filter(gas == "co2e_100yr",
         !source_name %in% nomes_uf,
         sub_sector %in% c("forest-land-clearing",
                           "forest-land-degradation",
                           "shrubgrass-fires",
                           "forest-land-fires",
                           "wetland-fires",
                           "removals")|
           sector_name=='forestry_and_land_use'
  ) |>
  group_by(sub_sector,year) %>%
  summarise(
    emission = sum(emissions_quantity, na.rm=TRUE)
  ) %>%
  ggplot(aes(fill=sub_sector,y=emission/1e9,group=year,
             x=forcats::as_factor(year)))+
  geom_col(position = 'dodge')+
  scale_fill_viridis_d()+
  labs(x='',
       y=expression('Emission (G ton )'),
       fill='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1),
        legend.position = 'bottom')

ggsave('img/figure_6a.png',
       units="in", width=8, height=6,
       dpi=300)

emissions_sources |>
  filter(year > 2014 & year < 2023,
         gas == "co2e_100yr",
         !source_name %in% nomes_uf,
         !sub_sector %in% c("forest-land-clearing")
  ) |>
  group_by(year) |>
  summarise(
    ghg_balance = sum(emissions_quantity, na.rm=TRUE)
  ) |>
  ggplot(aes(x=year,y=ghg_balance/1e9))+
  geom_col(position = 'dodge')+
  labs(x='',y='Emission (G ton)')+
  theme_bw()

ggsave('img/figure_6b.png',
       units="in", width=8, height=6,
       dpi=300)



### Extendend Figure

my_corr <- function(df,valor="coeficiente") {
  x <- df %>% pull(emission)
  y <- df %>% pull(prec_mean)
  correlation <- cor.test(x,y,method ="pearson")
  if(valor == "coeficiente") return(correlation$estimate)
  if(valor == "valor.p") return(correlation$p.value)
}

tab_aux %>%
  group_by(year,biome) %>%
  summarise(
    emission = sum(emission),
    prec_mean = mean(prec_mean)
  ) %>%
  group_by(biome) %>%
  nest() %>%
  mutate(
    correlation = map(data,my_corr,valor="coeficiente"),
    p.value = map(data,my_corr,valor="valor.p")
  ) %>%
  select(-data) %>%
  unnest()

tab_aux %>%
  group_by(year,biome) %>%
  summarise(
    emission = sum(emission),
    prec_mean = mean(prec_mean)
  ) %>%
  ungroup() %>%
  ggplot(aes(x=prec_mean, y=emission/1e9, color=biome)) +
  geom_point() +
  facet_wrap(~biome, scale="free") +
  geom_smooth(method = "lm", se=FALSE) +
  ggpubr::stat_cor()+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text.x = element_text(size = rel(1.25)),
                 axis.title.x = element_text(size = rel(1.5)),
                 axis.text.y = element_text(size = rel(1.25)),
                 axis.title.y = element_text(size = rel(1.5)),
                 legend.text = element_text(size = rel(1.3)),
                 legend.title = element_text(size = rel(1.3))
                 )+
  ggplot2::labs(
    x=expression('Precipitation (mm '~year^-1~')'),
    y='Emission (G ton)'
  )

ggsave('img/extendend_figure_1.png',
       units="in", width=12, height=6,
       dpi=300)
