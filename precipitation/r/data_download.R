library(tidyverse)
library(geobr)
library(nasapower)
source('r/my-function.R')


df <- read.csv('precipitation/brazil_coord.csv') |> select(lon,lat)


# download dados precipitação

for (i in 1:nrow(df)){
  repeat{
    dw <- try(
      power_data_download(df[i,1],df[i,2],
                          startdate='2014-01-01',
                          enddate = '2015-01-01')
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}


### criação base de dados

files_names <- list.files('precipitation/data-raw/',full.names = T)
for (i in 1:length(files_names)){
  if(i ==1){
    df <- read.csv(files_names[i])
  }else{
    df_a <- read.csv(files_names[i])
    df <- rbind(df,df_a)
  }
}


readr::write_rds(df,'precipitation/data/nasa_power_data.rds')
